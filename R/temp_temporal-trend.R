# packages
require(arm)
require(data.table)
require(effects)
require(ggplot2)
require(multcomp)
require(patchwork)

# color palette for plotting
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)

# function for quick plotting
plot_effects_holc <- function(
          model, data, 
          year = "year", grade = "holc_grade",
          palette = NULL, n = 100, ylab = "Sampling density",
          title_model = TRUE, title_text = NULL, ver = 'v1',
          title_size = 9, title_colour = "grey40",
          save_png = TRUE, outdir = ".", filename = NULL,
          width = 9, height = 4.8, dpi = 300, timestamp = FALSE
  ) {

     safe_slug <- function(x, max = 120) {
        x <- tolower(x)
        x <- gsub("\\s+", " ", x)
        x <- gsub("[^a-z0-9]+", "-", x)
        x <- gsub("(^-+|-+$)", "", x)
        substr(x, 1, max)
    }

     xr <- seq(min(data[[year]], na.rm = TRUE),
          max(data[[year]], na.rm = TRUE), length.out = n)

     eff <- suppressWarnings(effects::Effect(c(year, grade), model,
                         xlevels = setNames(list(xr), year)))

     pp <- as.data.frame(eff)
     pp[[grade]] <- factor(pp[[grade]])
     pp$fit_o   <- 10^pp$fit
     pp$lower_o <- 10^pp$lower
     pp$upper_o <- 10^pp$upper    

     lev <- levels(pp[[grade]])
     if (is.null(palette)) {
     palette <- setNames(scales::hue_pal()(length(lev)), lev)
     } else if (is.null(names(palette)) && length(palette) == length(lev)) {
     names(palette) <- lev
     }

     p1 = 
     ggplot(pp, aes(year, fit_o, color = holc_grade, fill = holc_grade)) +
     geom_ribbon(aes(ymin = lower_o, ymax = upper_o), alpha = .15, colour = NA) +
     geom_line() +
     scale_color_manual(values = holc_pal, name = 'HOLZ grade')+
     scale_fill_manual(values = holc_pal, name = 'HOLZ grade')+
     labs(x = "Year", y = ylab, subtitle = 'original-scale') +
     theme_light()+
     theme( plot.subtitle = element_text(size = 10, colour = "grey40"))

     p2 = 
     ggplot(pp, aes(year, fit_o, color = holc_grade, fill = holc_grade)) +
     geom_ribbon(aes(ymin = lower_o, ymax = upper_o), alpha = .15, colour = NA) +
     geom_line() +
     scale_color_manual(values = holc_pal, name = 'HOLZ grade')+
     scale_fill_manual(values = holc_pal, name = 'HOLZ grade')+
     labs(x = "Year", y = ylab, subtitle = 'log-scale') +
     scale_y_continuous(trans='log10')+
     theme_light()+
     theme( plot.subtitle = element_text(size = 10, colour = "grey40"))

     p2_ <- p2 + theme(axis.title.y = element_blank())

     title_str <- if (!is.null(title_text)) title_text else
        if (isTRUE(title_model))
            paste0("Model: ",
                gsub("\\s+", " ",
                        paste(trimws(deparse(formula(model), width.cutoff = 500)),
                            collapse = " ")))
        else NULL

     ann_theme <- ggplot2::theme(plot.title = element_text(size = title_size, colour = title_colour, hjust = 0))


     g <- (p1 + p2_) + patchwork::plot_layout(guides = "collect")
      if (!is.null(title_str)) {
        g <- g + patchwork::plot_annotation(title = title_str, theme = ann_theme)
      }
     g <- g & ggplot2::theme(legend.position = "right")

     # Optional save (timestamp at END)
     if (isTRUE(save_png)) {
      if (is.null(filename)) {
        stamp <- if (timestamp) paste0("__", format(Sys.time(), "%Y%m%d-%H%M")) else ""
        fname <- paste0(safe_slug(title_str), stamp, ver, ".png")
       } else {
        fname <- filename
       }
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
      ggplot2::ggsave(file.path(outdir, fname), plot = g,
                    width = width, height = height, dpi = dpi, bg = "white")
      message("Saved: ", file.path(outdir, fname))
     }

     g
}


# load year-polygon data
d = fread('Data/MaPe/Mape_R1_biodiv_sum_bird_obs_by_holc_id_year.csv')
d = d[!city_state%in%'city_state'] # remove duplicated headers

# make columns numeric
d[, year:=as.numeric(year)]
d[, lat:=as.numeric(lat)]
d[, lon:=as.numeric(lon)]
d[, area_holc_km2:=as.numeric(area_holc_km2)]
d[, sum_bird_obs:=as.numeric(sum_bird_obs)]

# add sampling density
d[, sampling_density:=sum_bird_obs/area_holc_km2]

# remove polygons with unknown area
d = d[!is.na(area_holc_km2)]
d = d[!area_holc_km2%in%0]
    #d[is.na(area_holc_km2), unique(city_state)]  
    #holc_[city_state%in%'Mobile, AL']

# exclude E category
d = d[!holc_grade%in%'E']

# relevel holc_grade for some analysis
d[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]
# limit to 2000-2020
dd = d[year >= 2000 & year <= 2020]

# limit to 2010-2020
dd10 = dd[year >= 2010 & year <= 2020]

# check distributions
ggplot(d, aes(sum_bird_obs))+geom_density() + scale_x_continuous(trans ='log10')
ggplot(d, aes(sampling_density))+geom_density() + scale_x_continuous(trans ='log10')

# MODELS 2000-2020

# model set
ma = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (1|id2), 
            dd)

mb = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state/city_state/holc_grade/id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
mas1 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

mbs1 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade/id2),
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

msab1 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 
            
# plot trends    
plot_effects_holc(ma, dd, palette = holc_pal, outdir='Output')        
plot_effects_holc(mb, dd, palette = holc_pal, outdir = 'Output')
plot_effects_holc(mas1, dd, palette = holc_pal, outdir = 'Output')
plot_effects_holc(mbs1, dd, palette = holc_pal, outdir = 'Output')
plot_effects_holc(msab1, dd, palette = holc_pal, outdir = 'Output')

# plot effect sizes for 2000-2020
# 1) reparametrize models to estimate separte intercepts and slopes for each holc grade 
mai = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
           (1|state) + (1|city_state) + (1|id2), 
            dd)

mbi = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
            (1|state/city_state/holc_grade/id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
mas1i = lmer(log10(sampling_density) ~
            0 + holc_grade + holc_grade:scale(year) + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

mbs1i = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
            (scale(year)|state/city_state/holc_grade/id2),
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

msab1i = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 

# 2) model set and labels
models_T0020<- list(
  mai       = mai,
  mbi       = mbi,
  mas1i       = mas1i,
  mbs1i      = mbs1i,
  msab1i      = msab1i
)

models_T0020_labels <- c(
  mai      = "(1 | state) + (1 | city) + (1 | polygon_id)",
  mbi      = "(1 | state/city/holc_grade/polygon_id)",
  mas1i    = "(1 | state) + (year | city) + (1 | polygon_id)",
  mbs1i    = "(year | state/city/holc_grade/polygon_id)",
  msab1i   = "(year| state/city/holc_grade) + (1 | polygon_id)"
)
# sort models
models_T0020_order <- c(
  "(1 | state) + (1 | city) + (1 | polygon_id)",
  "(1 | state/city/holc_grade/polygon_id)",
  "(1 | state) + (year | city) + (1 | polygon_id)",
  "(year | state/city/holc_grade/polygon_id)",
  "(year| state/city/holc_grade) + (1 | polygon_id)"
)

# 3) Extract fixed effects on the modeling scale (log10), fast Wald CIs

# function to extract fixed effects
ext_fixef <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))
  
  fe <- fixef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm <- names(fe); pick <- function(p) grep(p, nm, value = TRUE)

  ints_n <- pick("^holc_grade[^:]+$")
  slps_n <- pick("^holc_grade[^:]+:scale\\(year\\)$")

  ints <- tibble(
    type = "intercept",
    holc_grade = sub("^holc_grade","", ints_n),
    estimate = fe[ints_n],
    std.error = se[ints_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  )

  slps <- tibble(
    type = "slope_per_SDyear",
    holc_grade = sub(":.*","", sub("^holc_grade","", slps_n)),
    estimate = fe[slps_n],
    std.error = se[slps_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  ) |>
    mutate( # convert to per-year on the same log10 scale
      estimate_per_year = estimate / sdy,
      conf.low_per_year = conf.low / sdy,
      conf.high_per_year= conf.high / sdy
    )

  bind_rows(ints, slps)
}

# apply to the model set
coef_df_0020 <- purrr::imap_dfr(models_T0020, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

coef_df_0020 <- coef_df_0020 %>%
  mutate(model_label = factor(models_T0020_labels[model], levels = models_T0020_order)) %>% data.table()

coef_df_0020[, type2 := fcase(
  type == "intercept",        "Intercept (log10)",
  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
  default = as.character(type)
)]

tr1 = 
ggplot(coef_df_0020, aes(
        x= estimate, y = holc_grade, 
        xmin=conf.low, xmax=conf.high, 
        color=fct_rev(model_label))) +
  geom_pointrange(position = position_dodge2(width = 0.6)) +
  facet_grid(~ type2, scales="free_x") +
  labs(y="HOLC grade", x="Estimate", subtitle ='2000 - 2020') +
  scale_color_discrete(name = "Random-effect specification", 
                        limits = models_T0020_order) +
  theme_light() +
  theme(
    strip.background = element_blank(),       # remove grey panel background
    strip.text = element_text(color = "black"), # make labels black
    plot.subtitle = element_text(size = 10, colour = "grey40")
  )

# MODELS 2010-2020
ma_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (1|id2), 
            dd10)

mb_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state/city_state/holc_grade/id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )

mas1_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

mbs1_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade/id2),
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

msab1_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 
# plot
plot_effects_holc(ma_, dd10, palette = holc_pal, outdir='Output', ver ='v1_10-20')
plot_effects_holc(mb_, dd10, palette = holc_pal, outdir = 'Output', ver ='v1_10-20')
plot_effects_holc(mas1_, dd10, palette = holc_pal, outdir = 'Output', ver ='v1_10-20')
plot_effects_holc(mbs1_, dd10, palette = holc_pal, outdir = 'Output', ver ='v1_10-20')
plot_effects_holc(msab1_, dd10, palette = holc_pal, outdir = 'Output', ver ='v1_10-20')

# plot effect sizes for 2010-2020
# 1) reparametrize models to estimate separte intercepts and slopes for each holc grade 
ma_i = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
           (1|state) + (1|city_state) + (1|id2), 
            dd10)

mb_i = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
            (1|state/city_state/holc_grade/id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
mas1_i = lmer(log10(sampling_density) ~
            0 + holc_grade + holc_grade:scale(year) + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

mbs1_i = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
            (scale(year)|state/city_state/holc_grade/id2),
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

msab1_i = lmer(log10(sampling_density) ~ 
            0 + holc_grade + holc_grade:scale(year) + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 

# 2) model set and labels
models_T1020<- list(
  ma_i       = ma_i,
  mb_i       = mb_i,
  mas1_i       = mas1_i,
  mbs1_i      = mbs1_i,
  msab1_i      = msab1_i
)

models_T1020_labels <- c(
  ma_i      = "(1 | state) + (1 | city) + (1 | polygon_id)",
  mb_i      = "(1 | state/city/holc_grade/polygon_id)",
  mas1_i    = "(1 | state) + (year | city) + (1 | polygon_id)",
  mbs1_i    = "(year | state/city/holc_grade/polygon_id)",
  msab1_i   = "(year| state/city/holc_grade) + (1 | polygon_id)"
)
# sort models
models_T1020_order <- c(
  "(1 | state) + (1 | city) + (1 | polygon_id)",
  "(1 | state/city/holc_grade/polygon_id)",
  "(1 | state) + (year | city) + (1 | polygon_id)",
  "(year | state/city/holc_grade/polygon_id)",
  "(year| state/city/holc_grade) + (1 | polygon_id)"
)

# 3) Extract fixed effects on the modeling scale (log10), fast Wald CIs

# apply to the model set
coef_df_1020 <- purrr::imap_dfr(models_T1020, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

coef_df_1020 <- coef_df_1020 %>%
  mutate(model_label = factor(models_T1020_labels[model], levels = models_T1020_order)) %>% data.table()

coef_df_1020[, type2 := fcase(
  type == "intercept",        "Intercept (log10)",
  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
  default = as.character(type)
)]

tr2 =     
ggplot(coef_df_1020, aes(
        x= estimate, y = holc_grade, 
        xmin=conf.low, xmax=conf.high, 
        color=fct_rev(model_label))) +
  geom_pointrange(position = position_dodge2(width = 0.6)) +
  facet_grid(~ type2, scales="free_x") +
  labs(y="HOLC grade", x="Estimate", subtitle ='2010 - 2020') +
  scale_color_discrete(name = "Random-effect specification", 
                        limits = models_T0020_order) +
  theme_light() +
  theme(
    strip.background = element_blank(),       # remove grey panel background
    strip.text = element_text(color = "black"), # make labels black
    plot.subtitle = element_text(size = 10, colour = "grey40")
  )


# COMBINE - x-axis correspond accros facets
library(dplyr)
library(ggh4x)

# facet-wise ranges across BOTH datasets
rng <- bind_rows(coef_df_0020, coef_df_1020) |>
  group_by(type2) |>
  summarise(
    xmin = min(pmin(conf.low, conf.high), na.rm = TRUE),
    xmax = max(pmax(conf.low, conf.high), na.rm = TRUE),
    .groups = "drop"
  )

add_035_break <- function(lims) sort(unique(c(scales::pretty_breaks()(lims), 0.35)))
add_15_break  <- function(lims) sort(unique(c(scales::pretty_breaks()(lims), 1.5)))

facet_scales <- lapply(seq_len(nrow(rng)), function(i) {
  tl <- rng$type2[i]
  lo <- rng$xmin[i]
  hi <- rng$xmax[i]

  if (grepl("^Slope", tl)) {
    rlang::new_formula(
      lhs = bquote(type2 == .(tl)),
      rhs = scale_x_continuous(
        limits = c(lo, 0.35),
        breaks = add_035_break,
        oob = scales::oob_keep,              # keep data for stats, don't drop rows
        expand = expansion(mult = c(0, 0))
      )
    )
  } else if (grepl("^Intercept", tl)) {
    rlang::new_formula(
      lhs = bquote(type2 == .(tl)),
      rhs = scale_x_continuous(
        limits = c(lo, 1.5),
        breaks = add_15_break,
        oob = scales::oob_keep,
        expand = expansion(mult = c(0, 0))
      )
    )
  } else {
    rlang::new_formula(
      lhs = bquote(type2 == .(tl)),
      rhs = scale_x_continuous(
        limits = c(lo, hi),
        oob = scales::oob_keep,
        expand = expansion(mult = c(0, 0))
      )
    )
  }
})

tr1_adj <- tr1 + ggh4x::facetted_pos_scales(x = facet_scales)
tr2_adj <- tr2 + ggh4x::facetted_pos_scales(x = facet_scales)

((tr1_adj + labs(x = NULL)) / tr2_adj) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave('Output/yr-trend_model-compar.png', width= 25, height = 15, units ='cm') 

# adjust subtitle spacing
# how big a gap you want between the two rows (in points)
gap_pt <- 12
t1 <- tr1_adj +
  labs(x = NULL) +
  theme(
    # smaller space under subtitle
    plot.subtitle = element_text(size = 10, colour = "grey40",
                                 margin = margin(b = -16)),
    # thinner strip (less padding inside the facet label)
    #strip.text.x = element_text(margin = margin(t = 1, r = 2, b = 1, l = 2)),
    # add bottom margin to create space to the next plot
    plot.margin = margin(t = 3, r = 3, b = gap_pt, l = 3)
  )

t2 <- tr2_adj +
  theme(
    plot.subtitle = element_text(size = 10, colour = "grey40",
                                 margin = margin(b = 2)),
    #strip.text.x = element_text(margin = margin(t = 1, r = 2, b = 1, l = 2)),
    # add top margin so the gap is symmetric
    plot.margin = margin(t = gap_pt, r = 3, b = 5.5, l = 3),
    strip.text.x = element_blank()
  )
(t1 / t2) + plot_layout(guides = "collect") & theme(legend.position = "right")

ggsave('Output/yr-trend_model-compar_v3.png', width= 25, height = 15, units ='cm') 


# COMBINE = x-axis same in all facets

library(ggh4x)
# get facet-specific ranges 
    rng <- bind_rows(coef_df_0020, coef_df_1020) |> 
      group_by(type2) |> 
      summarise( 
        xmin = min(pmin(conf.low, conf.high), na.rm = TRUE), 
        xmax = max(pmax(conf.low, conf.high), na.rm = TRUE), 
        .groups = "drop" 
      ) 
# build list of facet-specific scales 
facet_scales <- lapply(seq_len(nrow(rng)), function(i) { 
    tl <- rng$type2[i] 
    lo <- rng$xmin[i] 
    hi <- rng$xmax[i] 
    # return formula of the form: type2 == "Intercept" ~ scale_x_continuous(limits = c(...)) 
    rlang::new_formula( 
    lhs = bquote(type2 == .(tl)), 
    rhs = scale_x_continuous(limits = c(lo, hi)) 
    ) 
}) 

# add to tr1 and tr2
tr1_adj <- tr1 + ggh4x::facetted_pos_scales(x = facet_scales)
tr2_adj <- tr2 + ggh4x::facetted_pos_scales(x = facet_scales)
((tr1_adj + labs(x = NULL)) / tr2_adj) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")


((tr1 + labs(x = NULL) + coord_cartesian(xlim = common_xlim)) /
 (tr2 + coord_cartesian(xlim = common_xlim))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

(tr1/tr2) + plot_layout(guides = "collect") & # collect legends to one
theme(legend.position = "right") 


ggsave('Output/yr-trend_model-compar.png', width= 25, height = 15, units ='cm') 



# old
summary(ma_)
summary(glht(ma_))
summary(mb_)  
summary(glht(mb_))
summary(mas1_)  
summary(glht(mas1_))
summary(mbs1_)  
summary(glht(mbs1_))

summary(msab1_)  
summary(glht(msab1_))