library(cowplot)
library(dplyr)
library(ggh4x)

# Difference from holc D
# function to extract fixed effects from lmer
ext_fixef_D <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))
  
  fe <- fixef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm <- names(fe)

  ints_n <- grep("^holc_grade_D[^:]+$", 
    nm, perl = TRUE, value = TRUE
  )
  slps_n <- grep(
    "^(?:holc_grade_D[^:]+:scale\\(year\\)|scale\\(year\\):holc_grade_D[^:]+)$",
    nm, perl = TRUE, value = TRUE
  )
  
  # extract the level letter (A/B/C)
  lev <- function(term) sub(".*holc_grade_D([^:]+).*", "\\1", term)

  ints <- tibble(
    type = "intercept",
    holc_grade = lev(ints_n),
    estimate = fe[ints_n],
    std.error = se[ints_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  )

  slps <- tibble(
    type = "slope_per_SDyear",
    holc_grade = lev(slps_n),
    estimate = fe[slps_n],
    std.error = se[slps_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  ) |>
    dplyr::mutate( # convert to per-year on the same log10 scale
      estimate_per_year = estimate / sdy,
      conf.low_per_year = conf.low / sdy,
      conf.high_per_year= conf.high / sdy
    )

  dplyr::bind_rows(ints, slps)
}

# function to extract fixed effects from lm objects (D baseline; matches lmer extractor)
ext_fixef_D_lm <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))
  fe  <- coef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm  <- names(fe)
  ints_n <- grep("^holc_grade_D[^:]+$", nm, perl = TRUE, value = TRUE)
  slps_n <- grep("^(?:holc_grade_D[^:]+:scale\\(year\\)|scale\\(year\\):holc_grade_D[^:]+)$",
                 nm, perl = TRUE, value = TRUE)
  lev <- function(term) sub(".*holc_grade_D([^:]+).*", "\\1", term)

  ints <- tibble(
    type = "intercept", holc_grade = lev(ints_n),
    estimate = fe[ints_n], std.error = se[ints_n]
  ) |>
    mutate(conf.low = estimate - z*std.error,
           conf.high = estimate + z*std.error)

  slps <- tibble(
    type = "slope_per_SDyear", holc_grade = lev(slps_n),
    estimate = fe[slps_n], std.error = se[slps_n]
  ) |>
    mutate(conf.low = estimate - z*std.error,
           conf.high = estimate + z*std.error,
           estimate_per_year = estimate / sdy,
           conf.low_per_year = conf.low / sdy,
           conf.high_per_year= conf.high / sdy)

  bind_rows(ints, slps)
}

# load year data (sum per holc)
holc <- fread('original_paper/Data/Biodiv_Greeness_Social/soc_dem_max_2022_03_12 17_31_11.csv')
holc_area = holc[, list(sum_area_holc_km2 = sum(area_holc_km2)), holc_grade]
holc_area_dt = data.table(holc_area)  

t = fread('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') #tt = fread('Data/from_script_04/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(t) <- c('year','holc_grade', 'Sum')

t = t[holc_grade != 'E'] #d = data.table(temporal_trend)
tt = t[, .(n_obs = sum(Sum)), by = list(year, holc_grade)]
tt = tt[order(holc_grade,year)]

# add holc grade
tt = merge(tt,holc_area_dt, all.x = TRUE)
tt[, sampling_density := n_obs/sum_area_holc_km2]
options(contrasts = c("contr.treatment", "contr.poly"))
tt[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]
tt = tt[year >= 2000 & year <= 2020]
tt10 = tt[year >= 2010]

# Ensure treatment coding with D as baseline (IMPORTANT)
options(contrasts = c("contr.treatment", "contr.poly"))
dd[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]

# MODELS 2000-2020
# 0) model per holc grade
sum_m  <- lm(log10(sampling_density) ~ holc_grade_D*scale(year), tt)

# 1) model set per polygon
maD = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
            (1|state) + (1|city_state) + (1|id2), 
            dd)

mbD = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
            (1|state/city_state/holc_grade/id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
mas1D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

mbs1D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
            (scale(year)|state/city_state/holc_grade/id2),
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

msab1D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            dd,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 

models_T0020_D <- rlang::set_names(
  list(maD, mbD, mas1D, mbs1D, msab1D),
  c("maD","mbD","mas1D","mbs1D","msab1D")
)

# labels
models_T0020_labels_D <- c(
  maD      = "(1 | state) + (1 | city) + (1 | polygon_id)",
  mbD      = "(1 | state/city/holc_grade/polygon_id)",
  mas1D    = "(1 | state) + (year | city) + (1 | polygon_id)",
  mbs1D    = "(year | state/city/holc_grade/polygon_id)",
  msab1D   = "(year| state/city/holc_grade) + (1 | polygon_id)"
)

# sort models
models_T0020_order_D <- c(
  "(1 | state) + (1 | city) + (1 | polygon_id)",
  "(1 | state/city/holc_grade/polygon_id)",
  "(1 | state) + (year | city) + (1 | polygon_id)",
  "(year | state/city/holc_grade/polygon_id)",
  "(year| state/city/holc_grade) + (1 | polygon_id)"
)

# 2) Extract fixed effects on the modeling scale (log10), fast Wald CIs
# lm
lm_df <- ext_fixef_D_lm(sum_m) |>
  mutate(
    type2 = fcase(type == "intercept", "Intercept (log10)",
                  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
                  default = as.character(type)),
    holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
  )

lm_lab <- sprintf("linear model on density per year (n = %s)", nobs(sum_m)) # Legend label text (no title, single key)

# lmer
coef_df_0020_D <- purrr::imap_dfr(models_T0020_D, ~ ext_fixef_D(.x) |> dplyr::mutate(model=.y))

coef_df_0020_D <- coef_df_0020_D %>%
  mutate(model_label = factor(models_T0020_labels_D[model], levels = models_T0020_order_D)) %>% data.table()

coef_df_0020_D[, type2 := fcase(
  type == "intercept",      "Intercept (log10)",
  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
  default = as.character(type)
)]

coef_df_0020_D[, holc_grade_dif := paste0(holc_grade, ' vs D')]

tr1D = 
ggplot(coef_df_0020_D, aes(
        x= estimate, y = holc_grade_dif, 
        xmin=conf.low, xmax=conf.high, 
        color=fct_rev(model_label))) +
  geom_pointrange(position = position_dodge2(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  facet_grid(~ type2, scales="free_x") +
  labs(y="Contrast (relative to HOLC grade D)", x=NULL, subtitle ='2000 - 2020') +
  scale_color_discrete(name = "Random-effect specification", 
                        limits = models_T0020_order_D) +
  # add lm model
  ggnewscale::new_scale_color() +  # start a NEW color scale (separate legend)
  geom_pointrange(
    data = lm_df,
    aes(x = estimate, y = holc_grade_dif,
        xmin = conf.low, xmax = conf.high,
        color = lm_lab),
    position = position_nudge(y = 0.36),  # small vertical offset; remove if undesired
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  scale_color_manual(
    name = NULL,                               # no legend title
    values = setNames("black", lm_lab),
    breaks = lm_lab,
    guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
  ) +                     
  theme_light() +
  theme(
    strip.background = element_blank(), # remove grey panel background
    strip.text = element_text(color = "black", margin = margin(b=10)), # make labels black
    plot.subtitle = element_text(size = 10, colour = "grey40",
                                 margin = margin(b = -16)),
    plot.margin = margin(t = 3, r = 3, b = gap_pt, l = 3)                             
  )

# MODELS 2010-2020
# 0) lm model
  sum_m_10  <- lm(log10(sampling_density) ~ holc_grade_D*scale(year), tt10)

# 1) model set
ma_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
            (1|state) + (1|city_state) + (1|id2), 
            dd10)

mb_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
            (1|state/city_state/holc_grade/id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )

mas1_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

mbs1_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
            (scale(year)|state/city_state/holc_grade/id2),
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )

msab1_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            dd10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 

models_T1020_D <- rlang::set_names(
  list(ma_D, mb_D, mas1_D, mbs1_D, msab1_D),
  c("ma_D","mb_D","mas1_D","mbs1_D","msab1_D")
)

# labels
models_T1020_labels_D <- c(
  ma_D      = "(1 | state) + (1 | city) + (1 | polygon_id)",
  mb_D      = "(1 | state/city/holc_grade/polygon_id)",
  mas1_D    = "(1 | state) + (year | city) + (1 | polygon_id)",
  mbs1_D    = "(year | state/city/holc_grade/polygon_id)",
  msab1_D   = "(year| state/city/holc_grade) + (1 | polygon_id)"
)

# sort models
models_T1020_order_D <- c(
  "(1 | state) + (1 | city) + (1 | polygon_id)",
  "(1 | state/city/holc_grade/polygon_id)",
  "(1 | state) + (year | city) + (1 | polygon_id)",
  "(year | state/city/holc_grade/polygon_id)",
  "(year| state/city/holc_grade) + (1 | polygon_id)"
)

# 2) Extract fixed effects on the modeling scale (log10), fast Wald CIs
# lm
lm_df_10 <- ext_fixef_D_lm(sum_m_10) |>
  mutate(
    type2 = fcase(type == "intercept", "Intercept (log10)",
                  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
                  default = as.character(type)),
    holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
  )

lm_lab_10 <- sprintf("linear model on sum per year (n = %s)", nobs(sum_m_10)) # Legend label text (no title, single key) 

# lmer
coef_df_1020_D <- purrr::imap_dfr(models_T1020_D, ~ ext_fixef_D(.x) |> dplyr::mutate(model=.y))

coef_df_1020_D <- coef_df_1020_D %>%
  mutate(model_label = factor(models_T1020_labels_D[model], levels = models_T1020_order_D)) %>% data.table()

coef_df_1020_D[, type2 := fcase(
  type == "intercept",      "Intercept (log10)",
  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
  default = as.character(type)
)]

coef_df_1020_D[, holc_grade_dif := paste0(holc_grade, ' vs D')]

tr2D =     
ggplot(coef_df_1020_D, aes(
        x= estimate, y = holc_grade_dif, 
        xmin=conf.low, xmax=conf.high, 
        color=fct_rev(model_label))) +
  geom_pointrange(position = position_dodge2(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  facet_grid(~ type2, scales="free_x") +
  labs(y="Contrast (relative to HOLC grade D)", x="Estimate", subtitle ='2010 - 2020') +
  scale_color_discrete(name = "Random-effect specification", 
                        limits = models_T1020_order_D) +
  # lm 
  ggnewscale::new_scale_color() +
  geom_pointrange(
    data = lm_df_10,
    aes(x = estimate, y = holc_grade_dif,
        xmin = conf.low, xmax = conf.high,
        color = lm_lab_10),
    position = position_nudge(y = 0.36),
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  scale_color_manual(
    name = NULL,
    values = setNames("black", lm_lab_10),
    breaks = lm_lab,
    guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
  ) +                       
  theme_light() +
  theme(
    strip.background = element_blank(),       # remove grey panel background
    strip.text = element_text(color = "black", margin = margin(b=10)),
    plot.subtitle = element_text(size = 10, colour = "grey40",
                                 margin = margin(b = 2)),
    plot.margin = margin(t = gap_pt, r = 3, b = 5.5, l = 3),
    strip.text.x = element_blank()                             
  )

# COMBINE -  x-axis correspond accros facets  
# facet-wise ranges across BOTH datasets
rng <- bind_rows(coef_df_0020, coef_df_1020) |>
  group_by(type2) |>
  summarise(
    xmin = min(pmin(conf.low, conf.high), na.rm = TRUE),
    xmax = max(pmax(conf.low, conf.high), na.rm = TRUE),
    .groups = "drop"
  )

add_02_break  <- function(lims) sort(unique(c(scales::pretty_breaks()(lims), 0.2)))  

facet_scales <- lapply(seq_len(nrow(rng)), function(i) {
  tl <- rng$type2[i]
  lo <- rng$xmin[i]
  hi <- rng$xmax[i]

  if (grepl("^Slope", tl)) {
    rlang::new_formula(
      lhs = bquote(type2 == .(tl)),
      rhs = scale_x_continuous(
        limits = c(-0.1, 0.2), 
        breaks = add_02_break, 
        oob = scales::oob_keep,              # keep data for stats, don't drop rows 
        expand = expansion(mult = c(0, 0)) 
      ) 
    ) 
  } else if (grepl("^Intercept", tl)) { 
    rlang::new_formula( 
      lhs = bquote(type2 == .(tl)), 
      rhs = scale_x_continuous( 
        limits = c(0, .45), 
        #breaks = add_15_break, 
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

tr1D_adj <- tr1D + ggh4x::facetted_pos_scales(x = facet_scales) 
tr2D_adj <- tr2D + ggh4x::facetted_pos_scales(x = facet_scales) 
  
design <- " 
AB 
C# 
" 

# one common y-axis title
# a) drop y-axis titles on both panels
tr1D_adj <- tr1D_adj + labs(y = NULL)
tr2D_adj <- tr2D_adj + labs(y = NULL)

# b) combine 
p <- (tr1D_adj + guide_area() + tr2D_adj) +
  plot_layout(design = design, guides = "collect", widths = c(1, 0.46)) &
  theme(plot.margin = margin(l = 9, r = 3, t = 3, b = 3))  # a bit of left room

# c) get font size
axis_title_size <- ggplot2::theme_get()$text$size

# d) plot and save
p_common_y <- ggdraw(p) +
  draw_label("Contrast (relative to HOLC grade D)",
             x = 0.01, y = 0.5, angle = 90, vjust = 0.5,
             size = axis_title_size)
p_common_y

ggsave("Output/yr-trend_model-compar_contrasts_v2.png",plot = p_common_y, width = 25, height = 15, units = "cm")


# delete
#(tr1D_adj + guide_area() + tr2D_adj) + 
  plot_layout(design = design, guides = "collect", widths = c(1, 0.42))   

#(tr1D_adj / tr2D_adj) + plot_layout(guides = "collect") & theme(legend.position = "right")


# TEST START - add lm model values


# ---- 2) fit the LM (your code) and prep overlay data
dd[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]
sum_m  <- lm(log10(sampling_density) ~ holc_grade_D*scale(year), dd)

lm_df <- ext_fixef_D_lm(sum_m) |>
  mutate(
    type2 = fcase(type == "intercept", "Intercept (log10)",
                  type == "slope_per_SDyear", "Slope (per 1 SD of year)",
                  default = as.character(type)),
    holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
  )

# (optional) if you also want to overlay on the 2010–2020 panel (tr2D),
# make sure its y labels match with a space, or change this one line above to:
#   holc_grade_dif = paste0(holc_grade, "vs D")
# depending on which version you keep in the tr2D data.

# Legend label text (no title, single key)
lm_lab <- sprintf("linear model on density per year and holc (n = %s)", nobs(sum_m))

# ---- 3) add the overlay to tr1D (2000–2020)
tr1D_adj <- tr1D_adj +
  ggnewscale::new_scale_color() +  # start a NEW color scale (separate legend)
  geom_pointrange(
    data = lm_df,
    aes(x = estimate, y = holc_grade_dif,
        xmin = conf.low, xmax = conf.high,
        color = lm_lab),
    position = position_nudge(y = 0.2),  # small vertical offset; remove if undesired
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  scale_color_manual(
    name = NULL,                               # no legend title
    values = setNames("black", lm_lab),
    breaks = lm_lab,
    guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
  )

# ---- 4) (optional) also overlay on the 2010–2020 panel
# If your tr2D y labels are '... vs D' (with space), reuse lm_df as-is.
# If not, make a copy with the matching y text (no space):
# lm_df10 <- lm_df |> mutate(holc_grade_dif = gsub(" vs D", "vs D", holc_grade_dif))
lm_df10 <- lm_df  # use this if labels already match

tr2D_adj <- tr2D_adj +
  ggnewscale::new_scale_color() +
  geom_pointrange(
    data = lm_df10,
    aes(x = estimate, y = holc_grade_dif,
        xmin = conf.low, xmax = conf.high,
        color = lm_lab),
    position = position_nudge(y = 0),
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  scale_color_manual(
    name = NULL,
    values = setNames("black", lm_lab),
    breaks = lm_lab,
    guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
  )

# ---- 5) combine as before (guides='collect' keeps both legends, separately boxed)
(tr1D_adj / tr2D_adj) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# TEST END