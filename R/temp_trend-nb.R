### FOR METHODS
“To accommodate slight heteroskedasticity across HOLC grades and city area, we modelled dispersion as a function of these predictors (dispformula = ~ holc_grade_D + log(area_holc_km2)).”

#' #### A. Contrasts
#+ F_Z1g, fig.width = 25/2.5, fig.height = 15/2.5

#TODO test the function wiith lmer as well
# Extract fixed effects (vs. HOLC D) and year slopes (per 1 SD of year)
# Works for: lmer/glmer/glmer.nb (lme4) AND glmmTMB (nbinom2/1 etc.)
ext_fixef_D <- function(m) {
  # --- 1) grab fixed effects and VCOV depending on class ---
  if (inherits(m, "glmmTMB")) {
    fe <- fixef(m)$cond
    V  <- as.matrix(vcov(m)$cond)
    mf <- model.frame(m)              # model frame (has scale(year))
  } else {
    fe <- lme4::fixef(m)
    V  <- as.matrix(stats::vcov(m))
    mf <- stats::model.frame(m)
  }

  # --- 2) SD of 'year' used inside scale(year) ---
  # Try to read the 'scaled:scale' attribute first; fall back to sd(year)
  sdy <- NA_real_
  if ("scale(year)" %in% names(mf)) {
    sdy <- suppressWarnings(as.numeric(attr(mf[["scale(year)"]], "scaled:scale")))
  }
  if (!is.finite(sdy)) {
    if ("year" %in% names(mf)) {
      sdy <- stats::sd(mf[["year"]], na.rm = TRUE)
    } else {
      stop("Cannot determine SD of year: no 'scale(year)' attribute and 'year' not in model.frame().")
    }
  }

  # --- 3) pick the coefficients we need ---
  nm <- names(fe)

  # intercept (differences vs D) terms: holc_grade_D*
  ints_n <- grep("^holc_grade_D[^:]+$", nm, perl = TRUE, value = TRUE)

  # slope interaction terms with scale(year)
  slps_n <- grep(
    "^(?:holc_grade_D[^:]+:scale\\(year\\)|scale\\(year\\):holc_grade_D[^:]+)$",
    nm, perl = TRUE, value = TRUE
  )

  # helper: extract the grade letter after 'holc_grade_D'
  lev <- function(term) sub(".*holc_grade_D([^:]+).*", "\\1", term)

  z <- stats::qnorm(0.975)

  # --- 4) build tidy tibbles (handle empty sets gracefully) ---
  make_ints <- function() {
    if (length(ints_n) == 0) {
      return(tibble::tibble(
        type = character(), holc_grade = character(),
        estimate = numeric(), std.error = numeric(),
        conf.low = numeric(), conf.high = numeric()
      ))
    }
    se <- sqrt(diag(V))[ints_n]
    tibble::tibble(
      type = "intercept",
      holc_grade = lev(ints_n),
      estimate = unname(fe[ints_n]),
      std.error = unname(se),
      conf.low = estimate - z * std.error,
      conf.high = estimate + z * std.error
    )
  }

  make_slps <- function() {
    if (length(slps_n) == 0) {
      return(tibble::tibble(
        type = character(), holc_grade = character(),
        estimate = numeric(), std.error = numeric(),
        conf.low = numeric(), conf.high = numeric(),
        estimate_per_year = numeric(),
        conf.low_per_year = numeric(),
        conf.high_per_year = numeric()
      ))
    }
    se <- sqrt(diag(V))[slps_n]
    out <- tibble::tibble(
      type = "slope_per_SDyear",
      holc_grade = lev(slps_n),
      estimate = unname(fe[slps_n]),
      std.error = unname(se),
      conf.low = estimate - z * std.error,
      conf.high = estimate + z * std.error
    )
    dplyr::mutate(
      out,
      estimate_per_year = estimate / sdy,
      conf.low_per_year = conf.low / sdy,
      conf.high_per_year = conf.high / sdy
    )
  }

  dplyr::bind_rows(make_ints(), make_slps())
}

# composite tick labels: multiplicative interpretation on tick labels
lab_log_rr <- function(x) sprintf("%0.2f\n(×%0.2f)", x, exp(x))

# composite tick labels: first line = log, second line = % change
lab_log_plus_pct <- function(x) sprintf("%.2f\n(%+d%%)", x, round((exp(x)-1)*100))

  require(MASS); require(glmmTMB)
  # MODELS 2000-2020
  # 0) model per holc grade
  sum_m_nb  <- MASS::glm.nb(n_obs ~ holc_grade_D*scale(year) + 
              offset(log(sum_area_holc_km2)), 
              tt00)

  m_ass(file_name = 'test4', mo = sum_m_nb, dat = tt00, offset = TRUE, cont = c("year", "sum_area_holc_km2"), categ = 'holc_grade_D',show_binned = TRUE, PNG = TRUE)

  # 1) model set per polygon
  maD_nb <- glmmTMB(
      sum_bird_obs ~ holc_grade_D * scale(year) +
        offset(log(area_holc_km2)) +
        (1|state) + (1|city_state) + (1|id2),
      data = d00,
      family = nbinom2()
    )

    m_ass(file_name = 'test6', mo = maD_nb, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade_D',show_binned = TRUE, PNG = TRUE)

 
####
  mbD_nb = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (1|state/city_state/holc_grade/id2), 
              d00,
              family = nbinom2()
            )
  

  mas1D_nb = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d00,
              family = nbinom2(), dispformula = ~ holc_grade_D + scale(log(area_holc_km2))
              )
  m_ass(file_name = 'test7disper-modelled1', mo = mas1D_nb, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade_D',show_binned = TRUE, PNG = TRUE, show_temporal_grouped = 'year') # spatial asymetry without dispformula, given ok remaining plots, is a cosmetic imbalance, not a structural failure; if dispformula = ~ holc_grade_D + scale(log(area_holc_km2)) spatial asymetry disapears: the dispersion parameter (θ) varies slightly by socioeconomic class and area size, so smaller or denser areas — where variance tends to be higher relative to mean — are no longer “forced” into the same variance structure as large areas, which removes the systematic asymmetry between large negative and small positive residuals. The residual distribution now looks balanced, dispersion ≈ 1, and all structural panels are clean — that’s a near-optimal model.
 
  # not needed 
  mas1D_nb_year = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (1|year) + (1|state) + (scale(year)|city_state) + (1|id2),
              d00,
              family = nbinom2()
              )
   m_ass(file_name = 'test7b', mo = mas1D_nb_year, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade_D',show_binned = TRUE, PNG = TRUE)
  
  d00[, year_f := factor(year, levels = sort(unique(year)))]
  
  # not needed (nbinom2 does the job)
  mas1D_nb_ou = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (1|state) + (scale(year)|city_state) + (1|id2) + 
              ou(numFactor(year) + 0 | city_state), 
              d00,
              family = nbinom2()
              )            
  m_ass(file_name = 'test9ou', mo = mas1D_nb_ou, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade_D',show_binned = TRUE, PNG = TRUE)

  mas1D_nb_dis = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (1|state) + (scale(year)|city_state) + (1|id2),
              dispformula = ~ holc_grade_D,
              d00,
              family = nbinom1()
              )            
  m_ass(file_name = 'test9c', mo = mas1D_nb_dis, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade_D',show_binned = TRUE, PNG = TRUE)

  mas2D_nb= glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d00,
              family = nbinom2()
              )
  mbs1D_nb = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
              offset(log(area_holc_km2)) +
              (scale(year)|state/city_state/holc_grade/id2),
              d00,
              family = nbinom2()
              )

  #dropped  msab1D_nb as it woudl not converge
   #msab1D_nb = glmmTMB(sum_bird_obs ~ holc_grade_D*scale(year) + 
   #           offset(log(area_holc_km2)) +
   #           (scale(year)|state/city_state/holc_grade) + (1|id2), 
   #           d00,
   #           family = nbinom2(),
   #           control = glmmTMBControl(
   #             optimizer = nlminb,
   #             optArgs  = list(iter.max = 1e5, eval.max = 1e5)  # <- no nested 'control'
   #           )
   #           ) 

  models_T0020_D_nb <- rlang::set_names(
    list(maD_nb, mbD_nb, mas1D_nb, mas2D_nb, mbs1D_nb),
    c("maD_nb","mbD_nb","mas1D_nb", "mas2D_nb", "mbs1D_nb")
  )

  # labels
  models_T0020_labels_D_nb <- c(
    maD_nb      = "(1 | state) + (1 | city) + (1 | polygon)",
    mbD_nb      = "(1 | state / city / HOLC grade / polygon)",
    mas1D_nb    = "(1 | state) + (year | city) + (1 | polygon)",    
    mas2D_nb    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1D_nb    = "(year | state / city / HOLC grade / polygon)"
  )

  # sort models
  models_T0020_order_D_nb <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade / polygon)"
  )

  # 2) Extract fixed effects on the modeling scale (ln), fast Wald CIs
  # lm
  lm_df_nb <- ext_fixef_D_lm(sum_m_nb) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept",
                    type == "slope_per_SDyear", "Slope",
                    default = as.character(type)),
      holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
    )

  lm_lab_nb <- paste0(
    sprintf("Negative binomial model on density per year\n(n = %s for 2000-2020)", nobs(sum_m)), "\n(n = ", nrow(tt10), " for 2010-2010)") # Legend label text (no title, single key)

  # lmer
  coef_df_0020_D_nb <- purrr::imap_dfr(models_T0020_D_nb, ~ ext_fixef_D(.x) |> dplyr::mutate(model=.y))

  coef_df_0020_D_nb <- coef_df_0020_D_nb %>%
    mutate(model_label = factor(models_T0020_labels_D_nb[model], levels = models_T0020_order_D_nb)) %>% data.table()

  coef_df_0020_D_nb[, type2 := fcase(
    type == "intercept", "Intercept",
    type == "slope_per_SDyear", "Slope",
    default = as.character(type)
  )]

  coef_df_0020_D_nb[, holc_grade_dif := paste0(holc_grade, ' vs D')]

  gap_pt <- 5 # adjusts subtitle spacing: how big a gap you want between the two rows (in points)

  leg_tit = paste0("Mixed-effect negative binomial model<br>random-effects specification:<br><span style='font-weight:400;font-size:9pt;'>(n = ", comma(nrow(d00)),' for 2000-2020)<br>(n = ', comma(nrow(d10)), ' for 2010-2020)</span>') #  leg_tit = paste0('Mixed-effect model\nrandom-effects specification:\n(n = ', nrow(d00),' for 2000-2020)\n(n = ', nrow(d10), ' for 2010-2020)') # legend title

  tr1D_nb = 
  ggplot(coef_df_0020_D_nb, aes(
          x= estimate, y = holc_grade_dif, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="Contrasts (relative to HOLC grade D)", x=NULL, subtitle ='2000 - 2020') +
    ggsci::scale_color_locuszoom(
      name   = leg_tit,
      breaks = models_T0020_order_D_nb,
      limits = models_T0020_order_D_nb  # keeps legend/order consistent
    ) +
    # add lm model
    ggnewscale::new_scale_color() +  # start a NEW color scale (separate legend)
    geom_pointrange(
      data = lm_df_nb,
      aes(x = estimate, y = holc_grade_dif,
          xmin = conf.low, xmax = conf.high,
          color = lm_lab),
      position = position_nudge(y = 0.36),  # small vertical offset; remove if undesired
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,                        # no legend title
      values = setNames("black", lm_lab),
      breaks = lm_lab,
      guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
    ) +                     
    theme_light() +
    theme(
      plot.margin = margin(t = 3, r = 3, b = gap_pt, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = -22)), # adjust position above the box
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(), # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=15)), # make labels black
      axis.text.x = element_blank(),
      legend.title = element_markdown()                             
    )

  # MODELS 2010-2020
  # 0) lm model
  sum_m_10_nb <- MASS::glm.nb(n_obs ~ holc_grade_D*scale(year) + 
              offset(log(sum_area_holc_km2)), 
              tt10)

  # 1) model set per polygon
  maD_nb <- glmmTMB(
      sum_bird_obs ~ holc_grade_D * scale(year) +
        offset(log(area_holc_km2)) +
        (1|state) + (1|city_state) + (1|id2),
      data = d10,
      family = nbinom2()
    )

  # 1) model set
  ma_D_nb = glmmTMB(sum_bird_obs ~ 
               scale(year)*holc_grade_D + 
               offset(log(area_holc_km2)) +
               (1|state) + (1|city_state) + (1|id2), 
               d10,
              family = nbinom2()
              )

  mb_D_nb = glmmTMB(sum_bird_obs ~  scale(year)*holc_grade_D + 
              offset(log(area_holc_km2)) +
              (1|state/city_state/holc_grade/id2), 
              d10,
              family = nbinom2()
          )

  mas1_D_nb = glmmTMB(sum_bird_obs ~  scale(year)*holc_grade_D +            
                offset(log(area_holc_km2)) +
                (1|state) + (scale(year)|city_state) + (1|id2), 
                d10,
                family = nbinom2()
              )

  mas2_D_nb = glmmTMB(sum_bird_obs ~  scale(year)*holc_grade_D +              offset(log(area_holc_km2)) +
                (1|state) + (1|city_state) + (scale(year)|id2), 
                d10,
                family = nbinom2()
              )

  mbs1_D_nb = glmmTMB(sum_bird_obs ~  scale(year)*holc_grade_D +             offset(log(area_holc_km2)) +
                (scale(year)|state/city_state/holc_grade/id2),
                d10,
                family = nbinom2()
              )

  models_T1020_D_nb <- rlang::set_names(
    list(ma_D_nb, mb_D_nb, mas1_D_nb, mas2_D_nb, mbs1_D_nb),
    c("ma_D_nb","mb_D_nb","mas1_D_nb", "mas2_D_nb", "mbs1_D_nb")
  )

  # labels
  models_T1020_labels_D_nb <- c(
    ma_D_nb      = "(1 | state) + (1 | city) + (1 | polygon)",
    mb_D_nb      = "(1 | state / city / HOLC grade / polygon)",
    mas1_D_nb    = "(1 | state) + (year | city) + (1 | polygon)",
    mas2_D_nb    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1_D_nb    = "(year | state / city / HOLC grade  / polygon)"
  )

  # sort models
  models_T1020_order_D_nb <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade  / polygon)"
  )

  # 2) Extract fixed effects on the modeling scale (ln), fast Wald CIs
  # lm
  lm_df_10_nb <- ext_fixef_D_lm(sum_m_10) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept",
                    type == "slope_per_SDyear", "Slope",
                    default = as.character(type)),
      holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
    )

  lm_lab_10_nb <-  paste0(
    sprintf("Negative binomial model on density per year\n(n = %s for 2000-2020, ", nobs(sum_m)), "n = ", nrow(tt10), " for 2010-2010)")  # Legend label text (no title, single key) 

  # lmer
  coef_df_1020_D_nb <- purrr::imap_dfr(models_T1020_D_nb, ~ ext_fixef_D(.x) |> dplyr::mutate(model=.y))

  coef_df_1020_D_nb <- coef_df_1020_D_nb %>%
    mutate(model_label = factor(models_T1020_labels_D_nb[model], levels = models_T1020_order_D_nb)) %>% data.table()

  coef_df_1020_D_nb[, type2 := fcase(
    type == "intercept", "Intercept",
    type == "slope_per_SDyear", "Slope",
    default = as.character(type)
  )]

  coef_df_1020_D_nb[, holc_grade_dif := paste0(holc_grade, ' vs D')]

  tr2D_nb =     
  ggplot(coef_df_1020_D_nb, aes(
          x= estimate, y = holc_grade_dif, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    # lmer
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="Contrasts (relative to HOLC grade D)", 
        x="Log (ln) sampling density per km²\n(% change in sampling density per km²)", subtitle ='2010 - 2020') +
    ggsci::scale_color_locuszoom(
      name   = leg_tit,
      breaks = models_T1020_order_D_nb,
      limits = models_T1020_order_D_nb  # keeps legend/order consistent
    ) +
    # lm 
    ggnewscale::new_scale_color() +
    geom_pointrange(
      data = lm_df_10_nb,
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
    scale_x_continuous(labels = lab_log_plus_pct) + #scale_x_continuous(labels = lab_log_rr) +                   
    theme_light() +
    theme(
      plot.margin = margin(t = gap_pt, r = 3, b = 5.5, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = 2)),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(),       # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=10)),
      strip.text.x = element_blank(),
      legend.position = "none"                            
    )

  # COMBINE - x-axis correspond accros facets  
  # facet-wise ranges across BOTH datasets
  rng <- bind_rows(coef_df_0020_D_nb, coef_df_1020_D_nb,lm_df_nb, lm_df_10_nb) |>
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
          limits = c(-0.4, 0.4), 
          breaks = add_02_break,
          labels = lab_log_plus_pct, 
          oob = scales::oob_keep, # keep data for stats, don't drop rows 
          expand = expansion(mult = c(0, 0)) 
        ) 
      ) 
    } else if (grepl("^Intercept", tl)) { 
      rlang::new_formula( 
        lhs = bquote(type2 == .(tl)), 
        rhs = scale_x_continuous( 
          limits = c(-0.2, 1.2), 
          breaks = add_02_break,
          labels = lab_log_plus_pct, 
          oob = scales::oob_keep, 
          expand = expansion(mult = c(0, 0)) 
        ) 
      ) 
    } else { 
      rlang::new_formula( 
        lhs = bquote(type2 == .(tl)), 
        rhs = scale_x_continuous( 
          limits = c(lo, hi),
          breaks = add_02_break, 
          labels = lab_log_plus_pct,
          oob = scales::oob_keep, 
          expand = expansion(mult = c(0, 0)) 
        ) 
      ) 
    } 
  })  

  tr1D_nb_adj <- tr1D_nb + ggh4x::facetted_pos_scales(x = facet_scales) 
  tr2D_nb_adj <- tr2D_nb + ggh4x::facetted_pos_scales(x = facet_scales)
  
  (tr1D_nb_adj / tr2D_nb_adj) + plot_layout(axis_titles = "collect") #; ggsave('Output/rev_Fig_Z1_nb_yr-trend_model-compar_D_standardised.png', width= 27, height = 16, units ='cm')

#' <a name="F_Z1_nb">
#' **Figure Z1_nb</a> | Change in HOLC grade sampling density (rate per km²) over time.** Dots represent fixed-effect contrasts relative to grade D from negative binomial mixed models with log link and offset(ln(area)) (TODO: Peto yes/no: for actual values see Fig. [Z2_nb](F_Z2_nb)). The Intercept panel shows the differences in ln sampling density per km² at the mean of year, the Slope panel per standard deviation of year. Horizontal lines are 95% Wald confidence intervals. Colour denotes random-effects structures (variables left of `|` are random slopes, right of `|` random intercepts, and '/' indicates nesting). Top row: 2000-2020 (n = `r comma(nrow(d00))` polygons with known area); bottom row: 2010-2020 (n = `r comma(nrow(d10))`).  
#' 
#'  **Figure Z1_nb</a> | Change in HOLC grade sampling density over time.** Dots represent differences (in mean values or slopes) relative to HOLC grade D (TODO: Peto yes/no: for actual values see Fig. [Z2_nb](F_Z2_nb)), specifically differences in ln sampling density per km² for grades A/B/C relative to D at the mean year (Intercepts) and per standard deviation of year (Slopes)., specifically differences in ln sampling density per km² for grades A/B/C relative to D at the mean year (Intercepts) and per standard deviation of year (Slopes). Horizontal lines indicate 95% confidence intervals, colour a random-effect model structure (with variables left from `|` indicating random slopes and right from `|` indicating random intercepts, whereas '/' indicates nested intercepts). Top row contains estimates for a dataset spanning from 2000 until 2020 (n = `r comma(nrow(d00))` polygons with known area) and bottom row for a dataset from 2010 until 2020 (n = `r comma(nrow(d10))`).  
#' <br>  
#'  
#' #### B. Mean and slope values
#+ F_Z2, fig.width = 25/2.5, fig.height = 15/2.5
  #TODO:add poisson models as well
  # MODELS 2000-2020
  
  # 1) reparametrize models to estimate separte intercepts and slopes for each holc grade 
  # lm model on sum per holc grade
  sum_mi  <- lm(scale(log(sampling_density)) ~ 0 + holc_grade_D*scale(year), tt00)
  
  # lmer models on all polygons
  mai = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
            (1|state) + (1|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
            )

  mbi = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state/city_state/holc_grade/id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )
  mas1i = lmer(scale(log(sampling_density)) ~
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )


  mas2i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (1|city_state) + (scale(year)|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

  mbs1i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade/id2),
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

  msab1i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade) + (1|id2), 
              d00,
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
    mas2i       = mas2i,
    mbs1i      = mbs1i,
    msab1i      = msab1i
  )

  models_T0020_labels <- c(
    mai      = "(1 | state) + (1 | city) + (1 | polygon)",
    mbi      = "(1 | state / city / HOLC grade / polygon)",
    mas1i    = "(1 | state) + (year | city) + (1 | polygon)",
    mas2i    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1i    = "(year | state / city / HOLC grade / polygon)",
    msab1i   = "(year | state / city / HOLC grade) + (1 | polygon)"
  )
  # sort models
  models_T0020_order <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade / polygon)",
    "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # 3) Extract fixed effects on the modeling scale (ln), fast Wald CIs
  # lm
  lmi_df <- ext_fixef_lm(sum_mi) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept",
                    type == "slope_per_SDyear", "Slope",
                    default = as.character(type))
    )

  lm_lab <- paste0(
    sprintf("Linear model on density per year\n(n = %s for 2000-2020, ", nobs(sum_m)), "n = ", nrow(tt10), " for 2010-2010)") # Legend label text (no title, single key)

  # lmer
  coef_df_0020 <- purrr::imap_dfr(models_T0020, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

  coef_df_0020 <- coef_df_0020 %>%
    mutate(model_label = factor(models_T0020_labels[model], levels = models_T0020_order)) %>% data.table()

  coef_df_0020[, type2 := fcase(
    type == "intercept",        "Intercept",
    type == "slope_per_SDyear", "Slope",
    default = as.character(type)
  )]

  leg_tit = paste0("Mixed-effect model<br>random-effects specification:<br><span style='font-weight:400;font-size:9pt;'>(n = ", comma(nrow(d00)),' for 2000-2020)<br>(n = ', comma(nrow(d10)), ' for 2010-2020)</span>') # legend title

  tr1 = 
  ggplot(coef_df_0020, aes(
          x= estimate, y = holc_grade, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    # lmer
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="HOLC grade", x= NULL, subtitle ='2000 - 2020') +
    ggsci::scale_color_locuszoom(
      name   = leg_tit,
      breaks = models_T0020_order_D,
      limits = models_T0020_order_D  # keeps legend/order consistent
    ) +
    # lm model
    ggnewscale::new_scale_color() +  # start a NEW color scale (separate legend)
    geom_pointrange(
      data = lmi_df,
      aes(x = estimate, y = holc_grade,
          xmin = conf.low, xmax = conf.high,
          color = lm_lab),
      position = position_nudge(y = 0.36),  # small vertical offset; remove if undesired
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,                        # no legend title
      values = setNames("black", lm_lab),
      breaks = lm_lab,
      guide = guide_legend(order = 1, override.aes = list(linewidth = 0.8))
    ) + 
    theme_light() +
    theme(
      plot.margin = margin(t = 3, r = 3, b = gap_pt, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = -22)), # adjust position above the box
      axis.text.x = element_blank(),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(), # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=15)), # make labels black
      legend.title = element_markdown() 

    )

  # MODELS 2010-2020
  # 1) reparametrize models to estimate separte intercepts and slopes for each holc grade 
  # lm model on sum per holc grade
    sum_m_i  <- lm(scale(log(sampling_density)) ~ 0 + holc_grade_D*scale(year), tt10)

  # lmer models on all polygons  
    ma_i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
            (1|state) + (1|city_state) + (1|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )

    mb_i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state/city_state/holc_grade/id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )
    mas1_i = lmer(scale(log(sampling_density)) ~
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

    mas2_i = lmer(scale(log(sampling_density)) ~
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (1|city_state) + (scale(year)|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

    mbs1_i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade/id2),
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

    msab1_i = lmer(scale(log(sampling_density)) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade) + (1|id2), 
              d10,
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
    mas2_i       = mas2_i,
    mbs1_i      = mbs1_i,
    msab1_i      = msab1_i
  )

  models_T1020_labels <- c(
    ma_i      = "(1 | state) + (1 | city) + (1 | polygon)",
    mb_i      = "(1 | state / city / HOLC grade / polygon)",
    mas1_i    = "(1 | state) + (year | city) + (1 | polygon)",
    mas2_i    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1_i    = "(year | state / city / HOLC grade / polygon)",
    msab1_i   = "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # sort models
  models_T1020_order <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade / polygon)",
    "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # 3) Extract fixed effects on the modeling scale (ln), fast Wald CIs
  # lm
  lmi_df_10 <- ext_fixef_lm(sum_m_i) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept",
                    type == "slope_per_SDyear", "Slope",
                    default = as.character(type))
    )

  lm_10_lab <- paste0(
    sprintf("Linear model on density per year\n(n = %s for 2000-2020)", nobs(sum_m)), "\n(n = ", nrow(tt10), " for 2010-2010)") # Legend label text (no title, single key)
  
  # lmer
  coef_df_1020 <- purrr::imap_dfr(models_T1020, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

  coef_df_1020 <- coef_df_1020 %>%
    mutate(model_label = factor(models_T1020_labels[model], levels = models_T1020_order)) %>% data.table()

  coef_df_1020[, type2 := fcase(
    type == "intercept",        "Intercept",
    type == "slope_per_SDyear", "Slope",
    default = as.character(type)
  )]

  tr2 =     
  ggplot(coef_df_1020, aes(
          x= estimate, y = holc_grade, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    # lmer
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="HOLC grade", x="Standardised estimates", subtitle ='2010 - 2020') +
    ggsci::scale_color_locuszoom(
      name   = leg_tit,
      breaks = models_T0020_order_D,
      limits = models_T0020_order_D  # keeps legend/order consistent
    ) +
    # lm 
    ggnewscale::new_scale_color() +
    geom_pointrange(
      data = lmi_df_10,
      aes(x = estimate, y = holc_grade,
          xmin = conf.low, xmax = conf.high,
          color = lm_10_lab),
      position = position_nudge(y = 0.36),
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,
      values = setNames("black", lm_10_lab),
      breaks = lm_lab,
      guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
    ) +  
    theme_light() +
    theme(
      plot.margin = margin(t = gap_pt, r = 3, b = 5.5, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = 2)),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(),       # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=10)),
      strip.text.x = element_blank(),
      legend.position = "none"                            
    )

  # COMBINE  
    rng <- bind_rows(coef_df_0020, coef_df_1020,lmi_df, lmi_df_10) |>
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
            limits = c(-0.4, 1), 
            breaks = add_02_break, 
            oob = scales::oob_keep, # keep data for stats, don't drop rows 
            expand = expansion(mult = c(0, 0)) 
          ) 
        ) 
      } else if (grepl("^Intercept", tl)) { 
        rlang::new_formula( 
          lhs = bquote(type2 == .(tl)), 
          rhs = scale_x_continuous( 
            limits = c(-.6, .8), 
            breaks = add_02_break, 
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
    
    (tr1_adj / tr2_adj) + plot_layout(axis_titles = "collect") #; ggsave('Output/rev_Fig_Z2_yr-trend_model-compar_D_standardised.png', width= 25, height = 15, units ='cm')

#' <a name="F_Z2">
#' **Figure Z2</a> | Standardised estimates of sampling density between HOLC grades over time.** Dots represent estimates (standard deviation of ln-scaled sampling density [Intercepts] per standard deviation of year [slopes]), horizontal lines 95% confidence intervals, colour indicates a random-effects model structure (with variables  left from `|` indicating random slopes and right from `|` indicating random intercepts, whereas '/' indicates nested intercepts). The depicted estimates represent true values (as oposed to the contrasts depicted in Fig. [Z1](Z1)) Top row contains estimates for a dataset spanning from 2000 until 2020 (n = `r comma(nrow(tt00))` sampling densities per grade and year for the linear model, and n = `r comma(nrow(d00))` sampling densities for each polygon with known area) and bottom row for a dataset from 2010 until 2020 (n = `r comma(nrow(tt10))` and n = `r comma(nrow(d10))`, respectively).  
#' <br>
#'  
#' #### C. visualise predicted relationships
#' **Figure Z3</a> | Predictions of sampling density between HOLC grades  over time.** In the below figures  titles indicate model structures (with terms in parentheses indicating random term: variables  left from `|` indicating random slopes and right from `|` indicating random intercepts, whereas '/' indicates nested intercepts). The lines and shaded areas represent predicted relationship between sampling density (km² - right panels, log(km²) - left panels) and year of data collection according to HOLC grade (colour). The first plot in each section (2000-2020 and 2010-2020) is based on a linear model fitted to sampling density for each year (overal number of observations devided by overall HOLC grade area; n = `r comma(nrow(tt00))` for 2000-2020 and `r comma(nrow(tt10))` for 2010-2020) and the remaining plots are based on mixed-effect models fitted to samplind density for each HOLC neighbourhood (polygon; n = `r comma(nrow(d00))` for 2000-2020 and `r comma(nrow(d10))` for 2010-2020).  
#' <br>
#' 
#' ##### 2000-2020
#+ F_Z3x, fig.width = 14/2.5, fig.height = 6/2.5
sum_m_  <- lm(log(sampling_density) ~ scale(year)*holc_grade, tt00)
print(plot_effects_holc(sum_m_, tt00, palette = holc_pal, outdir='Output/trend/Oct', save_png = FALSE))    

#+ F_Z3a, fig.width = 14/2.5, fig.height = 6/2.5
ma = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (1|id2), 
            d00)
print(plot_effects_holc(ma, d00, palette = holc_pal, outdir='Output/trend/Oct', save_png = FALSE))    

#+ F_Z3b, fig.width = 14/2.5, fig.height = 6/2.5
mb = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state/city_state/holc_grade/id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
plot_effects_holc(mb, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3c, fig.width = 14/2.5, fig.height = 6/2.5
mas1 = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas1, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3d, fig.width = 14/2.5, fig.height = 6/2.5
mas2 = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (scale(year)|id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas2, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3e, fig.width = 14/2.5, fig.height = 6/2.5
mbs1 = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade/id2),
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mbs1, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3f, fig.width = 14/2.5, fig.height = 6/2.5
msab1 = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 
plot_effects_holc(msab1, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)
#' 
#' <br>
#'  
#' ##### 2010-2020
#+ F_Z4x, fig.width = 14/2.5, fig.height = 6/2.5
sum_m_10  <- lm(log(sampling_density) ~ scale(year)*holc_grade, tt10)
print(plot_effects_holc(sum_m_10, tt10, palette = holc_pal, outdir='Output/trend/Oct', save_png = FALSE))  
#+ F_Z4a, fig.width = 14/2.5, fig.height = 6/2.5
ma_ = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (1|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
plot_effects_holc(ma_, d10, palette = holc_pal, outdir='Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4b, fig.width = 14/2.5, fig.height = 6/2.5
mb_ = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state/city_state/holc_grade/id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
plot_effects_holc(mb_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4c, fig.width = 14/2.5, fig.height = 6/2.5
mas1_ = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas1_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20')

#+ F_Z4d, fig.width = 14/2.5, fig.height = 6/2.5
mas2_ = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (scale(year)|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas2_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4e, fig.width = 14/2.5, fig.height = 6/2.5
mbs1_ = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade/id2),
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mbs1_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4f, fig.width = 14/2.5, fig.height = 6/2.5
msab1_ = lmer(log(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 
plot_effects_holc(msab1_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)