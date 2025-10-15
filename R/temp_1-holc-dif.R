# 3a. load authors' data for HOLC comparison (uses code from the authors' 05_paper_1_analyses_R4.Rmd)
  holc <- read_csv('original_paper/Data/Biodiv_Greeness_Social/soc_dem_max_2022_03_12 17_31_11.csv'
                   , col_select = c(id : area_holc_km2
                                    , holc_tot_pop
                                    , msa_GEOID : msa_total_popE
                                    , msa_gini))
  birds_records <- 
    read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_sum_bird_obs_by_holc_id_1933_2022.csv') |> 
    mutate(id = str_remove(id, '_Aves_all_observations')) |> 
    select(id, records = N_samples)
  
  birds_completeness <-
    read_csv('original_paper/Data/Biodiv_Greeness_Social/bird_completeness_HOLC_cities_2022_R1.csv') |> 
    select(id, completeness = Completeness) |> 
    tidylog::mutate(id = ifelse(id == 'VA_Roanoke_B2\\r\\n2_B_9289', 'VA_Roanoke_B2\r\n2_B_9289', id))
  
  birds_source <- 
    read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_col_code_by_holc_id_2000_2020.csv') |> 
    mutate(id = str_remove(id, '_Aves_all_observations')) |> 
    select(id, records = N_samples, type = Collection_code) |> 
    pivot_wider(id_cols = id, names_from = type, values_from = records)
  
  clim <- read_csv('original_paper/Data/Biodiv_Greeness_Social/climatic_data_cities.csv'
                   , col_select = c(  -'...1'
                                    , mean_temp_c = mean_temp
                                    , mean_precip_mm = mean_precip)
                   )
  green <- read_csv('original_paper/Data/Biodiv_Greeness_Social/NDVI_unique_ID_updated.csv') %>% 
        mutate(ndvi = ifelse(is.na(ndvi), median(.$ndvi, na.rm = TRUE), ndvi)) |> # interpolated missing
        select(id, ndvi)
        
  pad <- read_csv('Data/NDVI_PAD_unique_ID.csv'#'original_paper/Data/Biodiv_Greeness_Social/NDVI_PAD_unique_ID.csv' 
                 , col_select = c(id, pct_pa = percent_pa))
  # combine
  comb <- 
    left_join(holc, birds_records, by = 'id') |> # adds bird biodiversity                
    left_join(birds_completeness, by = 'id') |> # adds bird completeness
    left_join(birds_source, by = 'id') |> # adds in sampling by source
    left_join(clim, by = 'city') |> # adds temp and precip
    left_join(green, by = 'id') |> # adds green 
    left_join(pad, by = 'id') |> # adds protected areas
    mutate(
        pop_per_km           = ifelse(is.na(holc_tot_pop), 0, holc_tot_pop / area_holc_km2)
        , sampling_density     = records / area_holc_km2
        , sampling_density_log = log(sampling_density)
        , completeness_log     = log(completeness)) |> 
    relocate(sampling_density, sampling_density_log, completeness, completeness_log, 
            .before = completeness) 

  h = data.table(comb)
  h=h[!holc_grade%in%'E']

  # additional variables
  h[, sample_binary := ifelse(is.na(sampling_density_log), 0, 1)]

  h[holc_grade%in%'A', holc_grade_num:=1]
  h[holc_grade%in%'C', holc_grade_num:=2]
  h[holc_grade%in%'B', holc_grade_num:=3]
  h[holc_grade%in%'D', holc_grade_num:=4]

  # add city lat lon
  latlon = fread('Data/MaPe_cities_coordinates.txt')
  h = merge(h,p, all.x =TRUE)

#' # 1. HOLC grade differences
#' TODO:ADD here the description about authors using xx variables to test for the differences, but that the model specs were not clear, etc. 
#' <br>
#'  
#' ## A. Sampled or not
#+ F_r1, fig.width = 25/2.5, fig.height = 8/2.5
  # binary models

    # specified by authors (second one fails to converge)
    samp_d_binary_holc = lme4::glmer(sample_binary ~ holc_grade + 
        (1 | msa_NAME), 
        data = h, 
        family = binomial(link = "logit")) 

    samp_d_binary_holc_rirs = lme4::glmer(sample_binary ~ holc_grade + 
        (holc_grade | msa_NAME),
        data = h, 
        family = binomial(link = "logit"))

    # specified by us
    m0 = lme4::glmer(sample_binary ~ holc_grade + 
        (1 | city_state), 
        data = h, 
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
        )

    m1= lme4::glmer(sample_binary ~ holc_grade + 
        (holc_grade | city_state), 
        data = h, 
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
        )

    m2= lme4::glmer(sample_binary ~ holc_grade + 
        (holc_grade_num | city_state), 
        data = h, 
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
        )
    m1p= lme4::glmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
        (holc_grade | city_state), 
        data = h, 
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
        )

    m2p= lme4::glmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
        (holc_grade_num | city_state), 
        data = h, 
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5)))  

  # PLOT for logit
    # add models to a list
    models_A<- list(
    samp_d_binary_holc      = samp_d_binary_holc,
    samp_d_binary_holc_rirs = samp_d_binary_holc_rirs,
    m0       = m0,
    m1       = m1,
    m2       = m2,
    m1p      = m1p,
    m2p      = m2p
    )

    # model labels
    model_labels_A <- c(
    samp_d_binary_holc      = "HOLC grade + (1 | metropoly)",
    samp_d_binary_holc_rirs    = "HOLC grade + (1 + HOLC grade | metropoly)",
    m0       = "HOLC grade + (1 | state city)",
    m1       = "HOLC grade + (HOLC grade | state city)",
    m2       = "HOLC grade + (HOLC grade numeric | state city)",
    m1p      = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
    m2p      = "HOLC grade + ndvi + protected area + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )

    # sort models
    models_A_order <- c(
        "HOLC grade + (1 | metropoly)",
        "HOLC grade + (1 | state city)",
        "HOLC grade + (1 + HOLC grade | metropoly)",
        "HOLC grade + (HOLC grade | state city)",
        "HOLC grade + (HOLC grade numeric | state city)",
        "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
        "HOLC grade + ndvi + protected area + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )


    # extract
    coef_df_A <- purrr::imap_dfr(models_A, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

    coef_df_A <- coef_df_A %>%
    mutate(model_label = factor(model_labels_A[model], levels = models_A_order)) %>% data.table()

    # distinquish original models from our alternative ones
    coef_df_A <- coef_df_A %>%
    mutate(
        model_group = ifelse(grepl("^samp", model), "Authors' original", "Our new"),
        model_label = model_labels_A[model]
    )

    coef_df_A <- coef_df_A %>%
    mutate(model_label = factor(model_label, levels = models_A_order))

    red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
    blue_ =  "#46B8DAFF"

    # plot
    A1 = 
    ggplot(coef_df_A, aes(x = estimate, y = fct_rev(model_label))) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
        color = model_group),
        position = position_dodge(width = 0.6), height = 0) +
    geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_wrap(~ holc_grade) +
    scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
    scale_fill_manual(values = c("white","#46B8DAFF")) +
    #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
    theme_minimal(base_size = 8) +
    labs(
        x = "",
        y = "Model structure",
        color = NULL,
        fill = NULL,
        subtitle = 'logit scale'
    ) +
    theme(
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        plot.subtitle = element_text(size = 7, colour = "grey40", margin = margin(b=-14))
        )#; ggsave('Output/Fig_r1_Sampled01_v2.jpg', width = 25, height = 5, units = 'cm')
  
  # gaussian
    # as specified abvoe by the authors 
    samp_m0_g = lmer(sample_binary ~ holc_grade + 
        (1 | msa_NAME), 
        data = h)  

    samp_m1_g= lmer(sample_binary ~ holc_grade + 
        (holc_grade | msa_NAME), 
        data = h,
        control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))) 

    # specified by us
    m0_g = lmer(sample_binary ~ holc_grade + 
        (1 | city_state), 
        data = h)

    m1_g= lmer(sample_binary ~ holc_grade + 
        (holc_grade | city_state), 
        data = h,
        control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))) 

    m2_g= lmer(sample_binary ~ holc_grade + 
        (holc_grade_num | city_state), 
        data = h, 
        control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5)))    

    m1p_g= lmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
        (holc_grade | city_state), 
        data = h,
        control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
        )   

    m2p_g= lmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
        (holc_grade_num | city_state), 
        data = h, 
        control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
        )

  # PLOT for gaussian
    # add models to a list
    models_A_g<- list(
        samp_m0_g      = samp_m0_g,
        samp_m1_g = samp_m1_g,
        m0_g      = m0_g,
        m1_g      = m1_g,
        m2_g       = m2_g,
        m1p_g     = m1p_g,
        m2p_g      = m2p_g
        )

    # model labels
    model_labels_A_g <- c(
        samp_m0_g      = "HOLC grade + (1 | metropoly)",
        samp_m1_g    = "HOLC grade + (1 + HOLC grade | metropoly)",
        m0_g      = "HOLC grade + (1 | state city)",
        m1_g       = "HOLC grade + (HOLC grade | state city)",
        m2_g       = "HOLC grade + (HOLC grade numeric | state city)",
        m1p_g      = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
        m2p_g      = "HOLC grade + ndvi + protected area + population density + temperature * precipitation + (HOLC grade numeric | state city)"
        )

    # sort models
    models_A_order_g <- c(
        "HOLC grade + (1 | metropoly)",
        "HOLC grade + (1 | state city)",
        "HOLC grade + (1 + HOLC grade | metropoly)",
        "HOLC grade + (HOLC grade | state city)",
        "HOLC grade + (HOLC grade numeric | state city)",
        "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
        "HOLC grade + ndvi + protected area + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )


    # extract
    coef_df_A_g <- purrr::imap_dfr(models_A_g, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

    coef_df_A_g <- coef_df_A_g %>%
    mutate(model_label = factor(model_labels_A_g[model], levels = models_A_order_g)) %>% data.table()

    # distinquish original models from our alternative ones
    coef_df_A_g <- coef_df_A_g %>%
    mutate(
        model_group = ifelse(grepl("^samp", model), "Authors' original", "Our new"),
        model_label = model_labels_A_g[model]
    )

    coef_df_A_g <- coef_df_A_g %>%
    mutate(model_label = factor(model_label, levels = models_A_order_g))

    red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
    blue_ =  "#46B8DAFF"

    # plot
    A2 = 
    ggplot(coef_df_A_g, aes(x = estimate, y = fct_rev(model_label))) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
        color = model_group),
        position = position_dodge(width = 0.6), height = 0) +
    geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_wrap(~ holc_grade) +
    scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
    scale_fill_manual(values = c("white","#46B8DAFF")) +
    #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
    theme_minimal(base_size = 8) +
    labs(
        x = "Estimates of 'sampled or not' relative to HOLC grade A",
        y = "Model structure",
        color = NULL,
        fill = NULL,
        subtitle = "origianal scale"
    ) +
    theme(
        legend.position = "none",
        #legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        strip.text = element_blank(),
        plot.subtitle = element_text(size = 7, colour = "grey40",, margin = margin(b=-2))
        )#; ggsave('Output/Fig_r1_Sampled01_v2.jpg', width = 25, height = 5, units = 'cm')
  
  # combine
  (A1 / A2) + plot_layout(axis_titles = "collect"); #ggsave('Output/Fig_r1_Sampled01_bin&gaus.jpg', width = 25, height = 8, units = 'cm') 

  #' **Figure r1</a> | Differences in estimated presence of sampling  between HOLC grades.** Dots represent differences (in mean values) relative to HOLC grade A (TODO: peto do we need this: "for actual values see Fig. [r1b](F_r1b))", horizontal lines indicate 95%CIs, colour models specified by the authors (red empty circles) or by us (blue filled circles). The y-axis highlights specific model structure with variables in the paranthesis indicating random effects (left from `|` indicating random slopes and right from `|` indicating random intercepts). Top row contains estimates from a logit-models, bottom row from the Gaussian ones. n = `r nrow(h)` polygons (neighberhoods).
#' <br>
#' 
#' ## B. Sampling density
#+ F_r2, fig.width = 25/2.5, fig.height = 8/2.5
  
  # non zero data
    hB_ = h[!is.na(sampling_density)] # remove NAs (zeros)
    
    # authors'
    d_ri <- lme4::lmer(log(sampling_density) ~ holc_grade + 
                    (1 | msa_NAME), 
                    data = hB_)

    d_rirs <- lme4::lmer(log(sampling_density) ~ holc_grade + 
                (1 + holc_grade|msa_NAME),
                data = hB_)    

    d_fe_rirs <- lme4::lmer(log(sampling_density) ~ holc_grade + 
            scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
            (1 + holc_grade| msa_NAME), 
            data = hB_)          
    
    # us
    mB0 = lmer(log(sampling_density) ~  holc_grade + 
            (1 | city_state), 
            data = hB_
            ) 

    mB1 = lmer(log(sampling_density) ~ holc_grade + 
            (holc_grade | city_state), 
            data = hB_,
            control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
            )

    mB2 = lmer(log(sampling_density) ~ holc_grade + 
            (holc_grade_num | city_state), 
            data = hB_,
            control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
            )

    mB1p = lmer(log(sampling_density) ~ holc_grade + 
            scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
            scale(mean_temp_c)*scale(mean_precip_mm) + 
            (holc_grade | city_state), 
            data = hB_, 
            control = lmerControl(optimizer = "bobyqa",
                                optCtrl = list(maxfun = 2e5))
            )  

    mB2p = lmer(log(sampling_density) ~ holc_grade + 
            scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
            scale(mean_temp_c)*scale(mean_precip_mm) + 
            (holc_grade_num | city_state), 
            data = hB_, 
            control = lmerControl(optimizer = "bobyqa",
                                optCtrl = list(maxfun = 2e5))
            ) 
    
    # add models to a list
    models_B <- list(
    d_ri      = d_ri,
    d_rirs    = d_rirs,
    d_fe_rirs = d_fe_rirs,
    mB0       = mB0,
    mB1       = mB1,
    mB2       = mB2,
    mB1p      = mB1p,
    mB2p      = mB2p
    )

    # labels 
    model_labels_B <- c(
    d_ri      = "HOLC grade + (1 | metropoly)", 
    d_rirs    = "HOLC grade + (1 + HOLC grade | metropoly)", 
    d_fe_rirs = "HOLC grade + ndvi + protected area % + population density + (1 + HOLC grade | metropoly)",
    mB0       = "HOLC grade + (1 | state city)", 
    mB1       = "HOLC grade + (HOLC grade | state city)", 
    mB2       = "HOLC grade + (HOLC grade numeric | state city)",
    mB1p      = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
    mB2p      = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )

    # sort models
    models_B_order <- c(
        "HOLC grade + (1 | metropoly)",
        "HOLC grade + (1 | state city)",
        "HOLC grade + (1 + HOLC grade | metropoly)",
        "HOLC grade + (HOLC grade | state city)",
        "HOLC grade + (HOLC grade numeric | state city)", 
        "HOLC grade + ndvi + protected area % + population density + (1 + HOLC grade | metropoly)",
        "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
        "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )

    # extract
    coef_df_B <- purrr::imap_dfr(models_B, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

    coef_df_B <- coef_df_B %>%
    mutate(model_label = factor(model_labels_B[model], levels = models_B_order)) %>% data.table()

    # distinquish original models from our alternative ones
    coef_df_B <- coef_df_B %>%
    mutate(
        model_group = ifelse(grepl("^d_", model), "Authors' original", "Our new"),
        model_label = model_labels_B[model]
    )

    coef_df_B <- coef_df_B %>%
    mutate(model_label = factor(model_label, levels = models_B_order))

    red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
    blue_ =  "#46B8DAFF"

    # plot
    B1 = 
    ggplot(coef_df_B, aes(x = estimate, y = fct_rev(model_label))) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
        color = model_group),
        position = position_dodge(width = 0.6), height = 0) +
    geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_wrap(~ holc_grade) +
    scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
    scale_fill_manual(values = c("white","#46B8DAFF")) +
    #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
    theme_minimal(base_size = 8) +
    labs(
        x = "",
        y = "Model structure",
        color = NULL,
        fill = NULL,
        subtitle = 'non-zero data\n(n = 8,904)'
    ) +
    theme(
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        plot.subtitle = element_text(size = 7, colour = "grey40", margin = margin(b=-25))
        )#; ggsave('Output/Fig_r1_Sampled01_v2.jpg', width = 25, height = 5, units = 'cm')

  # including zero data (with a small offset to allow log)
    hB = copy(h)
    hB = hB[is.na(sampling_density), sampling_density := 0] # zeros were NAs, making them zeros 
    
    # offset function for log on zeros
    c_off <- function(x) {
        nz <- x[x > 0]
        if (!length(nz)) stop("all zeros")
        max(min(nz, na.rm=TRUE)/2, 1e-6)
        }
    
    hB = hB[, sd_ln := log(sampling_density + c_off(sampling_density))] # ~ offset of 0.1257409

    # authors'
    d_ri_a  <- lme4::lmer(sd_ln ~ holc_grade + 
                    (1 | msa_NAME), 
                    data = hB)

    d_rirs_a  <- lme4::lmer(sd_ln ~ holc_grade + 
                (1 + holc_grade|msa_NAME),
                data = hB)    

    d_fe_rirs_a  <- lme4::lmer(sd_ln ~ holc_grade + 
            scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
            (1 + holc_grade| msa_NAME), 
            data = hB)          
    
    # us
    mB0_a = lmer(sd_ln ~  holc_grade + 
            (1 | city_state), 
            data = hB
            ) 

    mB1_a  = lmer(sd_ln ~ holc_grade + 
            (holc_grade | city_state), 
            data = hB,
            control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
            )

    mB2_a  = lmer(sd_ln ~ holc_grade + 
            (holc_grade_num | city_state), 
            data = hB,
            control = lmerControl(optimizer = "bobyqa",
                            optCtrl = list(maxfun = 2e5))
            )

    mB1p_a  = lmer(sd_ln ~ holc_grade + 
            scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
            scale(mean_temp_c)*scale(mean_precip_mm) + 
            (holc_grade | city_state), 
            data = hB, 
            control = lmerControl(optimizer = "bobyqa",
                                optCtrl = list(maxfun = 2e5))
            )  

    mB2p_a  = lmer(sd_ln ~ holc_grade + 
            scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
            scale(mean_temp_c)*scale(mean_precip_mm) + 
            (holc_grade_num | city_state), 
            data = hB, 
            control = lmerControl(optimizer = "bobyqa",
                                optCtrl = list(maxfun = 2e5))
            ) 
    
    # add models to a list
    models_B_a  <- list(
    d_ri_a       = d_ri_a ,
    d_rirs_a     = d_rirs_a ,
    d_fe_rirs_a  = d_fe_rirs_a ,
    mB0_a       = mB0_a ,
    mB1_a        = mB1_a ,
    mB2_a        = mB2_a ,
    mB1p_a       = mB1p_a ,
    mB2p_a       = mB2p_a 
    )

    # labels 
    model_labels_B_a  <- c(
    d_ri_a      = "HOLC grade + (1 | metropoly)", 
    d_rirs_a     = "HOLC grade + (1 + HOLC grade | metropoly)", 
    d_fe_rirs_a  = "HOLC grade + ndvi + protected area % + population density + (1 + HOLC grade | metropoly)",
    mB0_a        = "HOLC grade + (1 | state city)", 
    mB1_a        = "HOLC grade + (HOLC grade | state city)", 
    mB2_a        = "HOLC grade + (HOLC grade numeric | state city)",
    mB1p_a       = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
    mB2p_a       = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )

    # sort models
    models_B_order_a  <- c(
        "HOLC grade + (1 | metropoly)",
        "HOLC grade + (1 | state city)",
        "HOLC grade + (1 + HOLC grade | metropoly)",
        "HOLC grade + (HOLC grade | state city)",
        "HOLC grade + (HOLC grade numeric | state city)", 
        "HOLC grade + ndvi + protected area % + population density + (1 + HOLC grade | metropoly)",
        "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
        "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )

    # extract
    coef_df_B_a  <- purrr::imap_dfr(models_B_a , ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

    coef_df_B_a  <- coef_df_B_a  %>%
    mutate(model_label = factor(model_labels_B_a [model], levels = models_B_order_a )) %>% data.table()

    # distinquish original models from our alternative ones
    coef_df_B_a  <- coef_df_B_a  %>%
    mutate(
        model_group = ifelse(grepl("^d_", model), "Authors' original", "Our new"),
        model_label = model_labels_B_a [model]
    )

    coef_df_B_a  <- coef_df_B_a  %>%
    mutate(model_label = factor(model_label, levels = models_B_order_a ))

    red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
    blue_ =  "#46B8DAFF"

    # plot
    B2 = 
    ggplot(coef_df_B_a , aes(x = estimate, y = fct_rev(model_label))) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
        color = model_group),
        position = position_dodge(width = 0.6), height = 0) +
    geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_wrap(~ holc_grade) +
    scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
    scale_fill_manual(values = c("white","#46B8DAFF")) +
    #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
    theme_minimal(base_size = 8) +
    labs(
        x = "Estimates of sampling density relative to HOLC grade A",
        y = "Model structure",
        color = NULL,
        fill = NULL,
        subtitle = 'all data\n(n = 9,847)'
    ) +
    theme(
        legend.position = "none",
        #legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        strip.text = element_blank(),
        plot.subtitle = element_text(size = 7, colour = "grey40", margin = margin(b=-10))
        )#; ggsave('Output/Fig_r1_Sampled01_v2.jpg', width = 25, height = 5, units = 'cm')
  
    # combine
    (B1 / B2) + plot_layout(axis_titles = "collect"); #ggsave('Output/Fig_r2_sampling-density.jpg', width = 25, height = 8, units = 'cm') 

#' **Figure r2</a> | Differences in estimated sampling density between HOLC grades.** Dots represent differences (in mean values) relative to HOLC grade A on ln-scale (TODO: peto do we need this: "for actual values see Fig. [r2b](F_r2b))", horizontal lines indicate 95%CIs, colour models specified by the authors (red empty circles) or by us (blue filled circles). The y-axis highlights specific model structure with variables in the paranthesis indicating random effects (left from `|` indicating random slopes and right from `|` indicating random intercepts). Top row contains estimates from the dataset with non-sampled polygons removed, bottom row the full dataset (where a small data-derived offset of 0.125 was added to the sampling density before ln-transformation).
#' <br>
#' 
#' ## C. Completeness of sampling
#+ F_r3, fig.width = 25/2.5, fig.height = 8/2.5

hC = h[!is.na(completeness)]

# specified by the authors
c_ri <- lme4::lmer(completeness ~ holc_grade + 
    (1 | msa_NAME), 
    data = hC)

c_rirs    <- lme4::lmer(completeness ~ holc_grade + 
    (1 + holc_grade | msa_NAME), 
    data = hC)

c_fe_rirs <- lme4::lmer(completeness ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    (1 + holc_grade| msa_NAME), 
    data = hC) # the authors' winnder

# specified by us
mC0 = lmer(completeness ~  holc_grade + 
    (1 | city), 
    data = hC
    ) 

mC1= lmer(completeness ~ holc_grade + 
    (holc_grade | city), 
    data = hC)

mC2= lmer(completeness ~ holc_grade + 
    (holc_grade_num | city), 
    data = hC)

mC1p= lmer(completeness ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city), 
    data = hC, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

mC2p= lmer(completeness~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city), 
    data = hC, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

# add models to a list
models_C <- list(
  c_ri      = c_ri,
  c_rirs    = c_rirs,
  c_fe_rirs = c_fe_rirs,
  mC0       = mC0,
  mC1       = mC1,
  mC2       = mC2,
  mC1p      = mC1p,
  mC2p      = mC2p
)

# labels 
model_labels_C <- c(
    c_ri      = "HOLC grade + (1 | metropoly)", 
    c_rirs    = "HOLC grade + (1 + HOLC grade | metropoly)", 
    c_fe_rirs = "HOLC grade + ndvi + protected area % + population density + (1 + HOLC grade | metropoly)",
    mC0       = "HOLC grade + (1 | state city)", 
    mC1       = "HOLC grade + (HOLC grade | state city)", 
    mC2       = "HOLC grade + (HOLC grade numeric | state city)",
    mC1p      = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
    mC2p      = "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade numeric | state city)"
    )

# sort models
models_C_order <- c(
    "HOLC grade + (1 | metropoly)",
    "HOLC grade + (1 | state city)",
    "HOLC grade + (1 + HOLC grade | metropoly)",
    "HOLC grade + (HOLC grade | state city)",
    "HOLC grade + (HOLC grade numeric | state city)", 
    "HOLC grade + ndvi + protected area % + population density + (1 + HOLC grade | metropoly)",
    "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade | state city)",
    "HOLC grade + ndvi + protected area % + population density + temperature * precipitation + (HOLC grade numeric | state city)"
)

# extract
coef_df_C <- purrr::imap_dfr(models_C, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

coef_df_C <- coef_df_C %>%
mutate(model_label = factor(model_labels_C[model], levels = models_C_order)) %>% data.table()

# distinquish original models from our alternative ones
coef_df_C <- coef_df_C %>%
mutate(
    model_group = ifelse(grepl("^c_", model), "Authors' original", "Our new"),
    model_label = model_labels_C[model]
)

coef_df_C <- coef_df_C %>%
mutate(model_label = factor(model_label, levels = models_C_order))

red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
blue_ =  "#46B8DAFF"

# plot
C1 = 
ggplot(coef_df_C, aes(x = estimate, y = fct_rev(model_label))) +
geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
    color = model_group),
    position = position_dodge(width = 0.6), height = 0) +
geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
facet_wrap(~ holc_grade) +
scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
scale_fill_manual(values = c("white","#46B8DAFF")) +
#scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
theme_minimal(base_size = 8) +
labs(
    x = "Estimates of completeness relative to HOLC grade A",
    y = "Model structure",
    color = NULL,
    fill = NULL
) +
theme(
    legend.key.height = unit(0.25, "cm")  # reduce vertical spacing between items 
    #plot.subtitle = element_text(size = 7, colour = "grey40", margin = margin(b=-25))
    )#; ggsave('Output/Fig_r3_completeness.jpg', width = 25, height = 5, units = 'cm')

#' **Figure r3</a> | Differences in estimated sampling completeness between HOLC grades.** Dots represent differences (in mean values) relative to HOLC grade A (TODO: peto do we need this: "for actual values see Fig. [r3b](F_r3b))", horizontal lines indicate 95%CIs, colour models specified by the authors (red empty circles) or by us (blue filled circles). The y-axis highlights specific model structure with variables in the paranthesis indicating random effects (left from `|` indicating random slopes and right from `|` indicating random intercepts). n = `r nrow(hC)` polygons (neighberhoods).
#' <br>
#' 
#' ***
#' 
#' <br>
#' 