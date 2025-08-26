require(data.table)
comb$sample_binary = ifelse(is.na(comb$sampling_density_log), 0, 1)
d = data.table(comb)
d=d[!holc_grade%in%'E']

d[holc_grade%in%'A', holc_grade_num:=1]
d[holc_grade%in%'C', holc_grade_num:=2]
d[holc_grade%in%'B', holc_grade_num:=3]
d[holc_grade%in%'D', holc_grade_num:=4]

m0 = lme4::glmer(sample_binary ~ holc_grade + 
    (1 | city_state), 
    data = d, 
    family = binomial(link = "logit"))

m1= lme4::glmer(sample_binary ~ holc_grade + 
    (holc_grade | city_state), 
    data = d, 
    family = binomial(link = "logit"))

m2= lme4::glmer(sample_binary ~ holc_grade + 
    (holc_grade_num | city_state), 
    data = d, 
    family = binomial(link = "logit"))

m1_g= lmer(sample_binary ~ holc_grade + 
    (holc_grade | city_state), 
    data = d)

m2_g= lmer(sample_binary ~ holc_grade + 
    (holc_grade_num | city_state), 
    data = d, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))    


m1p= lme4::glmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city_state), 
    data = d, 
    family = binomial(link = "logit"))

m1p_g= lmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city_state), 
    data = d, 
    )   

m2p_g= lmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city_state), 
    data = d, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )

m1_g_ml <- update(m1_g, REML = FALSE)
m1p_g_ml <- update(m1p_g, REML = FALSE)
AIC(m1_g_ml,m1p_g_ml)  

m2_g_ml <- update(m2_g, REML = FALSE)
m2p_g_ml <- update(m2p_g, REML = FALSE)
AIC(m2_g_ml,m2p_g_ml) 

dB = d[sampling_density_log > -Inf]

mB0 = lmer(log10(sampling_density) ~  holc_grade + 
    (1 | city), 
    data = dB
    ) 

mB1= lmer(log10(sampling_density) ~ holc_grade + 
    (holc_grade | city), 
    data = dB)

mB2= lmer(log10(sampling_density) ~ holc_grade + 
    (holc_grade_num | city), 
    data = dB)


mB1p= lmer(log10(sampling_density) ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city), 
    data = dB, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

mB2p= lmer(log10(sampling_density) ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city), 
    data = dB, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

mB1_ml <- update(mB1, REML = FALSE)
mB1p_ml <- update(mB1p, REML = FALSE)

AIC(mB1_ml,mB1p_ml)    

mB1p |> plot_model(type = 'diag')
mB2p |> plot_model(type = 'diag') #TODO: compare the improved diagnostics with the initial c_fe_ri, c_fe_rirs models

 ggplot(d, aes(x = holc_grade_num, y = sample_binary)) +
  geom_smooth(aes(group = city), method = 'lm', se = FALSE) +
  theme_bw(10)

 ggplot(d, aes(x = holc_grade_num, y = sampling_density)) +
  geom_smooth(aes(group = msa_NAME), method = 'lm', se = FALSE) +
  scale_y_continuous(trans = 'log10')+
  theme_bw(10)


#ggplot(d, aes(x =  log10(sampling_density))) + geom_histogram()
  
ggplot(d, aes(x =  completeness)) + geom_histogram()  
 ggplot(d, aes(x = holc_grade_num, y = completeness)) +
  geom_smooth(aes(group = msa_NAME), method = 'lm', se = FALSE) +
  theme_bw(10)



comp_pred |>
  # filter(city %in% cities[1:40]) |>
  filter(msa_NAME %in% MSAs[1:40]) ->  a

  a = data.table(comp_pred)  
  a = a[msa_NAME %in% MSAs[1:40]]


a1 = copy(a[, .(holc_grade,FRb, msa_NAME)])
setnames(a1, 'FRb', 'sampling_density')
a1[, type:='model prediction']
a1[, sampling_density := exp(sampling_density)]
a2 = copy(a[, .(holc_grade,sampling_density, msa_NAME)])
a2[, type:='linear fit to raw data']
aa = rbind(a1,a2)

aa[holc_grade%in%'A', holc_grade_num:=1]
aa[holc_grade%in%'C', holc_grade_num:=2]
aa[holc_grade%in%'B', holc_grade_num:=3]
aa[holc_grade%in%'D', holc_grade_num:=4]

ggplot(data = aa, aes(x = holc_grade_num, y = sampling_density,  col = type)) + 
  #geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~str_wrap(msa_NAME, width = 25), scales = 'free_y', ncol = 5) +
  theme_bw(10) +
  NULL


ggplot(data = a, aes(x = holc_grade, y = sampling_density)) + 
  geom_point() +
  facet_wrap(~str_wrap(msa_NAME, width = 25), scales = 'free_y', ncol = 5) +
  theme_bw(10) +
  NULL


comp_pred |>
  # filter(city %in% cities[1:40]) |>
  filter(msa_NAME %in% MSAs[1:40]) |>
  ggplot(aes(holc_grade, exp(FRb), group = msa_NAME)) +
  geom_point() +
  facet_wrap(~str_wrap(msa_NAME, width = 25), scales = 'free_y', ncol = 5) +
  # facet_wrap(~holc_grade) +
  labs(
    title = 'Predicted Sampling Density of GBIF Observations by HOLC Grade'
    , subtitle = 'batch 1: MSAs 1 - 40 in alpha-order'
    , caption = 'model accounts for NDVI at 0.34, % protectect area at 3.02%, population per km^2 at 3,103'
    , x = "Home Owners’ Loan Corporation (HOLC)"
    , y = 'Predicted Sampling Density'
  ) +
  theme_bw(10) +
  NULL


comp_pred |>
  # filter(city %in% cities[1:40]) |>
  filter(msa_NAME %in% MSAs[1:40]) ->  a

ggplot(a, aes(holc_grade, exp(FRb), group = msa_NAME)) +
  geom_point() +
  facet_wrap(~str_wrap(msa_NAME, width = 25), scales = 'free_y', ncol = 5) +
  # facet_wrap(~holc_grade) +
  labs(
    title = 'Predicted Sampling Density of GBIF Observations by HOLC Grade'
    , subtitle = 'batch 1: MSAs 1 - 40 in alpha-order'
    , caption = 'model accounts for NDVI at 0.34, % protectect area at 3.02%, population per km^2 at 3,103'
    , x = "Home Owners’ Loan Corporation (HOLC)"
    , y = 'Predicted Sampling Density'
  ) +
  theme_bw(10) +
  NULL



  ggplot(a, aes(holc_grade, sampling_density, group = msa_NAME)) +
  geom_point() +
    geom_smooth(aes(group = msa_NAME), method = 'lm', se = FALSE) +

  facet_wrap(~str_wrap(msa_NAME, width = 25), scales = 'free_y', ncol = 5) +
  # facet_wrap(~holc_grade) +
  labs(
    title = 'Predicted Sampling Density of GBIF Observations by HOLC Grade'
    , subtitle = 'batch 1: MSAs 1 - 40 in alpha-order'
    , caption = 'model accounts for NDVI at 0.34, % protectect area at 3.02%, population per km^2 at 3,103'
    , x = "Home Owners’ Loan Corporation (HOLC)"
    , y = 'Predicted Sampling Density'
  ) +
  theme_bw(10) +
  NULL



c_fe_ri   <- lme4::lmer(completeness ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + (1 | msa_NAME)                , data = comb |> filter(holc_grade != 'E'), REML = TRUE)
c_fe_rirs <- lme4::lmer(completeness ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + (1 + holc_grade| msa_NAME)    , data = comb |> filter(holc_grade != 'E'), REML = TRUE)


# does not take into account area
(
  counts_grade_year <- 
    read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') |>
    filter(holc_grade != 'E') |> 
    arrange(year, holc_grade) |> 
    group_by(year, holc_grade) |>
    count() |> 
    group_by(year, holc_grade) |> 
    summarise(cumsum = cumsum(n))
  )


  counts_grade_year |> 
  filter(year >= 2000 & year <= 2020) |> 
  ggplot(aes(year, cumsum, col = holc_grade)) + 
  geom_line()

  counts_grade_year |> 
  ggplot(aes(year, cumsum, col = holc_grade)) + 
  geom_point()

    counts_grade_year |> 
  ggplot(aes(year, cumsum, col = holc_grade)) + 
  stat_smooth()
