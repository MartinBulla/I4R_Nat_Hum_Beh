# sampling cities
library(data.table)

# your polygon-level data.table
# dt has at least: city, year, holc_grade, sampling_density
# dt <- copy(d)  # if your object is called d

# helper for geometric mean (ignores non-finite and non-positive values)
geo_mean <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(NA_real_)
  exp(mean(log(x)))
}

# 1) geometric mean sampling density per city-year-grade
city_ratio <- d[year>1999 & year<2023, .(ratio_AD = 
                  ((geo_mean(sampling_density[holc_grade=="A"]) /
                    geo_mean(sampling_density[holc_grade=="D"])
                    ))),
                 by = .(state_city, year)
                 ]

city_ratio = city_ratio[!is.na(ratio_AD)& is.finite(ratio_AD)]

# 2) city-level summaries
city_summ <- city_ratio[order(state_city, year),
  {
    r <- ratio_AD

    # sign relative to 1 (A>D vs D>A)
    s <- ifelse(r > 1,  1L,
         ifelse(r < 1, -1L, 0L))
    s <- s[s != 0L]  # drop exact ties if any

    n_switch <- if (length(s) < 2L) 0L else sum(diff(s) != 0L)

    .(
      n_years    = .N,                             # # of usable years
      mean_ratio = mean(r, na.rm = TRUE),
      median_ratio = median(r, na.rm = TRUE),
      p_A_gt_D  = mean(r > 1, na.rm = TRUE),      # proportion of years A>D
      p_D_gt_A  = mean(r < 1, na.rm = TRUE),      # proportion of years D>A
      n_switch  = n_switch
    )
  },
  by = state_city
]

city_summ[, class := fifelse(
  n_years < 5,  
  "unclassified",  # < 5 usable years or doesn’t meet any of the below
  fifelse(
    mean_ratio > 1.3 & p_A_gt_D >= 0.7, "A-skewed", # mean(A/D) > 1.3 and A>D in ≥ 70% of years
    fifelse(
      mean_ratio < 1 / 1.3 & p_D_gt_A >= 0.7, "D-skewed", # A-skewed: mean(A/D) > 1.3 and A>D in ≥ 70% of years
      fifelse(
        n_switch >= 2 | (mean_ratio >= 1 / 1.3 & mean_ratio <= 1.3),
        "mixed", # at least 2 sign switches or 0.77 < mean(A/D) < 1.3
        "unclassified"
      )
    )
  )
)]
g_geo = 
ggplot(city_summ, aes(x = class, fill = class)) + geom_bar() + 
  coord_cartesian(ylim = c(0,90)) + 
  scale_fill_manual(values = c("#92BC6B", "#E47D67", 'grey30', 'grey60'), guide = 'none') +
  scale_y_continuous(breaks = seq(0,90, by = 30), expan = c(0.0)) + 
  labs(x ="Sampling density", y = "", subtitle = 'Based on geometric mean') + 
  theme_minimal(base_size = 8)
#ggsave('Output/within-city-skew.png', width = 8, height = 8, units = 'cm')


# ARITHMETIC mean alternative
# 1) geometric mean sampling density per city-year-grade
city_ratio_ari <- d[year>1999 & year<2023, .(ratio_AD = 
                  ((mean(sampling_density[holc_grade=="A"]) /
                    mean(sampling_density[holc_grade=="D"])
                    ))),
                 by = .(state_city, year)
                 ]

city_ratio_ari = city_ratio_ari[!is.na(ratio_AD)& is.finite(ratio_AD)]

# 2) city-level summaries
city_summ_ari <- city_ratio_ari[order(state_city, year),
  {
    r <- ratio_AD

    # sign relative to 1 (A>D vs D>A)
    s <- ifelse(r > 1,  1L,
         ifelse(r < 1, -1L, 0L))
    s <- s[s != 0L]  # drop exact ties if any

    n_switch <- if (length(s) < 2L) 0L else sum(diff(s) != 0L)

    .(
      n_years    = .N,                             # # of usable years
      mean_ratio = mean(r, na.rm = TRUE),
      median_ratio = median(r, na.rm = TRUE),
      p_A_gt_D  = mean(r > 1, na.rm = TRUE),      # proportion of years A>D
      p_D_gt_A  = mean(r < 1, na.rm = TRUE),      # proportion of years D>A
      n_switch  = n_switch
    )
  },
  by = state_city
]

city_summ_ari[, class := fifelse(
  n_years < 5,  
  "unclassified",  # < 5 usable years or doesn’t meet any of the below
  fifelse(
    mean_ratio > 1.3 & p_A_gt_D >= 0.7, "A-skewed", # mean(A/D) > 1.3 and A>D in ≥ 70% of years
    fifelse(
      mean_ratio < 1 / 1.3 & p_D_gt_A >= 0.7, "D-skewed", # A-skewed: mean(A/D) > 1.3 and A>D in ≥ 70% of years
      fifelse(
        n_switch >= 2 | (mean_ratio >= 1 / 1.3 & mean_ratio <= 1.3),
        "mixed", # at least 2 sign switches or 0.77 < mean(A/D) < 1.3
        "unclassified"
      )
    )
  )
)]

g_ari =
ggplot(city_summ_ari, aes(x = class, fill = class)) + geom_bar() + 
  coord_cartesian(ylim = c(0,90)) + 
  scale_fill_manual(values = c("#92BC6B", "#E47D67", 'grey30', 'grey60'), guide = 'none') +
  scale_y_continuous(breaks = seq(0,90, by = 30), expan = c(0.0)) + 
  labs(x ="Sampling density", y = "# of cities", subtitle = 'Based on arithmetic mean') + 
  theme_minimal(base_size = 8)
#ggsave('Output/within-city-skew_arithmetic.png', width = 8, height = 8, units = 'cm')

# COMBINE into 1st panel
city_summ_ari[, method := 'Based on arithmetic mean']
city_summ[, method := 'Based on geometric mean']
cc = rbind(city_summ_ari,city_summ )

g_cc =
ggplot(cc, aes(x = class, fill = class)) + geom_bar() + 
  scale_fill_manual(values = c("#92BC6B", "#E47D67", 'grey30', 'grey60'), guide = 'none') +
  facet_wrap(~method) + 
  labs(x ="Sampling density", y = "# of cities") + 
  theme_light()

# SECOND/THIRD panel
city_summ_ari[,ar_based := class]
city_summ_ari[,ari_mean_ratio := mean_ratio]

city_summ[,geo_based := class]
city_summ[,geo_mean_ratio := mean_ratio]

cit = merge(city_summ, city_summ_ari[,.(state_city, ar_based, ari_mean_ratio)])

g_comp = 
ggplot(cit, aes(x = ari_mean_ratio, y = geo_mean_ratio)) + 
  geom_abline(slope = 1, intercept = 0, lty = 3, col = 'red') + 
  geom_point(alpha = 0.5) +
  coord_cartesian(xlim = c(0.004, 1300), ylim = c(0.004, 1300)) + 
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), name = 'A/D ratio of arithmetic means', labels = scales::label_number(drop0trailing = TRUE), minor_breaks = minor_breaks_log10, ) + 
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000), name = 'A/D ratio of geometric means', labels = scales::label_number(drop0trailing = TRUE), minor_breaks = minor_breaks_log10, ) + 
  theme_minimal(base_size = 8)

#ggsave('Output/mean_city_compar.png', width = 7.2, height = 7, units = 'cm')  # Urban sampling bias classification depends on the metric;   

# BOTTOM example row

#cit[n_years == 23 & median_ratio>1.5]
di_A = d[state_city%in% c("MI, Detroit")] 
ex_A = 
 ggplot(di_A, aes(x = year, y = sampling_density_shifted, col = holc_grade)) + 
    geom_jitter(size = 0.5, alpha = 0.2) + 
    stat_smooth(se = FALSE, na.rm = TRUE) + 
    coord_cartesian(xlim=c(2000, 2020), ylim=c(.1, 10000))+
    scale_y_log10(
        name   = "Sampling density [km²]",
        breaks = c(0.1, 1, 10, 100, 1000, 10000),
        minor_breaks = minor_breaks_log10,      # many minor lines
        labels = c("0", "1", "10", "100", "1 000", "10 000")#labels = scales::label_number(drop0trailing = TRUE)
    ) +
    scale_x_continuous(breaks = c(2000, 2010, 2020), name = 'Year')  +
    scale_color_manual(values = holc_pal, name = 'HOLC grade') +
    labs(subtitle = 'A-skewed') + 
    theme_minimal(base_size = 8) +
    theme(
        #plot.subtitle = element_text(size = 10, colour = "grey40"),
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        legend.position=c(.85,.8),
        strip.background = element_blank()
    )  

#cit[n_years == 23 & median_ratio<2/3]
di_D = d[state_city%in% c("TX, Galvest")] 
ex_D = 
 ggplot(di_D, aes(x = year, y = sampling_density_shifted, col = holc_grade)) + 
    geom_jitter(size = 0.5, alpha = 0.2) + 
    stat_smooth(se = FALSE, na.rm = TRUE) + 
    coord_cartesian(xlim=c(2000, 2020), ylim=c(.1, 10000))+
    scale_y_log10(
        name   = "",
        breaks = c(0.1, 1, 10, 100, 1000, 10000),
        minor_breaks = minor_breaks_log10,      # many minor lines
        labels = c("0", "1", "10", "100", "1 000", "10 000")#labels = scales::label_number(drop0trailing = TRUE)
    ) +
    scale_x_continuous(breaks = c(2000, 2010, 2020), name = 'Year')  +
    scale_color_manual(values = holc_pal, guide = 'none') +
    labs(subtitle = 'D-skewed') + 
    theme_minimal(base_size = 8) +
    theme(
        #plot.subtitle = element_text(size = 10, colour = "grey40"),
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        #legend.position=c(.85,.85),
        strip.background = element_blank()
    )  

#cit[n_years == 23 & median_ratio>.75 & median_ratio<1.33]
di_m = d[state_city%in% c("MD, Baltimo")] 
ex_mix = 
 ggplot(di_m, aes(x = year, y = sampling_density_shifted, col = holc_grade)) + 
    geom_jitter(size = 0.5, alpha = 0.2) + 
    stat_smooth(se = FALSE, na.rm = TRUE) + 
    coord_cartesian(xlim=c(2000, 2020), ylim=c(.1, 10000))+
    scale_y_log10(
        name   = "",
        breaks = c(0.1, 1, 10, 100, 1000, 10000),
        minor_breaks = minor_breaks_log10,      # many minor lines
        labels = c("0", "1", "10", "100", "1 000", "10 000")#labels = scales::label_number(drop0trailing = TRUE)
    ) +
    scale_x_continuous(breaks = c(2000, 2010, 2020), name = 'Year')  +
    scale_color_manual(values = holc_pal, guide = 'none') +
    labs(subtitle = 'Mixed') + 
    theme_minimal(base_size = 8) +
    theme(
        #plot.subtitle = element_text(size = 8, colour = "grey40"),
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        #legend.position=c(.85,.85),
        strip.background = element_blank()
    )  

# EXPORT
top = ((g_ari | g_geo) + plot_layout(axis_titles = "collect") ) | g_comp
bottom = (ex_A | ex_mix | ex_D) + plot_layout(axis_titles = "collect_x")  
ggsave('Output/Fig_SX2_examples.png', top / bottom,  width = 20, height = 12.5, units = 'cm')
top / bottom

# our classification
A-skewed could be: "CA, Oakland", "MA, Boston", "NY, Manhatt", "PA, Philade" 
Mixed (tough): "MN, Minneap", "OR, Portland", "TN, Chattan", 
D-skewed: "CA, San Fra", "MI, Detroit", "NJ, Bergen", "NY, Staten"


city_summ_ari[state_city%in%c("CA, Oakland", "MA, Boston", "NY, Manhatt", "PA, Philade")]    
city_summ_ari[state_city%in%c("MN, Minneap", "OR, Portlan", "TN, Chattan")]    
city_summ_ari[state_city%in%c("CA, San Fra", "MI, Detroit", "NJ, Bergen ", "NY, Staten ")]   

sThe plot reveals metric instability in city-level summaries, not a consistent inflation or deflation


cit[state_city%in%c("CA, Oakland", "MA, Boston", "NY, Manhatt", "PA, Philade"), martin :='A-skewed']    
cit[state_city%in%c("MN, Minneap", "OR, Portlan", "TN, Chattan"), martin :='mixed']    
cit[state_city%in%c("CA, San Fra", "MI, Detroit", "NJ, Bergen ", "NY, Staten "), martin :='D-skewed']    
print(city_summ, nrow = 178)

xx = cit[state_city%in%c("CA, Oakland", "MA, Boston", "NY, Manhatt", "PA, Philade","MN, Minneap", "OR, Portlan", "TN, Chattan", "CA, San Fra", "MI, Detroit", "NJ, Bergen ", "NY, Staten ")]
xx[order(martin)]


cit[n_years == 23 & geo_based == 'D-skewed'] 
cit[n_years == 23 & geo_based == 'mixed' & ar_based == "mixed" ] 
cit[n_years == 23 & geo_based == 'mixed' & ar_based == "mixed" ] 
crex = city_ratio[state_city%in%c("MI, Detroit",  "MD, Baltimo", "TX, Galvest",)]
crex[, dispar := 100*(ratio_AD-1)]
crex[state_city%in%c("MI, Detroit"), disparity := paste('A-skewed;', state_city)]
crex[state_city%in%c("MD, Baltimo"), disparity := paste('mixed;', state_city)]
crex[state_city%in%c("TX, Galvest"), disparity := paste('D-skewed;', state_city)]

ggplot(crex, aes(x = year, y = dispar, col = disparity, fill=disparity)) +
stat_smooth() + 
geom_point() +
labs(subtitle = 'Relative disparity in geometric mean sampling density', y = 'Percentage difference [A relative to D]') +
theme_light()
ggsave('Output/dispar_city_examples.png', width = 13, height = 8, units = 'cm')  


# TEST MEDIAN vs MEAN

ggplot(cit, aes(x = median_ratio, y = geo_mean_ratio)) + 
  geom_abline(slope = 1, intercept = 0, lty = 3, col = 'red') + 
  geom_point(alpha = 0.5) +
  coord_cartesian(xlim = c(0.004, 1300), ylim = c(0.004, 1300)) + 
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), name = 'A/D ratio of median', labels = scales::label_number(drop0trailing = TRUE), minor_breaks = minor_breaks_log10, ) + 
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000), name = 'A/D ratio of geometric means', labels = scales::label_number(drop0trailing = TRUE), minor_breaks = minor_breaks_log10, ) + 
  theme_minimal(base_size = 8)



# MEDIAN check
# 1) geometric mean sampling density per city-year-grade
city_ratio_median <- d[year>1999 & year<2023, .(ratio_AD = 
                  ((median(sampling_density[holc_grade=="A"]) /
                    median(sampling_density[holc_grade=="D"])
                    ))),
                 by = .(state_city, year)
                 ]

city_ratio_median = city_ratio_median[!is.na(ratio_AD)& is.finite(ratio_AD)]

# 2) city-level summaries
city_summ_median <- city_ratio_median[order(state_city, year),
  {
    r <- ratio_AD

    # sign relative to 1 (A>D vs D>A)
    s <- ifelse(r > 1,  1L,
         ifelse(r < 1, -1L, 0L))
    s <- s[s != 0L]  # drop exact ties if any

    n_switch <- if (length(s) < 2L) 0L else sum(diff(s) != 0L)

    .(
      n_years    = .N,                             # # of usable years
      mean_ratio = mean(r, na.rm = TRUE),
      median_ratio = median(r, na.rm = TRUE),
      p_A_gt_D  = mean(r > 1, na.rm = TRUE),      # proportion of years A>D
      p_D_gt_A  = mean(r < 1, na.rm = TRUE),      # proportion of years D>A
      n_switch  = n_switch
    )
  },
  by = state_city
]

city_summ_median[, class := fifelse(
  n_years < 5,  
  "unclassified",  # < 5 usable years or doesn’t meet any of the below
  fifelse(
    mean_ratio > 1.3 & p_A_gt_D >= 0.7, "A-skewed", # mean(A/D) > 1.3 and A>D in ≥ 70% of years
    fifelse(
      mean_ratio < 1 / 1.3 & p_D_gt_A >= 0.7, "D-skewed", # A-skewed: mean(A/D) > 1.3 and A>D in ≥ 70% of years
      fifelse(
        n_switch >= 2 | (mean_ratio >= 1 / 1.3 & mean_ratio <= 1.3),
        "mixed", # at least 2 sign switches or 0.77 < mean(A/D) < 1.3
        "unclassified"
      )
    )
  )
)]

g_med = 
ggplot(city_summ_median, aes(x = class, fill = class)) + geom_bar() + 
  coord_cartesian(ylim = c(0,90)) + 
  scale_fill_manual(values = c("#92BC6B", "#E47D67", 'grey30', 'grey60'), guide = 'none') +
  scale_y_continuous(breaks = seq(0,90, by = 30), expan = c(0.0)) + 
  labs(x ="Sampling density", y = "", subtitle = 'Based on median') + 
  theme_minimal(base_size = 8)
#ggsave('Output/within-city-skew_median_based.png', width = 8, height = 8, units = 'cm')


city_summ_median[,median_based := class]
city_summ_median[,median_based_ratio := mean_ratio]

city_summ[,geo_based := class]
city_summ[,geo_mean_ratio := mean_ratio]

citm = merge(city_summ, city_summ_median[,.(state_city, median_based, median_based_ratio)])

citm[, median_based_ratio_shifted := median_based_ratio + 0.1]

g_comp_med = 
ggplot(citm, aes(x = median_based_ratio_shifted, y = geo_mean_ratio)) + 
  geom_abline(slope = 1, intercept = 0, lty = 3, col = 'red') + 
  geom_point(alpha = 0.5) +
  coord_cartesian(xlim = c(0.004, 1300), ylim = c(0.004, 1300)) + 
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), name = 'A/D ratio of median', labels = c("0", "1", "10", "100", "1 000")) +#labels = scales::label_number(drop0trailing = TRUE), minor_breaks = minor_breaks_log10, ) + 
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000), name = 'A/D ratio of geometric means', labels = scales::label_number(drop0trailing = TRUE), minor_breaks = minor_breaks_log10, ) + 
  theme_minimal(base_size = 8)
#ggsave('Output/median-geom-mean_city_compar.png', width = 7.2, height = 7, units = 'cm')  # Urban sampling bias classification depends on the metric;  


# MEDIAN
## relative
med_den <- d[, .(median_density = median(sampling_density)),
              by = .(year, holc_grade)]
w_med_den <- med_den[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "median_density")]

w_med_den[, dispar := 100*((A/D)-1)]
g33a = ggplot(w_int_log[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "Median sampling density", y ='Disparity in A relative to D [%]') + theme_light() #TODO decide which heading to use "Relative geometric-mean sampling density (A/D)"
g33b = ggplot(w_int_log, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + theme_light()

## absolute
w_med_den[, diff_density := A - D]
g33a_ = ggplot(w_int_log[year>1999 & year<2021], aes(x = year, y = diff_density)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light() #TODO decide which heading to use "Relative geometric-mean sampling density (A/D)"
g33b_ = ggplot(w_int_log, aes(x = year, y = diff_density)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = '') + theme_light()

left = ((g33a|g33b) + plot_layout(axis_titles = "collect")) 
right = ((g33a_|g33b_)+ plot_layout(axis_titles = "collect"))
left | right  + plot_layout(axis_titles = "collect")

left <- (
  g33a | g33b 
) + 
  plot_layout(ncol = 2, nrow = 1,
              axis_titles = "collect")  
right <- (
  g33a_ | g33b_ 
) + 
  plot_layout(ncol = 2, nrow = 1,
              axis_titles = "collect") 

(left |  plot_spacer() | right) +
  plot_layout(
    widths      = c(1,1, 0.1, 1, 1),
    nrow =1, ncol = 5,
    axis_titles = "collect_x"   # or "collect" if you want y merged when possible
  )
