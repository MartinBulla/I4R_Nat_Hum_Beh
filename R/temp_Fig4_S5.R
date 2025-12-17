# prepare authors' data
a = tta[holc_grade%in%c('A','D')]
aw <- a[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density")]
aw[, dispar := 100*((A/D)-1)] #aw[, dispar := A/D]

# prepare our data
dd_ = dd[holc_grade%in%c('A','D')]
w <- dd_[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density")]
w[, dispar := 100*((A/D)-1)] #w[, dispar := A/D]


# check whether our and author aggregation gives same answers it does
  g1a_ = ggplot(aw[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red') + labs(subtitle = "Author's aggregation; sum per year") + theme_light() 
  g1b_ = ggplot(aw, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_light()
    #(g1a|g1b) + plot_layout(axis_titles = "collect")

  g2a_ = ggplot(w[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "Our aggregation; sum per year") + theme_light()
  g2b_ = ggplot(w, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_light()
  #(g2a|g2b) + plot_layout(axis_titles = "collect")

#((g1a_|g1b_) / (g2a_|g2b_))+ plot_layout(axis_titles = "collect")

# PLOT

# TREND - year aggregates 
trim_to = quantile(d[sampling_density > 0 & year>1999 & year<2021, sampling_density],  probs = .995)
col_all = 'black'
col_trim = 'red'
# OVERALL DENSITY - sums/area - as authors 
sums <- d[holc_grade%in%c('A','D'), 
              .(sum_bird_obs = sum(sum_bird_obs)), 
              by = .(year, holc_grade)]
dens =  holc_area_sum_a_dt[sums, on = "holc_grade"] 
dens[, sampling_density := sum_bird_obs/sum_area_holc_km2]
dens_w <- dens[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density")]

## relative
dens_w[, dispar := 100*((A/D)-1)]

g1a = ggplot(dens_w[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5) + labs(subtitle = "Overall sampling density", y ='Disparity in A relative to D [%]') + theme_light()
g1b = ggplot(dens_w[year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

x_pos_AD <- 2002

g1c = ggplot() + 
  geom_point(data = dens_w[year>1999 & year<2021], aes(x = year, y = A), col = holc_pal[1], alpha = 0.5) + 
  geom_point(data = dens_w[year>1999 & year<2021], aes(x = year, y = D), col = holc_pal[4], alpha = 0.5) + 
  annotate("text", x = x_pos_AD, y = Inf, 
           label = "A", 
           hjust = 1, vjust = 2.5, 
           color = holc_pal[1], size = 3.2) +
  annotate("text", x = x_pos_AD, y = Inf, 
           label = "B", 
           hjust = 1, vjust = 5, 
           color = holc_pal[4], size = 3.2) + 
  labs(subtitle = "", y = 'Raw aggregated values') + theme_light()

## absolute
dens_w[, diff := A - D]

g1a_ = ggplot(dens_w[year>1999 & year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light()
g1b_ = ggplot(dens_w[year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

#((g1a_|g1b_) / (g1a|g1b))+ plot_layout(axis_titles = "collect")

# MEAN DENSITY - sums/area - as authors 
## relative
d_mean = d[holc_grade%in%c('A','D'), 
              .(sampling_density_mean = mean(sampling_density)), 
              by = .(year, holc_grade)]
w_mean <- d_mean[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density_mean")]
w_mean[, dispar := 100*((A/D)-1)]

d_mean_trim = d[sampling_density < trim_to & holc_grade%in%c('A','D'), 
              .(sampling_density_mean = mean(sampling_density)), 
              by = .(year, holc_grade)]
w_mean_trim <- d_mean_trim[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density_mean")]
w_mean_trim[, dispar := 100*((A/D)-1)]

x_pos <- max(w_mean$year[w_mean$year < 2021]) - 0.1

g2a = ggplot(w_mean[year>1999 & year<2021], aes(x = year, y = dispar)) + 
  stat_smooth(col = col_all, lwd = 0.5)+ 
  stat_smooth(data =  w_mean_trim[year>1999 & year<2021], aes(x = year, y = dispar), 
    col = 'red', fill = 'red', lty = 3, lwd = 0.5)+ 
  geom_point() + 
  geom_point(data = w_mean_trim[year>1999 & year<2021], 
    aes(x = year, y = dispar), col = 'red', cex = 0.5) +  
  labs(subtitle = "Mean sampling density", y ='Disparity in A relative to D [%]') + 
  theme_light() +
  annotate("text", x = x_pos, y = Inf, 
           label = "All data", 
           hjust = 1, vjust = 2.5, 
           color = col_all, size = 3.2) +
  annotate("text", x = x_pos, y = Inf, 
           label = "Top 0.5% trimmed", 
           hjust = 1, vjust = 14, 
           color = "red", size = 3.2)

g2b = ggplot(w_mean[year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

g2c = ggplot() + 
  geom_point(data = w_mean[year>1999 & year<2021], aes(x = year, y = A), col = holc_pal[1], alpha = 0.5) + 
  #stat_smooth(data = w_mean, aes(x = year, y = A), col = holc_pal[1], fill = holc_pal[1], lwd = 0.5)+ 
  geom_point(data = w_mean[year>1999 & year<2021], aes(x = year, y = D), col = holc_pal[4], alpha = 0.5) + 
  #stat_smooth(data = w_mean, aes(x = year, y = D), col = holc_pal[4], fill = holc_pal[4], lwd = 0.5)+ 
  labs(subtitle = "", y = 'Raw aggregated values') +  theme_light()

## absolute
w_mean[, diff_density := A - D]
g2a_ = ggplot(w_mean[year>1999 & year<2021], aes(x = year, y = diff_density)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5) + labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light() 
g2b_ = ggplot(w_mean[year<2021], aes(x = year, y = diff_density)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light() 

# In early years the A–D contrast was ~100% but corresponded to only ~10 vs 20 observations (or tiny density differences), whereas in recent years the relative contrast is similar but absolute sampling is orders of magnitude higher

# GEOMETRIC MEAN
# mean(log(sampling_intensity)) for non zero data; reduces the influence of extreme high values (typical sampling density among sampled polygons, ignoring extreme outliers)
## relative
int_log <- d[sampling_density > 0,
             .(mean_log_density = mean(log(sampling_density))),
             by = .(year, holc_grade)]
int_log[, geomean_density := exp(mean_log_density)]
w_int_log <- int_log[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "geomean_density")]

w_int_log[, dispar := 100*((A/D)-1)]

int_log_trim <- d[sampling_density > 0 & sampling_density < trim_to,
             .(mean_log_density = mean(log(sampling_density))),
             by = .(year, holc_grade)]
int_log_trim[, geomean_density := exp(mean_log_density)]
w_int_log_trim <- int_log_trim[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "geomean_density")]

w_int_log_trim[, dispar := 100*((A/D)-1)] # not used ad nearly identical

g3a = ggplot(w_int_log[year>1999 & year<2021], aes(x = year, y = dispar)) + 
  stat_smooth(col = col_all, lwd = 0.5)+ 
  #stat_smooth(data =  w_int_log_trim[year>1999 & year<2021], aes(x = year, y = dispar), col = 'red', fill = 'red', lty = 3, lwd = 0.5)+ 
  geom_point() + 
  #geom_point(data = w_int_log_trim[year>1999 & year<2021], aes(x = year, y = dispar), col = 'red', cex = 0.5) +   
  labs(subtitle = "Geometric mean sampling density of sampled polygons", y ='Disparity in A relative to D [%]') + 
  theme_light() #TODO decide which heading to use "Relative geometric-mean sampling density (A/D)"

g3b = ggplot(w_int_log[year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

g3c = ggplot() + 
  geom_point(data = w_int_log[year>1999 & year<2021], aes(x = year, y = A), col = holc_pal[1], alpha = 0.5) + 
  geom_point(data = w_int_log[year>1999 & year<2021], aes(x = year, y = D), col = holc_pal[4], alpha = 0.5) + 
  labs(subtitle = "", y = 'Raw aggregated values') +  theme_light()

## absolute
w_int_log[, diff_density := A - D]
g3a_ = ggplot(w_int_log[year>1999 & year<2021], aes(x = year, y = diff_density)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light() #TODO decide which heading to use "Relative geometric-mean sampling density (A/D)"
g3b_ = ggplot(w_int_log[year<2021], aes(x = year, y = diff_density)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

#((g6a|g6b) / (g6a_|g6b_))+ plot_layout(axis_titles = "collect")

# COVERAGE
coverage <- d[holc_grade%in%c('A','D'), 
              .(prop_sampled = mean(sampling_density > 0)), 
              by = .(year, holc_grade)]
         
w_coverage <- coverage[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "prop_sampled")]

## relative
w_coverage[, dispar := 100*((A/D)-1)]

g4a = ggplot(w_coverage[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "Proportion of sampled polygons", y ='Disparity in A relative to D [%]') + theme_light() 
g4b = ggplot(w_coverage[year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()
g4c = ggplot() + 
  geom_point(data = w_coverage[year>1999 & year<2021], aes(x = year, y = A), col = holc_pal[1], alpha = 0.5) + 
  geom_point(data = w_coverage[year>1999 & year<2021], aes(x = year, y = D), col = holc_pal[4], alpha = 0.5) + 
  labs(subtitle = "", y = 'Raw aggregated values') +  theme_light()

## absolute
w_coverage[, diff := A -D]

g4a_ = ggplot(w_coverage[year>1999 & year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light() 
g4b_ = ggplot(w_coverage[year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

# INTENSITY
intensity <- d[sampling_density > 0 & holc_grade%in%c('A','D'), 
               .(mean_density_nonzero = mean(sampling_density)), 
               by = .(year, holc_grade)]

w_intensity <- intensity[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "mean_density_nonzero")]

intensity_trim <- d[sampling_density > 0 & sampling_density < trim_to & holc_grade%in%c('A','D'), 
               .(mean_density_nonzero = mean(sampling_density)), 
               by = .(year, holc_grade)]

w_intensity_trim <- intensity_trim[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "mean_density_nonzero")]

## relative
w_intensity[, dispar := 100*((A/D)-1)]
w_intensity_trim[, dispar := 100*((A/D)-1)]

g5a = ggplot(w_intensity[year>1999 & year<2021], aes(x = year, y = dispar)) + 
  stat_smooth(col = col_all, lwd = 0.5)+ 
  stat_smooth(data =  w_intensity_trim[year>1999 & year<2021], aes(x = year, y = dispar), 
    col = 'red', fill = 'red', lty = 3, lwd = 0.5)+ 
  geom_point() + 
  geom_point(data = w_intensity_trim[year>1999 & year<2021], 
    aes(x = year, y = dispar), col = 'red', cex = 0.5) +  
  labs(subtitle = "Mean sampling density across sampled polygons", y ='Disparity in A relative to D [%]') + 
  theme_light() 
g5b = ggplot(w_intensity[year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y ="") + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()
g5c = ggplot() + 
  geom_point(data = w_intensity[year>1999 & year<2021], aes(x = year, y = A), col = holc_pal[1], alpha = 0.5) + 
  geom_point(data = w_intensity[year>1999 & year<2021], aes(x = year, y = D), col = holc_pal[4], alpha = 0.5) + 
  labs(subtitle = "", y = 'Raw aggregated values') +  theme_light()

## absolute
w_intensity[, diff := A -D]

g5a_ = ggplot(w_intensity[year>1999 & year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light() 
g5b_ = ggplot(w_coverage[year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

# EFFECTIVE SAMPLING - proove or principle - not plotted (same as mean)
sum_eff <- merge(coverage, intensity, by = c("year","holc_grade"))
sum_eff[, effective_density := prop_sampled * mean_density_nonzero]

w_sum_eff <- sum_eff[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "effective_density")]

## relative
w_sum_eff[, dispar := 100*((A/D)-1)]

g6a = ggplot(w_sum_eff[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "Effective sampling density across sampled polygons", y ='Disparity in A relative to D [%]') + theme_light() 
g6b = ggplot(w_sum_eff[year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()
g6c = ggplot() + 
  geom_point(data = w_sum_eff[year>1999 & year<2021], aes(x = year, y = A), col = holc_pal[1], alpha = 0.5) + 
  geom_point(data = w_sum_eff[year>1999 & year<2021], aes(x = year, y = D), col = holc_pal[4], alpha = 0.5) + 
  labs(subtitle = "", y = 'Raw aggregated values') +  theme_light()

## absolute
w_sum_eff[, diff := A -D]
g6a_ = ggplot(w_sum_eff[year>1999 & year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+  labs(subtitle = "", y = 'Absolute disparity A minus D [observations/km2]') + theme_light() 
g6b_ = ggplot(w_sum_eff[year<2021], aes(x = year, y = diff)) + geom_point() + stat_smooth(col = col_all, lwd = 0.5)+ labs(subtitle = "", y = '') + 
scale_x_continuous(breaks = seq(1940,2020, length.out = 5)) + theme_light()

# COMBINE for Fig. 4

left <- (
  g1a | g1b |
  g2a | g2b |
  g4a | g4b |
  g5a | g5b 
) + 
  plot_layout(ncol = 2, nrow = 4,
              axis_titles = "collect")  
right <- (
  g1c | 
  g2c |
  g4c | 
  g5c 
) +
  plot_layout(ncol = 1,nrow = 4,
              axis_titles = "collect") 

g_F4 = (left | plot_spacer() | right) +
  plot_layout(
    widths      = c(1, 0.1, 0.4),
    axis_titles = "collect_x"   # or "collect" if you want y merged when possible
  )
ggsave('Output/Fig_F4_v2.png', g_F4, units = 'cm', width = 18.75, height = 20)
 #ggsave('Output/Fig_4.png', left, units = 'cm', width = 12.5, height = 20)

# COMBINE for Fig. S5
left <- (
  g1a | g1b |
  g2a | g2b |
  g4a | g4b |
  g5a | g5b |
  g3a | g3b 
) + 
  plot_layout(ncol = 2, nrow = 5,
              axis_titles = "collect")  
middle <- (
  g1a_ | g1b_ |
  g2a_ | g2b_ |
  g4a_ | g4b_ |
  g5a_ | g5b_ |
  g3a_ | g3b_ 
) +
  plot_layout(ncol = 2,nrow = 5,
              axis_titles = "collect") 

right <- (
  g1c | 
  g2c |
  g4c | 
  g5c | 
  g3c 
) +
  plot_layout(ncol = 1,nrow = 5,
              axis_titles = "collect") 

g_all_SX = (left | plot_spacer() | middle | plot_spacer() | right) +
  plot_layout(
    widths      = c(1, 0.1, 1, 0.1, 0.4),
    axis_titles = "collect_x"   # or "collect" if you want y merged when possible
  )
ggsave('Output/Fig_S5_text.png', g_all_SX, units = 'cm', width = 31, height = 25)
