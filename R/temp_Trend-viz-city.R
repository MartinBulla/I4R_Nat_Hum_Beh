
x = unique(d$city_state)
di = d[city_state%in%x[1:50]]

 tr_des = 
 ggplot(di[year>1999 & year<2021], aes(x = year, y = sampling_density, col = holc_grade)) + 
    geom_jitter(size = 0.5) + 
    stat_smooth(se = FALSE) + 
    facet_wrap(~city_state, ncol = 10) + 
    scale_y_continuous(trans = 'log10', name = 'Sampling density [km²]') + 
    #scale_y_continuous(name = 'Sampling density [km²]') + 
    scale_color_manual(values = holc_pal, name = 'HOLC grade') +
    #scale_fill_manual(values = holc_pal, name = 'HOLC grade') +
    theme_minimal(base_size = 8) +
    theme(
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        strip.background = element_blank(),
    )  
ggsave('Output/rev_trend-raw_a.png', tr_des, units = 'cm', width = 20, height = 25)

ggplot(di[year>1999 & year<2021], aes(x = year, y = sampling_density, col = holc_grade, fill = holc_grade)) + 
    stat_smooth(se = FALSE) + 
    #facet_wrap(~city_state) + 
    scale_y_continuous(trans = 'log') + 
    scale_color_manual(values = holc_pal, name = 'HOLC grade') +
    scale_fill_manual(values = holc_pal, name = 'HOLC grade') +
    theme_minimal(base_size = 8) +
    theme(
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        strip.background = element_blank(),
    )

  ggplot(di[year>1999 & year<2021], aes(x = year, y = sampling_density, col = holc_grade)) + 
    stat_smooth(se = FALSE, aes(group = id2)) + 
    #facet_wrap(~city_state) + 
    scale_y_continuous(trans = 'log10', name = 'Sampling density [km²]') + 
    #scale_y_continuous(name = 'Sampling density [km²]') + 
    scale_color_manual(values = holc_pal, name = 'HOLC grade') +
    #scale_fill_manual(values = holc_pal, name = 'HOLC grade') +
    theme_minimal(base_size = 8) +
    theme(
        legend.key.height = unit(0.25, "cm"),  # reduce vertical spacing between items 
        strip.background = element_blank(),
    )  
