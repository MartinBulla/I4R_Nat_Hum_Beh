require(data.table)
# our check

# Load 1933-2022 data
temporal_trend = read.table('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv', header= T,sep=',')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(temporal_trend) <- c('Year','holc_grade', 'Sum')
temporal_trend = temporal_trend %>% filter(holc_grade != 'E')

temporal_all_data = plyr::ddply(temporal_trend, 'holc_grade', function(x){
  plyr::ddply(x, 'Year', function(z){
    data.frame(
      Year = unique(z$Year),
      holc_grade = unique(z$holc_grade),
      n_obs = sum(z$Sum,na.rm=T)
      #    n_obs_cum = cumsum(z$Sum)
    )
    
  })
})

# replicate their outputs with data.table
tt = fread('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
tt = fread('Data/from_script_04/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(tt) <- c('Year','holc_grade', 'Sum')


tt = tt[holc_grade != 'E'] #d = data.table(temporal_trend)
d = tt[, .(n_obs = sum(Sum)), by = list(Year, holc_grade)]
d = d[order(holc_grade,Year)]

# add holc grade
holc_area_dt = data.table(holc_area)
dh = merge(d,holc_area_dt, all.x = TRUE)
dh[, sampling_density := n_obs/area_sum]
dd = dh[Year >= 2000 & Year <= 2020]
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%

# number of observations
ggplot(dd,aes(x = Year, y = n_obs), fill = holc_grade) +
geom_point(aes(color = holc_grade)) +
geom_line(aes(color = holc_grade), size = 1) +
#stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
#theme(legend.position = 'none') +
ylab('# of observations')

ggplot(dd,aes(x = Year, y = n_obs), fill = holc_grade) +
geom_point(aes(color = holc_grade)) +
#geom_line(aes(color = holc_grade), size = 1) +
stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
#theme(legend.position = 'none') +
ylab('# of observations')

# density of observations per km2
ggplot(dd,aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) +
geom_line(aes(color = holc_grade), size = 1) +
#stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
#theme(legend.position = 'none') +
ylab('Sampling density\n (bird observations per 1km²)')

ggplot(dd,aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) +
#geom_line(aes(color = holc_grade), size = 1) +
stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
#theme(legend.position = 'none') +
ylab('Sampling density\n (bird observations per 1km²)')

# plot differences
b <- dcast(dh, Year ~ holc_grade, value.var = "sampling_density")

ggplot(b[Year >= 2000 & Year <= 2020], aes(D,A)) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
    geom_point(aes(col = Year)) + stat_smooth(method ='lm') +
    geom_text(aes(label = Year), vjust = -0.6, size = 3, check_overlap = TRUE) + expand_limits(x = 300)

ggplot(b[Year >= 2000 & Year <= 2020], aes(D,B)) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
    geom_point(aes(col = Year)) + stat_smooth(method ='lm') +
    geom_text(aes(label = Year), vjust = -0.6, size = 3, check_overlap = TRUE) + expand_limits(x = 300)


# all pairwise combinations you want
library(ggrepel)

pairs <- data.table(
  x = c("D","D","D","C","C","B"),
  y = c("A","B","C","A","B","A")
)

# join to create plotting dataset
plotdat <- pairs[, .(x, y)][
  , merge(dh[holc_grade==x], dh[holc_grade==y], by="Year", suffixes=c(".x",".y")), 
  by=.(x,y)
][, pair := paste0(y, " ~ ", x)]

ggplot(plotdat[Year >= 2000 & Year <= 2020], aes(sampling_density.x, sampling_density.y)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  geom_smooth(method="lm", col = 'red') +
  geom_point(aes(col=Year), show.legend = FALSE) +
  geom_text(aes(label=Year), vjust=-0.6, size=2.5, check_overlap=TRUE) +
  scale_x_continuous(breaks=seq(0,300,50), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,300,50), expand = c(0,0)) +
  coord_cartesian(xlim=c(0,300), ylim=c(0,300)) +  
  facet_wrap(~pair, scales="fixed") +
  labs(x="Sampling density for x [km^2]", y="Sampling density for y [km^2]")+
  theme_light()+
  theme(
    strip.background = element_blank(),       # remove grey panel background
    strip.text = element_text(color = "black") # make labels black
  )


### OLD

# MaPe - Plot temporal trend in number of obserevations
temporal_all_data %>%
filter(Year >= 2000 & Year <= 2020) %>%
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%
ggplot(aes(x = Year, y = n_obs), fill = holc_grade) +
geom_point(aes(color = holc_grade)) +
geom_line(aes(color = holc_grade), size = 1) +
#stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
#theme(legend.position = 'none') +
ylab('# of observations')

temporal_all_data %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  ggplot(aes(x = Year, y = cumsum_n_obs), fill = holc_grade) + 
  geom_line(aes(color = holc_grade), size = 1) +
  geom_point(aes(color = holc_grade)) +
  scale_color_manual(values = holc_pal) +
  theme_bw(16) + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') 
Its y-axis title indicates sampling density, but the data does not. CONTINUE SHOWING HOW WE FOUND OUT. 

