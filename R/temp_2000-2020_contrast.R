# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# [7] Plot temporal trends 1933-2022 and 2000-2020 ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Load 2000-2020 data
outdir = 'original_paper/Data/Biodiv_Greeness_Social' #MaPe
temporal_2000_2020 = read.table(paste0(outdir, "/R1_biodiv_col_code_by_holc_id_2000_2020.csv"), header= T,sep=',') 
names(temporal_2000_2020) <- c('Type', 'Sum', 'holc_polygon_id')
temporal_2000_2020$holc_grade = substr(sub(".*?_", "", (sub("_.*?", "", sub("_.*?", "", temporal_2000_2020$holc_polygon_id))) ), 1,1) # 2 holc polygons need to be correctly labeled based on the previous regex. These are all HOLC B polygons
# temporal_2000_2020[which(temporal_2000_2020$holc_grade =='2'),]$holc_grade <- 'B'
temporal_2000_2020 = temporal_2000_2020 %>% dplyr::filter(holc_grade  %in% c('A', 'B', 'C', 'D')) 

# A few HOLC polygons do not contain any bird observations from 2000-2020 which makes total sense
temporal_2000_2020 %>% dplyr::filter(Sum > 0) %>% summarise(length(unique(holc_polygon_id)))
sum(temporal_2000_2020$Sum)  # Most of bird biodiversity data in these cities was collected from 2000-2020
# temporal_2000_2020$Sum = as.numeric(temporal_2000_2020$Sum)
# Load 1933-2022 data
temporal_trend = read.table(paste0(outdir, "/R1_biodiv_trend_by_time_holc_id_1933_2022.csv"), header= T,sep=',')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(temporal_trend) <- c('Year','holc_grade', 'Sum')
temporal_trend = temporal_trend %>% dplyr::filter(holc_grade != 'E')
sum(temporal_2000_2020$Sum,na.rm=T) / sum(temporal_trend$Sum,na.rm=T) # 77.8 % of biodiversity data collected in last 20 years ! 

temporal_all_data = ddply(temporal_trend, 'holc_grade', function(x){
  ddply(x, 'Year', function(z){
    
    data.frame(
      Year = unique(z$Year),
      holc_grade = unique(z$holc_grade),
      n_obs = sum(z$Sum,na.rm=T)
      #    n_obs_cum = cumsum(z$Sum)
    )
    
  })
})

tmpppp = temporal_all_data %>% group_by(holc_grade, Year) # %>% mutate(cumsum = cumsum(n_obs))

trend_A = tmpppp %>% dplyr::filter(holc_grade == 'A') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_B  = tmpppp %>% dplyr::filter(holc_grade == 'B') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_C  = tmpppp %>% dplyr::filter(holc_grade == 'C') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_D  = tmpppp %>% dplyr::filter(holc_grade == 'D') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

temporal_all_data = rbind(trend_A,trend_B,trend_C,trend_D)

# Plot temporal trend: 2000-2020
temporal_all_data %>% 
  dplyr::filter(Year >= 2000 & Year <= 2020) %>% 
  ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) + 
  geom_line(aes(color = holc_grade), size = 1) +
  scale_color_manual(values = holc_pal) +
  theme_bw(16) + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') 
NULL

ggsave('/Users/diegoellis/Desktop/temporal_biodiv_2000_2020.png'
       , width = 4.42
       , height = 5
       , dpi = 600
)

#MaPe
d = data.table(temporal_all_data)
d[Year%in%c(2000,2020) & holc_grade%in%c('A','D')]

## disparity in ration (meaning of it unclear given the similarity in 2000)
(d[Year%in%c(2020) &  holc_grade%in%c('A'), sampling_density] / d[Year%in%c(2020) &  holc_grade%in%c('D'), sampling_density])/(d[Year%in%c(2000) &  holc_grade%in%c('A'), sampling_density]/
 d[Year%in%c(2000) &  holc_grade%in%c('D'), sampling_density]) #different values than in the main text

## disparity in ration from 2010 to 2020
(d[Year%in%c(2020) &  holc_grade%in%c('A'), sampling_density] / d[Year%in%c(2020) &  holc_grade%in%c('D'), sampling_density])/(d[Year%in%c(2010) &  holc_grade%in%c('A'), sampling_density]/
 d[Year%in%c(2010) &  holc_grade%in%c('D'), sampling_density])

## check relative disparity over time
dd = d[holc_grade%in%c('A','D')]
w <- dd[order(Year),
            data.table::dcast(.SD, Year ~ holc_grade, value.var = "sampling_density")]
w[, dispar := A/D]

ggplot(w, aes(x = Year, y = dispar)) + geom_point()
ggplot(w[Year>1999 & Year<2021], aes(x = Year, y = dispar)) + geom_point() + stat_smooth()
ggplot(w[Year<2021], aes(x = Year, y = dispar)) + geom_point() + stat_smooth()
ggplot(w[Year<2021], aes(x = Year, y = dispar)) + geom_point() + stat_smooth(method='lm')
ggplot(w[Year>1999 & Year<2021], aes(x = Year, y = dispar)) + geom_point() + stat_smooth(method='lm')

ggplot(w[Year>1999 & Year<2021], aes(x = Year, y = dispar)) + 
  geom_point() + 
  stat_smooth(method='gam', formula = y ~ s(x, k = 5),method.args = list(method = "REML") )

m = gam(dispar ~ s(Year), data = w)
summary(m)



# Save as pdf with nicer ylab axis
# Plot temporal trend: 2000-2020
temporal_all_data %>%
dplyr::filter(Year >= 2000 & Year <= 2020) %>%
ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_line(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
theme(legend.position = 'none') +
ylab('Sampling density\n (bird observations per 1km²)')
NULL
ggsave('/Users/diegoellis/Desktop/temporal_biodiv_2000_2020_bw.pdf'
, width = 4.42
, height = 5
, dpi = 600
)

temporal_all_data %>% 
  dplyr::filter(Year >= 2000 & Year <= 2020) %>% 
  ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) + 
  stat_smooth(aes(color = holc_grade), size = 1, se = FALSE) +
  scale_color_manual(values = holc_pal) +
  theme_bw(16) + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') 
