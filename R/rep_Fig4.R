# Load packages
require(dplyr)
require(ggplot2)

# Color palette for redlining
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)#, '#A9A9A9' # dark gray)

# copy of the L51-65 of 04_R4_uneven_biodiversity_data_2023.R, with changed folder path
  holc <- sf::st_read('Data/holc_ad_data.shp') %>% 
    sf::st_cast('POLYGON') %>% # IMPORTANT
    dplyr::filter(!sf::st_is_empty(.)) %>% 
    sf::st_make_valid(.) %>% 
    tibble::rowid_to_column() %>% 
    dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                    , city_state = paste0(city, ', ', state)
                    , area_holc_km2 = as.double(sf::st_area(.) / 1e+6)) %>% 
    dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) 

  # Calculate the area of holc polygons
  holc_area <-  holc %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(area_sum = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')  %>% as_tibble() %>% dplyr::select(-geometry)
 # gives
  #  $ holc_grade: chr [1:4] "A" "B" "C" "D"
  #  $ area_sum  : num [1:4] 1282 2948 4365 2689

#copy of the L379-430 of 04_R4_uneven_biodiversity_data_2023.R, with changed folder path
# Load 2000-2020 data
temporal_2000_2020 = read.table('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_col_code_by_holc_id_2000_2020.csv', header= T,sep=',') 
names(temporal_2000_2020) <- c('Type', 'Sum', 'holc_polygon_id')
temporal_2000_2020$holc_grade = substr(sub(".*?_", "", (sub("_.*?", "", sub("_.*?", "", temporal_2000_2020$holc_polygon_id))) ), 1,1) # 2 holc polygons need to be correctly labeled based on the previous regex. These are all HOLC B polygons
# temporal_2000_2020[which(temporal_2000_2020$holc_grade =='2'),]$holc_grade <- 'B'
temporal_2000_2020 = temporal_2000_2020 %>% filter(holc_grade  %in% c('A', 'B', 'C', 'D')) 

# A few HOLC polygons do not contain any bird observations from 2000-2020 which makes total sense
temporal_2000_2020 %>% filter(Sum > 0) %>% summarise(length(unique(holc_polygon_id)))
sum(temporal_2000_2020$Sum)  # Most of bird biodiversity data in these cities was collected from 2000-2020
# temporal_2000_2020$Sum = as.numeric(temporal_2000_2020$Sum)

# Load 1933-2022 data
temporal_trend = read.table('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv', header= T,sep=',')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(temporal_trend) <- c('Year','holc_grade', 'Sum')
temporal_trend = temporal_trend %>% filter(holc_grade != 'E')
sum(temporal_2000_2020$Sum,na.rm=T) / sum(temporal_trend$Sum,na.rm=T) # 77.8 % of biodiversity data collected in last 20 years ! 

### check START: the following should and give same results as above START
temporal_2000_2020 <-
temporal_trend %>% 
  filter(Year >= 2000 & Year <= 2020)
sum(temporal_2000_2020$Sum,na.rm=T) / sum(temporal_trend$Sum,na.rm=T)
### check END

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

tmpppp = temporal_all_data %>% group_by(holc_grade, Year) # %>% mutate(cumsum = cumsum(n_obs))

trend_A = tmpppp %>% filter(holc_grade == 'A') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_B  = tmpppp %>% filter(holc_grade == 'B') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_C  = tmpppp %>% filter(holc_grade == 'C') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_D  = tmpppp %>% filter(holc_grade == 'D') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

temporal_all_data_2 = rbind(trend_A,trend_B,trend_C,trend_D) # MaPe added 2 in the name to aid data investigation

# Plot temporal trend: 2000-2020; same as in the script, but showing legend
temporal_all_data_2 %>%
filter(Year >= 2000 & Year <= 2020) %>%
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%
ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) +
geom_line(aes(color = holc_grade), size = 1) +
#stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal) +
theme_bw(16) +
#theme(legend.position = 'none') +
ylab('Sampling density\n (bird observations per 1km²)')

# MaPe - Plot temporal trend in number of obserevations

# Plot temporal trend: 2000-2020; same as in the script, but showing legend
temporal_all_data_2 %>%
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
