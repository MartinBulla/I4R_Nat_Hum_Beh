library(dplyr)
library(sf)
library(ggplot2)
library(plyr)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load Holc Polygons from the Mapping Inequality project form the University of richmond
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

holc <- st_read('C:/Users/petom/OneDrive/Desktop/Published articles/Impactove/2025/Nature Human Behavior reproducibility project/Data/shapefile/shapefile/holc_ad_data.shp') %>% 
  sf::st_cast('POLYGON') %>% # IMPORTANT
  dplyr::filter(!st_is_empty(.)) %>% 
  sf::st_make_valid(.) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                  , city_state = paste0(city, ', ', state)
                  , area_holc_km2 = as.double(st_area(.) / 1e+6)) %>% 
  dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) 

# Calculate the area of holc polygons
holc_area <-  holc %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(area_sum = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')  %>% as_tibble() %>% dplyr::select(-geometry)

# List all .Rdata files in our input folder that contain bird biodiversity data:
#aves_obs = (list.files('/Users/diegoellis/Desktop/HOLC_newest/Download_GBIF_HOLC', pattern = 'Aves_all_observations.Rdata', full.names = T))

# Color palette for redlining
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)#, '#A9A9A9' # dark gray)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# [7] Plot temporal trends 1933-2022 and 2000-2020 ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Load 2000-2020 data
temporal_2000_2020 <- read.table("C:/Users/petom/OneDrive/Desktop/Published articles/Impactove/2025/Nature Human Behavior reproducibility project/Data/Biodiv_Greeness_Social/R1_biodiv_col_code_by_holc_id_2000_2020.csv",
                                 header = TRUE,sep = ",")
names(temporal_2000_2020) <- c('Type', 'Sum', 'holc_polygon_id')
temporal_2000_2020$holc_grade = substr(sub(".*?_", "", (sub("_.*?", "", sub("_.*?", "", temporal_2000_2020$holc_polygon_id))) ), 1,1) # 2 holc polygons need to be correctly labeled based on the previous regex. These are all HOLC B polygons
# temporal_2000_2020[which(temporal_2000_2020$holc_grade =='2'),]$holc_grade <- 'B'
library(dplyr)  # ensure
temporal_2000_2020 <- dplyr::filter(temporal_2000_2020,
                                    .data$holc_grade %in% c("A","B","C","D")) 

# A few HOLC polygons do not contain any bird observations from 2000-2020 which makes total sense
temporal_2000_2020 %>% filter(Sum > 0) %>% summarise(length(unique(holc_polygon_id)))
sum(temporal_2000_2020$Sum)  # Most of bird biodiversity data in these cities was collected from 2000-2020
# temporal_2000_2020$Sum = as.numeric(temporal_2000_2020$Sum)
# Load 1933-2022 data
temporal_trend = read.table("C:/Users/petom/OneDrive/Desktop/Published articles/Impactove/2025/Nature Human Behavior reproducibility project/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv",
                            header = TRUE,sep = ",")
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(temporal_trend) <- c('Year','holc_grade', 'Sum')
temporal_trend = temporal_trend %>% filter(holc_grade != 'E')
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

trend_A = tmpppp %>% filter(holc_grade == 'A') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_B  = tmpppp %>% filter(holc_grade == 'B') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_C  = tmpppp %>% filter(holc_grade == 'C') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_D  = tmpppp %>% filter(holc_grade == 'D') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

temporal_all_data = rbind(trend_A,trend_B,trend_C,trend_D)
readr::write_csv(temporal_all_data, "temporal_all_data_1933_2022_plyr.csv")


# Plot temporal trend: 2000-2020
temporal_all_data %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) + 
  geom_line(aes(color = holc_grade), size = 1) +
  scale_color_manual(values = holc_pal) +
  theme_bw(16) + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') 


#####now unload plyr package and you will receive different picture
detach("package:plyr", unload = TRUE, character.only = TRUE)
tmpppp = temporal_all_data %>% group_by(holc_grade, Year) # %>% mutate(cumsum = cumsum(n_obs))

trend_A = tmpppp %>% filter(holc_grade == 'A') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_B  = tmpppp %>% filter(holc_grade == 'B') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_C  = tmpppp %>% filter(holc_grade == 'C') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_D  = tmpppp %>% filter(holc_grade == 'D') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

temporal_all_data = rbind(trend_A,trend_B,trend_C,trend_D)
readr::write_csv(temporal_all_data, "temporal_all_data_1933_2022_dplyr.csv")

# Plot temporal trend: 2000-2020
temporal_all_data %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) + 
  geom_line(aes(color = holc_grade), size = 1) +
  scale_color_manual(values = holc_pal) +
  theme_bw(16) + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') 


########GAMs#############x
library(sjPlot)
library(mgcv)

# =========================
# 1) DPLYR VERSION
# =========================
# Load
temporal_all_data_1933_2022_dplyr <- read.csv("C:\\Users\\petom\\OneDrive\\Desktop\\Published articles\\Impactove\\2025\\Nature Human Behavior reproducibility project\\Data\\Biodiv_Greeness_Social\\new\\temporal_all_data_1933_2022_dplyr.csv")

# Prepare
temporal_all_data_tmp_dplyr <- data.frame(temporal_all_data_1933_2022_dplyr)
temporal_all_data_tmp_dplyr$Year <- as.integer(temporal_all_data_tmp_dplyr$Year)

# GAM (2000–2020)
model_gam_new_dplyr <- gam(
  sampling_density ~ Year * holc_grade,
  data = temporal_all_data_tmp_dplyr[temporal_all_data_tmp_dplyr$Year %in% c(2000:2020), ]
)
# tab_model(model_gam_new_dplyr, auto.label = TRUE)

# GLM baseline (2000–2020)
model_sampling_dplyr <- glm(
  sampling_density ~ Year * holc_grade,
  data = temporal_all_data_tmp_dplyr[temporal_all_data_tmp_dplyr$Year %in% c(2000:2020), ]
)
model_sampling_dplyr |> tab_model(show.aic = TRUE)
tab_model(model_sampling_dplyr, auto.label = TRUE)

# =========================
# 2) PLYR VERSION
# =========================
# Load
temporal_all_data_1933_2022_plyr <- read.csv("C:\\Users\\petom\\OneDrive\\Desktop\\Published articles\\Impactove\\2025\\Nature Human Behavior reproducibility project\\Data\\Biodiv_Greeness_Social\\new\\temporal_all_data_1933_2022_plyr.csv")

# Prepare
temporal_all_data_tmp_plyr <- data.frame(temporal_all_data_1933_2022_plyr)
temporal_all_data_tmp_plyr$Year <- as.integer(temporal_all_data_tmp_plyr$Year)

# GAM (2000–2020)
model_gam_new_plyr <- gam(
  sampling_density ~ Year * holc_grade,
  data = temporal_all_data_tmp_plyr[temporal_all_data_tmp_plyr$Year %in% c(2000:2020), ]
)
# tab_model(model_gam_new_plyr, auto.label = TRUE)

# GLM baseline (2000–2020)
model_sampling_plyr <- glm(
  sampling_density ~ Year * holc_grade,
  data = temporal_all_data_tmp_plyr[temporal_all_data_tmp_plyr$Year %in% c(2000:2020), ]
)
model_sampling_plyr |> tab_model(show.aic = TRUE)
tab_model(model_sampling_plyr, auto.label = TRUE)
