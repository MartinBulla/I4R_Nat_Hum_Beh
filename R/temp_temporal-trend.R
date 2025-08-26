# packages
require(data.table)

# load data
d = fread('Data/MaPe/Mape_R1_biodiv_sum_bird_obs_by_holc_id_year.csv')
names(d) = c("city_state", "city",  "year",  "id",   "holc_polygon",  "holc_grade",  "lat",  "lon" , "area_holc_km2", "sum_bird_obs")
d$area_holc_km2 = NULL # was generated wrongly due to unaligned holc ideas

# add polygon area
holc <- sf::st_read('Data/holc_ad_data.shp') %>% 
  sf::st_cast('POLYGON') %>% # IMPORTANT
  dplyr::filter(!st_is_empty(.)) %>% 
  sf::st_make_valid(.) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                  , city_state = paste0(city, ', ', state)
                  , area_holc_km2 = as.double(st_area(.) / 1e+6)) %>% 
  dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) 

holc_ = data.table(holc)

holc_[, id2 :=paste(paste(city,state, sep = ', '), holc_id)] # create unique ID

d[, id2 :=paste(city_state, holc_id)] # create unique ID

d[, area_holc_km2:=holc_[id2%in%d$id2[1], area_holc_km2]] # merge

b[, stat_city_holcID:= paste(city_state, holc_id)]

bb = b[, list(n_years=length(year)), stat_city_holcID]

summary(bb)

# MODELS

ma = lmer(sampling_density ~ year*holc_grade + 
    (1|state) + (1|city_state) + (1|id2), 
    dd)

mb = lmer(sampling_density ~ year*holc_grade + 
    (1|state/city_state/holc_grad/id2), 
    dd)

mas1 = lmer(sampling_density ~ year*holc_grade + (1|state) + (year|city_state) + (1|id2), dd)

mbs1 = lmer(sampling_density ~ year*holc_grade + 
    (year|state/city_state/holc_grad/id2), 
    dd)

msab1 = lmer(sampling_density ~ year*holc_grade + (year|state/city_state/holc_grade) + (1|id2), dd)    