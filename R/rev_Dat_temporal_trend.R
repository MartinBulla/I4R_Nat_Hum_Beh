# =============================================================
# Adjusted authors' code '04_R4_uneven_biodiversity_data_2023.R'
# Generates dataset aggregated by year abd neighberhood and 
# by year, neighberhood and data source
#❗ Runs relative to the project's root directory, requires
# XX, and exports Mape_R1_biodiv_sum_bird_obs_by_holc_id_year.csv into ./Data/MaPe.
# =============================================================---

# load packages
pkgs <- c("data.table","dplyr","here","sf","stringr","tibble")
install_if_missing <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, dependencies = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

install_if_missing(pkgs)

# --- --- ---
# Load downloaded Holc polygons from the Mapping Inequality project form the University of Richmond
# --- --- ---

holc <- st_read('Data/holc_ad_data.shp') %>% 
  sf::st_cast('POLYGON') %>% # IMPORTANT
  dplyr::filter(!st_is_empty(.)) %>% 
  sf::st_make_valid(.) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                  , city_state = paste0(city, ', ', state)
                  , area_holc_km2 = as.double(st_area(.) / 1e+6)) %>% 
  dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) 

holc_ = data.table(holc) # MaPe make it a data.table
h = holc_[!is.na(holc_id)] # MaPe remove 124 unnamed polygons nrow(holc_)-nrow(k)             
h[, id2 :=paste(paste(city,state, sep = ', '), holc_id)] # MaPe - create unique ID

# Calculate the area of holc polygons
holc_area <-  holc %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(area_sum = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')  %>% as_tibble() %>% dplyr::select(-geometry)

# List all .Rdata files in input folder that contain bird biodiversity data:
aves_obs = list.files(here::here('original_paper/Data/Biodiversity_holc_all'), pattern = 'Aves_all_observations.Rdata', full.names = T) # MaPe


# --- --- ---
# [1] Loop through all  HOLC polygons with bird biodiversity data and
# count the number of observations per single HOLC polygon, and year, and 
# polygon, data_source and year. 
# Note files with missing holc_ids cannot be merged with area and were removed
# --- --- ---

u <- unique(aves_obs)
n <- length(u)
pb <- txtProgressBar(min = 0, max = n, style = 3)

for(k in seq_along(u)) {
   # i = unique(aves_obs)[1]
  i <- u[k]
  setTxtProgressBar(pb, k)
  
  print(i)
  
  if(!any(str_detect(aves_obs, pattern = i))==TRUE){
    print(paste0(i, ' has no biodiversity data'))
    next
  }
  
  biodiv_data = aves_obs[str_detect(aves_obs, pattern = i)]
  
  # Load the single polygon with bird biodiversity data
  results <- sapply(biodiv_data, function(x) mget(load(x)), simplify = TRUE) 
  
  # Keep only desired columns as GBIF has 200+ columns
  mycols = c('species',
             'family',
             'genus',
             'decimalLongitude',
             'decimalLatitude',
             'collectionCode',
             'collectionID',
             'institutionCode',
             'year',
             'city',
             'city_state',
             'holc_id',
             'holc_grade',
             #'species', MaPe removed duplicated species name
             'id')
  
  results <- lapply( results , "[", , mycols) 
  
  df <- do.call(rbind, results)
  d = data.table(df)  
  
  # stop running if holc_id not unique, i.e. A, B, C, D, E
  if(unique(d$holc_grade)==unique(d$holc_id) | is.na(unique(d$holc_id)) ){
    print(paste0('no unique holc_id data ini', i))
    next # skips files without unique holc ids
  }
  
  # adjust variables
  d[, holc_polygon := gsub('.Rdata','', basename(i))]
  d[ ,lat:= decimalLatitude[1]] 
  d[ ,lon:= decimalLongitude[1]]
  d[, id2 :=paste(city_state, holc_id)] # create unique ID

 
  # add area  
  d = merge(d, h[,.(id2, state, area_holc_km2)], by = 'id2',all.x = TRUE)
  #d[, area_holc_km2:=holc_[id2%in%d$id2[1], area_holc_km2]] # merge

  # count per year and polygon (note that some ebird records have atlas data)
  dd = d[, list(sum_bird_obs = length(species)), by = list(city_state, city, state, year, id, id2, holc_polygon, holc_grade, lat, lon, area_holc_km2)]
  
  exists <- file.exists('Data/MaPe/mape_num-of-obs_by_holc_id_year.csv')
  fwrite(dd, file = 'Data/MaPe/mape_num-of-obs_by_holc_id_year.csv', append = exists, col.names = !exists)   #corresponds to the author's R1_biodiv_trend_by_time_holc_id_1933_2022.csv, but contains further variables

  #write.table(dd, file = "Data/MaPe/Mape_R1_biodiv_sum_bird_obs_by_holc_id_year.csv", append = T, row.names = F,col.names = F, sep = ",") 

  # 2000-2020 count per year, data source and polygon (note that some ebird records have atlas data)
  b =d[year >= 2000 & year <= 2020]  
  b[collectionCode %in%c('GBBC', 'EBIRD'), collectionCode := 'ebird']  
  b[institutionCode %in% 'iNaturalist', collectionCode := 'iNaturalist']
  b[!collectionCode%in%c('ebird','iNaturalist'), collectionCode:='other']
  
  bb = b[, list( sum_bird_obs = length(species)), by = list(city_state, city, state, year, collectionCode, id, id2, holc_polygon, holc_grade, lat, lon, area_holc_km2)]  
  exists2 <- file.exists('Data/MaPe/mape_num-of-obs_by_holc_id_year_data-source.csv')
  fwrite(bb, file = 'Data/MaPe/mape_num-of-obs_by_holc_id_year_data-source.csv', append = exists2, col.names = !exists2)  
  #write.table(dd, file = "Data/MaPe/Mape_R1_biodiv_sum_bird_obs_by_holc_id_year_data-source.csv", append = T, row.names = F, col.names = F, sep = ",") 
}
close(pb)