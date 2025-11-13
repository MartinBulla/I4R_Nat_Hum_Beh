# =============================================================
# Adjusted authors' code '04_R4_uneven_biodiversity_data_2023.R'
# Generates observation dataset aggregated by year and neighborhood along with neighborhood's area km2
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
#❗the original author's code uses shape file holc_ad_data.shp here, but that file has lower number of polygons and polygon ids that do not match those from raw Rdata observations; we thus use use their file that contains along holc parameters also holc soc dem information (albeit we do not know how that file was generated)
# --- --- ---


 holc <- readr::read_csv('original_paper/Data/Biodiv_Greeness_Social/soc_dem_max_2022_03_12 17_31_11.csv'
                   , col_select = c(id : area_holc_km2
                                    , holc_tot_pop
                                    , msa_GEOID : msa_total_popE
                                    , msa_gini))
holc_ = data.table(holc) # MaPe make it a data.table

# List all .Rdata files in input folder that contain bird biodiversity data:
aves_obs = list.files(here::here('original_paper/Data/Biodiversity_holc_all'), pattern = 'Aves_all_observations.Rdata', full.names = T)


# --- --- ---
# [1] Loop through all  HOLC polygons with bird biodiversity data and
# count the number of observations per single HOLC polygon, and year

# Note files with missing id cannot be merged with area and were removed
# --- --- ---

# test start
i = unique(aves_obs)[1]
biodiv_data = aves_obs[str_detect(aves_obs, pattern = i)]
results <- sapply(biodiv_data, function(x) mget(load(x)), simplify = TRUE)  
obj <- results[[1]]   
nm <- names(obj)
nm[order(nm)]  
dt <- as.data.table(results[[1]]) 
# test end

u <- unique(aves_obs)
n <- length(u)
pb <- txtProgressBar(min = 0, max = n, style = 3)

for(k in seq_along(u)) {
   # k = 5555
  i <- u[k]
  setTxtProgressBar(pb, k)
  
  print(paste(k, i))
  
  if(!any(str_detect(aves_obs, pattern = i))==TRUE){
    print(paste0(i, ' has no biodiversity data'))
    next
  }
  
  # Load the single polygon with bird biodiversity data
  biodiv_data = aves_obs[str_detect(aves_obs, pattern = i)]
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
             'state',
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
  if(is.na(unique(d$id)) ){
    print(paste0('no unique id in', i))
    next # skips files without unique holc ids
  }
  
  # adjust variables
  d[, holc_polygon := gsub('.Rdata','', basename(i))]
  d[ ,lat:= decimalLatitude] # taking one lat value out of all
  d[ ,lon:= decimalLongitude] # taking one lon value out of all
  d[, id2 :=paste(city_state, holc_id)] # create unique ID

  # add area  
   d0 = d[, .(city_state, city, state, year, id, id2, holc_polygon, holc_grade, lat, lon, species, family, genus)] # unique lat/lon per observation
  exists <- file.exists('Data/MaPe/mape_DAT_all.csv')
  fwrite(d0, file = 'Data/MaPe/mape_DAT_all.csv', append = exists, col.names = !exists) #corresponds to the author's R1_biodiv_trend_by_time_holc_id_1933_2022.csv, but contains year and km2

  # count per year and polygon (note that some ebird records have atlas data)
  d[ ,lat:= decimalLatitude[1]] # taking one lat value out of all
  d[ ,lon:= decimalLongitude[1]] # taking one lon value out of all

  dd = d[, list(sum_bird_obs = length(species)), by = list(city_state, city, state, year, id, holc_polygon, holc_grade, lat, lon)]
  
  exists1 <- file.exists('Data/MaPe/2025-11-12_mape_num-of-obs_by_grade_year_polygon.csv')
  fwrite(dd, file = 'Data/MaPe/2025-11-12_mape_num-of-obs_by_grade_year_polygon.csv', append = exists1, col.names = !exists1)   

  # 2000-2020 count per year, data source and polygon (note that some ebird records have atlas data)
  b =d[year >= 2000 & year <= 2020]  
  b[collectionCode %in%c('GBBC', 'EBIRD'), collectionCode := 'ebird']  
  b[institutionCode %in% 'iNaturalist', collectionCode := 'iNaturalist']
  b[!collectionCode%in%c('ebird','iNaturalist'), collectionCode:='other']
  
  bb = b[, list( sum_bird_obs = length(species)), by = list(city_state, city, state, year, collectionCode, id, holc_polygon, holc_grade, lat, lon)]  
  exists2 <- file.exists('Data/MaPe/2025-11-12_mape_num-of-obs_by_year_data-source_polygon.csv')
  fwrite(bb, file = 'Data/MaPe/2025-11-12_mape_num-of-obs_by_year_data-source_polygon.csv', append = exists2, col.names = !exists2)  
  #write.table(dd, file = "Data/MaPe/Mape_R1_biodiv_sum_bird_obs_by_holc_id_year_data-source.csv", append = T, row.names = F, col.names = F, sep = ",") 
}
close(pb)



#TODO: CHECK HOW WE STAND
biodiv_sum = fread('Data/MaPe/mape_DAT_all.csv')

sum(biodiv_sum$Sum) # Our paper says 10,043,533 georeferenced ocurrences but I have 10,048,895 here

# TODO merge on to holc dataset but in a loop and per year 