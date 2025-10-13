#' ---
#' title: <font size="2">Replicating </font><br><font size="6">Ellis-Soto et al 2023, Nat Hum Beh</font>
#' author: <font size="3">Martin Bulla & Peter Mikula</font><br><br><font size="2">created by Martin Bulla</font><br>
#' date: <font size="1.5">`r Sys.time()`</font>
#' bibliography: ../Resources/_bib.bib
#' output:
#'     html_document:
#'         toc: true
#'         toc_float: true
#'         toc_depth: 4
#'         code_folding: hide
#'         link-citations: yes
#'         css: ../Resources/styles.css
#' base:  href="/[I4R_Nat_Hum_Beh]/"
#' ---

#' <style> body {text-align: justify}</style>
 
#+ r setup, include=FALSE 
knitr::opts_knit$set(root.dir = normalizePath(".."))
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = FALSE)
 
#' # General note  
#' For the sake of reproducibility we stored the files from the [repository](https://doi.org/10.5281/zenodo.8052525) that acompanied their publication [@ellis-soto_historical_2023] in the [original_paper](https://github.com/MartinBulla/avian_FID_covid/tree/main/R/) folder (at the root project’s directory) with subfolders ‘Data’ and ‘Code’ (the latter two with the file structure as provided by the authors). We stored the additional data shared by the authors upon the request from The Institute for Replication at the ‘Data’ folder within the root project directory. Datasets that we recreated using the authors code `04_R4_uneven_biodiversity_data_2023.R` are at 'Data/from_code_04'. Additional data recreate by us using our script [rev_Dat_temporal_trend.R](R/rev_Dat_temporal_trend.R) (which is the adjusted version of the authors' `04_R4_uneven_biodiversity_data_2023.R`) are at 'Data/MaPe'.
#' 
#' **Scripts generting the outputs of this html are availalbe within the html upon clicking the `code` button at top right above each display item!**
#' 
#' ###### Code to load tools and data
#+ start, echo = T, results = 'hide', warning=FALSE


# 1. constants
recreate_data = FALSE # use TRUE, if you wish to recreate the data, instead of loading them from .Data/ 

# Color palette for redlining
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)#, '#A9A9A9' # dark gray)

# 2. load or install packages
pkgs <- c("cowplot","data.table","ggplot2","lme4", "patchwork")  # list of packages

install_if_missing <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, dependencies = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

install_if_missing(pkgs)

# 3. load authors' temporal data
t = fread('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') #tt = fread('Data/from_script_04/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(t) <- c('year','holc_grade', 'Sum')

t = t[holc_grade != 'E'] #d = data.table(temporal_trend)
tt = t[, .(n_obs = sum(Sum)), by = list(year, holc_grade)]
tt = tt[order(holc_grade,year)]

# add area per holc grade (as the authors used two ways to calculate this, we test both, but then use only the (b) as that seems to be the one eventually used) #TODO:how this changes if we first link the specific areas to polygons and only then calculate the sum
  # a)
  holc_a <- fread('original_paper/Data/Biodiv_Greeness_Social/soc_dem_max_2022_03_12 17_31_11.csv')

  holc_area_sum_a = holc_a[, list(sum_area_holc_km2 = sum(area_holc_km2)), holc_grade]
  holc_area_sum_a_dt = data.table(holc_area_sum_a)  
    # gives
      #  holc_grade: chr [1:4] "A" "B" "C" "D"
      #  sum_area_holc_km2  : num [1:4] 1279 2712 5179 3280

  # b) copy of the L54-65 of 04_R4_uneven_biodiversity_data_2023.R, with MaPe changed folder path
  holc_b <- suppressWarnings(sf::st_read('Data/holc_ad_data.shp', quiet = TRUE) %>% #MaPe changed folder path 
      sf::st_cast('POLYGON') %>% # IMPORTANT
      dplyr::filter(!sf::st_is_empty(.)) %>% 
      sf::st_make_valid(.) %>% 
      tibble::rowid_to_column() %>% 
      dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                      , city_state = paste0(city, ', ', state)
                      , area_holc_km2 = as.double(sf::st_area(.) / 1e+6)) %>% 
      dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) )

    # Calculate the total area of holc polygons
    holc_area_sum_b <-  holc_b %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(sum_area_holc_km2 = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')  %>% as_tibble() %>% dplyr::select(-geometry)    
    # gives
      #  holc_grade: chr [1:4] "A" "B" "C" "D"
      #  sum_area_holc_km2  : num [1:4] 1282 2948 4365 2689
   holc_area_sum_b_dt = data.table(holc_area_sum_b)  

tta = merge(tt,holc_area_sum_a_dt, all.x = TRUE)
tta[, sampling_density :=n_obs/sum_area_holc_km2]

ttb = merge(tt,holc_area_sum_b_dt, all.x = TRUE)
ttb[, sampling_density :=n_obs/sum_area_holc_km2]

tta[, sampling_density_b :=ttb$sampling_density]
ggplot(tta, aes(x = sampling_density, y = sampling_density_b)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", col = 'red') + 
  geom_point() + 
  facet_wrap(~holc_grade) +
  coord_equal(expand = FALSE)  


# sampling density
tta[, sampling_density := n_obs/sum_area_holc_km2]
ttb[, sampling_density := n_obs/sum_area_holc_km2]

# adjust contrasts
options(contrasts = c("contr.treatment", "contr.poly"))
ttb[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]
tt20 = ttb[year >= 2000 & year <= 2020]
tt10 = ttb[year >= 2010]

# estimate 2000 - 2020 A/D disparity
dispar = round((((ttb[year%in%c(2020) &  holc_grade%in%c('A'), sampling_density] / ttb[year%in%c(2020) &  holc_grade%in%c('D'), sampling_density])/(ttb[year%in%c(2000) &  holc_grade%in%c('A'), sampling_density]/ ttb[year%in%c(2000) &  holc_grade%in%c('D'), sampling_density]))-1)*100, 1)

# 4. load temporal data for year, category, neighberhood generated by us

if(recreate_data==TRUE){
  source('R/rev_Dat_temporal_trend.R')
}else{
  d = fread('Data/MaPe/mape_num-of-obs_by_holc_id_year.csv')
  b = fread('Data/MaPe/mape_num-of-obs_by_holc_id_year_data-source.csv')
}

d = d[!holc_grade%in%c('E')]
# create median per year and holc
d[, sampling_density:=sum_bird_obs/area_holc_km2]
d_med = d[, .(sampling_density_med = median(sampling_density)), by = .(year, holc_grade)]
#ggplot(d_med, aes(x = sampling_density_med)) +  geom_density()

# aggregate temporal data per year and holc (as the authors have done)
dd = d[, .(sum_bird_obs = sum(sum_bird_obs)), by = .(year, holc_grade)] # we have initially used the sum_km2 = sum(area_holc_km2) as well, but because our data are missing neighberhood that had no ID to link them with neighberhood area and hence has fewer sampled neighberhoods and hence smaller area, for consistency with the authors' values, we use their overall area per holc grade

dd = merge(dd,holc_area_sum_b_dt, all.x = TRUE)

# sampling density
dd[, sampling_density := sum_bird_obs/sum_area_holc_km2]

# disparity in ration (meaning of it unclear given the similarity in 2000)
dispar2 = round((((dd[year%in%c(2020) &  holc_grade%in%c('A'), sampling_density] / dd[year%in%c(2020) &  holc_grade%in%c('D'), sampling_density])/(dd[year%in%c(2000) &  holc_grade%in%c('A'), sampling_density]/
 dd[year%in%c(2000) &  holc_grade%in%c('D'), sampling_density]))-1)*100, 1) #Using the dataset per year, holc grade and neighberhood generated yet a different percentage.