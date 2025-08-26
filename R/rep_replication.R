#' ---
#' title: <font size="2">Replicating </font><br><font size="5">Ellis-Soto et al 2023, Nat Hum Beh</font>
#' authors: <font size="2">Martin Bulla & Peter Mikula</font><br><br><font size="2">created by Martin Bulla</font><br>
#' date: <font size="1.5">`r Sys.time()`</font>
#' bibliography: resources/_bib.bib
#' output:
#'     html_document:
#'         toc: true
#'         toc_float: true
#'         toc_depth: 4
#'         code_folding: hide
#'         link-citations: yes
#'         css: resources/styles.css 
#' base:  href="/[I4R_Nat_Hum_Beh]/"
#' ---

#' <style> body {text-align: justify}</style>

#+ r setup, include=FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)

#' ***

#' ###### Adjusted code of Elias et al to load tools and data
#' Included is only code that loads and adjusts the data (without the author's data checks, which are running well, but are not needed here
#' 
#' Any other code adjustments are indicated by "MaPe"

# load libraries
## packages we'll be using
packs <- c(
    'arm'  # MaPe for mixed effect modelling
  ,  'data.table'         # MaPe for easy data handling
  ,  'tidyverse'         # a must have!
  , 'tidylog'         # makes things very verbose for 2x checking 
  , 'magrittr'        # all of the pipes
  , 'janitor'         # cleans things up
  , 'sf'              # spatial support
  # , 'tidycensus'      # Census access
  , 'mapview'         # webmaps
  , 'tictoc'          # times things
  , 'beepr'           # makes noises
  # , 'segregation'     # segregation indecies like M
  # , 'dineq'           # Gini Coefficient (and decomposition?)
  , 'sjPlot'          # model diagnostics and plotting
  , 'performance'     # model diagnostics
  , 'ggpubr'          # adds pvals to boxplots
  , 'patchwork'       # for pulling ggplot objects together
  , 'gtsummary'       # easy summary tables
  )         

## check for all of the libraries
if (length(setdiff(packs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packs, rownames(installed.packages())))  
}

## load them
vapply(packs, library, character.only = TRUE, logical(1), logical.return = TRUE, quietly = TRUE)


# # setting for get_acs
# census_api_key('[SNIP]', install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")
# options(tigris_use_cache = TRUE)

## keep random things consistent
set.seed(19870630) # needed?


## redlining colors
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
              #, '#A9A9A9'
              ) # dark gray)


holc_pal_f<- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
              , '#A9A9A9'
              , '#000000')


## set custom function for getting spatial data
see_sf <- function(){
# what's in memory that are sf - spatial features?
keep(eapply(.GlobalEnv, class),      # gets the objects in the global environment
     ~ any(str_detect(., "sf"))) %>% # selects elements with sf in them
    names(.) %>% as.character(.)     # my simple features
}

see_sf() -> sf_in_memory

## what are the spatial references of those SF classes?
mget(sf_in_memory) %>% purrr::map(~st_crs(.x)$epsg) %>% unlist() #%>% View()


## custom function for "Not In"
`%nin%` <- Negate(`%in%`)


## fixes mapview
mapviewOptions(fgb = FALSE)

# 1 read in data

## A soc dem
(
  holc <- read_csv('original_paper/Data/Biodiv_Greeness_Social/soc_dem_max_2022_03_12 17_31_11.csv'
                   , col_select = c(id : area_holc_km2
                                    , holc_tot_pop
                                    , msa_GEOID : msa_total_popE
                                    , msa_gini))
  )

holc |> glimpse()

## are the ids unique?
holc |> distinct(id)
all.equal(length(unique(holc$id)), dim(holc)[1])

## are the city's unique?
holc |> distinct(city)
holc |> distinct(msa_NAME)
holc |> distinct(city_state) # MaPe

# what is the relationship between MSA (metros today) and city (as HOLC defined them yesteryear)
(
  holc |> 
    tabyl(city, msa_NAME) |> 
    as_tibble() |> 
    pivot_longer(cols = -city, names_to = 'MSA', values_to = 'holc_per_msa') |> 
    filter(holc_per_msa > 0) |> 
    group_by(city) |> 
    add_count(name = 'msa_per_city') |> 
    ungroup() |> 
    group_by(MSA) |> 
    add_count(name = 'city_per_msa') |> 
    ungroup() -> holc_city_msa
  )

holc_city_msa |> 
  filter(msa_per_city > 1)


### i polygons

tic(); (poly <- st_read('Data/holc_ad_data.shp'#(poly <- st_read('input_data/HOLC_shapefile/holc_ad_data.shp'
                        # , as_tibble = TRUE
                        ) %>% 
  # filter(!is.na(holc_grade) & holc_grade != 'E') %>% 
  st_cast('POLYGON') %>% # IMPORTANT
  filter(!st_is_empty(.)) %>% 
  st_make_valid(.) %>% 
  rowid_to_column() %>% 
  mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
         , city_state = paste0(city, ', ', state)
         , area_holc_km2 = as.double(st_area(.) / 1e+6)) %>% 
  select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2));toc() # < 3 seconds


### B birds
#### i records

(
  birds_records <- 
    read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_sum_bird_obs_by_holc_id_1933_2022.csv') |> 
    mutate(id = str_remove(id, '_Aves_all_observations')) |> 
    select(id, records = N_samples)
  )

# unique ids good?
birds_records |> distinct(id)
all.equal(length(unique(birds_records$id))
          , dim(birds_records)[1])

# general 2x checks
birds_records |> glimpse()
birds_records |> summary()
birds_records |> arrange(desc(records))

# will this join correctly?
holc |> left_join(birds_records, by = 'id')
holc |> anti_join(birds_records, by = 'id') # good
birds_records |> anti_join(holc, by = 'id') # perfect


#### ii completeness

(
  birds_completeness <-
    read_csv('original_paper/Data/Biodiv_Greeness_Social/bird_completeness_HOLC_cities_2022_R1.csv') |> 
    select(id, completeness = Completeness) |> 
    tidylog::mutate(id = ifelse(id == 'VA_Roanoke_B2\\r\\n2_B_9289', 'VA_Roanoke_B2\r\n2_B_9289', id))
  )

# unique ids good?
birds_completeness |> distinct(id)
all.equal(length(unique(birds_completeness$id))
          , dim(birds_completeness)[1])

# general 2x checks
birds_completeness |> glimpse()
birds_completeness |> summary()
birds_completeness |> arrange(desc(completeness))

# will this join correctly?
holc |> left_join(birds_completeness, by = 'id')
holc |> anti_join(birds_completeness, by = 'id') # good
birds_completeness |> anti_join(holc, by = 'id') # perfect



#### iii data by source

(
  birds_source <- 
    read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_col_code_by_holc_id_2000_2020.csv') |> 
    mutate(id = str_remove(id, '_Aves_all_observations')) |> 
    select(id, records = N_samples, type = Collection_code) |> 
    pivot_wider(id_cols = id, names_from = type, values_from = records)
  )

# test joings
holc |> left_join(birds_source, by = 'id')
birds_source |> anti_join(holc, by = 'id')


### C climate

(
  clim <- read_csv('original_paper/Data/Biodiv_Greeness_Social/climatic_data_cities.csv'
                   , col_select = c(  -'...1'
                                    , mean_temp_c = mean_temp
                                    , mean_precip_mm = mean_precip)
                   )
  )

clim |> distinct(city)

all.equal(clim |> distinct(city) |> dim(), holc |> distinct(city) |> dim())

holc |> anti_join(clim, by = 'city') # perfect!

# old is new? Yes
clim_holc <- 
  read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_climatic_data_cities.csv') |> 
  group_by(city) |> 
  summarise(mean_temp = mean(temperature)
            , mean_precip = mean(precip))

clim |> left_join(clim_holc, by = 'city') -> test_tab


### D NDVI

(green <- read_csv('original_paper/Data/Biodiv_Greeness_Social/NDVI_unique_ID_updated.csv') %>% 
   mutate(ndvi = ifelse(is.na(ndvi), median(.$ndvi, na.rm = TRUE), ndvi)) |> # interpolated missing
   select(id, ndvi))

#  impute missing
green |> map(~sum(is.na(.))) |> bind_rows() |> t()
green |> summary()
# green %>% 
#   mutate(ndvi = ifelse(is.na(ndvi), median(.$ndvi, na.rm = TRUE), ndvi)) %>% summary()

# test joins
holc |> select(id) |> left_join(green, by = 'id')
holc |> anti_join(green, by = 'id') # perfect

green |> anti_join(holc, by = 'id') # what happened?

# viz green vs holc: confirm prior findings - MaPe, but why not reported
holc |> 
  left_join(green, by = 'id') |> 
  ggplot(aes(holc_grade, ndvi)) + 
  geom_boxplot()



# poly |> left_join(green |> tidylog::select(id, ndvi), by = 'id')

# # map
# poly |> 
#   tidylog::left_join(green|> tidylog::select(id, ndvi), by = 'id') |> 
#   filter(state == 'CT') |> 
#   mapview::mapview(zcol = 'ndvi')


### E PAD

(pad <- read_csv('Data/NDVI_PAD_unique_ID.csv'#'original_paper/Data/Biodiv_Greeness_Social/NDVI_PAD_unique_ID.csv' 
                 , col_select = c(id, pct_pa = percent_pa)))

# 2x checks
summary(pad)
pad |> map(~sum(is.na(.))) |> bind_rows() |> t()

holc |> select(id) |> left_join(pad, by = 'id')
holc |> anti_join(pad, by = 'id')

pad |> anti_join(holc, by = 'id') # whats up with Florida (well, what's up with this polygon?)


# viz pad vs holc: a NEW FINDING - a little surprising!
holc |> 
  left_join(pad, by = 'id') |> 
  ggplot(aes(holc_grade, pct_pa)) + 
  geom_boxplot() + 
  # scale_y_log10()
  scale_y_continuous(trans = 'log2')

# does protected area vary by grade (and city)?
holc |> 
  select(id, holc_grade, city) %>%
  left_join(pad, by = 'id') %>% 
  aov(pct_pa ~ holc_grade #*city
  , data = .) |> 
  model.tables(type = 'means')

# overall cover seems low..


### F Coldspots

(coldspots <- read_csv('original_paper/Data/Biodiv_Greeness_Social/coldspots.csv'))

coldspot_regions <- plyr::ddply(coldspots, 'holc_grade', function(x){
  data.frame(
    n_row_total_n_coldspot_polygons = nrow(x),
    percent_coldspots = (  ( nrow(x) / nrow(coldspots) ) * 100 )
  )
})


# 2 joins
```{r}

# CLIMATE (test)
holc |> anti_join(clim, by = 'city') 
holc |> left_join(clim, by = 'city') 

# BIRDS
birds_records |> dim()
# comb <- left_join(holc, birds # adds bird biodiversity
comb <- left_join(holc, birds_records # adds bird biodiversity
                  , by = 'id') |> 
  left_join(birds_completeness, by = 'id') |> # adds bird completeness
  left_join(birds_source, by = 'id') |> # adds in sampling by source
  left_join(clim, by = 'city') |> # adds temp and precip
  left_join(green, by = 'id') |> # adds green 
  left_join(pad, by = 'id') |> # adds protected areas
# replace_na(list(records = 0, observed_richness = 0, richness = 0, slope = 0, completeness = 0, ratio = 0)) |> 
  mutate(
      pop_per_km           = ifelse(is.na(holc_tot_pop), 0, holc_tot_pop / area_holc_km2)
    , sampling_density     = records / area_holc_km2
    , sampling_density_log = log(sampling_density)
    , completeness_log     = log(completeness)) |> 
  # tidylog::select(
  #     id : city_state # holc ids, etc
  #   , sampling_density, sampmling_density_log, completeness, completeness_log # DVs
  #   , 
  # )
  relocate(sampling_density, sampling_density_log, completeness, completeness_log, 
           .before = completeness) 
  
# comb |> 
#   write_csv(paste0('int_data/comb/PNAS_main_comb_', Sys.Date(), '.csv'))


# TODO select to re order: ids, dvs, predictors, higher-level ids, higher-level vars.
comb |> glimpse()



# viz pad vs holc: a NEW FINDING - a little surprising!
holc |> 
  left_join(pad, by = 'id') |> 
  ggplot(aes(holc_grade, pct_pa)) + 
  geom_boxplot() + 
  # scale_y_log10()
  scale_y_continuous(trans = 'log2')

# does protected area vary by grade (and city)?
holc |> 
  select(id, holc_grade, city) %>%
  left_join(pad, by = 'id') %>% 
  aov(pct_pa ~ holc_grade #*city
  , data = .) |> 
  model.tables(type = 'means')
  #summary()


#  comb |> select(id, holc_tot_pop, pop_per_km) |> filter(is.na(holc_tot_pop)) |>  pull(id)
comb |> select(holc_tot_pop, pop_per_km) |> summary()

comb |> 
  ggplot(aes(pop_per_km)) + 
  geom_density()
  

# TODO transform vars here? Scale?
# mutate(across(X1: X4, round, 2)) |>


# how many GBIF observations?
sum(comb$records, na.rm = TRUE)
#sum(comb$observed_richness, na.rm = TRUE)

# in how many HOLC polygons
dim(comb)[1]

# in how many MSAs
comb |>  tabyl(msa_NAME) |> tibble() |> nrow()

# in how many cities
comb |>  tabyl(city) |> tibble() |> nrow()

# in how many States
comb |>  tabyl(state) |> tibble() |> nrow()


# 3 EDA

comb |> summary()
comb |> map(~sum(is.na(.))) |> bind_rows() |> t()

comb |> filter(is.na(holc_id)) |> View()
(comb |> filter(is.na(holc_tot_pop)) -> no_pop)
no_pop |> View()
# poly |> filter(id %in% no_pop$id) |> mapview()

comb |> glimpse()


comb |> tabyl(msa_NAME, city) |> tibble()


comb |> 
  select(id, sampling_density, sampling_density_log, completeness, completeness_log) |> 
  pivot_longer(-id) |>
  ggplot(aes(value)) + 
  # geom_density() + 
  geom_histogram() + 
  facet_wrap(~name, scales = 'free') + 
  NULL

# COMPLETENESS AND SAMPLING DENSITY LOG

comb |> 
  filter(holc_grade != 'E') |> 
  ggplot(aes(completeness)) +
  geom_density() + 
  facet_wrap(~holc_grade)


## A descriptive statistics

# MaPe the following original code recreates Table S1
comb |> # glimpse()
  filter(holc_grade != 'E') |> 
  select(holc_grade, ndvi, pct_pa, pop_per_km) |> 
  tbl_summary(by = holc_grade
              , statistic = list(all_continuous() ~ "{mean} ({sd})")
              # , label = list(age ~ "Patient Age")
              , label = list(ndvi ~ "NDVI"
                             , pct_pa ~ "Protected Area (% cover)"
                             , pop_per_km ~ "Population per Kilometer")
              ) |> 
  gtsummary::add_overall() |> 
  as_gt() #|> 
  # gt::gtsave(filename = paste0(getwd()
  #                              , '/output_tables/descriptives_mean_sd_x_grade_'
  #                              , Sys.Date()
  #                              , '.html')) # use extensions .html .tex .ltx .rtf


comb |> # glimpse()
  filter(holc_grade != 'E') |> 
  select(holc_grade, ndvi, pct_pa, pop_per_km, mean_precip_mm, mean_temp_c) |> 
  tbl_summary(by = holc_grade
              , statistic = list(all_continuous() ~ "{mean} ({sd})")
              # , label = list(age ~ "Patient Age")
              , label = list(ndvi ~ "NDVI"
                             , pct_pa ~ "Protected Area (% cover)"
                             , pop_per_km ~ "Population per Kilometer"
                             , mean_precip_mm ~ "Precipitation [mm]"
                             , mean_temp_c ~ "Temperature [°C]"
                             )
              ) |> 
  gtsummary::add_overall() |> 
  as_gt() 
#' **Table S1** | Same as the original table, but includes also precipitation and temperature, the proxies for climate used by the authors and us in the models. 

## B pub figure 2

my_comparisons <- list(c("A", "B"), c("A", "C"), c("A", "D"))

y1 <- expression(Sampling ~ Density: ~obs. ~ per ~ km^2)

# I sampling density
(
  I_density <- comb |> 
    filter(holc_grade != 'E') |>
    # mutate(holc_grade = factor(holc_grade, levels = LETTERS[1:4], ordered = TRUE)) |> 
    # arrange(id, holc_grade) |> 
    group_by(holc_grade) |> 
    summarise(total_sampling_density = sum(records, na.rm = TRUE)
              , total_area = sum(area_holc_km2, na.rm = TRUE))  |>  
    mutate(`Sampling Density` = total_sampling_density / total_area, `HOLC Grade` = holc_grade) |>
    ggplot(aes(`HOLC Grade`, `Sampling Density`, fill = holc_grade)) +
    geom_col() + 
    scale_fill_manual(values = holc_pal) + 
    # scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1600)) + 
    # theme_bw(16) + 
    theme_classic(16) + 
    theme(legend.position = 'none') + 
    ylab(y1) + 
    NULL
  ) 


# comb |> 
#   filter(holc_grade != 'E') |> 
#   ggplot(aes(holc_grade, sampling_density)) + 
#   geom_boxplot() + 
#   scale_y_log10()

comb |>
  filter(holc_grade != 'E') |>
  # select(`Samp` = completeness, `HOLC Grade` = holc_grade)
  rename(`HOLC Grade` = holc_grade) |> 
  ggboxplot(x = 'HOLC Grade', y = 'sampling_density'
            , palette = holc_pal
            , fill = 'HOLC Grade'
            , ggtheme = theme_pubr(base_size = 16)) +
  stat_compare_means(comparisons = my_comparisons) +  # Add pairwise comparisons p-value
  scale_y_continuous(expand = c(0, 0)) +
  ylim(0, 130) + 
  # theme_blank(16) +
  theme(legend.position = 'none')

# capitalize completeness, ditch horizontal axis lab
(# MaPe Fig. 2b
  II_completeness <- comb |> 
    filter(holc_grade != 'E') |> 
    select(Completeness = completeness, `HOLC Grade` = holc_grade) |> 
    # replace_na(list(Completeness = 0)) |> 
    ggboxplot(x = 'HOLC Grade', y = 'Completeness'
              , palette = holc_pal
              , fill = 'HOLC Grade'
              , ggtheme = theme_pubr(base_size = 16)) +
    stat_compare_means(comparisons = my_comparisons) +  # Add pairwise comparisons p-value
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = 'Completeness (%)') + 
    ylim(0, 130) + 
    # theme_blank(16) +
    theme(legend.position = 'none')
  )
#' TODO check whether  the p-values are pseudoreplicated 
#' 
# ggsave(paste0(getwd(), '/figures/fig_2b_NA', Sys.Date(), '.png')
#        , width = 3.42
#        , height = 4
#        , dpi = 600
#        )

(# MaPe Fig. 2c
  III_coldspots <- coldspot_regions |> 
  # mutate(`HOLC Grade` = factor(holc_grade)) |> 
    select(`HOLC Grade` = holc_grade, `% of polygons considered coldspots` = percent_coldspots) |> 
    ggplot(aes(`HOLC Grade`, `% of polygons considered coldspots`, fill = `HOLC Grade`)) + 
    geom_bar(stat="identity") + 
    theme_classic(16) + 
    scale_fill_manual(values = holc_pal) + 
    theme(legend.position = 'none') + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 45)) +
    NULL
  )


I_density / II_completeness / III_coldspots +  plot_annotation(tag_levels = 'I')

# ggsave(paste0(getwd(), '/figures/fig_2_', Sys.Date(), '.png')
#        , width = 3.42*2
#        , height = 6*2
#        , dpi = 600
#        )


# 4 model
comb$sample_binary = ifelse(is.na(comb$sampling_density_log), 0, 1)
d = data.table(comb)
d=d[!holc_grade%in%'E']

d[holc_grade%in%'A', holc_grade_num:=1]
d[holc_grade%in%'C', holc_grade_num:=2]
d[holc_grade%in%'B', holc_grade_num:=3]
d[holc_grade%in%'D', holc_grade_num:=4]

p = fread(here::here('Data/MaPe_cities_coordinates.txt'))
dB = merge(dB,p, all.x =TRUE)
dC = merge(dC,p, all.x =TRUE)
dA = merge(dA,p, all.x =TRUE)

## A sampled or not: logistic regression for TODO:finalize

comb |> glimpse()

(comb |> 
  mutate(sample_bin = ifelse(is.na(sampling_density_log), 0, 1)) |> 
  tabyl(holc_grade, sample_bin) |> adorn_percentages() |> 
  tibble() |> 
  setNames(c('HOLC Grade', 'not sampled', 'sampled')) |> 
  pivot_longer(-`HOLC Grade`) |> 
  filter(`HOLC Grade` != 'E') -> sampled_or_not)

sampled_or_not |>  
  ggplot(aes(`HOLC Grade`, value, fill = name)) + 
  geom_col(
    #position = 'dodge'
    ) + 
  theme_bw(16) + 
  NULL

sampled_or_not %>% lm(value ~ name*`HOLC Grade`, data = .) %>% broom::tidy()
sampled_or_not %>% aov(value ~ name*`HOLC Grade`, data = .) %>% broom::tidy()


comb$sample_binary = ifelse(is.na(comb$sampling_density_log), 0, 1) # MaPe 
comb %>%  filter(holc_grade != 'E') -> comb_ABCD # MaPe 

tic(); comb %>% 
  # sample_frac(.1) |> # for testing
  mutate(sample_binary = ifelse(is.na(sampling_density_log), 0, 1)) %>%
  filter(holc_grade != 'E') %>% # TODO make this D or drop?
  lme4::glmer(
    sample_binary ~ holc_grade + 
       (1 | msa_NAME)
    , data = .
    , family = binomial(link = "logit")) -> samp_d_binary_holc; toc() # < 2 seconds

tic(); comb %>% 
  # sample_frac(.1) |> # for testing
  mutate(sample_binary = ifelse(is.na(sampling_density_log), 0, 1)) %>%
  filter(holc_grade != 'E') %>% # TODO make this D or drop?
  lme4::glmer(
    sample_binary ~ holc_grade + 
       (holc_grade | msa_NAME)
    , data = .
    , family = binomial(link = "logit")) -> samp_d_binary_holc_rirs; toc() # < 2 seconds


# tic(); comb %>%
#   # sample_frac(.1) |> # for testing
#   mutate(sample_binary = ifelse(is.na(sampling_density_log), 0, 1)) %>%
#   filter(holc_grade != 'E') %>%
#   lme4::glmer(
#     sample_binary ~ holc_grade + ndvi + pct_pa + pop_per_km +
#       (1 + holc_grade + msa_gini | msa_NAME) +
#       (1 + mean_temp_c*mean_precip_mm | city)
#     , data = .
#     , family = binomial(link = "logit")) -> samp_d_binary_max; toc() # ~8 mins

# just load instead of re-run to save time
load('model_objects/samp_d_binary_max_2022-10-27.RData')

dA = d[!is.na(sample_binary)]
m0 = lme4::glmer(sample_binary ~ holc_grade + 
    (1 | city_state), 
    data = dA, 
    family = binomial(link = "logit"))

m1= lme4::glmer(sample_binary ~ holc_grade + 
    (holc_grade | city_state), 
    data = dA, 
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))

m2= lme4::glmer(sample_binary ~ holc_grade + 
    (holc_grade_num | city_state), 
    data = dA, 
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
                         )

m1p= lme4::glmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city_state), 
    data = dA, 
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )

m2p= lme4::glmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city_state), 
    data = dA, 
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))    

# gaussian
samp_m0_g = lmer(sample_binary ~ holc_grade + 
    (1 | msa_NAME), 
    data = dA)  

samp_m1_g= lmer(sample_binary ~ holc_grade + 
    (holc_grade | msa_NAME), 
    data = d,
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))) 

m0_g = lmer(sample_binary ~ holc_grade + 
    (1 | city_state), 
    data = dA)

m1_g= lmer(sample_binary ~ holc_grade + 
    (holc_grade | city_state), 
    data = d,
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))) 

m2_g= lmer(sample_binary ~ holc_grade + 
    (holc_grade_num | city_state), 
    data = d, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)))    

m1p_g= lmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city_state), 
    data = d, ,
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )   

m2p_g= lmer(sample_binary ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city_state), 
    data = d, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )

# PLOT for logit
# add models to a list
models_A<- list(
  samp_d_binary_holc      = samp_d_binary_holc,
  samp_d_binary_holc_rirs = samp_d_binary_holc_rirs,
  m0       = m0,
  m1       = m1,
  m2       = m2,
  m1p      = m1p,
  m2p      = m2p
)

# extract
coef_df_A <- map_df(
  names(models_A),
  ~ tidy(models_A[[.x]], effects = "fixed", conf.int = TRUE) %>%
    filter(grepl("^holc_grade", term)) %>%
    mutate(model = .x),
  .id = "model_id"
)

# make grade readable
coef_df_A <- coef_df_A %>%
  mutate(
    holc_grade = gsub("holc_grade", "", term),
    holc_grade = ifelse(holc_grade == "", "(ref)", holc_grade),
    holc_grade = factor(holc_grade)
  )

# labels
model_labels_A <- c(
  samp_d_binary_holc      = "holc_grade + (1 | metropoly)",
  samp_d_binary_holc_rirs    = "holc_grade + (1 + holc_grade | metropoly)",
  m0       = "holc_grade + (1 | state_city)",
  m1       = "holc_grade + (holc_grade | state_city)",
  m2       = "holc_grade + (holc_grade_numeric | state_city)",
  m1p      = "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  m2p      = "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

# distinquish original models from our alternative ones
coef_df_A <- coef_df_A %>%
  mutate(
    model_group = ifelse(grepl("^samp", model), "Author's original", "Our new"),
    model_label = model_labels_A[model]
  )

# sort models
A_order <- c(
  "holc_grade + (1 | metropoly)",
  "holc_grade + (1 | state_city)",
  "holc_grade + (1 + holc_grade | metropoly)",
  "holc_grade + (holc_grade | state_city)",
  "holc_grade + (holc_grade_numeric | state_city)",
  "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

coef_df_A <- coef_df_A %>%
  mutate(model_label = factor(model_label, levels = A_order))

red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
blue_ =  "#46B8DAFF"

# plot
ggplot(coef_df_A, aes(x = estimate, y = fct_rev(model_label))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_group),
                 position = position_dodge(width = 0.6), height = 0) +
  geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  facet_wrap(~ holc_grade) +
  scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
  scale_fill_manual(values = c("white","#46B8DAFF")) +
  #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
  theme_minimal(base_size = 8) +
  labs(
    x = "Estimate of 'sampled or not' relative to HOLC grade A\n[binomial]",
    y = "Model structure",
    color = NULL,
    fill = NULL
  ) +
   theme(
    legend.key.height = unit(0.25, "cm")  # reduce vertical spacing between items 
    )
ggsave('Output/Fig_r1_Sampled01.jpg', width = 22, height = 5, units = 'cm')
ggsave('Output/Fig_r1_Sampled01.png', width = 22, height = 5, units = 'cm')

# PLOT for gaus
# add models to a list
models_A_g<- list(
  samp_m0_g      = samp_m0_g,
  samp_m1_g = samp_m1_g,
  m0_g      = m0_g,
  m1_g      = m1_g,
  m2_g       = m2_g,
  m1p_g     = m1p_g,
  m2p_g      = m2p_g
)

# labels
model_labels_A_g <- c(
  samp_m0_g      = "holc_grade + (1 | metropoly)",
  samp_m1_g    = "holc_grade + (1 + holc_grade | metropoly)",
  m0_g       = "holc_grade + (1 | state_city)",
  m1_g     = "holc_grade + (holc_grade | state_city)",
  m2_g       = "holc_grade + (holc_grade_numeric | state_city)",
  m1p_g      = "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  m2p_g      = "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

# extract
coef_df_A_g <- map_df(
  names(models_A_g),
  ~ tidy(models_A_g[[.x]], effects = "fixed", conf.int = TRUE) %>%
    filter(grepl("^holc_grade", term)) %>%
    mutate(model = .x),
  .id = "model_id"
)

# make grade readable
coef_df_A_g <- coef_df_A_g %>%
  mutate(
    holc_grade = gsub("holc_grade", "", term),
    holc_grade = ifelse(holc_grade == "", "(ref)", holc_grade),
    holc_grade = factor(holc_grade)
  )

# distinquish original models from our alternative ones
coef_df_A_g <- coef_df_A_g %>%
  mutate(
    model_group = ifelse(grepl("^samp", model), "Author's original", "Our new"),
    model_label = model_labels_A_g[model]
  )

# sort models

coef_df_A_g <- coef_df_A_g %>%
  mutate(model_label = factor(model_label, levels = A_order))

red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
blue_ =  "#46B8DAFF"

# plot
ggplot(coef_df_A_g, aes(x = estimate, y = fct_rev(model_label))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_group),
                 position = position_dodge(width = 0.6), height = 0) +
  geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  facet_wrap(~ holc_grade) +
  scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
  scale_fill_manual(values = c("white","#46B8DAFF")) +
  #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
  theme_minimal(base_size = 8) +
  labs(
    x = "Estimate of 'sampled or not' relative to HOLC grade A\n[gaussian]",
    y = "Model structure",
    color = NULL,
    fill = NULL
  ) +
   theme(
    legend.key.height = unit(0.25, "cm")  # reduce vertical spacing between items 
    )
ggsave('Output/Fig_r1_Sampled01_gaus.jpg', width = 22, height = 5, units = 'cm')
ggsave('Output/Fig_r1_Sampled01_gaus.png', width = 22, height = 5, units = 'cm')

tic(); compare_performance(samp_d_binary_holc, samp_d_binary_holc_rirs, samp_d_binary_max
                           , rank = TRUE); toc()

samp_d_binary_holc_rirs |> summary() # wins
# samp_d_binary_max |> summary()
tab_model(samp_d_binary_holc_rirs)
tab_model(samp_d_binary_holc_rirs, transform = 'exp')
tab_model(samp_d_binary_holc_rirs, samp_d_binary_max, transform = 'exp')
plot_model(samp_d_binary_holc_rirs, type = 'pred')
plot_model(samp_d_binary_holc_rirs, type = 'est')

# tab_model(samp_d_binary_max)
# tab_model(samp_d_binary_max, transform = 'exp')
# plot_model(samp_d_binary_max, type = 'pred')$holc_grade
# plot_model(samp_d_binary_max, type = 'pred')
# plot_model(samp_d_max, type = 'est')
# plot_model(samp_d_max, type = 'est', transform = 'exp')
# plot_model(samp_d_max, type = 're')
# plot_model(samp_d_max, type = 'diag')


#' ## B sampling density
comb <- comb |> filter(holc_grade != 'E')

# fit models
                                
d_ri      <- lme4::lmer(log(sampling_density) ~ holc_grade + (1 | msa_NAME)                                , data = comb |> filter(sampling_density_log > -Inf))

d_rirs    <- lme4::lmer(log(sampling_density) ~ holc_grade + (1 + holc_grade | msa_NAME)                   , data = comb |> filter(sampling_density_log > -Inf))

# # #winners
# d_fe_ri2   <- lme4::lmer(log(sampling_density) ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + (1 | msa_NAME)                , data = comb |> filter(sampling_density_log > -Inf), REML = TRUE)
# d_fe_rirs2 <- lme4::lmer(log(sampling_density) ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + (1 + holc_grade| msa_NAME)    , data = comb |> filter(sampling_density_log > -Inf), REML = TRUE)

d_fe_rirs <- lme4::lmer(log(sampling_density) ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + (1 + holc_grade| msa_NAME)    , data = comb |> filter(sampling_density_log > -Inf))

dB = d[sampling_density_log > -Inf]

mB0 = lmer(log(sampling_density) ~  holc_grade + 
    (1 | city_state), 
    data = dB
    ) 

mB1= lmer(log(sampling_density) ~ holc_grade + 
    (holc_grade | city_state), 
    data = dB)

mB2= lmer(log(sampling_density) ~ holc_grade + 
    (holc_grade_num | city_state), 
    data = dB)


mB1p= lmer(log(sampling_density) ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city_state), 
    data = dB, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

mB2p= lmer(log(sampling_density) ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city_state), 
    data = dB, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

# generate figure
library(broom.mixed)  # for tidy() with lme4
library(dplyr)
library(purrr)
library(forcats)

# add models to a list
models <- list(
  d_ri      = d_ri,
  d_rirs    = d_rirs,
  d_fe_rirs = d_fe_rirs,
  mB0       = mB0,
  mB1       = mB1,
  mB2       = mB2,
  mB1p      = mB1p,
  mB2p      = mB2p
)
# extract
coef_df <- map_df(
  names(models),
  ~ tidy(models[[.x]], effects = "fixed", conf.int = TRUE) %>%
    filter(grepl("^holc_grade", term)) %>%
    mutate(model = .x),
  .id = "model_id"
)

# Make grade readable
coef_df <- coef_df %>%
  mutate(
    holc_grade = gsub("holc_grade", "", term),
    holc_grade = ifelse(holc_grade == "", "(ref)", holc_grade),
    holc_grade = factor(holc_grade)
  )

# labels 
model_labels <- c(
  d_ri      = "holc_grade + (1 | metropoly)",
  d_rirs    = "holc_grade + (1 + holc_grade | metropoly)",
  d_fe_rirs = "holc_grade + ndvi + protected_area_ + pop_per_km + (1 + holc_grade | metropoly)",
  mB0       = "holc_grade + (1 | state_city)",
  mB1       = "holc_grade + (holc_grade | state_city)",
  mB2       = "holc_grade + (holc_grade_numeric | state_city)",
  mB1p      = "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  mB2p      = "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

# distinquish original models from our alternative ones
coef_df <- coef_df %>%
  mutate(
    model_group = ifelse(grepl("^d_", model), "Author's original", "Our new"),
    model_label = model_labels[model]
  )

# sort models
complexity_order <- c(
  "holc_grade + (1 | metropoly)",
  "holc_grade + (1 | state_city)",
  "holc_grade + (1 + holc_grade | metropoly)",
  "holc_grade + (holc_grade | state_city)",
  "holc_grade + (holc_grade_numeric | state_city)",
  "holc_grade + ndvi + protected_area_ + pop_per_km + (1 + holc_grade | metropoly)",
  "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

coef_df <- coef_df %>%
  mutate(model_label = factor(model_label, levels = complexity_order))

red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
blue_ =  "#46B8DAFF"

# plot
# Make grade readable
ggplot(coef_df, aes(x = estimate, y = fct_rev(model_label))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_group),
                 position = position_dodge(width = 0.6), height = 0) +
  geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  facet_wrap(~ holc_grade) +
  scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
  scale_fill_manual(values = c("white","#46B8DAFF")) +
  #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
  theme_minimal(base_size = 8) +
  labs(
    x = "Estimate of sampling density (ln-scale) relative to HOLC grade A",
    y = "Model structure",
    color = NULL,
    fill = NULL
  ) +
   theme(
    legend.key.height = unit(0.25, "cm")  # reduce vertical spacing between items 
    )
ggsave('Output/Fig_r1_sampling-density.png', width = 22, height = 5, units = 'cm')
ggsave('Output/Fig_r1_sampling-density.jpg', width = 22, height = 5, units = 'cm')

# point estimates for each holc grade figure
library(dplyr)
library(purrr)
library(lme4)
library(tibble)
library(forcats)
library(ggplot2)

# 1) Build absolute A/B/C/D estimates from each model using beta & vcov
abs_by_grade <- function(mod, model_name, model_label, model_group) {
  beta <- lme4::fixef(mod)
  V    <- as.matrix(vcov(mod))

  # terms may be "(Intercept)", "holc_gradeB", "holc_gradeC", "holc_gradeD"
  hasB <- "holc_gradeB" %in% names(beta)
  hasC <- "holc_gradeC" %in% names(beta)
  hasD <- "holc_gradeD" %in% names(beta)

  L <- list(
    A = c("(Intercept)" = 1),
    B = c("(Intercept)" = 1, "holc_gradeB" = as.numeric(hasB)),
    C = c("(Intercept)" = 1, "holc_gradeC" = as.numeric(hasC)),
    D = c("(Intercept)" = 1, "holc_gradeD" = as.numeric(hasD))
  )

  map_dfr(names(L), function(g) {
    w <- rep(0, length(beta)); names(w) <- names(beta)
    w[names(L[[g]])] <- L[[g]]
    est <- sum(w * beta)
    se  <- sqrt(drop(t(w) %*% V %*% w))
    tibble(
      model       = model_name,
      model_label = model_label,
      model_group = model_group,
      holc_grade  = factor(g, levels = c("A","B","C","D")),
      estimate    = est,
      conf.low    = est - 1.96*se,
      conf.high   = est + 1.96*se
    )
  })
}

# 2) Generate the absolute-estimates data frame (reuses your models + labels)

abs_df <- imap_dfr(models, ~{
  model_group <- ifelse(grepl("^d_", .y), "Author's original", "Our new")
  abs_by_grade(.x, .y, model_labels[[.y]], model_group)
})

abs_df <- abs_df %>%
  mutate(model_label = factor(model_label, levels = complexity_order)) 

# 3) Plot: grades stacked (A→D), model specification = color, model family = fill

pd <- position_dodge(width = 0.55)
#pd <- position_dodge2(width = 0.55, preserve = "single", reverse = TRUE)

ggplot(
  abs_df,
  aes(x = estimate, y = fct_rev(holc_grade),
      color = fct_rev(model_label), shape = fct_rev(model_group), group = fct_rev(model_label))
) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, position = pd, linewidth = 0.3, alpha = 0.9) +
  geom_point(size = 1.5, stroke = 0.8, position = pd, fill = "white") +
  scale_shape_manual(values = c(21,22),guide = guide_legend(reverse = TRUE, order = 1)) +
  # keep the point border aligned with family color for empty red / filled blue look:
  scale_color_discrete( name = "Model specification", 
                        limits = complexity_order) +
  #scale_y_discrete(expand = expansion(mult = c(0.06, 0.01)))+
  theme_minimal(base_size = 9) +
  labs(
    x = "Estimated sampling density by HOLC grade\n(ln-scale)",
    y = "HOLC grade",
    shape = NULL,
  ) +
  theme(
    legend.position = "right",
    legend.key.height = grid::unit(0.45, "cm"),
    legend.spacing.y  = grid::unit(0.1, "cm")
  )
ggsave('Output/Fig_r1_sampling-density_ABCD.png', width = 22, height = 8, units = 'cm')
ggsave('Output/Fig_r1_sampling-density_ABCD.jpg', width = 22, height = 8, units = 'cm')

# TODO - show the most complex models
tab_model(d_fe_rirs, mB1p , mB2p) # also solid d_fe_rirs_clim_inc
tab_model(d_fe_rirs, mB1p )
# tab_model(d_fe_rirs, d_fe_rirs2, d_fe_ri, d_fe_ri2, transform = 'exp', show.aic = TRUE)



# TODO are you back transformed?
d_fe_rirs |> plot_model()
d_fe_rirs |> plot_model(type = 'std')
d_fe_rirs |> plot_model(type = 'pred')
plot_model(d_fe_rirs, type = 'pred')$holc_grade


# # trying to reset colors
# y1 <- expression(Sampling ~ Density ~ (observations ~ per ~ km^2))
# (plot_model(d_fe_rirs, colors = holc_pal, terms = 'holc_grade', type = 'pred') +  aes( color='holc_gradeB'))
# 
#   # ylim(-10, 315) +
#   theme_bw(16) +
#   scale_y_continuous(expand = c(0.01, .15), breaks = seq(0, 300, 50)) +
#   labs(y = y1, x = 'HOLC Grade', title = 'Predicted Sampling Density') +
#   NULL -> p_samling_density)

# equatiomatic::extract_eq(d_fe_rirs)

y1 <- expression(Sampling ~ Density: ~observations ~ per ~ km^2)

(plot_model(d_fe_rirs, type = 'pred')$holc_grade + 
  # ylim(-10, 315) +
  theme_bw(14) + 
  scale_y_continuous(expand = c(0.0, .15), breaks = seq(0, 350, 50), limits = c(0, 300)) +
  labs(y = y1, x = 'HOLC Grade', title = 'Predicted Sampling Density') + 
  NULL -> p_sampling_density)


# ggsave(  filename = paste0(getwd(), '/figures/p_sampling_density_', Sys.Date(), '.png')
#        , width = 8.7, height = 10, units = 'cm', dpi = 450
#        , scale = 1.25)
  
plot_model(d_fe_rirs, type = 'pred', transform = 'exp')$holc_grade
plot_model(d_fe_rirs, type = 'pred', transform = 'exp')$holc_grade + ylim(0, 300) + theme_bw(16)


d_fe_rirs |> plot_model(type = 're') #, sort.est = TRUE)
d_fe_rirs |> plot_model(type = 'diag')


## C completeness
c_ri      <- lme4::lmer(completeness ~ holc_grade + (1 | msa_NAME)                                , data = comb |> filter(holc_grade != 'E'), REML = TRUE)

c_rirs    <- lme4::lmer(completeness ~ holc_grade + (1 + holc_grade | msa_NAME)                   , data = comb |> filter(holc_grade != 'E'), REML = TRUE)

# winners
c_fe_rirs <- lme4::lmer(completeness ~ holc_grade + scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + (1 + holc_grade| msa_NAME)    , data = comb |> filter(holc_grade != 'E'), REML = TRUE)

# MaPe suggested models
dC = d[!is.na(completeness)]

mC0 = lmer(completeness ~  holc_grade + 
    (1 | city), 
    data = dC
    ) 

mC1= lmer(completeness ~ holc_grade + 
    (holc_grade | city), 
    data = dC)

mC2= lmer(completeness ~ holc_grade + 
    (holc_grade_num | city), 
    data = dC)


mC1p= lmer(completeness ~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade | city), 
    data = dC, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  

mC2p= lmer(completeness~ holc_grade + 
    scale(ndvi) + scale(pct_pa) + scale(pop_per_km) + 
    scale(mean_temp_c)*scale(mean_precip_mm) + 
    (holc_grade_num | city), 
    data = dC, 
    control = lmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
    )  
# PLOT
# add models to a list
models_C <- list(
  c_ri      = c_ri,
  c_rirs    = c_rirs,
  c_fe_rirs = c_fe_rirs,
  mC0       = mC0,
  mC1       = mC1,
  mC2       = mC2,
  mC1p      = mC1p,
  mC2p      = mC2p
)
# extract
coef_df_C <- map_df(
  names(models_C),
  ~ tidy(models_C[[.x]], effects = "fixed", conf.int = TRUE) %>%
    filter(grepl("^holc_grade", term)) %>%
    mutate(model = .x),
  .id = "model_id"
)

# Make grade readable
coef_df_C <- coef_df_C %>%
  mutate(
    holc_grade = gsub("holc_grade", "", term),
    holc_grade = ifelse(holc_grade == "", "(ref)", holc_grade),
    holc_grade = factor(holc_grade)
  )

# labels 
model_labels_C <- c(
  c_ri      = "holc_grade + (1 | metropoly)",
  c_rirs    = "holc_grade + (1 + holc_grade | metropoly)",
  c_fe_rirs = "holc_grade + ndvi + protected_area_ + pop_per_km + (1 + holc_grade | metropoly)",
  mC0       = "holc_grade + (1 | state_city)",
  mC1       = "holc_grade + (holc_grade | state_city)",
  mC2       = "holc_grade + (holc_grade_numeric | state_city)",
  mC1p      = "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  mC2p      = "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

# distinquish original models from our alternative ones
coef_df_C <- coef_df_C %>%
  mutate(
    model_group = ifelse(grepl("^c_", model), "Author's original", "Our new"),
    model_label = model_labels_C[model]
  )

# sort models
complexity_order <- c(
  "holc_grade + (1 | metropoly)",
  "holc_grade + (1 | state_city)",
  "holc_grade + (1 + holc_grade | metropoly)",
  "holc_grade + (holc_grade | state_city)",
  "holc_grade + (holc_grade_numeric | state_city)",
  
  "holc_grade + ndvi + protected_area_ + pop_per_km + (1 + holc_grade | metropoly)",
  "holc_grade + ndvi + protected_area_% + population_dencitty + temperature * precipitation + (holc_grade | state_city)",
  "holc_grade + ndvi + protected_area_ + pop_per_km + temp * precip + (holc_grade_numeric | state_city)"
)

coef_df_C <- coef_df_C %>%
  mutate(model_label = factor(model_label, levels = complexity_order))

red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
blue_ =  "#46B8DAFF"

# plot
# Make grade readable
ggplot(coef_df_C, aes(x = estimate, y = fct_rev(model_label))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = model_group),
                 position = position_dodge(width = 0.6), height = 0) +
  geom_point(aes(color = model_group, fill = model_group), position = position_dodge(width = 0.6), size = 1.5, shape =21) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  facet_wrap(~ holc_grade) +
  scale_color_manual(values = c("#D43F3AFF","#46B8DAFF")) +
  scale_fill_manual(values = c("white","#46B8DAFF")) +
  #scale_shape_manual(values = c(21, 16), guide = "none") +  # shapes fixed, no shape legend
  theme_minimal(base_size = 8) +
  labs(
    x = "Estimate of completeness relative to HOLC grade A",
    y = "Model structure",
    color = NULL,
    fill = NULL
  ) +
   theme(
    legend.key.height = unit(0.25, "cm")  # reduce vertical spacing between items 
    )
ggsave('Output/Fig_r1_completeness.png', width = 22, height = 5, units = 'cm')
ggsave('Output/Fig_r1_completeness.jpg', width = 22, height = 5, units = 'cm')

# point estimates for each holc grade figure

# 2) Generate the absolute-estimates data frame (reuses your models + labels)

abs_df_C <- imap_dfr(models_C, ~{
  model_group <- ifelse(grepl("^c_", .y), "Author's original", "Our new")
  abs_by_grade(.x, .y, model_labels_C[[.y]], model_group)
})

abs_df_C <- abs_df_C %>%
  mutate(model_label = factor(model_label, levels = complexity_order))#%>%arrange(model_label)  # ensure dodge uses same ordering 

# 3) Plot: grades stacked (A→D), model specification = color, model family = fill

pd <- position_dodge(width = 0.55)
#pd <- position_dodge2(width = 0.55, preserve = "single", reverse = TRUE)
#TODO check why order of the points not as it should be
ggplot(
  abs_df,
  aes(x = estimate, y = fct_rev(holc_grade),
      color = fct_rev(model_label), shape = fct_rev(model_group), group = fct_rev(model_label))
) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, position = pd, linewidth = 0.3, alpha = 0.9) +
  geom_point(size = 1.5, stroke = 0.8, position = pd, fill = "white") +
  scale_shape_manual(values = c(21,22),guide = guide_legend(reverse = TRUE, order = 1)) +
  # keep the point border aligned with family color for empty red / filled blue look:
  scale_color_discrete( name = "Model specification", 
                        limits = complexity_order) +
  #scale_y_discrete(expand = expansion(mult = c(0.06, 0.01)))+
  theme_minimal(base_size = 9) +
  labs(
    x = "Estimated completeness by HOLC grade\n",
    y = "HOLC grade",
    shape = NULL,
  ) +
  theme(
    legend.position = "right",
    legend.key.height = grid::unit(0.45, "cm"),
    legend.spacing.y  = grid::unit(0.1, "cm")
  )
ggsave('Output/Fig_r1_completeness_ABCD.png', width = 22, height = 8, units = 'cm')
ggsave('Output/Fig_r1_completeness_ABCD.jpg', width = 22, height = 8, units = 'cm')


#TODO: ENDED HERE
# examine to most popular models #MaPe - they chose c_fe_ri
tab_model(c_fe_rirs,mC1p, mC2p) # also solid 

library(patchwork)
p_sampling_density / p_completeness + plot_annotation(tag_levels = 'I') 


c_fe_rirs |> plot_model(type = 're') #, sort.est = TRUE)
c_fe_rirs |> plot_model(type = 'diag')



## D nice tables of 3 best mods
```{r}

# tab_model(samp_d_binary_holc_rirs)  # no longer best
tab_model(samp_d_binary_holc) 
tab_model(d_fe_rirs, transform  = 'exp')
tab_model(c_fe_ri)


# 6 trends

# get holc area
(holc_area <- 
  holc |> 
  group_by(holc_grade) |> 
  summarise(sum_area_holc_km2 = sum(area_holc_km2)))


# does not take into account area
(
  counts_grade_year <- 
    read_csv('/Users/dlocke/_students/Diego/int_data/Biodiversity_data_R1/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') |> #MaPe read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') |> 
    filter(holc_grade != 'E') |> 
    arrange(year, holc_grade) |> 
    group_by(year, holc_grade) |>
    count() |> 
    group_by(year, holc_grade) |> 
    summarise(cumsum = cumsum(n))
  )

((2022 - 1932) + 1) *4

read_csv('/Users/dlocke/_students/Diego/int_data/Biodiversity_data_R1/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') |> group_by() |> summarise(sum_N_samples = sum(N_samples)) 
#MaPe 
read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') |> group_by() |> summarise(sum_N_samples = sum(N_samples))

t = fread('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
tt=t[year >= 2000 & year <= 2020]

# # this one was hashtagged out
read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv') |>
    filter(holc_grade != 'E') |>
     arrange(year, holc_grade) |>
     group_by(year, holc_grade) |>
     count() |>
     arrange(holc_grade) |>
    group_by(holc_grade) |>
    mutate(cumsum = cumsum(n)) |> #tail()
     filter(year >= 2000 & year <= 2020) |>
     left_join(holc_area, by = 'holc_grade') |> 
     mutate(sample_d = cumsum / sum_area_holc_km2) |> 
     # ggplot(aes(year, cumsum, group = holc_grade)) +
     ggplot(aes(year, sample_d, group = holc_grade)) +
     geom_line()

counts_grade_year |> 
  filter(year == 2000 | year == 2020) |> 
  left_join(holc_area, by = 'holc_grade') |> 
  mutate(sample_d = cumsum / sum_area_holc_km2)


counts_grade_year |> 
  filter(year >= 2000 & year <= 2020) |> 
  ggplot(aes(year, cumsum, group = holc_grade)) + 
  geom_line()
  
```


# END

### OLD

# viz

```{r}

p_samp <- plot_model(samp_d_rirs, type = 'pred')
p_comp <- plot_model(comp_d_rirs, type = 'pred')

ggpubr::ggarrange(p_samp, p_comp)

ggpubr::ggarrange(as.grob(p_samp), as.grob(p_comp))
```


ggsave(file = paste0(getwd(), '/figures/Fig_S1_pancake_v_donuts_',
gsub('[[:punct:]]', '_', Sys.Date()), '.png')
, width = 6.5*2
, height = 6.5*2)




## completeness diff by gini
```{r}
# # completeness by gini... obvs doesn't work, hence random effects
# comb |> 
#   ggplot(aes(
#     msa_gini, 
#     completeness
#     # completeness_log
#     # sampling_density
#     # sampling_density_log
#     , color = holc_grade)) + 
#   scale_color_manual(values = holc_pal_f) + 
#   geom_point(alpha = .5) + 
#   geom_smooth(color = 'black') + 
#   theme_bw(15) + 
#   facet_wrap(~holc_grade) + 
#   NULL

(
  comb |> 
    filter(holc_grade == 'A' | holc_grade == 'D') |>
    group_by(city, holc_grade) |> 
    summarise(mean_complete = mean(completeness, na.rm = TRUE)) |> 
    pivot_wider(id_cols = city
                , names_from = holc_grade
                , values_from = mean_complete) |> 
    mutate(diff_complete = A - D) |> 
    tidylog::select(city, diff_complete) |> 
    drop_na() -> city_diff_complete
  )

holc |> distinct(city, msa_gini)


city_diff_complete |> 
  inner_join(holc |> 
               distinct(city
                        # , msa_medhhincE
                        # , msa_p
                        # , msa_M
                        # , msa_ent_ratio
                        , msa_H
                        # , msa_total_popE
                        )
             , by = 'city'
             ) |> 
  ggplot(aes(diff_complete
             # , msa_medhhincE
             # , msa_p
             # , msa_M
             # , msa_ent_ratio
             , msa_H
             # , msa_total_popE
             )) + 
  geom_point() + 
  geom_smooth() + 
  theme_bw(16) + 
  NULL

  


