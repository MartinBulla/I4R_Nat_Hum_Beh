
#+ f4_alternative, fig.width=17*0.393701,fig.height=12*0.393701 
#TODO: START HEREmake sure that this part run and if not, check the leftover 2025-10-09 13:11:23
# load packages
require(ggrepel)

# add holc grade
holc_area_dt = data.table(holc_area)
dh = merge(tt,holc_area_sum_b_dt, all.x = TRUE)
dh[, sampling_density := n_obs/area_sum]

#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%



#' ## Robustness
#' To account for non-independence of unique polygons and their data across years, we have created a dataset with number of observations for each unique polygon (i.e. city-specific HOLC-grades and sampling polygon ids). We then run specified mixed-effect models with sampling density (in km2) as a response and year (continuous) in interaction with HOLC grade (four-level factor) as predictors while controlling for non-independence of data points by random effect. We specified 6 models varying in the random effects and compared their estimates for the fixed effect predictors.
#' (1) Random intercept of state, city within state and unique sampling polygon id
#' (2) Same as (1) but explicitely neste
#' (3) Same as (1), but with random slope of year within city
#' (4) Same as (2), but with random slope of year
#' (5) Random slope of year within HOLC grade, nested within city and state (random intercepts) and separate randome intercept for unique polygon id. 
#' 
#' The results reveal that TODO:continue here 