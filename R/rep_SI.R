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
#' For the sake of reproducibility we stored data and code provided by the authors with the paper in the folder ‘original_paper’ folder (at the root project’s directory) with subfolders ‘Data’ and ‘Code’, the latter two with the structure as provided by the authors. We stored the additional data acquired upon the request from The Institute for Replication at the ‘Data’ folder within the root project directory. Datasets recreated from the `04_R4_uneven_biodiversity_data_2023.R` authors code are at 'Data/from_script_04' and additional data recreate by us by our script '04_R4_temp_trend_generation.R' (which is the adjusted version of the author's one) at 'Data/MaPe'.
#' 
#' **Code generting the outputs of this html are is availalbe within the html upon clicking the code button at top right above each display item!**
#' 
#' # Replicating Fig. 4
#' ## Computational
#' Code generating output that resembles Fig. 4 is located in two places, each yielding different results. 
#'
#' (1) Code with the heading “6 trends” from the key analysis file `05_paper_1_analyses_R4_check.Rmd` generates trend lines for all four HOLC categories, but instead for sampling density, supposedly depicted by Fig. 4, it depicts number of HOLC polygons that were sampled (A). To complete the picture, we also show the trends for number of sampling observations (b)

#+ r_f4_1, fig.width=17*0.393701,fig.height=8*0.393701 
# Here we show the authors original code with our adjustments indicate by MaPe

# Load packages
#suppressPackageStartupMessages(require(dplyr); require(ggplot2))
require(dplyr)
require(ggplot2)
require(patchwork)

# Color palette for redlining
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)#, '#A9A9A9' # dark gray)

suppressWarnings({ #MaPe added
  counts_grade_year <- 
    readr::read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv',#MaPe changed folder path from read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
    show_col_types = FALSE   # MaPe added to suppress readr column spec message
    ) |>
    filter(holc_grade != 'E') |> 
    arrange(year, holc_grade) |> 
    group_by(year, holc_grade) |>
    count() |> 
    group_by(year, holc_grade) |> 
    summarise(cumsum = cumsum(n), .groups = "drop") # MaPe added ', .groups = "drop"' to suppress information messages
  })

# MaPe added color mapping
gg1 =
counts_grade_year |>  
  filter(year >= 2000 & year <= 2020) |> 
  ggplot(aes(year, cumsum, col = forcats::fct_rev(holc_grade))) +  #MaPe changed group (group=holc_grade) to col to label the holc_grade lines
  geom_point() + #MaPe added to aid visualisation
  geom_line(size = 1) +
  scale_color_manual(values = holc_pal, guide = guide_legend(reverse = TRUE), name = 'HOLZ grade') + #MaPe added for consistent coloring
  labs(y = '# of sampled HOLC polygons', tag='A') + #MaPe added
  theme_light() #MaPe added for consistency

# MaPe added # of observations per year and HOLC 
# replicate their outputs with data.table
tt = fread('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
names(tt) <- c('Year','holc_grade', 'Sum')
tt = tt[holc_grade != 'E'] #d = data.table(temporal_trend)
d = tt[, .(n_obs = sum(Sum)), by = list(Year, holc_grade)]
d = d[order(holc_grade,Year)]

gg2 =
  ggplot(d[Year >= 2000 & Year <= 2020], aes(Year, n_obs, col = forcats::fct_rev(holc_grade))) +  #MaPe changed group (group=holc_grade) to col to label the holc_grade lines
  geom_point() + #MaPe added to aid visualisation
  geom_line(size = 1) +
  scale_color_manual(values = holc_pal, guide = guide_legend(reverse = TRUE), name = 'HOLZ grade') + #MaPe added for consistent coloring
  labs(y = '# of observations', tag='B') + #MaPe added
  theme_light() #MaPe added for consistency  

(gg1 + gg2) +
plot_layout(guides = "collect") & # collect legends to one
theme(legend.position = "right")  # place legend on right
#ggsave('Output/Fig_r4_count.jpg', width= 20, height = 10, units ='cm')  

#' (2) Code in `04_R4_uneven_biodiversity_data_2023.R`, section [7] was not running due to absolute folder assignments that were unclear about where to actually locate the files. We searched for the files `R1_biodiv_col_code_by_holc_id_2000_2020.csv` and `R1_biodiv_trend_by_time_holc_id_1933_2022.csv` among folders provided by the authors and respecified the folder paths. In addition, the original code loaded packages with conflicting functions. To smoothly reproduce the code, we thus made a new code (visible below) where we loaded only the relevant packages and only the relevant data. Then the script, thought to generate Fig. 4, run without issues. To aid visualisation we also plotted the points that the line was connecting and an alternative plot where the lines represent locally estimated scatterplot smoothing (LOESS).
 
#+ r_f4_2, fig.width=15*0.393701,fig.height=8*0.393701 
# Here we show the authors original code with our adjustments indicate by MaPe

# Load packages
  require(dplyr)
  require(ggplot2)
  require(patchwork)

# Color palette for redlining
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)#, '#A9A9A9' # dark gray)

# copy of the L51-65 of 04_R4_uneven_biodiversity_data_2023.R, with MaPe changed folder path
  holc <- suppressWarnings(sf::st_read('Data/holc_ad_data.shp', quiet = TRUE) %>% #MaPe changed folder path 
    sf::st_cast('POLYGON') %>% # IMPORTANT
    dplyr::filter(!sf::st_is_empty(.)) %>% 
    sf::st_make_valid(.) %>% 
    tibble::rowid_to_column() %>% 
    dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                    , city_state = paste0(city, ', ', state)
                    , area_holc_km2 = as.double(sf::st_area(.) / 1e+6)) %>% 
    dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) )

  # Calculate the area of holc polygons
  holc_area <-  holc %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(area_sum = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')  %>% as_tibble() %>% dplyr::select(-geometry)
  # MaPe gives
    #  $ holc_grade: chr [1:4] "A" "B" "C" "D"
    #  $ area_sum  : num [1:4] 1282 2948 4365 2689

#copy of the L391-430 of 04_R4_uneven_biodiversity_data_2023.R, with changed folder path
# Load 1933-2022 data
temporal_trend = read.table('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv', header= T,sep=',')
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(temporal_trend) <- c('Year','holc_grade', 'Sum')
temporal_trend = temporal_trend %>% filter(holc_grade != 'E')
# MaPe hashtagged out as it needed other data, not relevant for the current case: sum(temporal_2000_2020$Sum,na.rm=T) / sum(temporal_trend$Sum,na.rm=T) # 77.8 % of biodiversity data collected in last 20 years ! 

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

temporal_all_data = rbind(trend_A,trend_B,trend_C,trend_D) # MaPe added 2 in the name to aid data investigation

# Plot temporal trend: 2000-2020; same as in the script, but showing legend
g1 =
temporal_all_data %>%
filter(Year >= 2000 & Year <= 2020) %>%
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%
ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) + # MaPe added to aid visualisation
geom_line(aes(color = holc_grade), size = 1, show.legend = FALSE) +
#stat_smooth(aes(color = holc_grade), size = 1) +
scale_color_manual(values = holc_pal, name = 'HOLZ grade') +
labs(subtitle = 'Lines connect points') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
ylab('Sampling density\n (bird observations per 1km²)')+
  theme( plot.subtitle = element_text(size = 10, colour = "grey40"))

#g1; ggsave('Output/Fig_r4_density.jpg', width= 15, height = 15, units ='cm') 

# MaPe - LOESS smoothed
g2 =
temporal_all_data %>%
filter(Year >= 2000 & Year <= 2020) %>%
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%
ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) + # MaPe added to aid visualisation
#geom_line(aes(color = holc_grade), size = 1) +
stat_smooth(aes(color = holc_grade), size = 1, show.legend = FALSE) +
scale_color_manual(values = holc_pal, name = 'HOLZ grade') +
labs(subtitle = 'Lines represent LOESS') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
ylab('Sampling density\n (bird observations per 1km²)')+
theme( plot.subtitle = element_text(size = 10, colour = "grey40"))
#g2; ggsave('Output/Fig_r4_density_LOESS.jpg', width= 15, height = 15, units ='cm') 

# MaPe: combine the above plots 
g2_ <- g2 + theme(axis.title.y = element_blank(),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank())

(g1 + g2_) +
plot_layout(guides = "collect") & # collect legends to one
theme(legend.position = "right")  # place legend on right

#ggsave('Output/Fig_r4_density_ab.jpg', width= 25, height = 15, units ='cm') 

#' The generated plots show sampling density per km^2, which differs from the supposed sampling density per km^2 depicted in the main text Fig. 4 (copied below). 

#+ ori_F4, fig.width=8*0.393701,fig.height=10*0.393701 

library(png)       # or jpeg
library(grid)
library(cowplot)

# image panel
img_panel <- ggdraw() + draw_image("original_paper/ori_Fig_4.png")

# title as its own row
img_title <- ggdraw() +
  draw_label("Original",
             x = 0.25, hjust = 0, vjust = 1, size = 10, color='grey40') +
  theme(plot.margin = margin(0, 0, 0.5, 0))

# combine title + image
plot_grid(img_title, img_panel, ncol = 1,
                            rel_heights = c(0.06, 1))

#' We generated our figure with the dataset and code provided by the authors with their publication, as well as be  regenerating the dataset using the provided code`04_R4_uneven_biodiversity_data_2023.R` (L13-203). The regenaration created the same dataset as provided by the authors. We also manipulated the dataset with our own code only to get the same dataset that inputs into the ggplot() function that produces the figure.
#'   
#' We then realized that the convoluted chunk of code in `04_R4_uneven_biodiversity_data_2023.R` (L437-47) produces the correct dataset, only if `plyr` R-package is not loaded. When the package is loaded in R, the code multiplies the number of observations so that summing of all observation per HOLZ grade and year (cumsum_n_obs) gives about 7 times more observations than the acutal number of observations. In other words, **our figure is indeed the correct one!**
#' 
#' The corrected figure, we have generated, shows that (i) sampling for all categories was similar until ±2009, and (ii) the differences in sampling between categories likely increased. Note however, that such differences are easier to see if we plot the sampling densities of two categories agains each other.
#' 
#+ f4_alternative, fig.width=17*0.393701,fig.height=12*0.393701 
# load packages
require(data.table)
require(ggrepel)

# replicate data manipulatin  with data.table
tt = fread('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
#tt = fread('Data/from_script_04/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
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

# all pairwise combinations you want
pairs <- data.table(
  x = c("D","D","D","C","C","B"),
  y = c("A","B","C","A","B","A")
)

# join to create plotting dataset
plotdat <- pairs[, .(x, y)][
  , merge(dh[holc_grade==x], dh[holc_grade==y], by="Year", suffixes=c(".x",".y")), 
  by=.(x,y)
][, pair := paste0(y, " ~ ", x)]

# plot
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
# ggsave('Output/Fig_r4_alternative.jpg', width= 21, height = 14, units ='cm')  

#' Such figure highliths the effects discussed by the authors. However, same as the intended Fig. 4, the data behind these figures are heavily psedoreplicated because they do not account for spatial and temporal non-independence of data points. 

#' ## Robustness
#' To account for non-independence of unique polygons and their data across years, we have created a dataset with number of observations for each unique polygon (i.e. city-specific HOLC-grades and sampling polygon ids). We then run specified mixed-effect models with sampling density (in km2) as a response and year (continuous) in interaction with HOLC grade (four-level factor) as predictors while controlling for non-independence of data points by random effect. We specified 6 models varying in the random effects and compared their estimates for the fixed effect predictors.
#' (1) Random intercept of state, city within state and unique sampling polygon id
#' (2) Same as (1) but explicitely neste
#' (3) Same as (1), but with random slope of year within city
#' (4) Same as (2), but with random slope of year
#' (5) Random slope of year within HOLC grade, nested within city and state (random intercepts) and separate randome intercept for unique polygon id. 
#' 
#' The results reveal that TODO:continue here 