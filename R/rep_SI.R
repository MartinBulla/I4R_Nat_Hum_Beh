#' ---
#' title: <font size="2">Replicating </font><br><font size="6">Ellis-Soto et al 2023, Nat Hum Beh</font>
#' author: <font size="3">Martin Bulla & Peter Mikula</font><br><br><font size="2">created by Martin Bulla</font><br>
#' date: <font size="1.5">`r Sys.time()`</font>
#' bibliography: ../Resources/_bib.bib
#' reference-section-title: "References"
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
#' Scripts generting the outputs of this html are availalbe within the html upon clicking the `code` button at top right above each display item!
#' 
#' ##### Code to load tools and data
#+ start, echo = T, results = 'hide', warning=FALSE

# 1. constants and functions
recreate_data = FALSE # use TRUE, if you wish to recreate the data, instead of loading them from .Data/ 

# Color palette for redlining
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
)#, '#A9A9A9' # dark gray)

# add tag in a circle to a ggplot panel
tag_pos <- c(0.045, 0.94)   # x, y in panel [0..1]

offset_tag <- theme(
  plot.tag.location = "panel",
  plot.tag.position = tag_pos,
  plot.tag = element_text(hjust = 0.5, vjust = 0.5, size = 11)
)

circle_at_tag <- function(xy = tag_pos, r_pt = 6){
  annotation_custom(
    grob = grid::circleGrob(
      x = unit(xy[1], "npc"), y = unit(xy[2], "npc"),
      r = unit(r_pt, "pt"),
      gp = grid::gpar(fill = "white", col = "grey25", lwd = .6)
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
}               


# 2. load or install packages
pkgs <- c("cowplot","data.table","dplyr", "ggplot2","ggpp", "grid", "lme4", "patchwork")  # list of packages

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

# add area per holc grade (as the authors used two ways to calculate this, we test both, but then use only the (b) as that seems to be the one eventually used)
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
ttb = merge(tt,holc_area_sum_b_dt, all.x = TRUE)

# sampling density
tta[, sampling_density := n_obs/sum_area_holc_km2]
ttb[, sampling_density := n_obs/sum_area_holc_km2]
tta[, sampling_density_b :=ttb$sampling_density]

#ggplot(tta, aes(x = sampling_density, y = sampling_density_b)) + 
#  geom_abline(slope = 1, intercept = 0, linetype = "dotted", col = 'red') + 
#  geom_point() + 
#  facet_wrap(~holc_grade) +
#  coord_equal(expand = FALSE)  

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

#' # 2. Replicating temporal trends  
#' TODO: Peto, let's decide whether the current i-iii order below is ok. I somehow feel that it might be better to first discuss Fig. 4; highlight the issue with the data and then use the correct data.
#' 
#' The results on temporal trends contain three key outputs:  
#' i. Claim about 35.6% in relative disparity between HOLC grade A and D from 2000 to 2020.  
#' ii. Visualised temporal trends in Fig. 4.  
#' iii. General additive model on temporal trends in Table S4.  
#' 
#'
#' ## i. Claim about 35.6% change in disparity 
#' 
#' We did not find the code generating the 35.6% claim and hence could only speculate. Using the authors data on sampling density per year and holc grade, calculated ration between A/B ration for 2020 and 2000 generated a different reult (`r dispar`%). This results seems to reflect the one (40%) from  Fig. 4 legend.
#' 
#' However, plotting the disparity data shows non-linear temporal relatinship (Fig. [X](#F_X)A), questioning the use of arbitrary 2000 and 2020 comparison. Instead, the disparity varies in time and depending on the dataset may be even negative for A grade (Fig. [X](#F_X)B). Moreover, the authors do not justify the exclusion of <2000 years. Including those years shows even more complex picture, albeit the <2000 might be less reliable because online platforms were non-existend (and likely also represent post hoc data entry).
#' 
#+ F_X, fig.width = 15/2.5, fig.height = 15/2.5

# prepare author'sdata
a = ttb[holc_grade%in%c('A','D')]
aw <- a[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density")]
aw[, dispar := 100*((A/D)-1)] #aw[, dispar := A/D]

# prepare our data
dd_ = dd[holc_grade%in%c('A','D')]
w <- dd_[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density")]
w[, dispar := 100*((A/D)-1)] #w[, dispar := A/D]

# prepare out data - median
dd_med = d_med[holc_grade%in%c('A','D')]
w_med <- dd_med[order(year),
            data.table::dcast(.SD, year ~ holc_grade, value.var = "sampling_density_med")]
w_med[, dispar := 100*((A/D)-1)] #w[, dispar := A/D]

# plot
g1a = ggplot(aw[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red') + labs(subtitle = "Author's aggregation; sum per year") + theme_bw() 
g1b = ggplot(aw, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_bw()
  #(g1a|g1b) + plot_layout(axis_titles = "collect")

g2a = ggplot(w[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "Our aggregation; sum per year") + theme_bw()
g2b = ggplot(w, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_bw()
#(g2a|g2b) + plot_layout(axis_titles = "collect")

g3a = ggplot(w_med[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "Our aggregation; median per year") + theme_bw() 
g3b = ggplot(w_med, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_bw()

#(g3a|g3b) + plot_layout(axis_titles = "collect")

g1a_ = g1a + theme(axis.title = element_blank(),  axis.text.x  = element_blank()) +
  coord_cartesian(ylim = c(0, 200)) + scale_y_continuous(breaks = seq(0,200, by = 50)) 
g1b_ = g1b + theme(axis.title = element_blank(),axis.text.x  = element_blank()) + 
  coord_cartesian(xlim = c(1925, 2025), ylim = c(-100, 500)) + scale_y_continuous(breaks = seq(-100, 500, by = 100))  

g2a_ = g2a + theme(axis.title = element_blank(), axis.text.x  = element_blank()) + coord_cartesian(ylim = c(0, 200)) + scale_y_continuous(breaks = seq(0,200, by = 50)) 
g2b_ = g2b + theme(axis.title = element_blank(),axis.text.x  = element_blank()) + coord_cartesian(xlim = c(1925, 2025), ylim = c(-100, 500))+ scale_y_continuous(breaks = seq(-100, 500, by = 100))  

g3a_ = g3a + theme(axis.title = element_blank()) +coord_cartesian(ylim = c(0, 200)) + scale_y_continuous(breaks = seq(0,200, by = 50)) 
g3b_ = g3b + theme(axis.title = element_blank()) + coord_cartesian(xlim = c(1925, 2025), ylim = c(-100, 500))

p <- ((g1a_  / g2a_ / g3a_) | (g1b_ / g2b_ / g3b_)) & theme(plot.subtitle = element_text(size = 11, colour = 'grey60'))
  
  
# add one set of axis titles
ggdraw(xlim = c(-0.03, 1), ylim = c(-0.03, 1)) +
  draw_plot(p)+
  draw_label("Year", x = 0.5, y = -0.03, vjust = -1, size = 11) +
  draw_label("Relative disparity between HOLC grade A and D\n[%; D as a baseline]", x = -0.03, y = 0.5, angle = 90, vjust = 1.5, size = 11) #ggsave('Output/Fig_X-trends_3-rows_holc-area-b-all.jpg', units = 'cm', width = 15, height = 21)

#' <a name="F_X">
#' **Figure X</a> | Change in relative disparity in sampling density between HOLC grade A and D over time.** Each point represents percentage difference in sampling density of A given D (i.e. D being the baseline) based on overall sampling density (i.e. sum of all A or D devided by the overall A or D area;  the first two rows) or median sampling density per HOLC grade and year. Lines represent  local regression non-parametric smoothing and shaded areas its 95% confidence intervals. Top row represent the aggregation done by the author's, bottom row the aggregatin done by us. Note that the authors' dataset did not contain area per year and category; hence, we were unable to compute the median sampling density for their dataset.
#' 
#' TODO: Martin check whether the above is meaningful and whether authors' scripts cannot be tweeked to provide neighberhood ids.
#' 
#' TODO: Peto, let's decide whether we keep all three rows in the above figure or not. After fixing the area, it does not look as dramatic as we thought!!!.
#' 
#' ## ii. Visualised temporal trends in Fig. 4.
#' The output that resembles Fig. 4 is located in two places, each yielding different results. 
#'
#' (a) According to the authors' [README](original_paper/Code/README_code.md), the code `05_paper_1_analyses_R4_check.Rmd` should contain all the key analyses for the paper. We did find a script with the heading “6 trends”, which generates trend lines for all four HOLC categories, but instead for sampling density,  depicted by authors' Fig. 4, it depicts number of HOLC polygons that were sampled (A). To complete the picture, we also show the trends for number of sampling observations (B).
#' 
#' TODO: PETO, let's decide whether the above is fair. They do inndicate in their README_code.md the following:
#' **04_R4_uneven_biodiversity_data_2023.R**: The aim of this script is to create multiple summary tables and analysis which represent the foundation for statistical analysis on our study. Specifically, sampling density, survey completeness and biodiversity coldspots are calculated. Temporal trends of biodiversity across HOLC grades is calculating using a generalized additive model.
#'
#'**05_paper_1_analyses_R4.R**: Analysis and modeling steps exploring predictors of bird sampling density, biodiversity coldspots and biodiversity records by observation type.
#' 
#+ r_f4_obs, fig.width=20*0.393701,fig.height=9*0.393701 

# Here we show the authors original code with our adjustments indicate by MaPe

# a) prepare panel A and B

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
r_f4_A =
counts_grade_year |>  
  filter(year >= 2000 & year <= 2020) |> 
  ggplot(aes(year, cumsum, col = forcats::fct_rev(holc_grade))) +  #MaPe changed group (group=holc_grade) to col to label the holc_grade lines
  geom_point() + #MaPe added to aid visualisation
  geom_line(linewidth = 1) +
  scale_color_manual(values = holc_pal, guide = guide_legend(reverse = TRUE), name = 'HOLZ grade') + #MaPe added for consistent coloring
  labs(y = '# of sampled HOLC polygons', tag='A') + #MaPe added
  theme(legend.position = "right", plot.title = element_text(size = 10))+
  theme_light() #MaPe added for consistency

# MaPe added # of observations per year and HOLC 
r_f4_B =
  ggplot(tt[year >= 2000 & year <= 2020], aes(year, n_obs, col = forcats::fct_rev(holc_grade))) +  #MaPe changed group (group=holc_grade) to col to label the holc_grade lines
  geom_point() + #MaPe added to aid visualisation
  geom_line(size = 1) +
  scale_color_manual(values = holc_pal, guide = guide_legend(reverse = TRUE), name = 'HOLZ grade') + #MaPe added for consistent coloring
  labs(y = '# of observations', tag='B') + #MaPe added
  theme(legend.position = "right", plot.title = element_text(size = 10))+
  theme_light() #MaPe added for consistency  

(r_f4_A + r_f4_B) +
plot_layout(guides = "collect", axis_titles = 'collect') & # collect legends to one
theme(legend.position = "right")  #ggsave('Output/Fig_r4_count_v2.jpg', width= 20, height = 9, units ='cm')  

#' <a name="F_X2">
#' **Figure X2</a> | Change in # of sampled polygons and # of observations according to HOLC grade over time.** Each point represents the sum per year. Lines aid the visualisation by connecting the points.
#' 
#' (b) The code `04_R4_uneven_biodiversity_data_2023.R` contains section *[7] Plot temporal trends 1933-2022 and 2000-2020*. The script was not initially running due to absolute folder assignments that were unintuitive regarding the location of the files. We searched for the required files `R1_biodiv_col_code_by_holc_id_2000_2020.csv` and `R1_biodiv_trend_by_time_holc_id_1933_2022.csv` among folders provided by the authors and respecified the folder paths. In addition, the original script loaded packages with conflicting functions. To smoothly reproduce the code, we thus made a new script where we loaded only the relevant packages and only the relevant data. Then the script, thought to generate Fig. 4, run without issues. To aid visualisation we also plotted the points that the line was connecting and an alternative plot where the lines represent locally estimated scatterplot smoothing (LOESS). However, non of these outputs resembls the one from the authors' Fig. 4.<br><br>We then found out that the complex coding, along with the conflicting R-packages multiplied the number of observation (our Fig. O below) and only such data produced the authors' Fig. 4. Specifically, the convoluted chunk of code in `04_R4_uneven_biodiversity_data_2023.R` (L410-20) produces the correct dataset, only if `plyr` R-package is not loaded. When the `plyr` package is loaded in R, the code multiplies the number of observations so that summing of all observation per HOLC grade and year (cumsum_n_obs) gives about 7 times more observations than the acutal number of observations and hence sampling densities per km2 are high. In other words, **our Figure X3 C and D are indeed the correct ones!**. Note, in the initial Fig. 4 the ploted lines just connected the data points (our Fig. X3 A), but fitting a line through the data  produces a more realistic picture (our Fig. X3 B; albeit here using only LOESS, while using model predictions would be more fitting - see later.)
#'  
#' 
#' TODO: PETO IS THE ABOVE ENOUGH or  we include also the following paragraph that we initially had: *Specifically, w generated our figure with the dataset and code provided by the authors with their publication, as well as be  regenerating the dataset using the provided code`04_R4_uneven_biodiversity_data_2023.R` (L13-203). The regenaration created the same dataset as provided by the authors. We also manipulated the dataset with our own code only to get the same dataset that inputs into the ggplot() function that produces the figure.*
#'  


#+ r_check_trend_d, fig.width=23*0.393701,fig.height=10*0.393701 

# prepare the multiplied dataset (chunk of code from `04_R4_uneven_biodiversity_data_2023.R` (L410-20))
require(plyr) # the package causeing the multiplications
temporal_trend = read.table("original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv",
                            header = TRUE,sep = ",")
# names(temporal_trend) <- c('Year','holc_grade','Type','holc_polygon_id', 'Sum')
names(temporal_trend) <- c('Year','holc_grade', 'Sum')
temporal_trend = temporal_trend %>% filter(holc_grade != 'E')

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

mul = data.table(rbind(trend_A,trend_B,trend_C,trend_D))

# load the correct dataset (note that for consistency we use the same code as above, but without the plyr package. Note, the code can be drastically simplified by removing the redundant parts and using data.table as we do in the above section [Code to load tools and data](Code to load tools and data).

detach("package:plyr", unload = TRUE, character.only = TRUE) #unload plyr package to recieve a correct picture

# authors' code (L412-20); note that the following code that multiplied the datase is unnecessary as tmppp already contains number of observations and hence only area needed to be joined
trend_A = tmpppp %>% filter(holc_grade == 'A') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_B  = tmpppp %>% filter(holc_grade == 'B') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_C  = tmpppp %>% filter(holc_grade == 'C') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

trend_D  = tmpppp %>% filter(holc_grade == 'D') %>% mutate(cumsum_n_obs = cumsum(n_obs)) %>% left_join(holc_area) %>% mutate(sampling_density = cumsum_n_obs /area_sum )

ok = data.table(rbind(trend_A,trend_B,trend_C,trend_D))

ok[, cumsum_n_obs_mul:=mul$cumsum_n_obs]
ok[, sampling_density_ok := cumsum_n_obs/area_sum]
ok[, sampling_density_mul := cumsum_n_obs_mul/area_sum]

# prepare for plotting
cor_a = 
ggplot(ok, aes(x = cumsum_n_obs, y = cumsum_n_obs_mul, col = holc_grade)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", col = 'red') +
  labs(x = "# of observations\n[true]", y = "# of observations\n[excidently multiplied]")+
  scale_color_manual(values = holc_pal, name = 'HOLC grade') + 
  theme_bw() +
  circle_at_tag() + labs(tag = "A") + offset_tag

cor_b = 
ggplot(ok, aes(x = sampling_density_ok, y = sampling_density_mul, col = holc_grade))  +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", col = 'red') +
  labs(x = "Sampling density per km²\n[true]", y = "Sampling density per km²\n[based on excidently multiplied observations]") +
  scale_color_manual(values = holc_pal, name = 'HOLC grade') + 
  theme_bw() +
  circle_at_tag() + labs(tag = "A") + offset_tag

(cor_a | cor_b) + plot_layout(guides = "collect")#ggsave('Output/Fig_check_wrong_data.jpg', width= 23, height = 10, units ='cm') 

#' <a name="F_O">
#' **Figure O</a> | Relationship between the correct, true dataset and authors' accidently multiplied one.** Each point represents # of observatins (A) and sampling density in km2 (B) per HOLC grade and year, point color indicates HOLC grade. Dotted line indicates unity - same value for true and authors' data. 
#' 
#+ r_f4_dens, fig.width=20*0.393701,fig.height=15*0.393701 
# b) 
# prepare panels A and B with the original figure 
r_f4_A =
ggplot(mul[Year >= 2000 & Year <= 2020], aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) + # MaPe added to aid visualisation
geom_line(aes(color = holc_grade), size = 1, show.legend = FALSE) +
#stat_smooth(aes(color = holc_grade), size = 1) +
#coord_cartesian(ylim = c(0, 300)) + scale_y_continuous(breaks = seq(0, 300, by = 100)) + # MaPe added
scale_color_manual(values = holc_pal, name = 'HOLZ grade') +
labs(title = "Multiplied # of observation", subtitle = 'Lines connect points', tag='A', y = 'Sampling density\n[bird observations per km²]') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
theme( plot.subtitle = element_text(size = 10, colour = "grey40"), plot.title = element_text(size = 10))

r_f4_B =
ggplot(mul[Year >= 2000 & Year <= 2020], aes(x = Year, y = sampling_density), fill = holc_grade) +
stat_smooth(aes(color = holc_grade), size = 1, show.legend = FALSE) +
geom_point(aes(color = holc_grade)) + 
#stat_smooth(aes(color = holc_grade), size = 1) +
#coord_cartesian(ylim = c(0, 300)) + scale_y_continuous(breaks = seq(0, 300, by = 100)) + # MaPe added
scale_color_manual(values = holc_pal, name = 'HOLZ grade') +
labs(title = "", subtitle = 'Lines represent LOESS, shading its 95%CIs', tag='B', y = 'Sampling density\n [bird observations per 1km²]') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
theme(legend.position = 'none', plot.subtitle = element_text(size = 10, colour = "grey40"), plot.title = element_text(size = 10))


# prepare panels D and C with the corrected data

# copy of the L390-430 of 04_R4_uneven_biodiversity_data_2023.R, with changed folder path (TODO check whether this produces what it supposed to - it should be the figure on corrected data)
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

temporal_all_data = rbind(trend_A,trend_B,trend_C,trend_D) 

# Plot temporal trend: 2000-2020; same as in the script, but showing legend
r_f4_C =
temporal_all_data %>%
filter(Year >= 2000 & Year <= 2020) %>%
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%
ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) +
geom_point(aes(color = holc_grade)) + # MaPe added to aid visualisation
geom_line(aes(color = holc_grade), size = 1, show.legend = FALSE) +
#stat_smooth(aes(color = holc_grade), size = 1) +
coord_cartesian(ylim = c(0, 300)) + scale_y_continuous(breaks = seq(0, 300, by = 100)) + # MaPe added
scale_color_manual(values = holc_pal, name = 'HOLZ grade') +
labs(title = "non-duplicated data", subtitle = 'Lines connect points', tag='C', y = 'Sampling density\n [bird observations per 1km²]') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
theme( plot.subtitle = element_text(size = 10, colour = "grey40"), plot.title = element_text(size = 10))

# ggsave('Output/Fig_r4_density.jpg', width= 15, height = 15, units ='cm') 

# MaPe - LOESS smoothed
r_f4_D =
temporal_all_data %>%
filter(Year >= 2000 & Year <= 2020) %>%
#filter(Year >= 2010 & Year <= 2019) %>%
#filter(Year <= 2020) %>%
ggplot(aes(x = Year, y = sampling_density), fill = holc_grade) +
stat_smooth(aes(color = holc_grade), size = 1, show.legend = FALSE) +
geom_point(aes(color = holc_grade)) + # MaPe added to aid visualisation
#geom_line(aes(color = holc_grade), size = 1) +
coord_cartesian(ylim = c(0, 300)) + scale_y_continuous(breaks = seq(0, 300, by = 100)) + # MaPe added
scale_color_manual(values = holc_pal, name = 'HOLZ grade') +
labs(subtitle = 'Lines represent LOESS; shading its 95%CIs', tag='D', y = 'Sampling density\n [bird observations per 1km²]') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
theme( legend.position = 'none', plot.subtitle = element_text(size = 10, colour = "grey40"), plot.title = element_text(size = 10))
#g2; ggsave('Output/Fig_r4_density_LOESS.jpg', width= 15, height = 15, units ='cm') 

# COMBINE ALL
#(r_f4_C + r_f4_D) +
#plot_layout(guides = "collect", axis_titles = 'collect') 
#ggsave('Output/Fig_r4_AB.jpg', width= 20, height = 11, units ='cm')

#((r_f4_A + r_f4_B) / (r_f4_C + r_f4_D)) + plot_layout(guides = "collect", axis_titles = 'collect')

r_f4_A_ = r_f4_A +  theme(axis.title = element_blank(),  axis.text.x  = element_blank()) + circle_at_tag() + labs(tag = "A") + offset_tag
r_f4_B_ = r_f4_B + theme(axis.title = element_blank(),axis.text  = element_blank()) + circle_at_tag() + labs(tag = "B") + offset_tag
r_f4_C_ = r_f4_C + theme(axis.title = element_blank()) + circle_at_tag() + labs(tag = "C") + offset_tag
r_f4_D_ = r_f4_D + theme(axis.title = element_blank(), axis.text.y  = element_blank()) + circle_at_tag() + labs(tag = "D") + offset_tag 

pp = ((r_f4_A_ + r_f4_B_) / (r_f4_C_ + r_f4_D_))+ plot_layout(guides = "collect") #+ plot_layout(guides = "collect", axis_titles = 'collect')

ggdraw(xlim = c(-0.03, 1), ylim = c(-0.03, 1)) +
  draw_plot(pp)+
  draw_label("Year", x = 0.5, y = -0.03, vjust = -1, size = 11) +
  draw_label('Sampling density\n [bird observations per 1km²]', x = -0.04, y = 0.5, angle = 90, vjust = 1.5, size = 11); # ggsave('Output/Fig_rFig4_abcd_circle.jpg', width= 20, height = 15, units ='cm') 

#' <a name="F_X3">
#' **Figure X3</a> | Change in sampling density between HOLC grades over time.** Each point represents sampling density per HOLC grade and year (sum of all observations per devided by the total are for the given grade). Lines in the left pannels connect the points, in the right panels represent local regression non-parametric smoothing and shaded areas its 95% confidence intervals. Top row represent the accidently multiplied observations used by the author's, bottom row the correct number of observation.  
#' 
#' The corrected figure, we have generated, shows that (i) sampling for all categories was similar until ±2009, then (ii) the differences in sampling between categories likely increased, but (iii) leveled off. Note however, that such differences are easier to see if we plot the relative difference (Fig. [X](F_X)) or the sampling densities of two categories agains each other (Fig. [X4](F_X4)).
#' 
#+ r_fX4_dens, fig.width=21*0.393701,fig.height=14*0.393701
# all pairwise combinations you want
pairs <- data.table(
  x = c("D","D","D","C","C","B"),
  y = c("A","B","C","A","B","A")
)

# join to create plotting dataset
plotdat <- pairs[, .(x, y)][
  , merge(ttb[holc_grade==x], ttb[holc_grade==y], by="year", suffixes=c(".x",".y")), 
  by=.(x,y)
][, pair := paste0(y, " ~ ", x)]

# plot
ggplot(plotdat[year >= 2000 & year <= 2020], aes(sampling_density.x, sampling_density.y)) +
  geom_abline(slope=1, intercept=0, linetype="dotted", color="black") +
  geom_smooth(col = 'red') +
  geom_point(aes(col=year), show.legend = FALSE) +
  geom_text(aes(label=year), hjust=-0.2, size=2.5, check_overlap=TRUE) +
  scale_x_continuous(breaks=seq(0,300,50), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,300,50), expand = c(0,0)) +
  coord_cartesian(xlim=c(0,300), ylim=c(0,300)) +  
  facet_wrap(~pair, scales="fixed") +
  labs(x="Sampling density for x [km²]", y="Sampling density for y [km²]")+
  theme_light()+
  theme(
    strip.background = element_blank(),       # remove grey panel background
    strip.text = element_text(color = "black") # make labels black
  ) #; ggsave('Output/Fig_r4_alternative_new.jpg', width= 21, height = 14, units ='cm')  

#' <a name="F_X4">
#' **Figure X4</a> | Change in sampling density between HOLC grades over time.** Each point represents sampling density per HOLC grade and year (sum of all observations per devided by the total are for the given grade) with the year indicated for some of those. Red lines with shaded area represent local regression non-parametric smoothing with its 95% confidence intervals. Dotted line indicates unity, i.e. point on the line highlight same sampling density, above the line higher sampling for y and below the line higher sampling for x.
#' 
#'    
#' TODO: Peto, we have initially noted "Such figures highlighy the effects discussed by the authors. However, same as the intended Fig. 4, the data behind these figures are heavily psedoreplicated because they do not account for spatial and temporal non-independence of data points.", but I am not sure its totally correct. The figures show that 2020 is an off year and that likely there was the increase areound 2009 and that disparity remained (did not increase much after). 
#' 
#' ## iii. General additive model on temporal trends in Table S4.  
#' ### Computational reproducibility.
#' We were unable to generate the results found in Table S4. The authors' script provided different outputs, both using multiplied and corrected dataset (see below Table X TODO:)  

require(mgcv)

summary(gam(sampling_density ~ Year * holc_grade, data = mul[Year %in% c(2000:2020)]))

summary(gam(sampling_density ~ Year * holc_grade, data = ok[Year %in% c(2000:2020)]))

model_sampling = glm((sampling_density) ~ Year * holc_grade, data = mul[Year %in% c(2000:2020)])

model_sampling |> tab_model( show.aic = TRUE)

tab_model(model_sampling, auto.label = T)

#' ### Robustness reproducibility
#' To account for non-independence of unique polygons and their data across years, we have created a dataset with the number of observations for each unique polygon (i.e. city-specific HOLC-grades and sampling polygon ids). Note that some polygons are missing polygon ids and hence merging with polygon area was not possible (TODO: n = XX, n xx used records).  
#' 
#' We then specified mixed-effect models with sampling density (km²) as a response and year (continuous) in interaction with HOLC grade (four-level factor) as predictors while controlling for non-independence of data points in the random effects. We specified 6 models varying in the random effects and compared their estimates for the fixed effect predictors:
#'   
#' (1) Random intercept of state, city within state and unique sampling polygon id  
#' (2) Same as (1) but explicitely nested  
#' (3) Same as (1), but with random slope of year within city.  
#' (4) Same as (2), but with random slope of year.  
#' (5) Random slope of year within HOLC grade, nested   within city and state (random intercepts) and separate randome intercept for unique polygon id. 
#' 
#' The results reveal that TODO:continue here 
#' 
#' ***