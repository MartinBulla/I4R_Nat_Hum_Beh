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
#' For the sake of reproducibility we stored the files from the [repository](https://doi.org/10.5281/zenodo.8052525) that acompanied original publication [@ellis-soto_historical_2023] in the folder [original_paper](https://github.com/MartinBulla/avian_FID_covid/tree/main/R/) folder (at the root project’s directory) with subfolders ‘Data’ and ‘Code’ (the latter two with the file structure as provided by the authors). We stored the additional data shared by the authors upon the request from The Institute for Replication in the ‘Data’ folder within the root project directory. Datasets that we recreated using the authors code `04_R4_uneven_biodiversity_data_2023.R` are at 'Data/from_code_04'. Additional data recreated by us using our script [rev_Dat_temporal_trend.R](R/rev_Dat_temporal_trend.R) (which is the adjusted version of the authors' `04_R4_uneven_biodiversity_data_2023.R`) are at 'Data/MaPe'.
#' 
#' Scripts genereting the outputs of this html are available upon clicking the `code` button at top right above each display item!
#' 
#' ***
#' 
#' ##### Code to load tools and data
#+ start, echo = T, results = 'hide', warning=FALSE

# 1. load or install packages
#  TODO:add model assumptions to the below models.
pkgs <- c("cowplot","data.table","dplyr", "forcats", "ggh4x","ggplot2","ggpp", "ggtext", "grid", "kableExtra", "lme4", "patchwork", "scales","tibble")  # list of packages

install_if_missing <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, dependencies = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

install_if_missing(pkgs)

# 2. constants and functions
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

# function for quick plotting
plot_effects_holc <- function(
          model, data, 
          year = "year", grade = "holc_grade",
          palette = NULL, n = 100, ylab = "Sampling density",
          title_model = TRUE, title_text = NULL, ver = 'v1',
          title_size = 9, title_colour = "grey40",
          save_png = TRUE, outdir = ".", filename = NULL,
          width = 9, height = 4.8, dpi = 300, timestamp = FALSE
  ) {

     safe_slug <- function(x, max = 120) {
        x <- tolower(x)
        x <- gsub("\\s+", " ", x)
        x <- gsub("[^a-z0-9]+", "-", x)
        x <- gsub("(^-+|-+$)", "", x)
        substr(x, 1, max)
    }

     xr <- seq(min(data[[year]], na.rm = TRUE),
          max(data[[year]], na.rm = TRUE), length.out = n)

     eff <- suppressWarnings(effects::Effect(c(year, grade), model,
                         xlevels = setNames(list(xr), year)))

     pp <- as.data.frame(eff)
     pp[[grade]] <- factor(pp[[grade]])
     pp$fit_o   <- 10^pp$fit
     pp$lower_o <- 10^pp$lower
     pp$upper_o <- 10^pp$upper 

     # integer 5-year ticks within data range
      yr_rng   <- range(pp[[year]], na.rm = TRUE)
      yr_breaks <- seq(ceiling(yr_rng[1]/5)*5, floor(yr_rng[2]/5)*5, by = 5)   

     lev <- levels(pp[[grade]])
     if (is.null(palette)) {
     palette <- setNames(scales::hue_pal()(length(lev)), lev)
     } else if (is.null(names(palette)) && length(palette) == length(lev)) {
     names(palette) <- lev
     }

     p1 = 
     ggplot(pp, aes(year, fit_o, color = holc_grade, fill = holc_grade)) +
     geom_ribbon(aes(ymin = lower_o, ymax = upper_o), alpha = .15, colour = NA) +
     geom_line() +
     scale_color_manual(values = palette, name = 'HOLC grade')+
     scale_fill_manual(values = palette, name = 'HOLC grade')+
     scale_x_continuous(breaks = yr_breaks,
                     labels = scales::number_format(accuracy = 1),
                     minor_breaks = NULL) +
     labs(x = "Year", y = ylab, subtitle = 'original-scale') +
     theme_light()+
     theme( plot.subtitle = element_text(size = 10, colour = "grey40"))

     p2 = 
     ggplot(pp, aes(year, fit_o, color = holc_grade, fill = holc_grade)) +
     geom_ribbon(aes(ymin = lower_o, ymax = upper_o), alpha = .15, colour = NA) +
     geom_line() +
     scale_color_manual(values = holc_pal, name = 'HOLC grade')+
     scale_fill_manual(values = holc_pal, name = 'HOLC grade')+
     scale_x_continuous(breaks = yr_breaks,
                     labels = scales::number_format(accuracy = 1),
                     minor_breaks = NULL) +
     labs(x = "Year", y = ylab, subtitle = 'log-scale') +
     scale_y_continuous(trans='log10')+
     theme_light()+
     theme( plot.subtitle = element_text(size = 10, colour = "grey40"))

     p2_ <- p2 + theme(axis.title.y = element_blank())

     title_str <- if (!is.null(title_text)) title_text else
        if (isTRUE(title_model))
            paste0("Model: ",
                gsub("\\s+", " ",
                        paste(trimws(deparse(formula(model), width.cutoff = 500)),
                            collapse = " ")))
        else NULL

      g <- (p1 + p2_) + patchwork::plot_layout(guides = "collect")

      if (!is.null(title_str)) {
        g <- g + patchwork::plot_annotation(
          title = title_str,
          theme = ggplot2::theme(
            plot.title = element_text(
              size = title_size,
              colour = title_colour,
              hjust = 0))
        )
      }
   
     # Optional save (timestamp at END)
     if (isTRUE(save_png)) {
      if (is.null(filename)) {
        stamp <- if (timestamp) paste0("__", format(Sys.time(), "%Y%m%d-%H%M")) else ""
        fname <- paste0(safe_slug(title_str), stamp, "__", ver, ".png")
       } else {
        fname <- filename
       }
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
      ggplot2::ggsave(file.path(outdir, fname), plot = g,
                    width = width, height = height, dpi = dpi, bg = "white")
      message("Saved: ", file.path(outdir, fname))
     }

     g
}

# function to extract fixed effects from lmer
ext_fixef <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))
  
  fe <- fixef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm <- names(fe); pick <- function(p) grep(p, nm, value = TRUE)

  ints_n <- pick("^holc_grade[^:]+$")
  slps_n <- pick("^holc_grade[^:]+:scale\\(year\\)$")

  ints <- tibble(
    type = "intercept",
    holc_grade = sub("^holc_grade","", ints_n),
    estimate = fe[ints_n],
    std.error = se[ints_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  )

  slps <- tibble(
    type = "slope_per_SDyear",
    holc_grade = sub(":.*","", sub("^holc_grade","", slps_n)),
    estimate = fe[slps_n],
    std.error = se[slps_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  ) |>
    mutate( # convert to per-year on the same log10 scale
      estimate_per_year = estimate / sdy,
      conf.low_per_year = conf.low / sdy,
      conf.high_per_year= conf.high / sdy
    )

  bind_rows(ints, slps)
}

# function to extract fixed effects from lm
ext_fixef_lm <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))

  fe <- coef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm <- names(fe)

  ints_n <- nm[grepl("^holc_grade", nm) & !grepl(":", nm)]
  slps_n <- nm[grepl("^holc_grade", nm) & grepl(":.*scale\\(year\\)", nm)]
  base_slope_n <- nm[grepl("^scale\\(year\\)$", nm)]

  lvls <- levels(mf$holc_grade)

  lvl_from <- function(x) {
    x <- sub("^holc_grade[_]?", "", x)
    x <- sub(":.*$", "", x)
    if (x %in% lvls) return(x)
    # patterns like DA/DB/DC/DD with ref first
    if (nchar(x) == 2 && substr(x,1,1) == lvls[1]) {
      y <- substr(x,2,2)
      if (y %in% lvls) return(y)
      if (x == paste0(lvls[1], lvls[1])) return(lvls[1])  # DD -> D
    }
    x
  }

  ints <- tibble::tibble(
    type       = "intercept",
    holc_grade = vapply(ints_n, lvl_from, character(1)),
    estimate   = fe[ints_n],
    std.error  = se[ints_n],
    conf.low   = estimate - z * std.error,
    conf.high  = estimate + z * std.error
  )

  slps_core <- tibble::tibble(
    type       = "slope_per_SDyear",
    holc_grade = vapply(sub(":.*$", "", slps_n), lvl_from, character(1)),
    estimate   = fe[slps_n],
    std.error  = se[slps_n],
    conf.low   = estimate - z * std.error,
    conf.high  = estimate + z * std.error
  )

  ref_level <- lvls[1]
  if (length(base_slope_n) == 1) {
    base_row <- tibble::tibble(
      type       = "slope_per_SDyear",
      holc_grade = ref_level,
      estimate   = fe[base_slope_n],
      std.error  = se[base_slope_n],
      conf.low   = fe[base_slope_n] - z * se[base_slope_n],
      conf.high  = fe[base_slope_n] + z * se[base_slope_n]
    )
    slps <- dplyr::bind_rows(slps_core, base_row)
  } else {
    slps <- slps_core
  }

  slps <- slps |>
    dplyr::mutate(
      estimate_per_year  = estimate / sdy,
      conf.low_per_year  = conf.low / sdy,
      conf.high_per_year = conf.high / sdy
    ) |>
    dplyr::arrange(factor(holc_grade, lvls))

  dplyr::bind_rows(
    ints |> dplyr::arrange(factor(holc_grade, lvls)),
    slps
  )
}


# function to extract fixed effects from lmer as difference from holc D
ext_fixef_D <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))
  
  fe <- fixef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm <- names(fe)

  ints_n <- grep("^holc_grade_D[^:]+$", 
    nm, perl = TRUE, value = TRUE
  )
  slps_n <- grep(
    "^(?:holc_grade_D[^:]+:scale\\(year\\)|scale\\(year\\):holc_grade_D[^:]+)$",
    nm, perl = TRUE, value = TRUE
  )
  
  # extract the level letter (A/B/C)
  lev <- function(term) sub(".*holc_grade_D([^:]+).*", "\\1", term)

  ints <- tibble(
    type = "intercept",
    holc_grade = lev(ints_n),
    estimate = fe[ints_n],
    std.error = se[ints_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  )

  slps <- tibble(
    type = "slope_per_SDyear",
    holc_grade = lev(slps_n),
    estimate = fe[slps_n],
    std.error = se[slps_n],
    conf.low = estimate - z*std.error,
    conf.high = estimate + z*std.error
  ) |>
    dplyr::mutate( # convert to per-year on the same log10 scale
      estimate_per_year = estimate / sdy,
      conf.low_per_year = conf.low / sdy,
      conf.high_per_year= conf.high / sdy
    )

  dplyr::bind_rows(ints, slps)
}

# function to extract fixed effects from lm objects (D baseline; matches lmer extractor)
ext_fixef_D_lm <- function(m) {
  mf  <- model.frame(m)
  sdy <- as.numeric(attr(mf[["scale(year)"]], "scaled:scale"))
  fe  <- coef(m); V <- as.matrix(vcov(m)); se <- sqrt(diag(V)); z <- qnorm(0.975)
  nm  <- names(fe)
  ints_n <- grep("^holc_grade_D[^:]+$", nm, perl = TRUE, value = TRUE)
  slps_n <- grep("^(?:holc_grade_D[^:]+:scale\\(year\\)|scale\\(year\\):holc_grade_D[^:]+)$",
                 nm, perl = TRUE, value = TRUE)
  lev <- function(term) sub(".*holc_grade_D([^:]+).*", "\\1", term)

  ints <- tibble(
    type = "intercept", holc_grade = lev(ints_n),
    estimate = fe[ints_n], std.error = se[ints_n]
  ) |>
    mutate(conf.low = estimate - z*std.error,
           conf.high = estimate + z*std.error)

  slps <- tibble(
    type = "slope_per_SDyear", holc_grade = lev(slps_n),
    estimate = fe[slps_n], std.error = se[slps_n]
  ) |>
    mutate(conf.low = estimate - z*std.error,
           conf.high = estimate + z*std.error,
           estimate_per_year = estimate / sdy,
           conf.low_per_year = conf.low / sdy,
           conf.high_per_year= conf.high / sdy)

  bind_rows(ints, slps)
}

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

# adjust contrasts - ensure treatment coding with D as baseline
options(contrasts = c("contr.treatment", "contr.poly"))
ttb[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]
tt00 = ttb[year >= 2000 & year <= 2020]
tt10 = tt00[year >= 2010]

# estimate 2000 - 2020 A/D disparity
dispar = round((((ttb[year%in%c(2020) &  holc_grade%in%c('A'), sampling_density] / ttb[year%in%c(2020) &  holc_grade%in%c('D'), sampling_density])/(ttb[year%in%c(2000) &  holc_grade%in%c('A'), sampling_density]/ ttb[year%in%c(2000) &  holc_grade%in%c('D'), sampling_density]))-1)*100, 1)

# 4. load temporal data for year, category, neighberhood (polygon) generated by us

if(recreate_data==TRUE){
  source('R/rev_Dat_temporal_trend.R')
}else{
  d = fread('Data/MaPe/mape_num-of-obs_by_holc_id_year.csv')
  b = fread('Data/MaPe/mape_num-of-obs_by_holc_id_year_data-source.csv')
}

d = d[!holc_grade%in%c('E')]
d = d[!area_holc_km2%in%0] # remove polygons with zero area

# create median per year and HOLC
d[, sampling_density:=sum_bird_obs/area_holc_km2]
d_med = d[, .(sampling_density_med = median(sampling_density)), by = .(year, holc_grade)]
#ggplot(d_med, aes(x = sampling_density_med)) +  geom_density()

# aggregate temporal data per year and HOLC (as the authors have done)
dd = d[, .(sum_bird_obs = sum(sum_bird_obs)), by = .(year, holc_grade)] # we have initially used the sum_km2 = sum(area_holc_km2) as well, but because our data are missing neighberhood that had no ID to link them with neighberhood area and hence has fewer sampled neighberhoods and hence smaller area, for consistency with the authors' values, we use their overall area per HOLC grade.

dd = merge(dd,holc_area_sum_b_dt, all.x = TRUE)

# sampling density
dd[, sampling_density := sum_bird_obs/sum_area_holc_km2]

# disparity in ration (meaning of it unclear given the similarity in 2000)
dispar2 = round((((dd[year%in%c(2020) &  holc_grade%in%c('A'), sampling_density] / dd[year%in%c(2020) &  holc_grade%in%c('D'), sampling_density])/(dd[year%in%c(2000) &  holc_grade%in%c('A'), sampling_density]/
 dd[year%in%c(2000) &  holc_grade%in%c('D'), sampling_density]))-1)*100, 1) #Using the dataset per year, HOLC grade and neighberhood generated yet a different percentage.
#' 
#' ***
#' 
#' # 2. Replicating temporal trends  
#' TODO: Peto, let's decide whether the current i-iii order below is ok. I somehow feel that it might be better to first discuss Fig. 4; highlight the issue with the data and then use the correct data.
#' 
#' PETO: Why not, we can try it this way
#' 
#' The results on temporal trends contain three key outputs:  
#' i. Claim about 35.6% in relative disparity between HOLC grade A and D from 2000 to 2020.  
#' ii. Visualised temporal trends in Fig. 4.  
#' iii. General additive model on temporal trends in Table S4.  
#' 
#'
#' ## i. Claim about 35.6% change in disparity 
#' 
#' We did not find the code generating the 35.6% claim (Abstract p. 1869 & Results p. 1871), hence could only speculate how this was calculated. Using the authors data on sampling density per year and HOLC grade and calculating the ratio between A/D for 2020 and A/D for 2000 generated a different reult (`r dispar`%). This result seems to reflect the one (~40%) from  Fig. 4 legend.
#' 
#' However, plotting the disparity data shows a non-linear temporal relationship (Fig. [X](#F_X)A) both after 2000 (left panel) and before 2000 right panel) and depending on the dataset may be even negative for A grade (Fig. [X](#F_X)B). Such results question the use of arbitrary 2000 and 2020 comparison, unjustified by the authors, who also do not justify the exclusion of <2000 years. Including <2000 years shows even more complex picture, albeit the <2000 data might be less reliable because online platforms were non-existent (and likely also represent post-hoc data entries after GBIF platform was lounched).
#' 
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
g1a = ggplot(aw[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red') + labs(subtitle = "Author's aggregation; sum per year") + theme_light() 
g1b = ggplot(aw, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_light()
  #(g1a|g1b) + plot_layout(axis_titles = "collect")

g2a = ggplot(w[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "Our aggregation; sum per year") + theme_light()
g2b = ggplot(w, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_light()
#(g2a|g2b) + plot_layout(axis_titles = "collect")

g3a = ggplot(w_med[year>1999 & year<2021], aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "Our aggregation; median per year") + theme_light() 
g3b = ggplot(w_med, aes(x = year, y = dispar)) + geom_point() + stat_smooth(col = 'red')+ labs(subtitle = "") + theme_light()

#(g3a|g3b) + plot_layout(axis_titles = "collect")

g1a_ = g1a + theme(axis.title = element_blank(),  axis.text.x  = element_blank()) +
  coord_cartesian(ylim = c(0, 200)) + scale_y_continuous(breaks = seq(0,200, by = 50)) 
g1b_ = g1b + theme(axis.title = element_blank(),axis.text.x  = element_blank()) + 
  coord_cartesian(xlim = c(1925, 2025), ylim = c(-100, 500)) + scale_y_continuous(breaks = seq(-100, 500, by = 100))  

g2a_ = g2a + theme(axis.title = element_blank(), axis.text.x  = element_blank()) + coord_cartesian(ylim = c(0, 200)) + scale_y_continuous(breaks = seq(0,200, by = 50)) 
g2b_ = g2b + theme(axis.title = element_blank(),axis.text.x  = element_blank()) + coord_cartesian(xlim = c(1925, 2025), ylim = c(-100, 500))+ scale_y_continuous(breaks = seq(-100, 500, by = 100))  

g3a_ = g3a + theme(axis.title = element_blank()) +coord_cartesian(ylim = c(0, 200)) + scale_y_continuous(breaks = seq(0,200, by = 50)) 
g3b_ = g3b + theme(axis.title = element_blank()) + coord_cartesian(xlim = c(1925, 2025), ylim = c(-100, 500))

p <- ((g1a_  / g2a_ / g3a_) | (g1b_ / g2b_ / g3b_)) + 
  patchwork::plot_annotation(theme = ggplot2::theme(plot.subtitle = element_text(size = 11, colour = 'grey60'))) #& theme(plot.subtitle = element_text(size = 11, colour = 'grey60'))
  
  
# add one set of axis titles
ggdraw(xlim = c(-0.04, 1), ylim = c(-0.03, 1)) +
  draw_plot(p)+
  draw_label("Year", x = 0.5, y = -0.03, vjust = -1, size = 11) +
  draw_label("Relative disparity between HOLC grade A and D\n[%; D as a baseline]", x = -0.06, y = 0.5, angle = 90, vjust = 1.5, size = 11) #ggsave('Output/Fig_X-trends_3-rows_holc-area-b-all.jpg', units = 'cm', width = 15, height = 21)

#' <a name="F_X">
#' **Figure X</a> | Change in relative disparity in sampling density between HOLC grade A and D over time.** Each point represents percentage difference in sampling density of A given D (with D being the baseline) based on overall sampling density (i.e. sum of all A or D observation divided by the total are of A or D; the first two rows) or median sampling density per HOLC grade and year (bottom row). Lines represent  local regression non-parametric smoothing and shaded areas its 95% confidence intervals. Top row represent the aggregation done by the authors, middle and bottom row the aggregation done by us. Note that the authors' dataset did not contain area per year and category; hence, we were unable to compute the median sampling density for their dataset.
#' 
#' TODO: Martin check whether the above is meaningful and whether authors' scripts cannot be tweeked to provide neighberhood ids.  
#'  <br>
#'  
#' ## ii. Visualised temporal trends in Fig. 4.
#' The output that resembles Fig. 4 is located in two places, each yielding different results. 
#'
#' (a) According to the authors' [README](original_paper/Code/README_code.md), the code `05_paper_1_analyses_R4_check.Rmd` should contain all key analyses for the paper. We did find a script with the heading “6 trends”, which generates trend lines for all four HOLC categories, but instead for sampling density,  depicted by authors' Fig. 4, it depicts number of HOLC polygons that were sampled (A). To complete the picture, we also show the trends for number of sampling observations (B).
#' 
#+ r_f4_obs, fig.width=20*0.393701,fig.height=9*0.393701 

# Here we show the authors original code with our adjustments indicate by MaPe

# a) prepare panel A and B

suppressWarnings({ #MaPe added, as well as package specs below
  counts_grade_year <- 
    readr::read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv',#MaPe changed folder path from read_csv('original_paper/Data/Biodiv_Greeness_Social/R1_biodiv_trend_by_time_holc_id_1933_2022.csv')
    show_col_types = FALSE   # MaPe added to suppress readr column spec message
    ) |>
    dplyr::filter(holc_grade != 'E') |> 
    dplyr::arrange(year, holc_grade) |> 
    dplyr::group_by(year, holc_grade) |>
    dplyr::count() |> 
    dplyr::summarise(cumsum = cumsum(n), .groups = "drop") # MaPe added ', .groups = "drop"' to suppress information messages
  })

tag_pos <- c(0.045, 0.94)   # x, y in panel [0..1]  

# MaPe added color mapping
r_f4_A =
counts_grade_year |>  
  dplyr::filter(year >= 2000 & year <= 2020) |> 
  ggplot(aes(year, cumsum, col = forcats::fct_rev(holc_grade))) +  #MaPe changed group (group=holc_grade) to col to label the holc_grade lines
  geom_point() + #MaPe added to aid visualisation
  geom_line(linewidth = 1) +
  scale_color_manual(values = holc_pal, guide = guide_legend(reverse = TRUE), name = 'HOLC grade') + #MaPe added for consistent coloring
  circle_at_tag() + 
  labs(y = '# of sampled HOLC polygons', tag='A') + #MaPe added
  theme_light() +  #MaPe added for consistency
  theme(legend.position = "right", plot.title = element_text(size = 10))+
  offset_tag #MaPe added 


# MaPe added # of observations per year and HOLC 
r_f4_B =
  ggplot(tt[year >= 2000 & year <= 2020], aes(year, n_obs, col = forcats::fct_rev(holc_grade))) +  #MaPe changed group (group=holc_grade) to col to label the holc_grade lines
  geom_point() + #MaPe added to aid visualisation
  geom_line(size = 1) +
  scale_color_manual(values = holc_pal, guide = guide_legend(reverse = TRUE), name = 'HOLC grade') + #MaPe added for consistent coloring
  circle_at_tag() + 
  labs(y = '# of observations', tag='B') + #MaPe added
  theme_light() +#MaPe added for consistency
  theme(legend.position = "right", plot.title = element_text(size = 10))+
  offset_tag #MaPe added 
    

(r_f4_A + r_f4_B) +
plot_layout(guides = "collect", axis_titles = 'collect') #; ggsave('Output/Fig_r4_count_v2.jpg', width= 20, height = 9, units ='cm')  

#' <a name="F_X2">
#' **Figure X2</a> | Change in # of sampled polygons (A) and # of observations (B) according to HOLC grade over time.** Each point represents the sum per year. Lines aid the visualisation by connecting the points.
#' 
#' (b) We then found that the code `04_R4_uneven_biodiversity_data_2023.R` contains section *[7] Plot temporal trends 1933-2022 and 2000-2020*. The script was not initially running due to absolute folder assignments that were unintuitive regarding the location of the files. We searched for the required files `R1_biodiv_col_code_by_holc_id_2000_2020.csv` and `R1_biodiv_trend_by_time_holc_id_1933_2022.csv` among folders provided by the authors and respecified the folder paths. In addition, the original script loaded packages with conflicting functions. To smoothly reproduce the code, we thus made a new script where we loaded only the relevant packages and only the relevant data. Then the script, thought to generate Fig. 4, run without issues. To aid visualisation we also plotted the points that the line was connecting and an alternative plot where the lines represent locally estimated scatterplot smoothing (LOESS). However, non of these outputs resembls the one from the authors' Fig. 4.<br><br>We then found out that the complex coding, along with the conflicting R-packages multiplied the number of observation (our Fig. O below) and only such data produced the authors' Fig. 4. Specifically, the convoluted chunk of code in `04_R4_uneven_biodiversity_data_2023.R` (L410-20) produces the correct dataset, only if `plyr` R-package is not loaded. When the `plyr` package is loaded in R, the code multiplies the number of observations so that summing of all observation per HOLC grade and year (cumsum_n_obs) gives about 7 times more observations than the acutal number of observations and hence sampling densities per km² are high. In other words, **our Figure X3 C and D are indeed the correct ones!**. Note, in the initial Fig. 4 the ploted lines just connected the data points (our Fig. X3 A), but fitting a line through the data  produces a more realistic picture (our Fig. X3 B; albeit here using only LOESS, while using model predictions would be more fitting - see later.)
#'  

#+ r_check_trend_d, fig.width=23*0.393701,fig.height=10*0.393701 

holc_area = copy(holc_area_sum_b)
setnames(holc_area, 'sum_area_holc_km2', 'area_sum')
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
  labs(x = "# of observations\n[true]", y = "# of observations\n[accidentally multiplied]")+
  scale_color_manual(values = holc_pal, name = 'HOLC grade') + 
  theme_light() +
  circle_at_tag() + labs(tag = "A") + offset_tag

cor_b = 
ggplot(ok, aes(x = sampling_density_ok, y = sampling_density_mul, col = holc_grade))  +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", col = 'red') +
  labs(x = "Sampling density per km²\n[true]", y = "Sampling density per km²\n[based on accidentally multiplied observations]") +
  scale_color_manual(values = holc_pal, name = 'HOLC grade') + 
  theme_light() +
  circle_at_tag() + labs(tag = "B") + offset_tag

(cor_a | cor_b) + plot_layout(guides = "collect")#ggsave('Output/Fig_check_wrong_data.jpg', width= 23, height = 10, units ='cm') 

#' <a name="F_O">
#' **Figure O</a> | Relationship between the correct, true dataset and authors' accidently multiplied one.** Each point represents # of observations (A) and sampling density in km2 (B) per HOLC grade and year, point color indicates HOLC grade. Dotted line indicates unity - same value for true and authors' data. 
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
scale_color_manual(values = holc_pal, name = 'HOLC grade') +
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
scale_color_manual(values = holc_pal, name = 'HOLC grade') +
labs(title = "", subtitle = 'Lines represent LOESS, shading its 95%CIs', tag='B', y = 'Sampling density\n [bird observations per 1km²]') + # MaPe added subtitle
theme_light()+
#theme(legend.position = 'none') + # MaPe hashtagged out to aid visualisation
theme(legend.position = 'none', plot.subtitle = element_text(size = 10, colour = "grey40"), plot.title = element_text(size = 10))


# prepare panels D and C with the corrected data

# copy of the L390-430 of 04_R4_uneven_biodiversity_data_2023.R, with changed folder path
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
scale_color_manual(values = holc_pal, name = 'HOLC grade') +
labs(title = "Non-duplicated data", subtitle = 'Lines connect points', tag='C', y = 'Sampling density\n [bird observations per km²]') + # MaPe added subtitle
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
scale_color_manual(values = holc_pal, name = 'HOLC grade') +
labs(subtitle = 'Lines represent LOESS; shading its 95%CIs', tag='D', y = 'Sampling density\n [bird observations per km²]') + # MaPe added subtitle
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
  draw_label('Sampling density\n [bird observations per 1km²]', x = -0.04, y = 0.5, angle = 90, vjust = 1.5, size = 11)#; ggsave('Output/Fig_rFig4_abcd_circle.jpg', width= 20, height = 15, units ='cm') 

#' <a name="F_X3">
#' **Figure X3</a> | Change in sampling density between HOLC grades over time.** Each point represents sampling density per HOLC grade and year (sum of all observations divided by the total are for the given grade). Lines in the left pannels connect the points, in the right panels represent local regression non-parametric smoothing and shaded areas its 95% confidence intervals. Top row represent the accidently multiplied observations used by the author's, bottom row the correct number of observation.  
#' 
#' <br>
#' 
#' The corrected figure, we have generated, shows that (i) sampling for all categories was similar until ±2009, then (ii) the differences in sampling between categories likely increased, but then (iii) leveled off. Note however, that such differences are easier to see if we plot the relative difference (Fig. [X](F_X)) or the sampling densities of two categories against each other (Fig. [X4](F_X4)).
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
#' **Figure X4</a> | Change in sampling density between HOLC grades over time.** Each point represents sampling density per HOLC grade and year (sum of all observations per divided by the total are for the given grade) with the year indicated for some of those. Red lines with shaded area represent local regression non-parametric smoothing with its 95% confidence intervals. Dotted line indicates unity, i.e. point on the line highlight same sampling density, above the line higher sampling for y and below the line higher sampling for x.
#' 
#' <br>
#'  
#' Such figures highlight the effects discussed by the authors. However, same as the intended Fig. 4, the data behind these figures are heavily psedoreplicated because they do not account for spatial and temporal non-independence of data points. The figures show that 2020 is an off year and that likely there was an increase around 2009 and that disparity remained (did not increase much after).  
#' <br>
#'  
#' ## iii. General additive model on temporal trends in Table S4 
#' ### Computational reproducibility
#' We were initially unable to generate the results found in Table S4. The authors' script provided different outputs, both using multiplied and corrected dataset. However, the scripts contained models where sampling density was not ln-transformed, whereas the description of the authors' Table S4 indicated log-transformation. Using ln-transfomred sampling density and multiplied dataset did produce the outcome that is in authors' Table S4 (see below Table [S1](T_S1)).  
#' 
#' <br>
#'    
#' <a name="T_S1a"> 
#' **Table S1a | Change in sampling density across time in relations to HOLC grade**</a>

require(mgcv); require(sjPlot)

m_density_mul = glm((sampling_density) ~ Year * holc_grade, data = mul[Year %in% c(2000:2020)])
m_density_ok = glm((sampling_density) ~ Year * holc_grade, data = ok[Year %in% c(2000:2020)])

tab_model(m_density_mul, m_density_ok,  auto.label = T, string.ci='95%CI', title = "Generalized linear model", dv.labels = c("Multiplied # of observations", "Actual # of observations")) # m = lm(log(sampling_density) ~ Year * holc_grade, data = ok[Year %in% c(2000:2020)]); summary(glht(m)) # gives even less clear relationship
#' ***
#' 
#' <br>
#' 
#' <a name="T_S1b"> 
#' **Table S1b | Change in sampling density across time in relations to HOLC grade**</a>

m_density_mul_ln = glm(log(sampling_density) ~ Year * holc_grade, data = mul[Year %in% c(2000:2020)])
m_density_ok_ln = glm(log(sampling_density) ~ Year * holc_grade, data = ok[Year %in% c(2000:2020)])

tab_model(m_density_mul_ln, m_density_ok_ln,  auto.label = T, string.ci='95%CI', title = "Generalized linear model on ln(sampling density)", dv.labels = c("Multiplied # of observations", "Actual # of observations"))
#' ***
#' 
#' <br>
#'  
#' <a name="T_S1c"> 
#' **Table S1c | Change in sampling density across time in relations to HOLC grade**</a>

gam_density_mul = gam(sampling_density ~ Year * holc_grade, data = mul[Year %in% c(2000:2020)])

gam_density_ok = gam(sampling_density ~ Year * holc_grade, data = ok[Year %in% c(2000:2020)])

tab_model(gam_density_mul, gam_density_ok, auto.label = T, string.ci='95%CI', title = "Generalized additive model", dv.labels = c("Multiplied # of observations", "Actual # of observations"))
#' ***
#' 
#' <br>
#' 
#' <a name="T_S1d"> 
#' **Table S1d | Change in sampling density across time in relations to HOLC grade**</a>

gam_density_mul_ln = gam(log(sampling_density) ~ Year * holc_grade, data = mul[Year %in% c(2000:2020)])

gam_density_ok_ln = gam(log(sampling_density) ~ Year * holc_grade, data = ok[Year %in% c(2000:2020)])

tab_model(gam_density_mul_ln, gam_density_ok_ln, auto.label = T, string.ci='95%CI', title = "Generalized additive model on ln(sampling density)", dv.labels = c("Multiplied # of observations", "Actual # of observations")) #summary(glht(gam_density_ok_ln)) corrected p-values
#' ***
#' 
#' <br>
#' 
#' Note, it is unclear why generalised additive model was used as no wave form was fitted (i.e. simple Gaussian model was used)
#' 

#+ r_F_Y, fig.width=12*0.393701,fig.height=15*0.393701 
# plot predictions
# newdata grid
newD <- CJ(Year = 2000:2020, holc_grade = unique(ok$holc_grade))

# predictions + 95% CI on log scale
pr <- predict(gam_density_ok_ln, newdata = newD, se.fit = TRUE)
newD[, `:=`(fit = pr$fit, se = pr$se.fit)]
newD[, `:=`(lwr = fit - 1.96 * se, upr = fit + 1.96 * se)]

# plot on log scale
fy1 =
ggplot(newD, aes(x = Year, y = fit, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = holc_pal, name = 'HOLC grade') + 
  scale_fill_manual(values = holc_pal, name = 'HOLC grade') + 
  labs(y = "Predicted sampling density", x = 'Year', subtitle = 'Ln-scale') +
  theme_light() +
  theme(plot.subtitle = element_text(size = 10, colour = "grey40"),
        axis.title.x = element_blank(),
        axis.text.x  = element_blank()
        )

# plot: back-transform to original scale (median on original scale)
newD_bt <- copy(newD)[, `:=`(fit_bt = exp(fit), lwr_bt = exp(lwr), upr_bt = exp(upr))]

fy2 =
ggplot(newD_bt, aes(x = Year, y = fit_bt, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr_bt, ymax = upr_bt), alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = holc_pal, name = 'HOLC grade') + 
  scale_fill_manual(values = holc_pal, name = 'HOLC grade') + 
  labs(y = "Predicted sampling density", x = 'Year', subtitle = 'Original scale [km²]') +
  theme_light() + 
  theme(legend.position = 'none',
        plot.subtitle = element_text(size = 10, colour = "grey40")
        )

 (fy1 / fy2)+ plot_layout(axis_title = "collect") #; ggsave('Output/Fig_Y.jpg', width= 12, height = 15, units ='cm')   

#' <a name="F_Y">
#' **Figure Y</a> | Predicted sampling density between HOLC grades over time.** Lines with shaded areas represent generalized additive model predictions of sampling density on ln-scale (top) and km² bottom. N = 84 observations (sum of all observations per year and grade divided by the total area for the given grade). Note, however, that it is unclear why generalised additive model was used as no wave form was fitted (i.e. simple Gaussian model was used)
#' 
#' 
#' ### Robustness reproducibility
#' To account for non-independence of unique polygons and their data across years, we have created a dataset with the number of observations for each unique polygon (i.e. city-specific HOLC-grades and sampling polygon ids). Note that some polygons are missing polygon ids and hence merging with polygon area was not possible (n = TODO:XX, n = `r nrow(d)` used records).  
#' 
#' We then specified mixed-effect models with sampling density (km²) as a response and year (continuous) in interaction with HOLC grade (four-level factor) as predictors while controlling for non-independence of data points in the random effects. We specified 6 models varying in the random effects and compared their estimates for the fixed effect predictors:
#'   
#' (1) Random intercept of state, city within state and unique sampling polygon id  
#' (2) Same as (1) but explicitly nested  
#' (3) Same as (1), but with random slope of year within city. 
#' (4) Same as (1), but with random slope of year within polygon.  
#' (5) Same as (2), but with random slope of year.  
#' (6) Random slope of year within HOLC grade, nested   within city and state (random intercepts) and separate randome intercept for unique polygon id.   
#' <br>  
#'  
#' #### A. Contrasts
#+ F_Z1, fig.width = 25/2.5, fig.height = 15/2.5
  # Ensure treatment coding with D as baseline (IMPORTANT)
  options(contrasts = c("contr.treatment", "contr.poly"))
  d[, holc_grade_D := factor(holc_grade, levels = c("D","B","C","A"))]

  # limit to 2010-2020
  d00 = d[year >= 2000 & year <= 2020]
  d10 = d[year >= 2010 & year <= 2020]

  # check distributions
  #ggplot(d, aes(sum_bird_obs))+geom_density() + scale_x_continuous(trans ='log10')
  #ggplot(d, aes(sampling_density))+geom_density() + scale_x_continuous(trans ='log10')
  #ggplot(d00, aes(sampling_density))+geom_density() + scale_x_continuous(trans ='log10')

  # MODELS 2000-2020
  # 0) model per holc grade
  sum_m  <- lm(log10(sampling_density) ~ holc_grade_D*scale(year), tt00)

  # 1) model set per polygon
  maD = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
              (1|state) + (1|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
            )

  mbD = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
              (1|state/city_state/holc_grade/id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
            )
  
  mas1D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )
  mas2D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )
  mbs1D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
              (scale(year)|state/city_state/holc_grade/id2),
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

  msab1D = lmer(log10(sampling_density) ~ holc_grade_D*scale(year) + 
              (scale(year)|state/city_state/holc_grade) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              ) 

  models_T0020_D <- rlang::set_names(
    list(maD, mbD, mas1D, mas2D, mbs1D, msab1D),
    c("maD","mbD","mas1D", "mas2D", "mbs1D","msab1D")
  )

  # labels
  models_T0020_labels_D <- c(
    maD      = "(1 | state) + (1 | city) + (1 | polygon)",
    mbD      = "(1 | state / city / HOLC grade / polygon)",
    mas1D    = "(1 | state) + (year | city) + (1 | polygon)",    
    mas2D    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1D    = "(year | state / city / HOLC grade / polygon)",
    msab1D   = "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # sort models
  models_T0020_order_D <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade / polygon)",
    "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # 2) Extract fixed effects on the modeling scale (log10), fast Wald CIs
  # lm
  lm_df <- ext_fixef_D_lm(sum_m) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept\n[log10]",
                    type == "slope_per_SDyear", "Slope difference\n[per standard deviation of year]",
                    default = as.character(type)),
      holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
    )

  lm_lab <- paste0(
    sprintf("Linear model on density per year\n(n = %s for 2000-2020)", nobs(sum_m)), "\n(n = ", nrow(tt10), " for 2010-2010)") # Legend label text (no title, single key)

  # lmer
  coef_df_0020_D <- purrr::imap_dfr(models_T0020_D, ~ ext_fixef_D(.x) |> dplyr::mutate(model=.y))

  coef_df_0020_D <- coef_df_0020_D %>%
    mutate(model_label = factor(models_T0020_labels_D[model], levels = models_T0020_order_D)) %>% data.table()

  coef_df_0020_D[, type2 := fcase(
    type == "intercept", "Intercept\n[log10]",
    type == "slope_per_SDyear", "Slope difference\n[per standard deviation of year]",
    default = as.character(type)
  )]

  coef_df_0020_D[, holc_grade_dif := paste0(holc_grade, ' vs D')]

  gap_pt <- 5 # adjusts subtitle spacing: how big a gap you want between the two rows (in points)

  leg_tit = paste0("Mixed-effect model<br>random-effects specification:<br><span style='font-weight:400;font-size:9pt;'>(n = ", comma(nrow(d00)),' for 2000-2020)<br>(n = ', comma(nrow(d10)), ' for 2010-2020)</span>') #  leg_tit = paste0('Mixed-effect model\nrandom-effects specification:\n(n = ', nrow(d00),' for 2000-2020)\n(n = ', nrow(d10), ' for 2010-2020)') # legend title

  tr1D = 
  ggplot(coef_df_0020_D, aes(
          x= estimate, y = holc_grade_dif, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="Contrast (relative to HOLC grade D)", x=NULL, subtitle ='2000 - 2020') +
    ggplot2::scale_color_manual(
     name = leg_tit,
     breaks = models_T0020_order_D,
     values = setNames(scales::hue_pal()(length(models_T0020_order_D)),
                      models_T0020_order_D)
    ) +
    # add lm model
    ggnewscale::new_scale_color() +  # start a NEW color scale (separate legend)
    geom_pointrange(
      data = lm_df,
      aes(x = estimate, y = holc_grade_dif,
          xmin = conf.low, xmax = conf.high,
          color = lm_lab),
      position = position_nudge(y = 0.36),  # small vertical offset; remove if undesired
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,                        # no legend title
      values = setNames("black", lm_lab),
      breaks = lm_lab,
      guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
    ) +                     
    theme_light() +
    theme(
      plot.margin = margin(t = 3, r = 3, b = gap_pt, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = -30)),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(), # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=15)), # make labels black
      axis.text.x = element_blank(),
      legend.title = element_markdown()                             
    )

  # MODELS 2010-2020
  # 0) lm model
  sum_m_10  <- lm(log10(sampling_density) ~ holc_grade_D*scale(year), tt10)

  # 1) model set
  ma_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
               (1|state) + (1|city_state) + (1|id2), 
               d10,
               control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
               )
              )

  mb_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
              (1|state/city_state/holc_grade/id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )

  mas1_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
                (1|state) + (scale(year)|city_state) + (1|id2), 
                d10,
                control = lmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                )
              )

  mas2_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
                (1|state) + (1|city_state) + (scale(year)|id2), 
                d10,
                control = lmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                )
              )

  mbs1_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
                (scale(year)|state/city_state/holc_grade/id2),
                d10,
                control = lmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                )
              )

  msab1_D = lmer(log10(sampling_density) ~ scale(year)*holc_grade_D + 
                (scale(year)|state/city_state/holc_grade) + (1|id2), 
                d10,
                control = lmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                )
              ) 

  models_T1020_D <- rlang::set_names(
    list(ma_D, mb_D, mas1_D, mas2_D, mbs1_D, msab1_D),
    c("ma_D","mb_D","mas1_D", "mas2_D", "mbs1_D","msab1_D")
  )

  # labels
  models_T1020_labels_D <- c(
    ma_D      = "(1 | state) + (1 | city) + (1 | polygon)",
    mb_D      = "(1 | state / city / HOLC grade / polygon)",
    mas1_D    = "(1 | state) + (year | city) + (1 | polygon)",
    mas2_D    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1_D    = "(year | state / city / HOLC grade  / polygon)",
    msab1_D   = "(year | state / city / HOLC grade ) + (1 | polygon)"
  )

  # sort models
  models_T1020_order_D <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade  / polygon)",
    "(year | state / city / HOLC grade ) + (1 | polygon)"
  )

  # 2) Extract fixed effects on the modeling scale (log10), fast Wald CIs
  # lm
  lm_df_10 <- ext_fixef_D_lm(sum_m_10) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept\n[log10]",
                    type == "slope_per_SDyear", "Slope difference\n[per standard deviation of year]",
                    default = as.character(type)),
      holc_grade_dif = paste0(holc_grade, " vs D")  # match tr1D y labels
    )

  lm_lab_10 <-  paste0(
    sprintf("Linear model on density per year\n(n = %s for 2000-2020, ", nobs(sum_m)), "n = ", nrow(tt10), " for 2010-2010)")  # Legend label text (no title, single key) 

  # lmer
  coef_df_1020_D <- purrr::imap_dfr(models_T1020_D, ~ ext_fixef_D(.x) |> dplyr::mutate(model=.y))

  coef_df_1020_D <- coef_df_1020_D %>%
    mutate(model_label = factor(models_T1020_labels_D[model], levels = models_T1020_order_D)) %>% data.table()

  coef_df_1020_D[, type2 := fcase(
    type == "intercept", "Intercept\n[log10]",
    type == "slope_per_SDyear", "Slope difference\n[per standard deviation of year]",
    default = as.character(type)
  )]

  coef_df_1020_D[, holc_grade_dif := paste0(holc_grade, ' vs D')]

  tr2D =     
  ggplot(coef_df_1020_D, aes(
          x= estimate, y = holc_grade_dif, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    # lmer
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="Contrast (relative to HOLC grade D)", x="Estimate", subtitle ='2010 - 2020') +
    ggplot2::scale_color_manual(
      name = leg_tit,
      breaks = models_T1020_order_D,
      values = setNames(scales::hue_pal()(length(models_T1020_order_D)),
                      models_T1020_order_D)
    ) +
    # lm 
    ggnewscale::new_scale_color() +
    geom_pointrange(
      data = lm_df_10,
      aes(x = estimate, y = holc_grade_dif,
          xmin = conf.low, xmax = conf.high,
          color = lm_lab_10),
      position = position_nudge(y = 0.36),
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,
      values = setNames("black", lm_lab_10),
      breaks = lm_lab,
      guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
    ) +                       
    theme_light() +
    theme(
      plot.margin = margin(t = gap_pt, r = 3, b = 5.5, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = 2)),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(),       # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=10)),
      strip.text.x = element_blank(),
      legend.position = "none"                            
    )

  # COMBINE - x-axis correspond accros facets  
  # facet-wise ranges across BOTH datasets
  rng <- bind_rows(coef_df_0020_D, coef_df_1020_D,lm_df, lm_df_10) |>
    group_by(type2) |>
    summarise(
      xmin = min(pmin(conf.low, conf.high), na.rm = TRUE),
      xmax = max(pmax(conf.low, conf.high), na.rm = TRUE),
      .groups = "drop"
    )

  add_02_break  <- function(lims) sort(unique(c(scales::pretty_breaks()(lims), 0.2)))  

  facet_scales <- lapply(seq_len(nrow(rng)), function(i) {
    tl <- rng$type2[i]
    lo <- rng$xmin[i]
    hi <- rng$xmax[i]

    if (grepl("^Slope", tl)) {
      rlang::new_formula(
        lhs = bquote(type2 == .(tl)),
        rhs = scale_x_continuous(
          limits = c(-0.4, 0.4), 
          breaks = add_02_break, 
          oob = scales::oob_keep, # keep data for stats, don't drop rows 
          expand = expansion(mult = c(0, 0)) 
        ) 
      ) 
    } else if (grepl("^Intercept", tl)) { 
      rlang::new_formula( 
        lhs = bquote(type2 == .(tl)), 
        rhs = scale_x_continuous( 
          limits = c(-.4, .6), 
          #breaks = add_15_break, 
          oob = scales::oob_keep, 
          expand = expansion(mult = c(0, 0)) 
        ) 
      ) 
    } else { 
      rlang::new_formula( 
        lhs = bquote(type2 == .(tl)), 
        rhs = scale_x_continuous( 
          limits = c(lo, hi), 
          oob = scales::oob_keep, 
          expand = expansion(mult = c(0, 0)) 
        ) 
      ) 
    } 
  })  

  tr1D_adj <- tr1D + ggh4x::facetted_pos_scales(x = facet_scales) 
  tr2D_adj <- tr2D + ggh4x::facetted_pos_scales(x = facet_scales) 
  
  (tr1D_adj / tr2D_adj) + plot_layout(axis_titles = "collect") #; ggsave('Output/rev_Fig_Z1_yr-trend_model-compar_D.png', width= 25, height = 15, units ='cm')

#' <a name="F_Z1">
#' **Figure Z1</a> | Estimates of sampling density between HOLC grades and over time.** Dots represent estimates (on log-scale), horizontal lines 95%CIs, color indicates a random-effects model structure (with variables  left from `|` indicating random slopes and right from `|` indicating random intercepts, whereas '/' indicates nested intercepts). The depicted estimates represent differences (in mean values or slopes) relative to HOLC grade D (for actual values see Fig. [Z2](F_Z2)). Top row contains estimates for a dataset spanning from 2000 until 2020 (n = `r nrow(d00)` polygons with known area) and bottom row for a dataset from 2010 until 2020 (n = `r nrow(d10)`).
#' <br>  
#'  
#' #### B. Mean and slope values
#+ F_Z2, fig.width = 25/2.5, fig.height = 15/2.5
  # MODELS 2000-2020
  
  # 1) reparametrize models to estimate separte intercepts and slopes for each holc grade 
  # lm model on sum per holc grade
  sum_mi  <- lm(log10(sampling_density) ~ 0 + holc_grade_D*scale(year), tt00)
  
  # lmer models on all polygons
  mai = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
            (1|state) + (1|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
            )

  mbi = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state/city_state/holc_grade/id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )
  mas1i = lmer(log10(sampling_density) ~
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )


  mas2i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (1|city_state) + (scale(year)|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

  mbs1i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade/id2),
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

  msab1i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade) + (1|id2), 
              d00,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              ) 

  # 2) model set and labels
  models_T0020<- list(
    mai       = mai,
    mbi       = mbi,
    mas1i       = mas1i,
    mas2i       = mas2i,
    mbs1i      = mbs1i,
    msab1i      = msab1i
  )

  models_T0020_labels <- c(
    mai      = "(1 | state) + (1 | city) + (1 | polygon)",
    mbi      = "(1 | state / city / HOLC grade / polygon)",
    mas1i    = "(1 | state) + (year | city) + (1 | polygon)",
    mas2i    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1i    = "(year | state / city / HOLC grade / polygon)",
    msab1i   = "(year | state / city / HOLC grade) + (1 | polygon)"
  )
  # sort models
  models_T0020_order <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade / polygon)",
    "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # 3) Extract fixed effects on the modeling scale (log10), fast Wald CIs
  # lm
  lmi_df <- ext_fixef_lm(sum_mi) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept\n[log10]",
                    type == "slope_per_SDyear", "Slope\n[per standard deviation of year]",
                    default = as.character(type))
    )

  lm_lab <- paste0(
    sprintf("Linear model on density per year\n(n = %s for 2000-2020, ", nobs(sum_m)), "n = ", nrow(tt10), " for 2010-2010)") # Legend label text (no title, single key)

  # lmer
  coef_df_0020 <- purrr::imap_dfr(models_T0020, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

  coef_df_0020 <- coef_df_0020 %>%
    mutate(model_label = factor(models_T0020_labels[model], levels = models_T0020_order)) %>% data.table()

  coef_df_0020[, type2 := fcase(
    type == "intercept",        "Intercept\n[log10]",
    type == "slope_per_SDyear", "Slope\n[per standard deviation of year]",
    default = as.character(type)
  )]

  leg_tit = paste0("Mixed-effect model<br>random-effects specification:<br><span style='font-weight:400;font-size:9pt;'>(n = ", comma(nrow(d00)),' for 2000-2020)<br>(n = ', comma(nrow(d10)), ' for 2010-2020)</span>') # legend title

  tr1 = 
  ggplot(coef_df_0020, aes(
          x= estimate, y = holc_grade, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    # lmer
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="HOLC grade", x= NULL, subtitle ='2000 - 2020') +
    ggplot2::scale_color_manual(
     name = leg_tit,
     breaks = models_T0020_order,
     values = setNames(scales::hue_pal()(length(models_T0020_order)),
                      models_T0020_order)          
    ) +
    # lm model
    ggnewscale::new_scale_color() +  # start a NEW color scale (separate legend)
    geom_pointrange(
      data = lmi_df,
      aes(x = estimate, y = holc_grade,
          xmin = conf.low, xmax = conf.high,
          color = lm_lab),
      position = position_nudge(y = 0.36),  # small vertical offset; remove if undesired
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,                        # no legend title
      values = setNames("black", lm_lab),
      breaks = lm_lab,
      guide = guide_legend(order = 1, override.aes = list(linewidth = 0.8))
    ) + 
    theme_light() +
    theme(
      plot.margin = margin(t = 3, r = 3, b = gap_pt, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = -30)),
      axis.text.x = element_blank(),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(), # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=15)), # make labels black
      legend.title = element_markdown() 

    )

  # MODELS 2010-2020
  # 1) reparametrize models to estimate separte intercepts and slopes for each holc grade 
  # lm model on sum per holc grade
    sum_m_i  <- lm(log10(sampling_density) ~ 0 + holc_grade_D*scale(year), tt10)

  # lmer models on all polygons  
    ma_i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
            (1|state) + (1|city_state) + (1|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )

    mb_i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state/city_state/holc_grade/id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
          )
    mas1_i = lmer(log10(sampling_density) ~
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (scale(year)|city_state) + (1|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

    mas2_i = lmer(log10(sampling_density) ~
              0 + holc_grade + holc_grade:scale(year) + 
              (1|state) + (1|city_state) + (scale(year)|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

    mbs1_i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade/id2),
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              )

    msab1_i = lmer(log10(sampling_density) ~ 
              0 + holc_grade + holc_grade:scale(year) + 
              (scale(year)|state/city_state/holc_grade) + (1|id2), 
              d10,
              control = lmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
              )
              ) 

  # 2) model set and labels
  models_T1020<- list(
    ma_i       = ma_i,
    mb_i       = mb_i,
    mas1_i       = mas1_i,
    mas2_i       = mas2_i,
    mbs1_i      = mbs1_i,
    msab1_i      = msab1_i
  )

  models_T1020_labels <- c(
    ma_i      = "(1 | state) + (1 | city) + (1 | polygon)",
    mb_i      = "(1 | state / city / HOLC grade / polygon)",
    mas1_i    = "(1 | state) + (year | city) + (1 | polygon)",
    mas2_i    = "(1 | state) + (1 | city) + (year | polygon)",
    mbs1_i    = "(year | state / city / HOLC grade / polygon)",
    msab1_i   = "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # sort models
  models_T1020_order <- c(
    "(1 | state) + (1 | city) + (1 | polygon)",
    "(1 | state / city / HOLC grade / polygon)",
    "(1 | state) + (year | city) + (1 | polygon)",
    "(1 | state) + (1 | city) + (year | polygon)",
    "(year | state / city / HOLC grade / polygon)",
    "(year | state / city / HOLC grade) + (1 | polygon)"
  )

  # 3) Extract fixed effects on the modeling scale (log10), fast Wald CIs
  # lm
  lmi_df_10 <- ext_fixef_lm(sum_m_i) |>
    mutate(
      type2 = fcase(type == "intercept", "Intercept\n[log10]",
                    type == "slope_per_SDyear", "Slope\n[per standard deviation of year]",
                    default = as.character(type))
    )

  lm_10_lab <- paste0(
    sprintf("Linear model on density per year\n(n = %s for 2000-2020)", nobs(sum_m)), "\n(n = ", nrow(tt10), " for 2010-2010)") # Legend label text (no title, single key)
  
  # lmer
  coef_df_1020 <- purrr::imap_dfr(models_T1020, ~ ext_fixef(.x) |> dplyr::mutate(model=.y))

  coef_df_1020 <- coef_df_1020 %>%
    mutate(model_label = factor(models_T1020_labels[model], levels = models_T1020_order)) %>% data.table()

  coef_df_1020[, type2 := fcase(
    type == "intercept",        "Intercept\n[log10]",
    type == "slope_per_SDyear", "Slope\n[per standard deviation of year]",
    default = as.character(type)
  )]

  tr2 =     
  ggplot(coef_df_1020, aes(
          x= estimate, y = holc_grade, 
          xmin=conf.low, xmax=conf.high, 
          color=forcats::fct_rev(model_label))) +
    # lmer
    geom_pointrange(position = position_dodge2(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    facet_grid(~ type2, scales="free_x") +
    labs(y="HOLC grade", x="Estimate", subtitle ='2010 - 2020') +
    ggplot2::scale_color_manual(
      name = leg_tit,
      breaks = models_T1020_order,
      values = setNames(scales::hue_pal()(length(models_T1020_order)),
                      models_T1020_order)
    ) +
    # lm 
    ggnewscale::new_scale_color() +
    geom_pointrange(
      data = lmi_df_10,
      aes(x = estimate, y = holc_grade,
          xmin = conf.low, xmax = conf.high,
          color = lm_10_lab),
      position = position_nudge(y = 0.36),
      inherit.aes = FALSE,
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = NULL,
      values = setNames("black", lm_10_lab),
      breaks = lm_lab,
      guide = guide_legend(order = 99, override.aes = list(linewidth = 0.8))
    ) +  
    theme_light() +
    theme(
      plot.margin = margin(t = gap_pt, r = 3, b = 5.5, l = 3),
      plot.subtitle = element_text(size = 10, colour = "grey40",
                                  margin = margin(b = 2)),
      panel.spacing = unit(1.1, "lines"),
      strip.background = element_blank(),       # remove grey panel background
      strip.text = element_text(color = "black", margin = margin(b=10)),
      strip.text.x = element_blank(),
      legend.position = "none"                            
    )

  # COMBINE  
    rng <- bind_rows(coef_df_0020, coef_df_1020,lmi_df, lmi_df_10) |>
      group_by(type2) |>
      summarise(
        xmin = min(pmin(conf.low, conf.high), na.rm = TRUE),
        xmax = max(pmax(conf.low, conf.high), na.rm = TRUE),
        .groups = "drop"
      )

    add_02_break  <- function(lims) sort(unique(c(scales::pretty_breaks()(lims), 0.2)))  

    facet_scales <- lapply(seq_len(nrow(rng)), function(i) {
      tl <- rng$type2[i]
      lo <- rng$xmin[i]
      hi <- rng$xmax[i]

      if (grepl("^Slope", tl)) {
        rlang::new_formula(
          lhs = bquote(type2 == .(tl)),
          rhs = scale_x_continuous(
            limits = c(-0.1, 0.4), 
            breaks = add_02_break, 
            oob = scales::oob_keep, # keep data for stats, don't drop rows 
            expand = expansion(mult = c(0, 0)) 
          ) 
        ) 
      } else if (grepl("^Intercept", tl)) { 
        rlang::new_formula( 
          lhs = bquote(type2 == .(tl)), 
          rhs = scale_x_continuous( 
            limits = c(-.075, 2), 
            #breaks = add_15_break, 
            oob = scales::oob_keep, 
            expand = expansion(mult = c(0, 0)) 
          ) 
        ) 
      } else { 
        rlang::new_formula( 
          lhs = bquote(type2 == .(tl)), 
          rhs = scale_x_continuous( 
            limits = c(lo, hi), 
            oob = scales::oob_keep, 
            expand = expansion(mult = c(0, 0)) 
          ) 
        ) 
      } 
    })  

    tr1_adj <- tr1 + ggh4x::facetted_pos_scales(x = facet_scales) 
    tr2_adj <- tr2 + ggh4x::facetted_pos_scales(x = facet_scales) 
    
    (tr1_adj / tr2_adj) + plot_layout(axis_titles = "collect") #; ggsave('Output/rev_Fig_Z2_yr-trend_model-compar_D.png', width= 25, height = 15, units ='cm')

#' <a name="F_Z2">
#' **Figure Z2</a> | Estimates of sampling density between HOLC grades and over time.** Dots represent estimates (on log-scale), horizontal lines 95%CIs, color indicates a random-effects model structure (with variables  left from `|` indicating random slopes and right from `|` indicating random intercepts, whereas '/' indicates nested intercepts). The depicted estimates represent true values (as oposed to the contrasts depicted in Fig. [Z1](Z1)) Top row contains estimates for a dataset spanning from 2000 until 2020 (n = `r nrow(tt00)` sampling densities per grade and year for the linear model, and n = `r nrow(d00)` sampling densities for each polygon with known area) and bottom row for a dataset from 2010 until 2020 (n = `r nrow(tt10)` and n = `r nrow(d10)` respectively).  
#' <br>
#'  
#' #### C. visualise predicted relationships
#' In the following graphs the lines and shaded areas represent predicted relationship between sampling density (km² right panels, log(km²) left panels) and year of data collection according to HOLC grade (color). The first plot in each setion is for the linear model on the aggregated observations per year and HOLC grade, the remaining mixed effect models use data on each polygon.
#' 
#' 
#' ##### 2000-2020
#+ F_Z3x, fig.width = 14/2.5, fig.height = 6/2.5
sum_m_  <- lm(log10(sampling_density) ~ scale(year)*holc_grade, tt00)
print(plot_effects_holc(sum_m_, tt00, palette = holc_pal, outdir='Output/trend/Oct', save_png = FALSE))    

#+ F_Z3a, fig.width = 14/2.5, fig.height = 6/2.5
ma = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (1|id2), 
            d00)
print(plot_effects_holc(ma, d00, palette = holc_pal, outdir='Output/trend/Oct', save_png = FALSE))    

#+ F_Z3b, fig.width = 14/2.5, fig.height = 6/2.5
mb = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state/city_state/holc_grade/id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
plot_effects_holc(mb, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3c, fig.width = 14/2.5, fig.height = 6/2.5
mas1 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas1, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3d, fig.width = 14/2.5, fig.height = 6/2.5
mas2 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (scale(year)|id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas2, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3e, fig.width = 14/2.5, fig.height = 6/2.5
mbs1 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade/id2),
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mbs1, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)

#+ F_Z3f, fig.width = 14/2.5, fig.height = 6/2.5
msab1 = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            d00,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 
plot_effects_holc(msab1, d00, palette = holc_pal, outdir = 'Output/trend/Oct', save_png = FALSE)
#'  
#'  
#' ##### 2010-2020
#+ F_Z4x, fig.width = 14/2.5, fig.height = 6/2.5
sum_m_10  <- lm(log10(sampling_density) ~ scale(year)*holc_grade, tt10)
print(plot_effects_holc(sum_m_10, tt10, palette = holc_pal, outdir='Output/trend/Oct', save_png = FALSE))  
#+ F_Z4a, fig.width = 14/2.5, fig.height = 6/2.5
ma_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (1|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
plot_effects_holc(ma_, d10, palette = holc_pal, outdir='Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4b, fig.width = 14/2.5, fig.height = 6/2.5
mb_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state/city_state/holc_grade/id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
        )
plot_effects_holc(mb_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4c, fig.width = 14/2.5, fig.height = 6/2.5
mas1_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (scale(year)|city_state) + (1|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas1_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20')

#+ F_Z4d, fig.width = 14/2.5, fig.height = 6/2.5
mas2_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (1|state) + (1|city_state) + (scale(year)|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mas2_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4e, fig.width = 14/2.5, fig.height = 6/2.5
mbs1_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade/id2),
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            )
plot_effects_holc(mbs1_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)

#+ F_Z4f, fig.width = 14/2.5, fig.height = 6/2.5
msab1_ = lmer(log10(sampling_density) ~ scale(year)*holc_grade + 
            (scale(year)|state/city_state/holc_grade) + (1|id2), 
            d10,
            control = lmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 2e5)
            )
            ) 
plot_effects_holc(msab1_, d10, palette = holc_pal, outdir = 'Output/trend/Oct', ver ='v1_10-20', save_png = FALSE)
#' TODO: try gam
#' 
#' ***
#' 
#' # References 
#' <div id="refs"></div>
#' 
#' ***
#' 
#' # Session info
#' <a name="T_S2">
#' **Table S2 | System session info.** </a>
df_session_platform <- devtools::session_info()$platform %>%
    unlist(.) %>%
    as.data.frame(.) %>%
    tibble::rownames_to_column(.)

colnames(df_session_platform) <- c("Item", "Value")

df_session_platform %>%
  kableExtra::kbl(align=c('r', 'l'), linesep = "") %>%
  kableExtra::kable_paper(c("striped", "condensed"), full_width = F, position = "left")
#'  
#'   
#'
#' <a name="T_S3">
#' **Table S3 | Info about used packages.** </a>
df_session_packages <- devtools::session_info()$packages %>%
    as.data.frame(.) %>%
    # filter(attached == TRUE) %>%
    dplyr::select(loadedversion, date, source) %>%
    tibble::rownames_to_column()

colnames(df_session_packages) <- c("Package", "Loaded version", "Date", "Source")
df_session_packages %>%
    kableExtra::kbl(align = c("l", "l","l","l"), linesep = "") %>%
    kableExtra::kable_paper(c("striped", "condensed"), full_width = F, position = "left") %>%
        kableExtra::scroll_box(width = "90%", height = "350px")
#'
#' ***
#' 