#' ---
#' title: <font size="2">Replicating </font><br><font size="5">Ellis-Soto et al 2023, Nat Hum Beh</font>
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
#' For the sake of reproducibility we stored data and code provided by the authors with the paper in the folder ‘original_paper’ folder (at the root project’s directory) with subfolders ‘Data’ and ‘Code’, the latter two with the structure as provided by the authors. We stored the additional data acquired upon the request from The Institute for Replication at the ‘Data’ folder within the root project directory.
#' 
#' # Fig. 4
#' Code generating output that resembles Fig. 4 is located in two places, each yielding different results. 

#' (1) Code headed “6 trends” in the key analysis file `05_paper_1_analyses_R4_check.Rmd` generates trend lines for all four HOLC categories, but instead for sampling density supposedly depicted by Fig. 4, it depicts number of HOLC polygons that were sampled.

#+ r_f4_1, fig.width=15*0.393701,fig.height=15*0.393701 
# Here we show the authors original code with our adjustments indicate by MaPe

# Load packages
#suppressPackageStartupMessages(require(dplyr); require(ggplot2))
require(dplyr)
require(ggplot2)

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
