# Script is used to generate a list of required packages used in this project

pkgs <- c(

  'tidyverse',          # for data wrangling and plotting: some of the packages used dplyr, ggplot2, tibble, readr
  'lubridate',          # working with dates and times
  'kableExtra',         # create nicely formatted tables
  'scales',             # Used to adjust scales on plots and other formatting
  'fuzzyjoin',          # used to do a fuzzy join between 2 tables (look up a value in a range in a 2nd table)
  'ggrepel',            # used for label manipulation in ggplot
  'cowplot',            # used for arranging plots on a grid
  'rmarkdown'           # used to generate pdf reports
  
)