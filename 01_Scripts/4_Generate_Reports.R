# This Script is used to generate all of the pdf files, either individually or all at once

# First time package install (uncomment to install packages) ----
# source('01_Scripts/1_install_packages.R')
# install.packages(pkgs)

# Load Libraries ----
library(tidyverse)
library(lubridate)
library(kableExtra)
library(scales)
library(fuzzyjoin)
library(ggrepel)
library(cowplot)
library(rmarkdown)

# Source the scripts used (data wrangling and plotting functions) ----
source('01_Scripts/2_data_wrangling_functions.R')
source('01_Scripts/2_data_wrangling_functions.R')
source('01_Scripts/3_plotting_functions.R')

# Load Data ----
trial_data_raw_tbl <- read_csv(file = '00_Data/Individual_Report_Data_2pages_scram_v2.csv',
                               na = c("","NA"," NA"))
reference_values_raw_tbl <- read_csv('00_Data/reference_values.csv',
                                     na = c("-",""))
variables_of_interest_raw_tbl <- read_csv('00_Data/variables_of_interest.csv')
astrological_signs_raw_tbl <- read_csv('00_Data/astrological_signs.csv')

# Data Processeing / Wrangling ----
  # Call the functions to manipulate the data into the proper formats
trial_data_cleaned_tbl <- trial_data_cleaned_function(data = trial_data_raw_tbl)
biomarkers_tbl <- biomarkers_tbl_function(data = trial_data_cleaned_tbl)
other_metrics_tbl <- other_measurements_function(data = trial_data_cleaned_tbl)

# Create the Pdf Reports ----
  # For each study ID in the dataset, a pdf file is created
#participantID <- unique(trial_data_cleaned_tbl$study_id) # Comment this line out if you want to test a single study_id

participantID <- 'VITD_1001' # Uncomment this line if you want to run report for a single patient

for (study_id in participantID){
  study_id == study_id
  render(input = '01_Scripts/5_Generate_pdf_reports.Rmd',
         output_format = "pdf_document",
         output_file = paste(study_id, Sys.Date(), ".pdf", sep='_'),
         output_dir = '03_Rmarkdown_Reports/1_Study_Participant_Report_files/')

}
