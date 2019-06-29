# Script is used to store data wrangling scripts

# Script 1: Tidy Raw Clinical Data ----
# trial_data_cleaned_function
  #Function is used to do some basic data processing on the raw clinical trial data
trial_data_cleaned_function <- function(data = trial_data_raw_tbl){
  
  # Remove completely blank rows
  trial_data_cleaned_tbl <- data %>% 
    filter(!is.na(study_id)) %>% 
    
    # Convert sex to text equivalent (female = 1)
    mutate(sex = 
             case_when(
               sex == 1 ~ "Female",
               TRUE ~ "Male"
             )
    ) %>% 
    
    # Create a day of month column
    mutate(day_of_month = lubridate::day(DOB)) %>% 
    
    # Bucket patients into age groups
    mutate(age_group = 
             case_when(
               time_point == 0 & age_enroll <= 59 ~ "55 - 59",
               time_point == 0 & age_enroll <= 69 ~ "60 - 69",
               time_point == 0 & age_enroll >= 70 ~ "70+",
               TRUE ~ NA_character_
             )) %>% 
    
    # Turn rando_group into vitamin d dosage
    mutate(vitD_dose = case_when(
      rando_group == 1 ~'400 IU',
      rando_group == 2 ~ '4,000 IU',
      TRUE ~ '10,000 IU'
    ))
  
  return(trial_data_cleaned_tbl)
} 

# Script 2: Biomarkers ----
# Function is used to process the trial_data_cleaned_tbl into format used for Biomarkers plots
biomarkers_tbl_function <- function(data = trial_data_cleaned_tbl){
  
  biomarkers_tbl <- data %>% 
    
    # select only needed variables for plots
    select(study_id,time_point,sex, vitd_nmol_l,CTx, PTH, serum_ca,fasting_glucose,hemoglobin) %>% 
    
    # Rename columns
    rename(
      Months = time_point,
      `Vitamin D` = vitd_nmol_l,
      `Parathyroid Hormone` = PTH,
      `Serum Calcium` = serum_ca,
      `Fasting Glucose` = fasting_glucose,
      Hemoglobin = hemoglobin
    ) %>% 
    
    # Unpivot data table to make plotting easier
    gather(key = 'metric', value = 'Measurement Value',-Months,-sex,-study_id) %>% 
  
    # Join varibles_of_interest_raw_tbl so I can have units of measure
    left_join(variables_of_interest_raw_tbl, by = c("metric" = "variable_label")) %>% 
    
    # Join on reference_values_raw_tbl to bring in normal range
    left_join(reference_values_raw_tbl, by = c('variable' = 'variable', 'sex' = 'applicable_sex'))
  
  return(biomarkers_tbl)
}


# Script 3: Table 1: Page Header Table ----
  # Used to create page header table
header_table_function <- function(data = trial_data_cleaned_tbl){
  header_table_tbl <- data %>% 
    
    # Select variables needed for plot, and DOB used for astrological sign generation
    select(study_id,sex,vitD_dose,DOB) %>% 
    slice(1) %>% 
    
    # rename variables
    rename(
      `Participant ID` = study_id,
      Sex = sex,
      `Daily Vitamin D` = vitD_dose
    )
  
    # Create a date in astrological table so a join can be completed
    dob_year <- year(header_table_tbl$DOB)

    astrological_signs_raw_tbl <-  astrological_signs_raw_tbl %>% 
      mutate(
        start_date = paste(dob_year,start_month,start_day, sep = '-') %>% ymd(),
        end_date = paste(dob_year,end_month,end_day, sep = '-') %>% ymd()
        ) %>% 
      select(sign, start_date,end_date)

    # Add the astrological signs
    header_table_tbl <-  header_table_tbl %>%
    fuzzyjoin::fuzzy_left_join(astrological_signs_raw_tbl,
                               by = c(
                                 'DOB' = 'start_date',
                                 'DOB' = 'end_date'
                               ),
                               match_fun = list(`>=`,`<=`)
    ) %>% 
      select(-start_date,-end_date,-DOB) %>% 
      rename(`Astrological Sign` = sign)
    
  return(header_table_tbl)
}


# Script 4: Page 2 Plots ----
  # Used to wrangle the data for page 2 visuals
  other_measurements_function <- function(data = trial_data_cleaned_tbl){
  other_metrics_tbl <- data %>% 
  
    # Select required variables for plots
    select(study_id,sex,time_point,age_enroll,TH_aBMD,LS_aBMD,rad_33_aBMD,
           ttbmd_r,ttbmd_t,Balance_mean,Grip_mean,failload_r, failload_t) %>% 
    
    # Round the measurement values
    mutate(
      TH_aBMD = round(TH_aBMD,2),
      LS_aBMD = round(LS_aBMD,2),
      rad_33_aBMD = round(rad_33_aBMD,2),
      ttbmd_r = round(ttbmd_r,0),
      ttbmd_t = round(ttbmd_t,0),
      failload_r = round(failload_r,0),
      failload_t = round(failload_t,0)
    ) %>%
    
    # Fill age down for joining
    fill(age_enroll) %>% 
    
    # Unpivot data table to make plotting easier
    gather(key = 'metric', value = 'Measurement Value',TH_aBMD:failload_t) %>%  # possible fail point if columns are in different order from my data set
    
    # Turn key into a factor so it appears in proper order
    mutate(key = as_factor(metric)) %>%
    
    # Join varibles_of_interest_raw_tbl so I can have units of measure
    left_join(variables_of_interest_raw_tbl, by = c("metric" = "variable")) %>% 
    
    # Use fuzzy join to look up ages between the range values
    fuzzyjoin::fuzzy_left_join(reference_values_raw_tbl,
                    by = c(
                      'metric' = 'variable',
                      'sex' = 'applicable_sex',
                      'age_enroll' = 'age_group_min',
                      'age_enroll' = 'age_group_max'
                    ),
                    match_fun = list(`==`,`==`,`>=`,`<=`)
                    ) %>% 
    
    # Remove unwanted columns after join
    select(-applicable_sex) %>% 
    
    # Rename columns
    rename(
      Months = time_point
    )
    
    return(other_metrics_tbl)

}

