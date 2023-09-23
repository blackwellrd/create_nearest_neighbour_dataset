## ************************************************************************* ##
##                                                                           ##
## Script name: create_nearest_neighbour_dataset.R                           ##
##                                                                           ##
## Purpose of script: To combine demographic, Quality Outcomes Framework,    ##
##                    workforce and census data to create the dataset that   ##
##                    is used to feed the Nearest Neighbour app              ##
##                                                                           ##
## Author: Richard Blackwell                                                 ##
##                                                                           ##
## Date Created: 2023-09-08                                                  ##
##                                                                           ##
## Email: richard.blackwell@swahsn.com                                       ##
##                                                                           ##
## Notes: The README.md in the git repo gives a more detailed explanation    ##
##        of the suggested datasets for nearest neighbour dataset creation   ##
##                                                                           ##
## ************************************************************************* ##

# 0. Load Libraries and Define Functions and Key Variables ----
# *************************************************************

library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(rgdal)
library(spaa)
library(factoextra)

# GP Registration - LSOA
gp_population_filename <- 'D:/Data/NHSD/GPREGLSOA/20230701/gp-reg-pat-prac-lsoa-all.csv'

# Current practices
epraccur_filename <- 'D:/Data/NHSD/EPRACCUR/20230825/epraccur.csv'

# PCNs and member practices
pcn_filename <- 'D:/Data/NHSD/EPCN/20230825/ePCN.xlsx'
pcn_detail_sheet <- 'PCNDetails'
pcn_member_sheet <- 'PCN Core Partner Details'

# Location to ICB to NHSE region lookup
loc_icb_nhser_lookup_filename <- 'D:/Data/OpenGeography/Lookups/LOC22_ICB22_NHSER22/LOC22_ICB22_NHSER22_EN_LU.xlsx'
loc_icb_nhser_lookup_sheet <- 'LOC22_ICB22_NHSER22_EN_LU'

# Postcode lookup
postcode_lookup_filename <- 'D:/Data/OpenGeography/Lookups/PCD/20230823/Data/ONSPD_AUG_2023_UK.csv'

# LSOA 2011 Shapefile
lsoa11_shapefile_dsn <- 'D:/Data/OpenGeography/Shapefiles/LSOA11'
lsoa11_shapefile_layer <- 'lsoa11'

# GP Registrations - Age and Gender
age_female_filename <- 'D:/Data/NHSD/GPREGSYOA/20230701/gp-reg-pat-prac-sing-age-female.csv'
age_male_filename <- 'D:/Data/NHSD/GPREGSYOA/20230701/gp-reg-pat-prac-sing-age-male.csv'
  
# QOF Prevalence
qof_prevalence_filename <- 'D:/Data/NHSD/QOF/2023/PREVALENCE_2223.csv'

# QOF Achievement
qof_achievement_ee_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_EAST_OF_ENGLAND_2223.csv'
qof_achievement_ln_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_LONDON_2223.csv'
qof_achievement_md_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_MIDLANDS_2223.csv'
qof_achievement_ne_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_NORTH_EAST_AND_YORKSHIRE_2223.csv'
qof_achievement_nw_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_NORTH_WEST_2223.csv'
qof_achievement_se_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_SOUTH_EAST_2223.csv'
qof_achievement_sw_filename <- 'D:/Data/NHSD/QOF/2023/ACHIEVEMENT_SOUTH_WEST_2223.csv'

# Practice Payments
# URL: https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice/england-2021-22
practice_income_filename <- 'D:/Data/NHSD/NHS_PAYMENTS/2022/nhspaymentsgp-21-22-prac-csv-v2.csv'
pcn_income_filename <- 'D:/Data/NHSD/NHS_PAYMENTS/2022/nhspaymentsgp-21-22-pcn-csv.csv'

# Workforce
practice_workforce_filename <- 'D:/Data/NHSD/WORKFORCE/20230731/3 General Practice – July 2023 Practice Level - High level.csv'
pcn_workforce_filename <- 'D:/Data/NHSD/WORKFORCE/20230731/Primary Care Networks - July 2023 Individual Level.csv'

# Census 2021 ethnicity
ethnicity_filename <- 'D:/Data/NOMIS/Census_2021/TS/census2021-ts021/census2021-ts021-lsoa.csv'

# IMD data
imd_filename <- 'D:/Data/GOV.UK/IMD/2019/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv'
imd_underlying_indicators_filename <- 'D:/Data/GOV.UK/IMD/2019/File_8_-_IoD2019_Underlying_Indicators.xlsx'
imd_underlying_indicators_income_sheet <- 'IoD2019 Income Domain'
imd_underlying_indicators_employment_sheet <- 'IoD2019 Employment Domain'
imd_underlying_indicators_education_sheet <- 'IoD2019 Education Domain'
imd_underlying_indicators_health_sheet <- 'IoD2019 Health Domain'
imd_underlying_indicators_barriers_sheet <- 'IoD2019 Barriers Domain'
imd_underlying_indicators_environment_sheet <- 'IoD2019 Living Env Domain'

# Declare the geographical to organisational data function
fnGeo2Org <- function(df_geo, df_weighting){
  # The data frame df_geo should contain the geographical based data, and the data frame 
  # df_weighting should contain the weighting value mapped to organisation and geography
  # Fields are as follows; [df_geo] geo_code, metric, value and [df_weighting] org_code, geo_code, weighting
  df_result <- df_weighting %>% 
    # Join the weighting data frame and geographic based data
    inner_join(df_geo, by = c('geo_code' = 'geo_code'), relationship = 'many-to-many') %>% 
    # Weight the geographical data fields by multiplying by the weighting value
    mutate(weighted_value = weighting * value,
           weighting = ifelse(is.na(value), NA, weighting)) %>%
    # Group by the organisation code...
    group_by(org_code, metric) %>% 
    # and summarise the weighted_value and weighting
    summarise(weighted_value = sum(weighted_value, na.rm = TRUE),
              weighting = sum(weighting, na.rm = TRUE),
              .groups = 'keep') %>%
    ungroup() %>%
    # Calculate the final metric_value by dividing the weighted_value by the weighting
    mutate(value = weighted_value / weighting) %>%
    # Drop the unnecessary fields
    select(-c(weighted_value, weighting))
  # Return the organisational data
  return(df_result)
}

# Used to check for duplicate field names (may need to use tolower before checking)
fnCheckFieldNames <- function(df_add, df_exist){
  # Get the field names
  additional_names <- names(df_add)
  existing_names <- names(df_exist)
  # Remove the org_code name from the additional list as we
  # know this is a required duplicate for joining purposes
  additional_names <- additional_names[additional_names!='org_code']
  # Loop through the field names and test for matches
  return(sum(unlist(lapply(additional_names, function(x){existing_names==x})))==0)
}

  
# 1. Process the organisational details ----
# ******************************************

# * 1.1. GP registration data ----
# ````````````````````````````````
# Load the master file which will be the GP registration data by LSOA 2011
df_gp_popn <- read.csv(gp_population_filename) %>% 
  select(3, 5, 7) %>%
  rename_with(.fn = ~c('prac_code', 'lsoa11cd', 'reg_popn'))

# Get the total number of rows
n_rows <- nrow(df_gp_popn)

# Remove any non-English LSOA patients
df_gp_popn <- df_gp_popn %>% filter(grepl('^E', lsoa11cd))
  
# Display the number of records removed and percentage
sprintf('Rows removed: %d (%.2f%%)', 
        n_rows - nrow(df_gp_popn),
        (n_rows - nrow(df_gp_popn))/n_rows*100)

# * 1.2. Current England practices ----
# `````````````````````````````````````
# Load the epraccur data
df_prac <- read.csv(epraccur_filename, header = FALSE) %>% 
  select(1, 2, 3, 4, 10, 12, 13, 26) %>% 
  rename_with(.fn = ~c('prac_code', 'prac_name', 'nhser_code', 'icb_code',
                       'postcode', 'close_date', 'status_code', 'presc_code')) %>%
  # Filter out any closed or non-active or non-GP prescribing setting entries
  filter(is.na(close_date) & status_code == 'A' & presc_code == 4) %>%
  select(-c(6:8))

# * 1.3. Current England primary care networks (PCN) ----
# ```````````````````````````````````````````````````````
# Load the PCN details
df_pcn_detail <- read_excel(path = pcn_filename,
                            sheet = pcn_detail_sheet) %>%
  select(1, 2, 3, 6, 12) %>%
  rename_with(.fn = ~c('pcn_code', 'pcn_name', 'loc_code', 'close_date', 'postcode')) %>%
  # Filter out any closed PCNs
  filter(is.na(close_date)) %>%
  select(-4)

# Load the PCN membership
df_pcn_members <- read_excel(path = pcn_filename,
                             sheet = pcn_member_sheet) %>% 
  select(1, 5, 10) %>%
  rename_with(.fn = ~c('prac_code', 'pcn_code', 'close_date')) %>%
  # Filter out any closed PCNs
  filter(is.na(close_date)) %>%
  select(-3)

# Add the PCN code to the GP registration data
df_gp_popn <- df_gp_popn %>% 
  left_join(df_pcn_members, by = 'prac_code') %>%
  select(1, 4, 2:3)

# * 1.4. Location to ICB to NHS England Region lookup ----
# ````````````````````````````````````````````````````````
df_loc_icb_nher_lu <- read_excel(path = loc_icb_nhser_lookup_filename,
           sheet = loc_icb_nhser_lookup_sheet) %>%
  select(2, 4, 5, 6, 8, 9) %>% 
  rename_with(.fn = ~c('loc22cd', 
                       'icb22ons', 'icb22cd', 'icb22nm', 
                       'nhser22cd', 'nhser22nm'))

# * 1.5. Postcode data ----
# `````````````````````````
df_postcode <- read.csv(postcode_lookup_filename) %>% 
  select(3, 43, 44) %>%
  rename_with(.fn = ~c('postcode', 'latitude', 'longitude'))

# * 1.6. Process practice data ----
# `````````````````````````````````
# Not all practices are present in both the current England practices and
# the GP registration data sets so only those present in both with be used
df_prac_index <- df_prac %>% 
  semi_join(df_gp_popn, by = 'prac_code') %>%
  # Join to the ICB to NHS Region lookup
  left_join(df_loc_icb_nher_lu %>% distinct(icb22cd, icb22nm, nhser22cd, nhser22nm),
            by = c('icb_code' = 'icb22cd', 'nhser_code' = 'nhser22cd')) %>% 
  mutate(
    org_code = prac_code, org_name = prac_name,
    icb_name = icb22nm, nhser_name = nhser22nm, .keep = 'unused') %>% 
  select(org_code, org_name, postcode,
         icb_code, icb_name,
         nhser_code, nhser_name) %>%
  # Join to the postcode data to get latitude and longitude
  left_join(df_postcode, by = 'postcode')

# * 1.7. Process PCN data ----
# ````````````````````````````
df_pcn_index <- df_pcn_detail %>% 
  # Join to the LOC to ICB to NHS Region lookup
  left_join(df_loc_icb_nher_lu,
            by = c('loc_code' = 'loc22cd')) %>% 
  mutate(org_code = pcn_code, org_name = pcn_name,
         icb_code = icb22cd, icb_name = icb22nm, 
         nhser_code = nhser22cd, nhser_name = nhser22nm, 
         .keep = 'unused') %>% 
  select(org_code, org_name, postcode,
         icb_code, icb_name,
         nhser_code, nhser_name) %>%
  # Join to the postcode data to get latitude and longitude
  left_join(df_postcode, by = 'postcode')

# 2. Process the organisational data ----
# ***************************************

# * 2.1. Gender and Age ----
# ``````````````````````````
df_age <- read.csv(age_female_filename) %>% 
  select(4, 7, 8) %>% 
  rename_with(.fn = ~c('prac_code', 'age', 'popn')) %>%
  mutate(gender = 'females') %>%
  bind_rows(
    read.csv(age_male_filename) %>% 
      select(4, 7, 8) %>% 
      rename_with(.fn = ~c('prac_code', 'age', 'popn')) %>%
      mutate(gender = 'males')
  ) %>%
  pivot_wider(names_from = gender, values_from = popn) %>%
  filter(age!='ALL') %>%
  mutate(persons = females + males,
         age = as.integer(age)) %>%
  replace_na(replace = list(age = 95)) %>%
  left_join(df_pcn_members, by = 'prac_code')

# Create the PCN version of the age data
df_age_pcn <- df_age %>% 
  group_by(pcn_code, age) %>%
    summarise(females = sum(females),
              males = sum(males),
              persons = sum(persons),
              .groups = 'keep') %>%
  ungroup() %>%
  pivot_longer(3:5, names_to = 'gender', values_to = 'popn') %>%
  group_by(pcn_code, gender) %>%
  reframe(value = c(quantile(rep(age, popn), 
                               probs = c(0.25, 0.5, 0.75), 
                               names = TRUE),
                      popn = sum(popn)))%>%
  mutate(metric = names(value),
         value = unname(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'metric', values_from = 'value') %>%
  rename_with(.fn = ~c('pcn_code', 'gender', 'q1', 'median', 'q3', 'popn')) %>%
  mutate(iqr = q3 - q1, .before = popn) %>%
  pivot_wider(names_from = 'gender', values_from = c('q1','median','q3','iqr','popn'))

# Create the practice version of the age data
df_age_prac <- df_age %>% 
  group_by(prac_code, age) %>%
  summarise(females = sum(females),
            males = sum(males),
            persons = sum(persons),
            .groups = 'keep') %>%
  ungroup() %>%
  pivot_longer(3:5, names_to = 'gender', values_to = 'popn') %>%
  group_by(prac_code, gender) %>%
  reframe(value = c(quantile(rep(age, popn), 
                             probs = c(0.25, 0.5, 0.75), 
                             names = TRUE),
                    popn = sum(popn)))%>%
  mutate(metric = names(value),
         value = unname(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'metric', values_from = 'value') %>%
  rename_with(.fn = ~c('prac_code', 'gender', 'q1', 'median', 'q3', 'popn')) %>%
  mutate(iqr = q3 - q1, .before = popn) %>%
  pivot_wider(names_from = 'gender', values_from = c('q1','median','q3','iqr','popn'))

# Add the practice age data to the practice index
df_prac_index <- df_prac_index %>% 
  left_join(df_age_prac, by = c('org_code' = 'prac_code'))

# Add the PCN age data to the PCN index
df_pcn_index <- df_pcn_index %>% 
  left_join(df_age_pcn, by = c('org_code' = 'pcn_code'))

# Tidy up
rm(list=c('df_age', 'df_age_prac', 'df_age_pcn'))

# * 2.2. QOF Prevalence ----
# ``````````````````````````

# Load the QOF prevalence data
df_qof_prev <- read.csv(qof_prevalence_filename) %>%
  select(1, 2, 3, 5) %>%
  mutate(GROUP_CODE = tolower(GROUP_CODE))

# Create the PCN version of the QOF prevalence
df_qof_prev_pcn <- df_qof_prev %>% 
  left_join(df_pcn_members, by = c('PRACTICE_CODE' = 'prac_code')) %>%
  group_by(pcn_code, GROUP_CODE) %>%
  summarise(REGISTER = sum(REGISTER, na.rm = TRUE),
            PRACTICE_LIST_SIZE = sum(PRACTICE_LIST_SIZE, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup() %>%
  mutate(PREV = REGISTER / PRACTICE_LIST_SIZE) %>%
  select(-c('REGISTER', 'PRACTICE_LIST_SIZE')) %>%
  pivot_wider(names_from = GROUP_CODE, 
              values_from = PREV)

# Create the practice version of the QOF prevalence
df_qof_prev_prac <- df_qof_prev %>% 
  mutate(PREV = REGISTER / PRACTICE_LIST_SIZE) %>%
  select(-c('REGISTER', 'PRACTICE_LIST_SIZE')) %>%
  pivot_wider(names_from = GROUP_CODE, 
              values_from = PREV)

# Add the practice data to the practice index file
df_prac_index <- df_prac_index %>% 
  left_join(df_qof_prev_prac, by = c('org_code' = 'PRACTICE_CODE'))

# Add the PCN data to the PCN index file
df_pcn_index <- df_pcn_index %>% 
  left_join(df_qof_prev_pcn, by = c('org_code' = 'pcn_code'))

# Tidy up
rm(list=c('df_qof_prev', 'df_qof_prev_prac', 'df_qof_prev_pcn'))

# * 2.3. QOF Achievement ----
# ```````````````````````````
df_qof_achv <- read.csv(qof_achievement_ee_filename) %>%
  bind_rows(read.csv(qof_achievement_ln_filename)) %>%
  bind_rows(read.csv(qof_achievement_md_filename)) %>%
  bind_rows(read.csv(qof_achievement_ne_filename)) %>%
  bind_rows(read.csv(qof_achievement_nw_filename)) %>%
  bind_rows(read.csv(qof_achievement_se_filename)) %>%
  bind_rows(read.csv(qof_achievement_sw_filename)) %>%
  select(4:7) %>%
  filter(!MEASURE %in% c('ACHIEVED_POINTS','REGISTER')) %>% 
  pivot_wider(names_from = 'MEASURE',
              values_from = 'VALUE') %>%
  left_join(df_pcn_members, by = c('PRACTICE_CODE' = 'prac_code'))

# Create the PCN version of the QOF achievement
df_qof_achv_pcn <- df_qof_achv %>% 
  group_by(pcn_code, INDICATOR_CODE) %>%
  summarise(NUMERATOR = sum(NUMERATOR, na.rm = TRUE),
            DENOMINATOR = sum(DENOMINATOR, na.rm = TRUE),
            PCAS = sum(PCAS, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup() %>%
  mutate(ACHV = NUMERATOR / DENOMINATOR,
         ACHV_RAW = NUMERATOR / (DENOMINATOR + PCAS)) %>% 
  select(1:2, 6:7) %>%
  pivot_wider(names_from = 'INDICATOR_CODE',
              names_glue = '{INDICATOR_CODE}{.value}',
              values_from = c('ACHV','ACHV_RAW')) %>%
  rename_with(.fn = function(x){tolower(gsub('ACHV', '', names(.)))})

# Create the practice version of the QOF achievement
df_qof_achv_prac <- df_qof_achv %>% 
  group_by(PRACTICE_CODE, INDICATOR_CODE) %>%
  summarise(NUMERATOR = sum(NUMERATOR, na.rm = TRUE),
            DENOMINATOR = sum(DENOMINATOR, na.rm = TRUE),
            PCAS = sum(PCAS, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup() %>%
  mutate(ACHV = NUMERATOR / DENOMINATOR,
         ACHV_RAW = NUMERATOR / (DENOMINATOR + PCAS)) %>% 
  select(1:2, 6:7) %>%
  pivot_wider(names_from = 'INDICATOR_CODE',
              names_glue = '{INDICATOR_CODE}{.value}',
              values_from = c('ACHV','ACHV_RAW')) %>%
  rename_with(.fn = function(x){tolower(gsub('ACHV', '', names(.)))})

# Add the practice data to the practice index file
df_prac_index <- df_prac_index %>% 
  left_join(df_qof_achv_prac, by = c('org_code' = 'practice_code'))

# Add the PCN data to the PCN index file
df_pcn_index <- df_pcn_index %>% 
  left_join(df_qof_achv_pcn, by = c('org_code' = 'pcn_code'))

# Tidy up
rm(list=c('df_qof_achv', 'df_qof_achv_prac', 'df_qof_achv_pcn'))

# * 2.4. Workforce ----
# `````````````````````

# Get the practice workforce data
df_workforce_prac <- read.csv(practice_workforce_filename) %>% 
  filter(DETAILED_STAFF_ROLE == 'Total' & MEASURE == 'FTE') %>%
  select(1, 3, 6) %>%
  rename_with(.fn = ~c('prac_code', 'staff_group', 'fte')) %>% 
  pivot_wider(names_from = 'staff_group', values_from = 'fte', values_fill= 0) %>%
  rename_with(.fn = ~c('prac_code', 'admin_fte', 'dpc_fte', 'gp_fte', 'nurse_fte'))

# Get the practice workforce data
df_workforce_pcn <- read.csv(pcn_workforce_filename) %>% 
  select(4, 12, 15) %>%
  rename_with(.fn = ~c('pcn_code', 'staff_group', 'fte')) %>%
  group_by(pcn_code, staff_group) %>%
  summarise(fte = sum(fte, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup() %>%
  pivot_wider(names_from = 'staff_group', values_from = 'fte', values_fill= 0) %>%
  rename_with(.fn = ~c('pcn_code', 'admin_fte', 'dpc_fte', 'dir_fte', 'gp_fte', 'nurse_fte'))

# Add the practice data to the practice index file
df_prac_index <- df_prac_index %>% 
  left_join(df_workforce_prac, by = c('org_code' = 'prac_code'))

# Add the PCN data to the PCN index file
df_pcn_index <- df_pcn_index %>% 
  left_join(df_workforce_pcn, by = c('org_code' = 'pcn_code'))

# Tidy up
rm(list=c('df_workforce_prac', 'df_workforce_pcn'))

# * 2.5. Income ----
# ``````````````````

# Load the practice NHS payments data
df_income_prac <- read.csv(practice_income_filename) %>% 
  select(7, 73) %>% 
  rename_with(.fn = ~c('prac_code', 'income_per_head_reg_popn'))

# Load the PCN NHS payments data and calculate the PCN funding income per 
# head of registered popn
df_income_pcn <- read.csv(pcn_income_filename) %>% 
  rowwise() %>% 
  mutate(income = sum(c_across(7:13), na.rm = TRUE)) %>% 
  select(3, 5, 14) %>% 
  rename_with(.fn = ~c('pcn_code', 'reg_popn', 'income')) %>%
  mutate(income_per_head_popn = income / reg_popn) %>% 
  select(1, 4)

# Load the practice NHS payments data and select the total practice funding 
# income per head of registered popn
df_income_prac <- read.csv(practice_income_filename) %>% 
  select(7, 73) %>% 
  rename_with(.fn = ~c('prac_code', 'income_per_head_popn'))

# Add the practice data to the practice index file
df_prac_index <- df_prac_index %>% 
  left_join(df_income_prac, by = c('org_code' = 'prac_code'))

# Add the PCN data to the PCN index file
df_pcn_index <- df_pcn_index %>% 
  left_join(df_income_pcn, by = c('org_code' = 'pcn_code'))

# Tidy up
rm(list=c('df_income_prac', 'df_income_pcn'))

# Tidy up the organisational data frames
rm(list=c('df_loc_icb_nher_lu', 'df_prac',
          'df_pcn_detail', 'df_pcn_members',
          'df_postcode'))

# 3. Process the geographical data ----
# *************************************

# * 3.0. Create the weighting data frames ----
# ````````````````````````````````````````````

# Practice level weighting
df_weighting_prac <- df_gp_popn %>% 
  transmute(org_code = prac_code, 
            geo_code = lsoa11cd, 
            weighting = reg_popn)

# PCN level weighting
df_weighting_pcn <- df_gp_popn %>% 
  transmute(org_code = pcn_code, 
            geo_code = lsoa11cd, 
            weighting = reg_popn) %>%
  group_by(org_code, geo_code) %>%
  summarise(weighting = sum(weighting, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# * 3.1. Population density ----
# ``````````````````````````````
# Load the LSOA 2011 shapefiles
sf_lsoa11 <- st_read(dsn = lsoa11_shapefile_dsn,
                     layer = lsoa11_shapefile_layer) %>%
  mutate(area_km2 = Shape__Are / 1e6) %>%
  filter(grepl('^E', LSOA11CD))

# Calculate the population density of each LSOA using the GP registration data and
# add the area of the lsoa
df_popn_density <- df_gp_popn %>% 
  group_by(lsoa11cd) %>%
  summarise(total_popn = sum(reg_popn),
            .groups = 'keep') %>%
  ungroup() %>%
  left_join(sf_lsoa11 %>% st_drop_geometry() %>% select(LSOA11CD, area_km2),
            by = c('lsoa11cd' = 'LSOA11CD')) %>% 
  mutate(popn_per_km2 = total_popn / area_km2) %>%
  transmute(geo_code = lsoa11cd, metric = 'popn_per_km2', value = popn_per_km2)

df_popn_density_prac <- fnGeo2Org(df_geo = df_popn_density, df_weighting = df_weighting_prac) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')

df_popn_density_pcn <- fnGeo2Org(df_geo = df_popn_density, df_weighting = df_weighting_pcn) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')

# Add the practice data to the practice index file
if(fnCheckFieldNames(df_popn_density_prac, df_prac_index)){
  df_prac_index <- df_prac_index %>% 
    left_join(df_popn_density_prac, by = 'org_code')
} else {
  'Duplicate fields exist'
}

# Add the PCN data to the PCN index file
# Add the practice data to the practice index file
if(fnCheckFieldNames(df_popn_density_pcn, df_pcn_index)){
  df_pcn_index <- df_pcn_index %>% 
    left_join(df_popn_density_pcn, by = 'org_code')
} else {
  'Duplicate fields exist'
}

# Tidy up
rm(list=c('sf_lsoa11', 'df_gp_popn', 'df_popn_density','df_popn_density_prac', 'df_popn_density_pcn'))

# * 3.2. Census ethnicity data ----
# `````````````````````````````````

# Get the census ethnicity
df_ethnicity <- read.csv(ethnicity_filename) %>% 
  select(3, 5, 11, 15, 20, 26, 4) %>% 
  rename_with(.fn = ~c('lsoa21cd', 'asian', 'black', 'mixed', 'white', 'other', 'total_popn'))

# Load the 2011 to 2021 LSOA lookup
df_lsoa11_lsoa21 <- read.csv('D:/Data/OpenGeography/Lookups/LSOA11_LSOA21/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv') %>%
  select(1, 3, 5) %>% 
  rename_with(.fn = function(x){c('lsoa11cd', 'lsoa21cd', 'chgind')}) %>%
  # Ignore the 'complex' mappings keeping the complex unchanged 'XU' mappings
  filter(!chgind=='X')

# Map ethnicity to 2011 LSOAs and calculate proportions
df_ethnicity_2011 <- df_lsoa11_lsoa21 %>% 
  select(-chgind) %>% 
  left_join(df_ethnicity, by = 'lsoa21cd') %>% 
  group_by(lsoa11cd) %>%
  summarise(across(.cols = 2:7, .fns = sum), .groups = 'keep') %>%
  ungroup() %>%
  mutate(across(.cols = 2:6, .fns = \(x) x / total_popn)) %>%
  select(-total_popn) %>%
  pivot_longer(cols = 2:6, names_to = 'metric', values_to = 'value')

# Create the practice level ethnicity
df_ethnicity_prac <- fnGeo2Org(df_geo = df_ethnicity_2011 %>% 
                                 mutate(geo_code = lsoa11cd, .keep = 'unused', .before = 1),
                               df_weighting = df_weighting_prac) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')

# Create the PCN level ethnicity
df_ethnicity_pcn <- fnGeo2Org(df_geo = df_ethnicity_2011 %>% 
                                mutate(geo_code = lsoa11cd, .keep = 'unused', .before = 1),
                              df_weighting = df_weighting_pcn) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')

# Add the practice data to the practice index file
if(fnCheckFieldNames(df_ethnicity_prac, df_prac_index)){
  df_prac_index <- df_prac_index %>% 
    left_join(df_ethnicity_prac, by = 'org_code')
} else {
  'Duplicate fields exist'
}

# Add the PCN data to the PCN index file
if(fnCheckFieldNames(df_ethnicity_pcn, df_pcn_index)){
  df_pcn_index <- df_pcn_index %>% 
    left_join(df_ethnicity_pcn, by = 'org_code')
} else {
  'Duplicate fields exist'
}

# Tidy up
rm(list=c('df_ethnicity', 'df_ethnicity_2011', 'df_ethnicity_prac', 'df_ethnicity_pcn'))

# * 3.3. Indices of multiple deprivation scores ----
# ``````````````````````````````````````````````````

# Get the IMD domain data selecting only the score data
df_imd_domains <- read.csv(imd_filename) %>% 
  select(1, contains('score')) %>%
  rename_with(.fn = ~c('lsoa11cd', 'imd', 'income_domain', 'employment_domain', 'education_domain',
                       'health_domain', 'crime_domain', 'housing_and_services_domain', 'environment_domain',
                       'idaci_subdomain', 'idaopi_subdomain', 'children_edu_subdomain', 'adult_edu_subdomain', 
                       'geographical_barriers_subdomain', 'wider_barriers_subdomain', 'indoors_subdomain', 'outdoors_subdomain'))

# Get the IMD underlying indicator data
df_imd_underlying_indicators_income <- read_excel(path = imd_underlying_indicators_filename,
                                                  sheet = imd_underlying_indicators_income_sheet) %>% 
  select(1, 5:7) %>%
  rename_with(.fn = ~c('lsoa11cd', 'income_numerator', 'idaci_numerator', 'idopi_numerator')) %>%
  # Replace any missing values with zero
  replace_na(list(income_numerator = 0, idaci_numerator = 0, idopi_numerator = 0))

df_imd_underlying_indicators_employment <- read_excel(path = imd_underlying_indicators_filename,
                                                      sheet = imd_underlying_indicators_employment_sheet) %>%
  select(1, 5) %>%
  rename_with(.fn = ~c('lsoa11cd', 'employment_numerator')) %>%
  # Replace any missing values with zero
  replace_na(list(employment_numerator = 0))

df_imd_underlying_indicators_education <- read_excel(path = imd_underlying_indicators_filename,
                                                     sheet = imd_underlying_indicators_education_sheet) %>% 
  select(1, 5:7) %>%
  rename_with(.fn = ~c('lsoa11cd', 'post_16_education_numerator', 'entry_to_higher_edu_numerator', 'adult_skills_numerator')) %>%
  # Replace any missing values with zero
  replace_na(list(entry_to_higher_edu_numerator = 0))

df_imd_underlying_indicators_health <- read_excel(path = imd_underlying_indicators_filename,
                                                  sheet = imd_underlying_indicators_health_sheet) %>% 
  select(1, 5:8) %>%
  rename_with(.fn = ~c('lsoa11cd', 'year_of_life_lost', 'comparative_illness_and_disability', 'acute_morbidity', 'mood_and_anxiety_disorders'))

df_imd_underlying_indicators_barriers <- read_excel(path = imd_underlying_indicators_filename,
                                                    sheet = imd_underlying_indicators_barriers_sheet) %>% 
  select(1, 5:13) %>%
  rename_with(.fn = ~c('lsoa11cd', 'post_office_distance', 'primary_school_distance', 'general_store_distance',
                       'gp_distance', 'household_overcrowding', 'homelessness_indicator', 'owner_occupation_affordability',
                       'private_rental_affordability', 'housing_affordability'))

df_imd_underlying_indicators_environment <- read_excel(path = imd_underlying_indicators_filename,
                                                       sheet = imd_underlying_indicators_environment_sheet) %>% 
  select(1, 5:12) %>%
  rename_with(.fn = ~c('lsoa11cd', 'poor_quality_housing', 'housing_without_central_heating', 'traffic_accidents', 
                       'nitrogen_dioxide', 'benzene', 'suplhur_dioxide', 'particulates', 'air_quality'))

# Combine all the underlying indicators and the domain data
df_imd <- df_imd_domains %>%
  left_join(df_imd_underlying_indicators_income, by = 'lsoa11cd') %>% 
  left_join(df_imd_underlying_indicators_employment, by = 'lsoa11cd') %>% 
  left_join(df_imd_underlying_indicators_education, by = 'lsoa11cd') %>% 
  left_join(df_imd_underlying_indicators_health, by = 'lsoa11cd') %>% 
  left_join(df_imd_underlying_indicators_barriers, by = 'lsoa11cd') %>% 
  left_join(df_imd_underlying_indicators_environment, by = 'lsoa11cd') %>% 
  pivot_longer(cols = 2:NCOL(.), names_to = 'metric', values_to = 'value')

# Create the practice level IMD
df_imd_prac <- fnGeo2Org(df_geo = df_imd %>% 
                           mutate(geo_code = lsoa11cd, .keep = 'unused', .before = 1),
                         df_weighting = df_weighting_prac) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')

# Create the PCN level ethnicity
df_imd_pcn <- fnGeo2Org(df_geo = df_imd %>% 
                           mutate(geo_code = lsoa11cd, .keep = 'unused', .before = 1),
                         df_weighting = df_weighting_pcn) %>%
  pivot_wider(names_from = 'metric', values_from = 'value')

# Add the practice data to the practice index file
if(fnCheckFieldNames(df_imd_prac, df_prac_index)){
  df_prac_index <- df_prac_index %>% 
    left_join(df_imd_prac, by = 'org_code')
} else {
  'Duplicate fields exist'
}

# Add the PCN data to the PCN index file
if(fnCheckFieldNames(df_imd_pcn, df_pcn_index)){
  df_pcn_index <- df_pcn_index %>% 
    left_join(df_imd_pcn, by = 'org_code')
} else {
  'Duplicate fields exist'
}

# Tidy up
rm(list=c('df_imd_domains',
          'df_imd_underlying_indicators_income', 'df_imd_underlying_indicators_employment', 'df_imd_underlying_indicators_education',
          'df_imd_underlying_indicators_health', 'df_imd_underlying_indicators_barriers', 'df_imd_underlying_indicators_environment',
          'df_imd_prac', 'df_imd_pcn'))

# 4. Output the unprocessed data ----
# ***********************************

dir.create('./output', showWarnings = FALSE, recursive = TRUE)
write.csv(df_prac_index, './output/practice_level_nn_data.csv', row.names = FALSE)
write.csv(df_pcn_index, './output/pcn_level_nn_data.csv', row.names = FALSE)
save(list=c('df_prac_index', 'df_pcn_index'), file = './output/nn_data.RObj')

# 5. Create the distance matrix ----
# **********************************

# Scale the data - ignoring the organisational details, the QOF achievement and the income
prac_dist_matrix <- dist(scale(df_prac_index[,c(10:45, 152:155, 157:206)]))

df_prac_distances <- dist2list(prac_dist_matrix) %>% 
  filter(col!=row) %>%
  transmute(
    orig = df_prac_index$org_code[row],
    dest = df_prac_index$org_code[col],
    distance = value
  )

# Scale the data - ignoring the organisational details, the QOF achievement and the income
pcn_dist_matrix <- dist(scale(df_pcn_index[,c(10:45, 152:155, 157:207)]))

df_pcn_distances <- dist2list(pcn_dist_matrix) %>% 
  filter(col!=row) %>%
  transmute(
    orig = df_pcn_index$org_code[row],
    dest = df_pcn_index$org_code[col],
    distance = value
  )

dir.create('./output', showWarnings = FALSE, recursive = TRUE)
write.csv(df_prac_distances, './output/practice_dist_matrix.csv', row.names = FALSE)
write.csv(df_pcn_distances, './output/pcn_dist_matrix.csv', row.names = FALSE)
write.csv(df_prac_distances %>% arrange(distance) %>% group_by(orig) %>% slice_head(n = 10), 
          './output/practice_nearest_10.csv', row.names = FALSE)
write.csv(df_pcn_distances %>% arrange(distance) %>% group_by(orig) %>% slice_head(n = 10), 
          './output/pcn_nearest_10.csv', row.names = FALSE)
write.csv(df_prac_distances %>% arrange(distance) %>% group_by(orig) %>% slice_tail(n = 10), 
          './output/practice_furthest_10.csv', row.names = FALSE)
write.csv(df_pcn_distances %>% arrange(distance) %>% group_by(orig) %>% slice_tail(n = 10), 
          './output/pcn_furthest_10.csv', row.names = FALSE)
save(list=c('df_prac_distances', 'df_pcn_distances'), file = './output/dist_data.RObj')


# 6. Create the clusters ----
# ***************************

# K-means clustering set-up
kmeans_input <- df_prac_index %>% select(c(10:45, 152:155, 157:206))
kmeans_input[is.na(kmeans_input)] <- 0
kmeans_input <- scale(kmeans_input, center = TRUE, scale = TRUE)

factoextra::fviz_nbclust(kmeans_input, kmeans, method = "wss", iter.max = 20)
factoextra::fviz_nbclust(kmeans_input, kmeans, method = "silhouette", iter.max = 20) + labs(subtitle = "Silhouette method")
res_kmeans <- kmeans(kmeans_input, centers = 7, iter.max = 20, nstart = 20)
df_prac_index$cluster <- res_kmeans$cluster

# K-means clustering set-up
kmeans_input <- df_pcn_index %>% select(c(10:45, 152:155, 157:207))
kmeans_input[is.na(kmeans_input)] <- 0
kmeans_input <- scale(kmeans_input, center = TRUE, scale = TRUE)

factoextra::fviz_nbclust(kmeans_input, kmeans, method = "wss", iter.max = 20)
factoextra::fviz_nbclust(kmeans_input, kmeans, method = "silhouette", iter.max = 20) + labs(subtitle = "Silhouette method")
res_kmeans <- kmeans(kmeans_input, centers = 5, iter.max = 20, nstart = 20)
df_pcn_index$cluster <- res_kmeans$cluster

# 7. Map ----
# ***********

# load('./output/nn_data.RObj')
# load('./output/dist_data.RObj')

palCluster <- colorFactor(palette = 'Set1', domain = df_prac_index$cluster)
labels = 

prac_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 1),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '1') %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 2),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '2') %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 3),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '3') %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 4),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '4') %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 5),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '5') %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 6),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '6') %>%
  addCircleMarkers(data = df_prac_index %>% 
                     select(org_code, org_name, postcode, popn_persons, 
                            icb_code, icb_name, 
                            nhser_code, nhser_name,
                            latitude, longitude,
                            cluster) %>%
                     filter(cluster == 7),
                   weight = 1,
                   radius = 5,
                   color = 'black',
                   fillColor = ~palCluster(cluster),
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     'Practice: ', org_name, ' - [', org_code, ']<br>',
                     'ICB: ', icb_name, ' - [', icb_code, ']<br>',
                     'Region: ', nhser_name, ' - [', nhser_code, ']<br>',
                     'Popn: ', prettyNum(popn_persons, big.mark = ',')
                   ),
                   group = '7') %>%
  addControl(
    html = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0<br>Contains OS data © Crown copyright and database right [2023]',
    position = "bottomleft", layerId = NULL, className = "info legend"
  ) %>%
  addLayersControl(overlayGroups = c('1','2','3','4','5','6','7'))
prac_map

# # 8. Read data ----
# ###################
# 
# load(file = './output/nn_data.RObj')
# load(file = './output/dist_data.RObj')
# df_prac_distances_n10 <- df_prac_distances %>% arrange(distance) %>% group_by(orig) %>% slice_head(n = 10)
# df_pcn_distances_n10 <- df_pcn_distances %>% arrange(distance) %>% group_by(orig) %>% slice_head(n = 10)
# save(list=c('df_prac_distances_n10', 'df_pcn_distances_n10'), file = './output/dist_data_n10.RObj')
