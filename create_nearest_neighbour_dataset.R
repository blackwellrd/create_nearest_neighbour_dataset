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
library(sf)
library(rgdal)

gp_population_filename <- 'D:/Data/NHSD/GPREGLSOA/20230701/gp-reg-pat-prac-lsoa-all.csv'
epraccur_filename <- 'D:/Data/NHSD/EPRACCUR/20230825/epraccur.csv'
pcn_filename <- 'D:/Data/NHSD/EPCN/20230825/ePCN.xlsx'
pcn_detail_sheet <- 'PCNDetails'
pcn_member_sheet <- 'PCN Core Partner Details'
loc_icb_nhser_lookup_filename <- 'D:/Data/OpenGeography/Lookups/LOC22_ICB22_NHSER22/LOC22_ICB22_NHSER22_EN_LU.xlsx'
loc_icb_nhser_lookup_sheet <- 'LOC22_ICB22_NHSER22_EN_LU'
postcode_lookup_filename <- 'D:/Data/OpenGeography/Lookups/PCD/20230823/Data/ONSPD_AUG_2023_UK.csv'
lsoa11_shapefile_dsn <- 'D:/Data/OpenGeography/Shapefiles/LSOA11'
lsoa11_shapefile_layer <- 'lsoa11'

age_female_filename <- 'D:/Data/NHSD/GPREGSYOA/20230701/gp-reg-pat-prac-sing-age-female.csv'
age_male_filename <- 'D:/Data/NHSD/GPREGSYOA/20230701/gp-reg-pat-prac-sing-age-male.csv'
  
qof_prevalence_filename <- 'D:/Data/NHSD/QOF/2023/PREVALENCE_2223.csv'
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


df_workforce_prac %>% head()
levels(as.factor(df_workforce_prac$MEASURE))
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





# * 2.1. Population density ----
# ``````````````````````````````
# Load the LSOA 2011 shapefiles
sf_lsoa11 <- st_read(dsn = lsoa11_shapefile_dsn,
                     layer = lsoa11_shapefile_layer) %>%
  mutate(area_km2 = Shape__Are / 1e6) %>%
  filter(grepl('^E', LSOA11CD))

# Add the population density to the index data frames
df_prac_index <- df_prac_index %>% 
  left_join(
    df_gp_popn %>% 
      mutate(popn_per_km2 = reg_popn * area_km2) %>%
      group_by(prac_code) %>%
      summarise(reg_popn = sum(reg_popn, na.rm = TRUE),
                popn_per_km2 = sum(popn_per_km2, na.rm = TRUE),
                .groups = 'keep') %>%
      mutate(popn_per_km2 = popn_per_km2 / reg_popn) %>%
      ungroup(),
    by = c('org_code' = 'prac_code')
  )


# Test Section ----
# *****************

library(sf)
library(rgdal)
library(leaflet)

sf_prac_index <- st_as_sf(df_prac_index, 
                          coords = c('longitude','latitude'),
                          crs = 4326)

palPopnDensity <- colorQuantile(palette = 'RdYlGn', 
                                domain = sf_prac_index$popn_per_km2)


leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = sf_prac_index,
                   label = ~popn_per_km2,
                   fillColor = ~palPopnDensity(popn_per_km2),
                   fillOpacity = 0.8)