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

gp_population_filename <- 'D:/Data/NHSD/GPREGLSOA/20230701/gp-reg-pat-prac-lsoa-all.csv'
epraccur_filename <- 'D:/Data/NHSD/EPRACCUR/20230825/epraccur.csv'
pcn_filename <- 'D:/Data/NHSD/EPCN/20230825/ePCN.xlsx'
pcn_detail_sheet <- 'PCNDetails'
pcn_member_sheet <- 'PCN Core Partner Details'
loc_icb_nhser_lookup_filename <- 'D:/Data/OpenGeography/Lookups/LOC22_ICB22_NHSER22/LOC22_ICB22_NHSER22_EN_LU.xlsx'
loc_icb_nhser_lookup_sheet <- 'LOC22_ICB22_NHSER22_EN_LU'

# 1. Process the organisational data ----
# ***************************************

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

# * 1.2. Current England practices
# ````````````````````````````````
# Load the epraccur data
df_prac <- read.csv(epraccur_filename, header = FALSE) %>% 
  select(1, 2, 3, 4, 10, 12, 13, 26) %>% 
  rename_with(.fn = ~c('prac_code', 'prac_name', 'nhser_name', 'icb_name',
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

# * 1.4. Location to ICB to NHS England Region lookup ----
# ````````````````````````````````````````````````````````
df_loc_icb_nher_lu <- read_excel(path = loc_icb_nhser_lookup_filename,
           sheet = loc_icb_nhser_lookup_sheet) %>%
  select(2, 4, 5, 6, 8, 9) %>% 
  rename_with(.fn = ~c('loc22cd', 
                       'icb22ons', 'icb22cd', 'icb22nm', 
                       'nhser22cd', 'nhser22nm'))


    

