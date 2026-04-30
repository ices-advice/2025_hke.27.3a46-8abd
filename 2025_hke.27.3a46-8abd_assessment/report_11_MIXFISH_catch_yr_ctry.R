################################################################################
#  hke.27.3a46-8abd : catch data for WGMIXFISH                                 # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI)                                                       #
#   created:  14/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# WGMIXFISH_catch_yr_ctry.R - catch data by category, year, country and area (BoB - 8.abd and outside)
# ~/2023_hke.27.3a46-8abd_assessment/WGMIXFISH_catch_yr_ctry.R 

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

rm(list = ls())

dtyr <- 2023


#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

library(dplyr)


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd.in  <- file.path("data","catch")
wd.out <- file.path("WGMIXFISH","output")

if(!dir.exists(wd.out)) dir.create(wd.out)

# Input data file
table1.R  <- file.path(wd.in,"Table_1.RData")

# Output data file
out.file <- file.path( wd.out, paste0("catch_yr_ctry_2014_",dtyr,"_nhke_4_wgmixfish.csv"))


#==============================================================================
# DATA                                                                     ----
#==============================================================================

load(table1.R)

ny <- dtyr - 2013 + 1


#==============================================================================
# CATCH INFO                                                               ----
#==============================================================================

# catch_a3_yr_ctry <- tab1 %>% group_by(a3, year, category, country) %>% summarise(caton = sum(caton))
# 
# catch_a3_yr_ctry_bob <- catch_a3_yr_ctry %>% filter(a3 == "8")
# catch_a3_yr_ctry_out <- catch_a3_yr_ctry %>% filter(a3 != "8")

catch_yr_ctry_bob <- tab1 %>% filter(year >= 2013) %>%  # 2013 excluded
  mutate(area = ifelse(a3 != "8", "oth", a3), stock = "HKE") %>% 
  group_by(stock, year, category, country, area) %>% summarise(tons = sum(caton))


#==============================================================================
# SAVE                                                                     ----
#==============================================================================

write.csv(catch_yr_ctry_bob, file = out.file, row.names = F)


#==============================================================================
# OTHER                                                                     ----
#==============================================================================

# catch_yr_ctry_area <- tab1 %>% filter(year >= 2005, category == "") %>%
#   group_by(year, category, country, area) %>% summarise(tons = sum(caton))
# 
# write.csv(catch_yr_ctry_area, 
#           file = file.path( wd.out, paste0("catch_yr_ctry_area_2005_",dtyr,"_nhke.csv")), row.names = F)


