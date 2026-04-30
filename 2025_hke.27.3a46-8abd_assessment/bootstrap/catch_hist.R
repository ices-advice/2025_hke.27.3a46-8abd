################################################################################
#  hke.27.3a46-8abd_assessment : historical catches                            #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# data_catch_hist.R - Load historical catch information
# ~/*_hke.27.3a46-8abd_assessment/bootstrap/data_catch_hist.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


# setwd("./bootstrap/data/catch_hist") # for testing

#==============================================================================
# DATA                                                                     ----
#==============================================================================

dtyr <- 2024
yy   <- substr(dtyr,3,4)

# # lastdat_wd <- "https://github.com/ices-taf/2024_hke.27.3a46-8abd_assessment/blob/main/bootstrap/data"
# lastdat_wd <- paste0("c:/use/GitHub/ICES_taf/",dtyr,"_hke.27.3a46-8abd_assessment/data/catch")
lastdat_wd <- file.path("..", "..", "initial", "ass_prev")

dat_files <- c("Table_1.RData", "Table_2.RData")#, "hist_catch_by_area.csv")

for (filen in dat_files)
  # download(file.path(lastdat_wd, filen), dir = file.path("..", filen)) #! NEEDS TOKEN??
  file.copy(file.path(lastdat_wd, filen), file.path("..", filen), copy.date = TRUE)

# # next year will be fixed
# lastdat_wd <- paste0("c:/use/GitHub/ICES_taf/",dtyr,"_hke.27.3a46-8abd_assessment/bootstrap/data")
file.copy(file.path(lastdat_wd, "hist_catch_by_area.csv"), 
          file.path("..", "hist_catch_by_area.csv"), copy.date = TRUE)


#==============================================================================
# REMOVE                                                                   ----
#==============================================================================

rm(lastdat_wd, dat_files, filen)

