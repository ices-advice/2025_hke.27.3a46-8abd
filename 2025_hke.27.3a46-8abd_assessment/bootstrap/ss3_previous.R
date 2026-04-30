################################################################################
#  hke.27.3a46-8abd_assessment : assessment files                              #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# ss3_previous.R - Load previous assessment files
# ~/*_hke.27.3a46-8abd_assessment/bootstrap/ss3_previous.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


# setwd("./bootstrap/data/ss3_previous") # for testing

#==============================================================================
# DATA                                                                     ----
#==============================================================================

dtyr <- 2024
yy   <- substr(dtyr,3,4)

# # lastass_wd <- "https://github.com/ices-taf/2023_hke.27.3a46-8abd_assessment/blob/main/model/final"
# lastass_wd <- paste0("c:/use/GitHub/ICES_taf/",dtyr,"_hke.27.3a46-8abd_assessment/model/final")
lastdat_wd <- file.path("..", "..", "initial", "ass_prev", "ss3_previous")

ass_files <- c(paste0("nhake-wg",yy,".dat"), paste0("nhake-wg",yy,".ctl"), "starter.ss", "forecast.ss", "ss.par")

for (filen in ass_files)
  # download(file.path(lastass_wd, filen), dir = file.path(".", filen)) #! NEEDS TOKEN??
  file.copy(file.path(lastdat_wd, filen), file.path(".", filen), copy.date = TRUE)


#==============================================================================
# REMOVE                                                                   ----
#==============================================================================

rm(lastdat_wd, ass_files, filen)

