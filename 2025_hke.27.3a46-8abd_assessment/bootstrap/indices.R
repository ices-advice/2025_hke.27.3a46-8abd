################################################################################
#  hke.27.3a46-8abd_assessment : survey indices                                #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# catch_indices.R - Copy updated survey indices
# ~/*_hke.27.3a46-8abd_assessment/bootstrap/catch_indices.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
# DATA                                                                     ----
#==============================================================================

inp_wd <- file.path("..","..","initial/data/indices")

for (filen in dir(inp_wd))
    file.copy(file.path(inp_wd, filen), file.path(".",filen), copy.date = TRUE)  


#==============================================================================
# REMOVE                                                                   ----
#==============================================================================

rm(inp_wd, filen)

