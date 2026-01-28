################################################################################
#  hke.27.3a46-8abd_assessment : retrospective patterns                        #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  03/05/2022                                                       #
#   modified:                                                                  #
################################################################################

# model_02_Retros.R - model-related code
# ~/*_hke.27.3a46-8abd_assessment/model_02_Retros.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

retros_wd <- file.path("model","retros") 
out_wd    <- file.path("model","output")


dir.create(retros_wd)


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

library(r4ss)


#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

finalrun <- 'final'


# Outputs

load(file.path(out_wd, 'SS3_final_output.RData'))


# Select those of interest

scsel <- c( 'hess', grep('0', grep('retro', names(output), value = TRUE), invert = TRUE, value = TRUE))
retro_out <- output[scsel]

names(retro_out) <- c("upd", "upd_retro-1", "upd_retro-2", "upd_retro-3", "upd_retro-4", "upd_retro-5")


# Save object

save(retro_out, file = file.path("model","retros",'retros_RD.RData'))

