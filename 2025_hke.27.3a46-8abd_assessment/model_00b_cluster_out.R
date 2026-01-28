################################################################################
#  hke.27.3a46-8abd_assessment : copy cluster final run                        #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  02/05/2023                                                       #
#   modified:                                                                  #
################################################################################

# model_00b_cluster_out.R - Copy input data to cluster folder
# ~/*_hke.27.3a46-8abd_assessment/model_00b_cluster_out.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


dir.create(file.path("model","final"))
dir.create(file.path("model","output"))


#==============================================================================
# WGBIE23 final assessment                                                 ----
#==============================================================================

# - saly : same as last year

from_wd <- file.path("cluster",finalrun)
to_wd   <- file.path("model","final")
out0_wd <- file.path("cluster","output")
out_wd  <- file.path("model","output")


run <- ifelse('hess' %in% dir(from_wd), 'hess', 'nohess')


# Assessment files

data.file    <- dir(file.path(from_wd, run), pattern = '.dat')
control.file <- dir(file.path(from_wd, run), pattern = '.ctl')

files <- c(data.file, control.file, "starter.ss", "forecast.ss", "ss.par", "Report.sso", "CompReport.sso")             

for (fl in files)
  file.copy(file.path(from_wd,run,fl),  file.path(to_wd, fl), overwrite = TRUE)


# Output summary files

file.copy( file.path(out0_wd, paste0('SS3_',finalrun,'_output.RData')), 
           file.path(out_wd, 'SS3_final_output.RData'), overwrite = TRUE)

