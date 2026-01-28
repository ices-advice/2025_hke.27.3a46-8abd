################################################################################
#  hke.27.3a46-8abd_assessment : prepare cluster information                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  28/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model_00a_cluster_in.R - Copy input data to cluster folder
# ~/*_hke.27.3a46-8abd_assessment/model_00a_cluster_in.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3



mkdir("cluster/logs")

library(icesTAF)
library(r4ss)

tafsoft_path <- taf.boot.path("software")


#==============================================================================
# Functions                                                                ----
#==============================================================================

mkdir("cluster/fun")

fun_file <- "retro_function.R"

file.copy(file.path(taf.boot.path("software"),fun_file), 
          file.path("cluster","fun",fun_file), overwrite = TRUE)


#==============================================================================
# WGBIE previous assessment                                                ----
#==============================================================================


# directory

mkdir("cluster/last_final")

# input files

# - saly : same as last year

file.copy(file.path(taf.boot.path("data"),"ss3_previous",datp_file),     file.path("cluster","last_final",datp_file), overwrite = TRUE)
file.copy(file.path(taf.boot.path("data"),"ss3_previous",ctlp_file),     file.path("cluster","last_final",ctlp_file), overwrite = TRUE)
file.copy(file.path(taf.boot.path("data"),"ss3_previous","starter.ss"),  file.path("cluster","last_final","starter.ss"), overwrite = TRUE)
file.copy(file.path(taf.boot.path("data"),"ss3_previous","forecast.ss"), file.path("cluster","last_final","forecast.ss"), overwrite = TRUE)
file.copy(file.path(tafsoft_path,"ss"), file.path("cluster","last_final","ss"), overwrite = TRUE)

if (file.exists(file.path(taf.boot.path("data"),"ss3_previous","ss.par")))
  file.copy(file.path(taf.boot.path("data"),"ss3_previous","ss.par"), file.path("cluster","last_final","ss.par"), overwrite = TRUE)


#==============================================================================
# WGBIE: same settings as last year                                        ----
#==============================================================================

# directory

bcrun     <- "saly"
bcrun.dir <- file.path("cluster", bcrun)

mkdir(bcrun.dir)

# input files

# - saly : same as last year

file.copy(file.path("data","ss3_saly",data_file),     file.path("cluster","saly",data_file), overwrite = TRUE)
file.copy(file.path("data","ss3_saly",ctrl_file),     file.path("cluster","saly",ctrl_file), overwrite = TRUE)
file.copy(file.path("data","ss3_saly","starter.ss"),  file.path("cluster","saly","starter.ss"), overwrite = TRUE)
file.copy(file.path("data","ss3_saly","forecast.ss"), file.path("cluster","saly","forecast.ss"), overwrite = TRUE)
file.copy(file.path(tafsoft_path,"ss"), file.path("cluster","saly","ss"), overwrite = TRUE)


# #==============================================================================
# # WGBIE: take one specific jitter with its parameter estimates             ----
# #==============================================================================
# 
# # directory
# 
# for (it in c(38,21,23,48)) { # it <- 38
#   
#   run     <- paste0("saly_jit",it)
#   run.dir <- file.path("cluster", run)
#   
#   mkdir(run.dir)
#   
#   jit.wd  <- paste0("jitter_", ceiling(it/2))
#   run.ref <- file.path(bcrun.dir, "jitter", jit.wd)
#   
#   # option 2 is used
#   file.copy(file.path("data","ss3_saly",data_file),     file.path(run.dir,data_file), overwrite = TRUE)
#   file.copy(file.path("data","ss3_saly",ctrl_file),     file.path(run.dir,ctrl_file), overwrite = TRUE)
#   file.copy(file.path("data","ss3_saly","starter.ss"),  file.path(run.dir,"starter.ss"), overwrite = TRUE)
#   file.copy(file.path("data","ss3_saly","forecast.ss"), file.path(run.dir,"forecast.ss"), overwrite = TRUE)
#   file.copy(file.path(run.ref,paste0("ss.par_",it,".sso")), file.path(run.dir,"ss.par"), overwrite = TRUE)
#   file.copy(file.path(tafsoft_path,"ss"), file.path(run.dir,"ss"), overwrite = TRUE)
#   
#   # use ss.par from jitter and modify starter.ss (0 to 1 for init_values_src)
#   starter <- SS_readstarter(file.path(run.dir,"starter.ss"))
#   starter[["init_values_src"]] <- 1 # not estimate
#   SS_writestarter(starter, dir = run.dir, verbose = FALSE, overwrite = TRUE)
#   
# }





# #==============================================================================
# # WGBIE: alternative runs for improving retros                             ----
# #==============================================================================
# 
# 
# # saly_selIAMSfix : same settings as last year + ss.par (from saly)
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # directory
# 
# run     <- "saly_selIAMSfix"
# run.dir <- file.path("cluster", run)
# 
# mkdir(run.dir)
# 
# run.ref <- ifelse('hess' %in% dir(bcrun.dir), 'hess', 'nohess')
# 
# # option 1: use ss.par and modify starter.ss (0 to 1 en #_init_values_srcxxx)
# # option 2: take controlss.new and rename it to *.ctrl + set phase of specific parameter to negative (not to estimate it)
# 
# # option 2 is used
# file.copy(file.path("data","ss3_saly",data_file),       file.path(run.dir,data_file), overwrite = TRUE)
# file.copy(file.path("cluster","saly",run.ref,"control.ss_new"), file.path(run.dir,ctrl_file), overwrite = TRUE)
# file.copy(file.path("data","ss3_saly","starter.ss"),    file.path(run.dir,"starter.ss"), overwrite = TRUE)
# file.copy(file.path("data","ss3_saly","forecast.ss"),   file.path(run.dir,"forecast.ss"), overwrite = TRUE)
# file.copy(file.path(tafsoft_path,"ss"), file.path(run.dir,"ss"), overwrite = TRUE)
# 
# 
# # set phase to negative for Size_DblN_peak_IAMS(17)
# 
# ctlfile <- file.path(run.dir,ctrl_file)
# 
# # ctl <- r4ss::SS_readctl(file = ctlfile)
# 
# ctrl <- readLines(ctlfile)
# ctrl_new <- ctrl
# 
# ctrl_line_par <- which(grepl("Size_DblN_peak_IAMS", ctrl))
# 
# ctrl_new[[ctrl_line_par]] <- gsub(" 2 ", "-2 ", ctrl[[ctrl_line_par]])
# 
# #write the new control file, it overwrites the old one
# writeLines(ctrl_new, con=ctlfile)
# 
# 
# 
# # saly_selIAMSrw : same settings as last year + ss.par (from saly_jit38)
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # directory
# 
# run     <- "saly_selIAMSrw"
# run.dir <- file.path("cluster", run)
# 
# mkdir(run.dir)
# 
# # copy SS inputs and model
# copy_SS_inputs(dir.old = file.path("data","ss3_saly"), dir.new = run.dir, create.dir = TRUE,
#                overwrite = TRUE, copy_par = FALSE, verbose = TRUE)
# file.copy(file.path(tafsoft_path,"ss"), file.path(run.dir,"ss"), overwrite = TRUE)
# 
# # Edit CTL file to use a random walk for:
# # - Size_DblN_peak_IAMS(17)
# # - Size_DblN_end_logit_IAMS(17)
# 
# ctlfile <- file.path(run.dir,ctrl_file)
# 
# # ctl <- r4ss::SS_readctl(file = ctlfile)
# 
# ctrl <- readLines(ctlfile)
# ctrl_new <- ctrl
# 
# # use_dev = 23 (for random walk) + year range (2016-dtyr)
# ctrl_new[[which(grepl("Size_DblN_peak_IAMS", ctrl))]] <- 
#   #      "             4            70        52.909            15          0.01             0          2          0          0          0          0          0          0          0  #  Size_DblN_peak_IAMS(17)"
#   paste0("             4            70        52.909            15          0.01             0          2          0         23       2016       ",dtyr,"          0          0          0  #  Size_DblN_peak_IAMS(17)")
# 
# ctrl_new[[which(grepl("Size_DblN_top_logit_IAMS", ctrl))]] <- 
#   #                  -16             2      -7.00004            -2          0.01             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_IAMS(17
#   paste0("           -16             2      -7.00004            -2          0.01             0          2          0         23       2016       ",dtyr,"          0          0          0  #  Size_DblN_top_logit_IAMS(17)")
# 
# # ctrl_new[[which(grepl("Size_DblN_end_logit_IAMS", ctrl))]] <- 
# #   #      "          -999          -999          -999          -999             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_IAMS(17)"
# #   paste0("          -999          -999          -999          -999             0             0         -2          0         23       2016       ",dtyr,"          0          0          0  #  Size_DblN_end_logit_IAMS(17)")
# 
# # define deviations (se & autocorr)
# ctrl_line_devselex_last <- which(grepl("# info on dev vectors created for selex parms", ctrl))
# ctrl_line_end           <- length(ctrl)
# 
# ctrl_new[ctrl_line_devselex_last + 0:3] <- 
#   c("        0.0001             2           1.5             1           0.5             6      -5  # Size_DblN_peak_IAMS(17)_dev_se", 
#     "         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_IAMS(17)_dev_autocorr", 
#     "        0.0001             2           1.5             1           0.5             6      -5  # Size_DblN_top_IAMS(17)_dev_se", 
#     "         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_top_IAMS(17)_dev_autocorr"
#     # "        0.0001             2           1.5             1           0.5             6      -5  # Size_DblN_end_logit_IAMS(17)_dev_se", 
#     # "         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_end_logit_IAMS(17)_dev_autocorr"
#     )
# 
# ctrl_new[ctrl_line_devselex_last:ctrl_line_end + 4] <- ctrl[ctrl_line_devselex_last:ctrl_line_end]
# 
# #write the new control file, it overwrites the old one
# writeLines(ctrl_new, con=ctlfile)
# 
# 
# #==============================================================================
# # XXXX                                                                     ----
# #==============================================================================

