################################################################################
#  hke.27.3a46-8abd_assessment : model code                                    #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model.R - model-related code
# ~/*_hke.27.3a46-8abd_assessment/model.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


library(icesTAF)

mkdir("model")


#==============================================================================
# ASSESSMENT FILES                                                         ----
#==============================================================================

dtyr       <- 2024
ass.yr <- as.numeric(substr(dtyr,3,4)) + 1

ctlp_file <- paste0("nhake-wg",ass.yr-1,".ctl")
datp_file <- paste0("nhake-wg",ass.yr-1,".dat")

ctrl_file    <- paste0("nhake-wg",ass.yr,".ctl")
data_file    <- paste0("nhake-wg",ass.yr,".dat")


#==============================================================================
# ASSESSMENT                                                               ----
#==============================================================================


## - MODEL RUNS

if (cluster == TRUE) {
  
  
  #==============================================================================
  # CLUSTER                                                                  ----
  #==============================================================================
  
  ## Copy files for CLUSTER runs
  
  source('model_00a_cluster_in.R')
  
  
  ## Cluster runs (see ./cluster/cluster.txt)
  
  
  ## Copy files from CLUSTER run outputs
  
  finalrun <- "wg25_def" # saly : same as last year with lower loglik (finally taken first run, as in complete agreement)
  finalnam <- paste0("wg",ass.yr)
  
  source('model_00b_cluster_out.R')
  
} else {
  
  ## Model runs (without cluster)
  source('model_01_Runs.R')
  
}

## - RETROS

## Retrospective patterns
source("model_02_Retros.R")


#==============================================================================
# SHORT-TERM FORECAST                                                      ----
#==============================================================================

# load packages
library(r4ss)
library(dplyr)
library(parallel)
library(doParallel)
library(icesAdvice)


## Select run -----------------------------------------------------------------

run <- 'model/final' # model folder

## RP --------------------------------------------------------------------------

Blim      <- 61563
Bpa       <- 78405
Flim      <- 0.734	
Fpa       <- 0.537
Fmsy      <- 0.243
FmsyLower <- 0.147
FmsyUpper <- 0.370	
Bmsy      <- 78405

nfleet <- 9

## Fix intermediate year --------------------------------------------------------

year_inter <- dtyr + 1

tacadv <- read.csv( file.path(taf.boot.path("data"),"advice_tac.csv"))

TAC         <- tacadv[tacadv$year == year_inter, "tac"]    # intermediate year TAC
TACadvice   <- tacadv[tacadv$year == year_inter, "advice"] # intermediate year advice


# directories
mod_path   <- file.path(getwd(), run)
stf_path   <- file.path("model","stf")
stfGM_path <- file.path("model","stfGM")


## STF runs
source('model_03a_Forecast_settings.R')
source('model_03b_Forecast_runs.R') # to run in cluster all values see ss_01c_w24_runs_stf.R
source('model_03c_Forecast_summary.R')
source('model_03d_Forecast_interpolation.R')

## STF runs with geometric mean recruitment
source('model_04a_ForecastGM_settings.R')
source('model_04b_ForecastGM_runs.R')
source('model_04c_ForecastGM_summary.R')
source('model_04d_ForecastGM_interpolation.R')

