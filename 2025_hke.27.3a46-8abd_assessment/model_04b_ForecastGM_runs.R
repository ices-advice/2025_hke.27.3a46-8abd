################################################################################
#  hke.27.3a46-8abd_assessment : short-term forecast GM runs                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model_02b_Forecast_runs.R - STF runs (with geometric mean recruitment)
# ~/*_hke.27.3a46-8abd_assessment/model_02b_Forecast_runs.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

# Code based on previous works by: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#                                                                                                        
#   Authors:                                       #                                                                     
#   Francesco Masnadi (CNR-IRBIM & UNIBO, Ancona)  #                                                                   
#   Massimiliano Cardinale (SLU Aqua, Lysekil)     #
#   Christopher Griffiths (SLU Aqua, Lysekil)      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Modified 05/04/2022 : Marta Cousido, Francisco Izquierdo & Santiago Cervino
# Modified 10/05/2022 : Dorleta Garcia


#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

require(parallel)
require(doParallel)


#==============================================================================
# SETTINGS                                                                 ----
#==============================================================================

sessionInfo() # check for ss3diags_2.0.1, r4ss_1.43.0, kobe_2.2.0

## pre-register parallel function 
doParallel::registerDoParallel(2)

## set seed for consistency
set.seed(1234)


dir <- getwd() # paste0(getwd(), "/") 
# create subfolder arrays for naming
dir.fored <- file.path(stfGM_path, "info_models")


# REQUIRED: from previous files
# dir.forecastTAC <- file.path(stfGM_path,"stf_files")
# load(file.path(stfGM_path, "info_models", "fmult.RData")) # fmult, Fmult_names


#==============================================================================
# FORECAST runs                                                            ----
#==============================================================================

## FIRST generate forecast scenarios/files with script "prepare forecast.R"

tacs <- paste0("Fmult_",fmult) # TAC levels for forecast

# Note: we set different F levels. 
# As in case of fixing catches, we force each fleet catching an specific catch percentage.
# However, the fleets' catches depend on their selectivity. Consequently is better to set F values.


##
for(i in 1:length(run)){
# create forecast folder and subfolders (if the first time)
  # main stf directory
  dir.runN <- file.path(dir,run[i])
  dir.runN.new <- stfGM_path
  dir.create(path=dir.runN.new, showWarnings = TRUE, recursive = TRUE)
  # fmult specific directory
  for(j in 1:length(tacs)){
    dir.tacN <- file.path(dir.runN.new,tacs[j])
    dir.create(path=dir.tacN, showWarnings = T, recursive = T)
    # copy the SS base files in every TAC subfolder 
    file.copy(file.path(dir.runN, "starter.ss"),
              file.path(dir.tacN, "starter.ss"))
    file.copy(paste(dir.runN, ctrl_file, sep="/"),
              paste(dir.tacN, ctrl_file, sep="/"))
    file.copy(paste(dir.runN, data_file, sep="/"),
              paste(dir.tacN, data_file, sep="/"))	
    #file.copy(paste(dir.runN, "wtatage.ss", sep="/"),
    #          paste(dir.tacN, "wtatage.ss", sep="/"))
    file.copy(paste(dir.runN, "ss.par", sep="/"),
              paste(dir.tacN, "ss.par", sep="/"))
    
    # copy the right forecast file from the "forecast_TAC" folder
    file.copy(paste(dir.forecastTAC,  paste0("forecastGM_",tacs[j], ".ss") , sep="/"),
              paste(dir.tacN, "forecast.ss", sep="/"))
    # Edit "starter.ss" 
    starter <- SS_readstarter(file.path(dir.tacN, "starter.ss"), verbose = FALSE)
    # - use ss.par estimates (0=use init values in control file; 1=use ss.par)
    starter$init_values_src <- 1 # to use the parameters estimates 
    # - turn off estimation for parameters entering after this phase
    starter$last_estimation_phase <- 0
    # Range of years for SD report (set to projection period)
    starter$minyr_sdreport <- dtyr+1
    starter$maxyr_sdreport <- dtyr+3
    # Save file
    SS_writestarter(starter, dir = dir.tacN, verbose = FALSE, overwrite = TRUE)
  }
}

# run forecasts for each model
# mc.cores <- 1 # set the number of cores as Nmodels x Nscenarios

for(i in 1:length(run)){
  
  dir.runN.new <- stfGM_path
  
  # parallel::mclapply( file.path(paste0(dir.runN.new,"/",tacs)),
  #                     r4ss::run, extras = "-nohess", exe = "ss", skipfinished = FALSE,
  #                     mc.cores=mc.cores)
  
  for (sc in 1:length(tacs)) {
    
    print(tacs[sc])
    
    command <- "ss -nohess -maxfn 0 -phase 99" # To run SS using ss.par information
    
    setwd(file.path(dir.runN.new,tacs[sc]))
    system(paste('ss.exe', command), intern = TRUE, invisible = TRUE)
    
    setwd(dir)
    
  }
}


#==============================================================================
# FORECAST OUTPUTS                                                         ----
#==============================================================================

forecastModels <- SSgetoutput(dirvec=file.path(stfGM_path, Fmult_names))
names(forecastModels) <- Fmult_names

# save(forecastModels, file=file.path(dir.fored, "forecast.RData"))


#==============================================================================
# FMSY runs - rerun with Hessian                                           ----
#==============================================================================

sc.prob <- 1e+100

# if (length(run)>1) stop("Ammend conde for more than 1 run.")
# 
# Fvals <- lapply(forecastModels, 
#                 function(x) 
#                   x$derived_quants %>% filter(Label == paste0("F_",dtyr+2)) %>% .$Value
#                 ) %>% unlist()
# 
# # Fmsy
# iLow_fmsy <- max(which(Fvals < Fmsy))
# iUpp_fmsy <- iLow_fmsy + 1
# 
# # FmsyLower
# iLow_flow  <- max(which(Fvals < FmsyLower))
# iUpp_flow <- iLow_flow + 1
# 
# # FmsyUpper
# iLow_fupp <- max(which(Fvals < FmsyUpper))
# iUpp_fupp<- iLow_fupp + 1
# 
# # Fpa
# iLow_fpa  <- max(which(Fvals < Fpa))
# iUpp_fpa <- iLow_fpa + 1
# 
# # Flim
# iLow_flim <- max(which(Fvals < Flim))
# iUpp_flim <- iLow_flim + 1
# 
# sc.prob <- c(1, iLow_fmsy, iUpp_fmsy, iLow_flow, iUpp_flow, iLow_fupp, iUpp_fupp, 
#              iLow_fpa, iUpp_fpa, iLow_flim, iUpp_flim)
# names(sc.prob) <- tacs[sc.prob]
# 
# 
# for (sc in sc.prob) {
#   
#   print(paste0("---- ", tacs[sc], " ----"))
#   
#   dir.tacN <- file.path(dir.runN.new,tacs[sc])
#   
#   # remove folder
#   unlink(dir.tacN, recursive = TRUE)
#   
#   # copy the SS base files in specific TAC subfolder
#   copy_SS_inputs(dir.old = dir.runN, dir.new = dir.tacN, create.dir = TRUE,
#                  overwrite = TRUE, copy_par = TRUE, verbose = TRUE)
#   
#   # Edit "starter.ss" 
#   starter <- SS_readstarter(file.path(dir.tacN, "starter.ss"), verbose = FALSE)
#   starter$last_estimation_phase <- 99
#   SS_writestarter(starter, dir = dir.tacN, verbose = FALSE, overwrite = TRUE)
#   # starter
#   
#   # Copy the fmult specific forecast
#   file.copy(paste(dir.forecastTAC,  paste0("forecast_",tacs[j], ".ss") , sep="/"),
#             paste(dir.tacN, "forecast.ss", sep="/"), overwrite = TRUE)
#   
#   # SS with Hessian (as SD estimates are required for estimating pBlim)
#   setwd(file.path(dir.runN.new,tacs[sc]))
#   
#   t1 <- Sys.time() #! REMOVE
#   
#   command <- "ss -maxfn 0 -phase 99" # To run SS using ss.par information
#   system(paste('ss.exe', command), intern = TRUE, invisible = FALSE)
#   
#   t2 <- Sys.time() #! REMOVE
#   
#   print(t2-t1) #! REMOVE
#   
#   setwd(dir)
#   
#   forecastModels[[sc]] <- SSgetoutput(dirvec = dir.tacN)[[1]]
#   
# }

save(forecastModels, sc.prob, file=file.path(dir.fored, "forecast.RData"))

