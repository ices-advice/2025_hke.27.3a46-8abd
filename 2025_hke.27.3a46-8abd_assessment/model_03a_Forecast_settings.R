################################################################################
#  hke.27.3a46-8abd_assessment : short-term forecast settings                  #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model_02a_Forecast_settings.R - STF general settings
# ~/*_hke.27.3a46-8abd_assessment/model_02a_Forecast_settings.R

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

require(r4ss)
require(dplyr)


#==============================================================================
# DIRECTORIES                                                              ----
#==============================================================================

# mod_path <- file.path(getwd(), run)
# stf_path <- file.path("model","stf")

dir.forecastTAC <- file.path(stf_path,"stf_files")
dir.fmult       <- file.path(stf_path,"stf_files")
dir.fored       <- file.path(stf_path, "info_models")

dir.create(path = stf_path, showWarnings = TRUE, recursive = TRUE)
dir.create(path = dir.forecastTAC, showWarnings = TRUE, recursive = TRUE)
dir.create(path = dir.fored, showWarnings = TRUE, recursive = TRUE)


#==============================================================================
# SETTINGS                                                                 ----
#==============================================================================


## Values of F in the intermediate year ----------------------------------------

## As the average of the values in the Naver years.
Naver <- 3 # number of average years

## Sequence of Fmult for year_inter+1 (see variable fsq)

# Number of models in the Fmult sequence between:
# - (s1) 0 and FmsyLower, 
# - (s2) FmsyLower+0.01 and FmsyUpper, and 
# - (s3) FmsyUpper and Flim+0.05.
s1 <- 50
s2 <- 50
s3 <- 50

## year_inter+2 use the multiplier for Fmsy not the previous sequence


#==============================================================================
# 1) Create scenarios                                                      ----
#==============================================================================

file.copy(file.path(mod_path, "forecast.ss"),
          file.path(dir.forecastTAC, "forecast.ss"))

## read forecast from forecast folder
fore <- r4ss::SS_readforecast(file = file.path(dir.forecastTAC, "forecast.ss"),
                              verbose = FALSE)

## Look for values of apical F for intermediate year dtyr+1 (report:14)
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE) ## read

# int year ---------------------------------------------------------------------

## prepare intermediate year data 
dat <- replist$exploitation
# keep year, seas and fleets
dat <- dat %>% select(-Seas_dur, -F_std, -annual_F, -annual_M)
# head(dat) 

# Number of forecast years
Nfor <- fore$Nforecastyrs

## average of the last 3 years across seasons and fleets
startyear <- max(dat$Yr)-Nfor-Naver+1
endyear   <- max(dat$Yr)-Nfor
data_int <- dat %>% filter(Yr>=startyear & Yr<=endyear) %>% 
  select(-Yr) %>% group_by(Seas) %>% 
  summarise_all(mean)

## input intermediate year data
dimen <- dim(data_int)
Year  <- rep(endyear+1,dimen[1]*(dimen[2]-1))
fore_dat_int       <- data.frame(Year)
fore_dat_int$Seas  <- data_int$Seas
fore_dat_int$Fleet <- rep(1:nfleet, each = length(data_int$Seas))
fore_dat_int$F     <- as.vector(as.matrix(data_int[,-which(names(data_int)=="Seas")]))

# define Fmult ---------------------------------------------------------------- 

# From reference points html
datmul <- replist$exploitation
datmul <- datmul %>% filter((Yr >= year_inter-Naver) & (Yr <= year_inter-1)) %>% 
  filter(!is.na(F_std)) %>% # whole year F stored in 1st season
  select(Yr, F_std)

# Mean of all Naver years
fsq <- mean(datmul$F_std)

# Fishing mortality multipliers
fmult_msyl <- FmsyLower/fsq
fmult_msyu <- FmsyUpper/fsq
fmult_lim  <- Flim/fsq
fmult_msy  <- Fmsy/fsq

fmult <- c( seq(0, fmult_msyl, length.out=s1), 
            seq(fmult_msyl+0.01, fmult_msyu, length.out=s2), 
            seq(fmult_msyu+0.01, fmult_lim+0.05, length.out=s3), 
            c(3, 4, 5, 6, 7, 1))
fmult <- sort(unique(fmult))
# # additional given results
# s4 <- 12; s5 <- 22
# fmult2 <- c( seq(0.696,0.749,length.out=s4)[2:(s4-1)], seq(0.756,0.864,length.out=s4)[2:(s4-1)], # around Fmsylow
#              seq(1.001,1.081,length.out=s4)[2:(s4-1)], seq(1.160,1.242,length.out=s4)[2:(s4-1)], # around Fmsy
#              seq(1.806,1.886,length.out=s4)[2:(s4-1)], seq(2.709,2.844,length.out=s4)[2:(s4-1)], # around Fmsyupp
#              seq(3,4,length.out=s5)[2:(s5-1)], seq(5,6,length.out=s5)[2:(s5-1)], 1)
# 
# fmult <- sort(c(fmult, fmult2))
# # fmult <- fmult2 #! PROVISIONAL: only running latest

Fmult_names <- paste0("Fmult_",fmult)

save(fmult, Fmult_names, file = file.path(dir.fored, "fmult.RData"))

## create data for following forecast years using int year and Fmult
for (i in 1:length(fmult)) {
  fore_dat <- aux_fore <- fore_dat_int
  # Forecast year (alternative Fs)
  for(j in 2:(Nfor-1)){
    aux_fore$Year <- endyear+j
    aux_fore$F    <- fmult[i]*fore_dat_int$F
    fore_dat <- rbind(fore_dat, aux_fore)
  }
  # Last year Fmsy
  j <- Nfor
  aux_fore$Year <- endyear+j
  aux_fore$F    <- fmult_msy*fore_dat_int$F
  fore_dat <- rbind(fore_dat, aux_fore)
  
  # input ------------------------------------------------------------------------
  
  fore$InputBasis <- 99 # options: 99 for F, 2 for Catch
  fore$ForeCatch  <- fore_dat # input ForeCatch(orF) data
  
  ## write all forecast files/scenarios
  r4ss::SS_writeforecast(fore, dir = dir.forecastTAC, file = paste0("forecast_",Fmult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}

