################################################################################
#  hke.27.3a46-8abd_assessment : short-term forecast summary                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model_02c_Forecast_summary.R - STF output summaries
# ~/*_hke.27.3a46-8abd_assessment/model_02c_Forecast_summary.R

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
# DIRECTORIES                                                              ----
#==============================================================================

# mod_path <- file.path(getwd(), run)
# stf_path <- file.path("model","stf")

# Catches, recruitment, F, SSB
dir.fored <- file.path(stf_path, "info_models")

## Retros for directories
dir.tablefore <- file.path(stf_path, "table")
dir.create(dir.tablefore)

dir.tableadv  <- file.path("model", "sum_advice")
if (!dir.exists(dir.tableadv)) dir.create(dir.tableadv)
# plotdir_fore <- file.path(stf_path, "plot")
# dir.create(plotdir_fore)


# # REQUIRED: from previous files
# load(file.path(dir.fored, "fmult.RData")) # fmult, Fmult_names


#==============================================================================
# FORECAST TABLES                                                          ----
#==============================================================================

# forecastModels <- SSgetoutput(dirvec=file.path(stf_path, Fmult_names))
# names(forecastModels) <- Fmult_names
# 
# save(forecastModels, file=file.path(dir.fored, "forecast.RData"))

load(file.path(dir.fored, "forecast.RData")) # forecastModels
load(file.path(dir.fored, "fmult.RData"))    # fmult, Fmult_names

forecastSummary <- SSsummarize(forecastModels)


# TABLES  -----

Table_Inter <- setNames(data.frame(matrix(0,ncol=7,nrow=1)), 
                        c(paste0("SSB",year_inter+1), 
                          paste0("F",year_inter), 
                          paste0("Rec",year_inter), paste0("Rec",year_inter+1),
                          paste0("Catches",year_inter), 
                          paste0("Landings",year_inter), paste0("Discards",year_inter)))

Table_fmult <- setNames(data.frame(matrix(0,ncol=7,nrow=length(Fmult_names))), 
                        c("Fmult", 
                          paste0("SSB",year_inter+2),
                          paste0("F",year_inter+1),
                          paste0("Catches",year_inter+1),
                          paste0("Landings",year_inter+1),
                          paste0("Discards",year_inter+1),
                          paste0("Catches",year_inter+2)))

Table_fmult$Fmult <- round(fmult,4)


# SSB -------------------------------------------------------------------------

SSB   <- as.data.frame(forecastSummary[["SpawnBio"]])
SSBiy <- SSB %>% filter(Yr == year_inter+1) %>% .[1]
SSBly <- SSB %>% filter(Yr == year_inter+2) %>% select(-Label, -Yr) %>% unlist()

Table_Inter[,paste0("SSB",year_inter+1)] <- SSBiy
Table_fmult[,paste0("SSB",year_inter+2)] <- SSBly

# probablity below Blim

SSBly.sd <- forecastSummary[["SpawnBioSD"]] %>% filter(Yr == year_inter+2) %>% select(-Label, -Yr) %>% unlist()
if (exists("sc.prob"))
  SSBly.sd[-sc.prob] <- NA

# # - normal distribution
# 
# Table_fmult[,paste0("pSSB",year_inter+2,"_Blim_norm")]  <- pnorm(Blim, mean = SSBly, sd = SSBly.sd)

# - lognormal distribution

logSSBly.mu    <- log(SSBly^2 / sqrt(SSBly^2 + SSBly.sd^2))
logSSBly.sigma <- sqrt(log(1 + SSBly.sd^2/SSBly^2))

Table_fmult[,paste0("pBlim_",year_inter+2)] <- plnorm(Blim, mean = logSSBly.mu, sd = logSSBly.sigma)


# F ----------------------------------------------------------------------------
# Note that F is from intermediate year

Fvalue <- as.data.frame(forecastSummary[["Fvalue"]])

Table_Inter[,paste0("F",year_inter)]   <- Fvalue %>% filter(Yr == year_inter) %>% .[1]
Table_fmult[,paste0("F",year_inter+1)] <- Fvalue %>% filter(Yr == year_inter+1) %>% select(-Label, -Yr) %>% unlist()

# Flim es distinto a Fcrash, mirar defs.
# Flim es F que da en equilibrio un 50% prob de que SSB este por encima de Blim


# Rec -------------------------------------------------------------------------
# Note constant recruitment!

Recr <- as.data.frame(forecastSummary[["recruits"]])

Table_Inter[,paste0("Rec",year_inter)]   <- Recr %>% filter(Yr == year_inter) %>% .[1]
Table_Inter[,paste0("Rec",year_inter+1)] <- Recr %>% filter(Yr == year_inter+1) %>% .[1]


# Catches ----------------------------------------------------------------------

for (i in 1:length(Fmult_names)){
  
  output <- forecastModels[[i]]
  
  fltnms <- setNames(output$definitions$Fleet_name, output$fleet_ID)
  
  ## Catch
  
  catch <- as_tibble(output$timeseries) %>% filter(Era == "FORE") %>% 
    select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) 
  names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:nfleet], sep = "_"), paste('LanEst', fltnms[1:nfleet], sep = "_"),
                    paste('CatEst', fltnms[1:nfleet], sep = "_"))
  
  aux1 <- (catch %>% select(starts_with('CatEst'))) - (catch %>% select(starts_with('LanEst')))
  names(aux1) <- paste('DisEst', fltnms[1:nfleet], sep = "_")
  
  catch <- catch %>% bind_cols(aux1)
  catch <- catch %>%  
    tidyr::pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
    mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
    select('year', 'season', 'fleet', 'indicator', 'value') %>% 
    mutate(year = as.factor(year))
  
  Landings <- catch %>% filter(indicator=="LanEst") %>% 
    group_by(year) %>% summarise(land=sum(value))
  
  Discards <- catch %>% filter(indicator=="DisEst") %>% 
    group_by(year) %>% summarise(disc=sum(value))
  
  Cat <- catch %>% filter(indicator=="CatEst") %>% 
    group_by(year) %>% summarise(cat=sum(value))
  
  Table_Inter[,paste0("Catches",year_inter)]  <- Cat %>% filter(year == year_inter) %>% .$cat
  Table_Inter[,paste0("Landings",year_inter)] <- Landings %>% filter(year == year_inter) %>% .$land
  Table_Inter[,paste0("Discards",year_inter)] <- Discards %>% filter(year == year_inter) %>% .$disc

  Table_fmult[i,paste0("Catches",year_inter+1)]  <- Cat %>% filter(year == year_inter+1) %>% .$cat
  Table_fmult[i,paste0("Landings",year_inter+1)] <- Landings %>% filter(year == year_inter+1) %>% .$land
  Table_fmult[i,paste0("Discards",year_inter+1)] <- Discards %>% filter(year == year_inter+1) %>% .$disc
  
  Table_fmult[i,paste0("Catches",year_inter+2)]  <- Cat %>% filter(year == year_inter+2) %>% .$cat
  
}


#==============================================================================
# SAVE                                                                     ----
#==============================================================================

Table_InterR <- Table_Inter %>% 
  mutate(across(starts_with("F"), ~ as.numeric(icesRound(.x))), 
         across(!starts_with("F"), ~ round(.x)))

# Table_fmultR <- Table_fmult %>%
#   mutate(across(starts_with("F") & !starts_with("Fmult"), ~ as.numeric(icesRound(.x))),
#          across(!starts_with("F"), ~ round(.x)))

write.csv(Table_Inter, file.path(dir.tablefore, "table_intermediate_year.csv"), row.names = FALSE)
write.csv(Table_InterR, file.path(dir.tableadv, "table_intermediate_year_icesRound.csv"), row.names = FALSE)
write.csv(Table_fmult, file.path(dir.tablefore, "table_Fmult.csv"), row.names = FALSE)
# write.csv(Table_fmultR, file.path(tabledir_xxx, "table_Fmult_icesRound.csv"), row.names = FALSE)

