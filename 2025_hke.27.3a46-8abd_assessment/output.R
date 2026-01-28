#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### EXTRACT AND ANALYSE THE OUTPUT
#### CREATE ALL THE NECESSARY DATA TO:
####             * WRITE THE REPORT 
####             * WRITE THE ADVICE SHEET
####
#### The only piece that needs to be incorporated (todo 2022) is the YPR part - NOW NOT NECESSARY (SSM)
#### that is done using the old code in the following R script.
####
#### Dorleta Garcia (AZTI)
#### 2021/05/11
#  modified:  2022-05-05 14:16:43 (ssanchez@azti.es) - adapt given WKANGHAKE2022 (2 sexes + F from SS? not Camen's code)
#             2023-05-04 11:32:43 (ssanchez@azti.es) - update 2023
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

library(icesTAF)
library(r4ss)
library(dplyr)
library(ggplot2)
library(ggridges)
# library(tidyverse)
library(icesAdvice)

pivot_longer <- tidyr::pivot_longer


mkdir('output')

plot_wd <- file.path("output","plots")
rep_wd  <- file.path("output","report")
adv_wd  <- file.path("output","advice")

dir.create(plot_wd)
dir.create(rep_wd)
dir.create(adv_wd)

dtyr   <- 2024
ass.yr <- as.numeric(substr(dtyr,3,4))+1

# The output of SS will be taken from this directory:
data.file <- paste0("nhake-wg",ass.yr,".dat")
run <- 'final'
modelFit.dir <- file.path("model", run)
stf.dir <- file.path("model","stf")

# Reference points
Fmsy <- 0.243
Flow <- 0.147
Fupp <- 0.37
Fpa  <- 0.537
Bpa  <- 78405
Blim <- 61563

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### SETTINGS  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

run <- paste0("_", run)


# Options to read the SS files.
run.SS_output <- FALSE
forecast <- TRUE

# Options for STF
nyearsaveragesel     <- 3
scaleFbartofinalyear <- FALSE

### Input files ---
# The retro is conducted in a cluster and the results are extracted with SS
# there and stored in an R data files.
inp_retros <- file.path("model", 'retros', 'retros_RD.RData')
data.file  <- file.path(modelFit.dir, data.file)
stfiy.file <- file.path("model", "stf", "table", "table_intermediate_year.csv")
stfop.file <- file.path("model", "stf", "table", "table_Fmult.csv")
stfsq.file <- file.path("model", "stf", 'Fmult_1')
stfiy_GM.file <- file.path("model", "stfGM", "table", "table_intermediate_year.csv")
stfop_GM.file <- file.path("model", "stfGM", "table", "table_Fmult.csv")
stfsq_GM.file <- file.path("model", "stfGM", 'Fmult_1')

### Output Files ----
if(!dir.exists(file.path(modelFit.dir, 'post'))) dir.create(file.path(modelFit.dir, 'post'))
SS3_R.name    <- file.path(modelFit.dir, 'post', 'ss3_R_output.RData')
run.SS_output <- ifelse(file.exists(SS3_R.name), FALSE, TRUE) #! Delete file if changes has been made

# Some plots for the presentation.
pre_EstObs_LanDis      <- file.path(plot_wd, paste0('Pres_AssOut_EstObs_LandDisc', run, '.pdf'))
pre_EstObs_LFD_fleets  <- file.path(plot_wd, paste0('Pres_AssOut_EstObs_LFD_Fleets', run, '.pdf'))
pre_EstObs_LFD_surveys <- file.path(plot_wd, paste0('Pres_AssOut_EstObs_LFD_Surveys', run, '.pdf'))
pre_EstObs_surveys     <- file.path(plot_wd, paste0('Pres_AssOut_EstObs_Surveys', run, '.pdf'))
pre_sumPlot            <- file.path(plot_wd, paste0('Pres_Summary', run, '.png'))
pre_retroCI            <- file.path(plot_wd, paste0('Pres_retroCI', run, '.png'))
pre_retroCIrec         <- file.path(plot_wd, paste0('Pres_retroCIrec', run, '.png'))
pre_retroPeels         <- file.path(plot_wd, paste0('Pres_retroPeels', run, '.png'))
pre_retroSel           <- file.path(plot_wd, paste0('Pres_retroSel', run, '.png'))
pre_asssum             <- file.path(plot_wd, paste0('Pres_ASS_Summary', run, '.png'))

#  'output' folder.
out_selatlen <- file.path('output', paste0('sel_ret_atlen', run,'.RData'))
out_estCatch <- file.path('output', paste0('estimated_catc', run, '.RData'))
out_estLFD   <- file.path('output', paste0('estimated_LFD', run, '.RData'))
out_sumTab   <- file.path('output', paste0('summary_table', run, '.RData'))
out_retroInd       <- file.path('output', paste0('retro', run, '.RData'))
out_catchOptTab    <- file.path('output', paste0('catchoptiontable', run, '.RData'))
out_catchOptTab_GM <- file.path('output', paste('catchoptiontable_GM', run, '.RData'))

#  'report' folder.
rep_estCatch   <- file.path(rep_wd, paste0('estimated_catch', run, '.RData'))
rep_estLFD     <- file.path(rep_wd, paste0('estimated_LFD', run, '.RData'))
rep_estSurveys <- file.path(rep_wd, paste0('estimated_surveys', run, '.RData'))
rep_retroInd   <- file.path(rep_wd, paste0('retro', run, '.RData'))
rep_sumTab.csv <- file.path(rep_wd, paste0('summary_table', run, '.csv'))
rep_cathOptTab.csv <- file.path(rep_wd, paste0('catchoptiontable', run, '.csv'))
rep_cathOptTab_GM.csv <- file.path(rep_wd, paste0('catchoptiontable_GM',     run, '.csv'))
rep_YPR        <- file.path(rep_wd, paste0('YPR', run, '.RData'))
rep_YPRtable   <- file.path(rep_wd, paste0('YPRtable', run, '.csv'))

#  'advice' folder.
adv_sumTab.csv <- file.path(adv_wd, paste0('summary_table', run, '.csv'))
adv_advTab.csv <- file.path("model", "sum_advice", paste0('catchoptiontable',  run, '.csv'))
adv_advTab_GM.csv <- file.path("model", "sum_advice", paste0('catchoptiontableGM',  run, '.csv'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### EXTRACT THE INPUT & OUTPUT ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
years     <- 1978:dtyr
yearsfore <- 1978:(dtyr+3) 
nyears    <- length(years)

# r4ss function
if(run.SS_output){
  
  # output <- SS_output(dir = modelFit.dir, repfile = "Report.sso", compfile = "CompReport.sso",
  #                     covarfile = "covar.sso", ncols = 200, forecast = forecast, warn = TRUE, covar = TRUE, 
  #                     checkcor = TRUE, cormax = 0.95, cormin = 0.01, printhighcor = 10, printlowcor = 10, 
  #                     verbose = TRUE, printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE, aalmaxbinrange=0)
  
  output <- SS_output(dir = modelFit.dir, 
                      repfile = "Report.sso", compfile = "CompReport.sso", 
                      covarfile = "covar.sso", forefile = "forecast.ss", 
                      # ncols = 200, 
                      forecast = forecast, warn = TRUE, covar = TRUE, 
                      # checkcor = TRUE, cormax = 0.95, cormin = 0.01, printhighcor = 10, printlowcor = 10, 
                      verbose = TRUE, printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE, aalmaxbinrange=0)
  output$nforecastyears <- ifelse(is.na(output$nforecastyears), 0, output$nforecastyears)
  save(output, file = SS3_R.name)
  
  # # Plots with r4ss
  # SS_plots(output, uncertainty = FALSE, pdf = TRUE, datplot = TRUE, png = FALSE)
  
}
if(run.SS_output == FALSE) load(SS3_R.name)

# Info in matrices
source(file.path(taf.boot.path("software"), "SS_matrices.R"))
source(file.path(taf.boot.path("software"), "mohn_plot.R"))
source(file.path(taf.boot.path("software"), "summary_plot.R"))
source(file.path(taf.boot.path("software"), "replace_recruitment.R"))

fltnms  <- setNames(output$definitions$Fleet_name, output$fleet_ID)
fltfish <- which(output$IsFishFleet)           # commercial fleets
surv <- which(output$fleet_type == 3)          # surveys
survo <- surv[grepl("RESSGAS", fltnms[surv])]  # old surveys
survc <- surv[!grepl("RESSGAS", fltnms[surv])] # current surveys

ss3Dat <- SS_readdat_3.30(data.file)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Selectivity and retention at length by fleet  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

selretatlen <- output$sizeselex %>% 
  pivot_longer(cols = 6:ncol(output$sizeselex),  names_to = 'lng', values_to = 'value') %>%
  mutate(lng = as.numeric(lng), Yr = as.factor(Yr))


selatlen <- selretatlen %>% filter(Factor == "Lsel")
retatlen <- selretatlen %>% filter(Factor == "Ret")

save(selatlen, retatlen, file = out_selatlen)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Observed and Fitted landings/discards by fleet  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
catch <- as_tibble(output$timeseries) %>% filter(Era == 'TIME') %>% 
  select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) 
names(catch) <- c('year', 'season', paste('LanObs', fltnms[fltfish], sep = "_"), paste('LanEst', fltnms[fltfish], sep = "_"),
                  paste('CatEst', fltnms[fltfish], sep = "_"))
aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
names(aux1) <- paste('DisEst', fltnms[fltfish], sep = "_")
catch <- catch %>% bind_cols(aux1) 
catch <- catch %>% 
  pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
  mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
  select('year', 'season', 'fleet', 'indicator', 'value')  

discdat <- as_tibble(ss3Dat$discard_data) %>% filter(Yr > 0)
discdat <- discdat %>% 
  mutate(fleet =  fltnms[Flt], season = recode(Seas, `2.5` = 1, `5.5` = 2, `8.5` = 3, `11.5` = 4), 
         indicator = 'DisObs') %>% 
  select('Yr', 'season', 'fleet', 'indicator', 'Discard') %>% 
  rename(year = Yr, value = Discard)

catch <- catch %>% bind_rows(discdat) %>%
  mutate(category = ifelse(substr(indicator,1,3) == 'Lan', 'Landings', ifelse(substr(indicator,1,3) == 'Dis', 'Discards', 'Catch')),
         type = ifelse(substr(indicator,4,6) == 'Est', 'Estimated', 'Observed'),
         time = year + 1.125 - 1/season)  %>% 
  select('year', 'season', 'time', 'fleet', 'category', 'type', 'indicator', 'value')  

save(catch, file = rep_estCatch)
save(catch, file = out_estCatch)

# Total landings & Discards: Fitted vs Observed.
estLandDisc <- ggplot(catch %>% group_by(year, category, type, indicator) %>% filter(indicator != 'CatEst' & year > 0) %>% 
                        summarise(value = sum(value))) +
                 geom_line(aes(year, value, group = indicator, color = type), linewidth = 1) + 
                 geom_point(aes(year, value, group = indicator, color = type), size = 2) +
                 facet_grid(category~., scales = 'free') + scale_color_manual(values = 2:1)

# By Fleet: Landings and Discards: Fitted vs Observed.
estLand_fl <- ggplot(catch %>% filter(indicator != 'CatEst' & category == 'Landings')) +
               geom_line(aes(time, value, group = indicator, color = type), linewidth = 0.5) + 
               geom_point(aes(time, value, group = indicator, color = type), size = 1) +
               facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = 2:1) +
               ggtitle('Landings by fleet')

estDisc_fl <- ggplot(catch %>% filter(indicator != 'CatEst' & category == 'Discards' & fleet != 'LONGLINE')) +
                geom_line(aes(time, value, group = indicator, color = type), linewidth = 0.5) + 
                geom_point(aes(time, value, group = indicator, color = type), size = 1) +
                facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = 2:1) +
                ggtitle('Discards by fleet')

# pdf(pre_EstObs_LanDis, height = 6.5, width = 11)
#   print(estLandDisc)
#   print(estLand_fl)
#   print(estDisc_fl)
# dev.off()

png(file.path(plot_wd,"Pres_ASS_01_landDisc.png"), width = 800, height = 480)
  print(estLandDisc)
dev.off()

png(file.path(plot_wd,"Pres_ASS_02a_landFleet.png"), width = 800, height = 480)
  print(estLand_fl)
dev.off()

png(file.path(plot_wd,"Pres_ASS_02b_discFleet.png"), width = 800, height = 480)
  print(estDisc_fl)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Length Frequencies: Distribution & bubble plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
LFD    <- as_tibble(output$lendbase) %>% 
  select(Yr, Seas, Time, YrSeasName,  Fleet, Part, Sex, Bin, Obs, Exp, Pearson)
fltNms <- setNames(output$definitions[,'Fleet_name'], output$definitions[,1]) 
CaComp <- setNames(c('cat','dis','lan'), 0:2) 

LFD <- LFD %>% mutate(FleetNm = fltNms[Fleet], CatchComponent = CaComp[as.character(Part)], 
                      Sex = ifelse(Sex == 1, "F", "M")) %>% 
  pivot_longer(cols = c('Obs', 'Exp', 'Pearson'), names_to = 'variable', values_to = 'value')

save(LFD, file = rep_estLFD)
save(LFD, file = out_estLFD)

lfd_plots <- setNames(vector('list', 12), c(fltNms[fltfish], 'RESSGASQ', 'SURVEYS', 'SURVEYS_sex'))
# The fleets
for(f in fltfish){
  lfd_plots[[f]] <- list()
  for(cc in c('dis','lan')){
  
    aux <- LFD %>% filter(Fleet == f, CatchComponent == cc, variable %in% c('Obs', 'Exp'))
    
    if(dim(aux)[1] == 0) next 
  
    lfd_plots[[f]][[cc]][['lfd']] <- ggplot(aux, aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
                   geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + facet_wrap(~Seas, ncol = 4) + 
                   ggtitle(paste(fltNms[f], ifelse(cc == 'dis', 'Discards', 'Landings'), sep = " - "))
  
    lfd_plots[[f]][[cc]][['bubbles']] <- ggplot(LFD %>% filter(Fleet == 1, variable == "Pearson", CatchComponent == 'lan')) + 
      geom_point(aes(Time, Bin, size=abs(value),col= value<0),alpha=0.5,pch=16) +
    #  scale_size(range=c(0,3)) +
      scale_color_manual(values = c('blue','red')) +
   #   xlim(min(LFD$Yr),max(LFD$Yr)+1) + ylim(0,100) +
      ggtitle(paste(fltNms[f], ifelse(cc == 'dis', 'Discards', 'Landings'), sep = " - "))
    
    }
}
# RESGASQ
lfd_plots[["RESSGASQ"]][['lfd']] <- ggplot(LFD %>% filter(Fleet %in% survo, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
    geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + facet_wrap(~FleetNm, ncol = 4) + 
    ggtitle("RESSGASQ")

lfd_plots[["RESSGASQ"]][["bubbles"]] <- ggplot(LFD %>% filter(Fleet %in% survo, variable == "Pearson", CatchComponent == 'cat')) + 
     geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
     scale_color_manual(values = c('blue','red')) +
     ggtitle("RESSGASQ")

# EVHOE - IRIGFS (both sexes) + IRIAMS - PORCUPINE (by sex)
lfd_plots[["SURVEYS"]][['lfd']] <- ggplot(LFD %>% filter(Fleet %in% survc, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + facet_grid(Sex~FleetNm) + 
  ggtitle("SURVEYS")
lfd_plots[["SURVEYS"]][["bubbles"]] <- ggplot(LFD %>% filter(Fleet %in% survc, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) + facet_grid(FleetNm~Sex) + 
  ggtitle("SURVEYS")

  
pdf(pre_EstObs_LFD_fleets, height = 6.5, width = 11)
  for(f in names(lfd_plots)[fltfish]){
    for(cat in names(lfd_plots[[f]])){
      for(pl in names(lfd_plots[[f]][[cat]])){
        print(lfd_plots[[f]][[cat]][[pl]])
      }
    }
  }
dev.off()

pdf(pre_EstObs_LFD_surveys, height = 6.5, width = 11)
  for(f in names(lfd_plots)[10:12]){
      for(pl in names(lfd_plots[[f]])){
        print(lfd_plots[[f]][[pl]])
    }
  }
dev.off()

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Surveys: Abundance ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Transform into log scale.
surveys <- as_tibble(output$cpue) %>% select('Fleet', 'Fleet_name', 'Yr', 'Month', 'Time', 'Obs', 'Exp', 'SE') %>% 
    mutate(Obs = log(Obs), Exp = log(Exp), residuals = Obs-Exp, upp = Obs + 2*SE, low = Obs - 2*SE) #%>% 
  #          pivot_longer(cols = c('Obs', 'Exp', 'residuals'), names_to = 'indicator', values_to = 'value')
save(surveys, file = rep_estSurveys)

surv_res_log <- ggplot(surveys) + 
  geom_line(aes(Yr, Obs), col = 'red') + 
  geom_ribbon(aes(x = Yr, ymin = low, ymax = upp), fill = 'red', alpha = 0.3) +
  geom_point(aes(Yr, Exp), col = 'blue') +
  facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y') +
  ggtitle("Survey residuals log scale")

surv_res_orig <- ggplot(surveys) + 
  geom_line(aes(Yr, exp(Obs)), col = 'red') + 
  geom_point(aes(Yr, exp(Exp)), col = 'blue') +
  facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y') +
  ggtitle("Survey residuals original scale")

surv_res_pears <- ggplot(surveys) + geom_line(aes(Yr, residuals)) + 
  geom_point(aes(Yr, residuals), col = 'blue') +
  facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y')+
  geom_hline(yintercept = 0) +
  ggtitle("Survey pearson residuals")


pdf(pre_EstObs_surveys, height = 6.5, width = 11)
  print(surv_res_log)
  print(surv_res_orig)
  print(surv_res_pears)
dev.off()

png(file.path(plot_wd,"Pres_ASS_03a_survRes.png"), width = 800, height = 480)
  print(surv_res_log)
dev.off()

png(file.path(plot_wd,"Pres_ASS_03b_survPearsRes.png"), width = 800, height = 480)
  print(surv_res_pears)
dev.off()
  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Summary Table  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

# fishing mortality
f <- output$derived_quants %>%  
  filter(Label %in% paste("F",years,sep="_")) %>% 
  select(Value, StdDev) %>% .$Value
# f <- ssMats$fbarlen[as.character(years)] # for Carmen's code (used before)
f  <- c(f, NA) # add NA for interim year value (dtyr+1)
fc <- f/f # variable for removing interim year F


# All to do with catches
cc <- catch %>% 
  group_by(year, indicator) %>% summarise(value = sum(value)) %>% 
  tidyr::pivot_wider(id_cols = year, names_from = indicator) %>% 
  filter(year > 0)
cc <- cc %>% rbind(c(year = dtyr+1)) # add NA for interim year value (dtyr+1)

# SSB, REC and F
ssbrecf <- output$derived_quants

# Bound of the CIs: ssb (# 90% confidence interval from lognormal distribution)
spb <- ssbrecf %>% filter(substr(Label,1,3)=="SSB")
ssb <- spb %>% 
  filter(Label %in% paste("SSB", yearsfore, sep = "_")) %>% 
  select(Value, StdDev)

upperssb <- exp(log(ssb[1:(length(years)+1),1]) + 
                  sqrt(log(1+(1.64*ssb[1:(length(years)+1),2]/ssb[1:(length(years)+1),1])^2))) # including also interim year SSB
lowerssb <- exp(log(ssb[1:(length(years)+1),1]) - 
                  sqrt(log(1+(1.64*ssb[1:(length(years)+1),2]/ssb[1:(length(years)+1),1])^2))) # including also interim year SSB

upperssb.proj <- exp(log(ssb[length(years) + 1:3, 1]) + 
                       sqrt(log(1+(1.64*ssb[length(years) + 1:3, 2]/ssb[length(years) + 1:3,1])^2)))
lowerssb.proj <- exp(log(ssb[length(years) + 1:3, 1]) - 
                       sqrt(log(1+(1.64*ssb[length(years) + 1:3, 2]/ssb[length(years) + 1:3,1])^2)))

# # 95% confidence interval from normal distribution
# upperssb <- ssb[1:(length(years)+1),1] + 1.96*ssb[1:(length(years)+1),2] # including also interim year SSB
# lowerssb <- ssb[1:(length(years)+1),1] - 1.96*ssb[1:(length(years)+1),2] # including also interim year SSB
# upperssb.proj <- ssb[length(years) + 1:3, 1] + 1.96*ssb[length(years) + 1:3, 2]
# lowerssb.proj <- ssb[length(years) + 1:3, 1] - 1.96*ssb[length(years) + 1:3, 2]

# Bound of the CIs: recruitment (#! 90% confidence interval from lognormal distribution)
recr <- ssbrecf %>% filter(substr(Label,1,4)=="Recr") %>% 
  filter(Label %in% paste("Recr", yearsfore[1:(length(years)+1)], sep = "_")) %>%  # including also interim year REC
  select(Value, StdDev)

upperrecr.90 <- exp(log(recr[,1]) + sqrt(log(1+(1.64*recr[,2]/recr[,1])^2)))
lowerrecr.90 <- exp(log(recr[,1]) - sqrt(log(1+(1.64*recr[,2]/recr[,1])^2)))

# # 95% confidence interval from lognormal distribution used before (but lognmormal not necessarily symmetric)
# upperrecr.90 <- exp(log(recr[,1]) + 1.96*sqrt(log(1+(recr[,2]/recr[,1])^2)))
# lowerrecr.90 <- exp(log(recr[,1]) - 1.96*sqrt(log(1+(recr[,2]/recr[,1])^2)))
# # 95% confidence interval from normal distribution
# upperrecr.90 <- recr[,1] + 1.96*recr[,2]
# lowerrecr.90 <- recr[,1] - 1.96*recr[,2]

# Bound of the CIs: fishing mortality (#! 90% confidence interval from lognormal distribution)
fratenum <- ssbrecf %>% filter(substr(Label,1,2)=="F_") %>% 
  select(Value, StdDev)

upperf <- exp(log(f) + sqrt(log(1+(1.64*fratenum[1:(nyears+1),'StdDev']/fratenum[1:(nyears+1),'Value']*fc)^2))) # /f*f only for removing interim year F
lowerf <- exp(log(f) - sqrt(log(1+(1.64*fratenum[1:(nyears+1),'StdDev']/fratenum[1:(nyears+1),'Value']*fc)^2)))

# # 95% confidence interval from normal distribution
# upperf <- f + 1.96*((fratenum[1:(nyears+1),'StdDev']/fratenum[1:(nyears+1),'Value'])*f) # /f*f only for removing interim year F
# lowerf <- f - 1.96*((fratenum[1:(nyears+1),'StdDev']/fratenum[1:(nyears+1),'Value'])*f)


sumTab <- as_tibble(output$timeseries) %>% filter(Era == 'TIME' | (Era == 'FORE' & Yr == dtyr+1)) %>% 
              mutate(Bio_all = ifelse(Seas == 1, Bio_all, 0)) %>% # to get only biomass 1st Jan
              select("Yr", "Seas", 'Bio_all', 'SpawnBio') %>% 
              group_by(Yr) %>% summarise(ssb = sum(SpawnBio, na.rm = TRUE), biomass = sum(Bio_all)) %>% 
              mutate(rec = recr[,1], f = f) %>% bind_cols(cc[,-1]) %>% 
              mutate(CatObs =  LanObs+ifelse(is.na(DisObs), 0, DisObs), yield.ssb = CatObs/ssb, 
                     upperf = upperf, lowerf = lowerf, lowerrec = lowerrecr.90, upperrec = upperrecr.90,
                     upperssb = upperssb, lowerssb = lowerssb) %>% 
  rename(year = Yr)

# set to NA all except SSB-related quantities and REC estimate
sumTab[sumTab$year==as.character(dtyr+1), 
        names(sumTab)[!names(sumTab) %in% c("year","ssb","biomass","rec","upperssb","lowerssb","lowerrec","upperrec")]] <- NA

sumTab.long <- sumTab %>% 
  pivot_longer(cols = ssb:yield.ssb, names_to = 'indicator', values_to = 'value') %>% 
  mutate(upper = ifelse(indicator == 'f', upperf, ifelse(indicator == 'rec', upperrec, 
                                                         ifelse(indicator == 'ssb', upperssb, NA))), 
         lower = ifelse(indicator == 'f', lowerf, ifelse(indicator == 'rec', lowerrec, 
                                                         ifelse(indicator == 'ssb', lowerssb, NA)))) %>% 
  select(year, indicator, value, upper, lower)

write.csv(sumTab, adv_sumTab.csv, row.names = FALSE)
write.csv(sumTab, rep_sumTab.csv, row.names = FALSE)
save(sumTab, sumTab.long, file = out_sumTab)
 
# SUMMARY PLOT
summary_plot(sumTab, fmsy = Fmsy, blim = Blim/1000, bpa = Bpa/1000)
# summary_plot(sumTab, fmsy = Fmsy, blim = Blim/1000, bpa = Bpa/1000, file = pre_sumPlot)

png(pre_asssum, width = 700, height = 500)
  summary_plot(sumTab, fmsy = Fmsy, blim = Blim/1000, bpa = Bpa/1000)
dev.off()
# ggplot(sumTab.long %>% filter(indicator %in% c('ssb', 'rec', 'f', 'CatObs')) %>% mutate(indicator = fct_relevel(indicator, c('')))) +
#   facet_wrap(~indicator, ncol = 2, scale = 'free') +
#   geom_line(data = sumTab.long %>% filter(indicator %in% c('ssb',  'f', 'CatObs')), aes(year, value), lwd = 1) +
#   geom_bar(data = sumTab.long %>% filter(indicator == 'rec'), aes(year, value), stat = 'identity')


# Interim year

ssbrecf[substr(ssbrecf$Label,1,4)=="Recr",][length(years)+3, ] # Rec(dtyr+1)

dat.iy <- as_tibble(output$timeseries) %>% # Total biomass and SSB
  mutate(Bio_all = ifelse(Seas ==1, Bio_all, 0)) %>% # only biomass 1st Jan
  select("Yr", "Seas", 'Bio_all', 'SpawnBio', 'Recruit_0') %>% 
  group_by(Yr) %>% summarise(ssb = sum(SpawnBio, na.rm = TRUE), biomass = sum(Bio_all), 
                             Recruit_0 = sum(Recruit_0)) %>% filter(Yr == dtyr+1)

# upperssb.proj[1]
# lowerssb.proj[1]

iy.sum <- data.frame(year = dtyr+1, rec = dat.iy$Recruit_0, 
                     biomass = dat.iy$biomass, 
                     ssb = dat.iy$ssb, upperssb = upperssb.proj[1], lowerssb = lowerssb.proj[1])
iy.sum

# # STF
# 
# stf.sum1 <- output$derived_quants %>%
#   separate(Label, c("indicator", "Yr"), "_") %>% 
#   mutate(Yr = as.numeric(Yr)) %>% 
#   filter(!is.na(Yr)) %>% 
#   filter(indicator %in% c("SSB", "Recr", "F", "ForeCatch", "OFLCatch", "ForeCatchret")) %>%
#   select(indicator, Yr, Value) %>% 
#   pivot_wider(names_from = indicator, values_from = Value)
# 
# stf.sum2 <- as_tibble(output$timeseries) %>% 
#     mutate(Bio_all = ifelse(Seas == 1, Bio_all, 0)) %>% # only biomass 1st Jan
#     select("Yr", "Seas", 'Bio_all', 'SpawnBio', 'Recruit_0', 
#            starts_with("F:_"), starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) %>% 
#     pivot_longer(cols = -c(1:5), names_to = "indicator", values_to = "value") %>% 
#     separate(indicator, c("indicator", "Fleet"), ":_") %>% 
#     group_by(Yr, Seas, Bio_all, SpawnBio, Recruit_0, indicator) %>% summarise(value = sum(value)) %>% 
#     mutate(indicator = case_when(indicator == "obs_cat" ~ 'LanObs',
#                                  indicator == "retain(B)" ~ 'LanEst',
#                                  indicator == "dead(B)" ~ 'CatEst',
#                                  TRUE ~ indicator)) %>% 
#     pivot_wider(names_from = indicator, values_from = value) %>% 
#     mutate(DisEst = CatEst - LanEst) %>% 
#     group_by(Yr) %>% 
#     summarise(ssb = sum(SpawnBio, na.rm = TRUE), biomass = sum(Bio_all), 
#               Recruit_0 = sum(Recruit_0), Ftot = sum(F), #LanObs = sum(LanObs), 
#               CatEst = sum(CatEst), LanEst = sum(LanEst), DisEst = sum(DisEst))
#   
#   
# full_join(stf.sum1, stf.sum2, by = "Yr") %>% filter(Yr >= 2020)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### RETROSPECTIVE:  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
load(inp_retros)
load(out_sumTab)

retro_sum <- SSsummarize(retro_out[1:6])

# Retro in selectivities, we only compare last year one, just to check
flts <- names(fltnms)[!grepl("RESSGAS",fltnms)] %>% as.numeric()
retro_sel <- retro_sum[['sizesel']] %>% 
  filter(Yr == dtyr & Fleet %in% flts) %>% 
  pivot_longer(cols = 6:78,  names_to = 'lng', values_to = 'value') %>% 
  mutate(lng = as.numeric(lng), FleetNm = fltNms[Fleet])

png(pre_retroSel, width = 700, height = 500)
  p_retroSel <- ggplot(retro_sel, aes(as.numeric(lng), value, group = name, colour = name)) + geom_line(lwd = 1) + facet_wrap(~FleetNm, ncol = 3)
  print(p_retroSel)
dev.off()

# Retro in SSB, F and Rec

retro_indicators <- bind_rows(
  as_tibble(retro_sum$SpawnBio) %>% filter(Yr>1977) %>% 
    pivot_longer(cols = starts_with('upd'), names_to = 'retro_yr', values_to = 'value') %>% 
    mutate(indicator = 'SSB'),
  as_tibble(retro_sum$Fvalue) %>% filter(Yr>1977) %>% 
    pivot_longer(cols = starts_with('upd'), names_to = 'retro_yr', values_to = 'value') %>% 
    mutate(indicator = 'F'),
  as_tibble(retro_sum$recruits) %>% filter(Yr>1977) %>% 
    pivot_longer(cols = starts_with('upd'), names_to = 'retro_yr', values_to = 'value') %>% 
    mutate(indicator = 'Recruitment')) %>% 
  filter(Yr < dtyr + 2)  #NOTE: SSB one year forward compared to F. REC

names(retro_indicators) <- c('label', 'year', 'retro',  'value', 'indicator')

# Remove the forecasted values in retro indicators
retro_indicators_ssb <- retro_indicators %>% 
  filter(indicator == "SSB" & !(year == dtyr+1 & retro != 'upd'), 
         indicator == "SSB" & !(year == dtyr & retro %in% paste0('upd_retro-', 5:2)),
         indicator == "SSB" & !(year == dtyr-1 & retro %in% paste0('upd_retro-', 5:3)),
         indicator == "SSB" & !(year == dtyr-2 & retro %in% paste0('upd_retro-', 5:4)),
         indicator == "SSB" & !(year == dtyr-3 & retro %in% paste0('upd_retro-', 5:5)),
  )
retro_indicators_oth <- retro_indicators %>% 
  filter(indicator != "SSB" & !(year == dtyr+1), 
         indicator != "SSB" & !(year == dtyr & retro != 'upd'), 
         indicator != "SSB" & !(year == dtyr-1 & retro %in% paste0('upd_retro-', 5:2)),
         indicator != "SSB" & !(year == dtyr-2 & retro %in% paste0('upd_retro-', 5:3)),
         indicator != "SSB" & !(year == dtyr-3 & retro %in% paste0('upd_retro-', 5:4)),
         indicator != "SSB" & !(year == dtyr-4 & retro %in% paste0('upd_retro-', 5:5))
  )

retro_indicators_all <- rbind(retro_indicators_ssb, retro_indicators_oth) %>%
  select(retro, year, indicator, value) %>% mutate(indicator = substr(tolower(indicator), 1,3)) %>% 
  mutate(retro = factor(ifelse(retro == 'upd', 0, substr(unlist(retro),10,11)), levels = 0:-5))

sumTab4retro <- sumTab.long %>% filter(indicator %in% c('ssb', 'rec', 'f')) #%>%

png(pre_retroCI, width = 700, height = 500)
  p_retroCI <- ggplot(retro_indicators_all %>% filter(retro %in% as.character(0:-5)), aes(year, value, group = retro, colour = retro)) + 
    geom_line(lwd = 1) + facet_grid(indicator~., scales = 'free') +
    geom_ribbon(data = sumTab4retro %>% filter(indicator %in% c('ssb', 'rec', 'f')) %>% mutate(retro = '0'), 
                aes(year, ymin = lower, ymax = upper), col = 'black', alpha = 0.3)
  print(p_retroCI)
dev.off()

endyrvec <- dtyr + 1:(-4)

# CHECKS
# # Retro prlot from r4ss
# SSplotComparisons(retro_sum, endyrvec = endyrvec, print = FALSE, pdf = TRUE, plotdir = "output") # just for checking

# Mohn's rho values 
# - from r4ss
mohn_ss <- SSmohnsrho(retro_sum, endyrvec = endyrvec-1)
mohn_ss[["AFSC_Hurtado_Rec"]]
mohn_ss[["AFSC_Hurtado_F"]]
SSmohnsrho(retro_sum, endyrvec = endyrvec)$AFSC_Hurtado_SSB # SSB (needs one year forward)
# end CHECKS
# - from ad-hoc function
mohn_vals <- function(df, file = NULL){
  
  yrs <- unique(df$year)
  ny <- length(yrs)
  
  nretro <- length(unique(df$retro))
  
  SSBs       <- matrix(NA,ny,nretro, dimnames = list(yrs, 0:(nretro-1)))
  Fs <- RECs <- matrix(NA,ny-1,nretro, dimnames = list(yrs[-length(yrs)], 0:(nretro-1)))
  
  for(id in 0:(nretro-1)){
    
    dat <- subset(df, retro == as.character(-id))
    
    if(id == 0){ 
      Fs[,id+1]   <- unlist(subset(dat, indicator == 'f')[,'value'])
      SSBs[,id+1] <- unlist(subset(dat, indicator == 'ssb')[,'value'])
      RECs[,id+1] <- unlist(subset(dat, indicator == 'rec')[,'value'])
    }
    
    if(id > 0){
      Fs[-((ny-id):(ny-1)),id+1]   <- unlist(subset(dat, indicator == 'f')[,'value'])
      SSBs[-((ny-id+1):ny),id+1] <- unlist(subset(dat, indicator == 'ssb')[,'value'])
      RECs[-((ny-id):(ny-1)),id+1] <- unlist(subset(dat, indicator == 'rec')[,'value'])
    }
  }
  
  mr <- mohn(RECs[(ny-6):(ny-1),])
  mf <- mohn(Fs[(ny-6):(ny-1),])
  ms <- mohn(SSBs[(ny-5):ny,])
  
  return(list(Rec = mr, F = mf, SSB = ms))
  
}
mohn_rhos <- mohn_vals(retro_indicators_all)


png(pre_retroCIrec, width = 700, height = 500)
  hrec  <- retro_indicators_all %>% 
    filter(retro %in% as.character(0:-5), indicator == 'rec', retro == 0, 
           year %in% c(1990:years[length(years)-2])) %>% .$value
  gmrec <- exp(mean(log(hrec)))
  gmrec
  
  p_retroCI <- ggplot(retro_indicators_all %>% filter(retro %in% as.character(0:-5), indicator == 'rec', year >2009), 
         aes(year, value, group = retro, colour = retro)) + 
    geom_line(lwd = 1) + facet_grid(indicator~., scales = 'free') +
    geom_ribbon(data = sumTab4retro %>% filter(indicator == 'rec', year >2009) %>% mutate(retro = '0'), 
                aes(year, ymin = lower, ymax = upper), col = 'black', alpha = 0.3) + 
    geom_hline(aes(yintercept = gmrec), lty = 2)
  print(p_retroCI)
dev.off()

mohn_plot(retro_indicators_all)
mohn_plot(retro_indicators_all, file = pre_retroPeels)

retro_indicators <- retro_indicators_all

save(retro_indicators, retro_sel, sumTab4retro, mohn_rhos, file = rep_retroInd)
save(retro_indicators, retro_sel, sumTab4retro, mohn_rhos, file = out_retroInd)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### ADVICE TABLE:  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

# see model_03a_Forecast_settings.R, model_03b_Forecast_runs.R, 
#     model_03c_Forecast_summary.R, model_03d_Forecast_interpolation.R

interimyear      <- read.csv(stfiy.file)
catchoptiontable <- read.csv(stfop.file)
advTab           <- read.csv(adv_advTab.csv)

# contribution_ages_msy <- CatchOptTb$contribution_ages_msy
#
# contribution_ages_msy <- rbind(contribution_ages_msy$catch*contribution_ages_msy$pland,
#                                contribution_ages_msy$catch*(1-contribution_ages_msy$pland))
# rownames(contribution_ages_msy) <- paste(rep(c('landings', 'discards'), each = 2), (dtyr+1):(dtyr+2), sep = "-")
#
# par(mfrow = c(2,1))
# barplot(contribution_ages_msy[c(1,3),], main = dtyr+1, col = c('darkcyan', 'tomato'))
# barplot(contribution_ages_msy[-c(1,3),], main = dtyr+2, col = c('darkcyan', 'tomato'))

save(catchoptiontable, interimyear, advTab, file = out_catchOptTab) #, contribution_ages_msy


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### ADVICE TABLE  RECRUITMENT REPLACED in dtyr & dtyr-1  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

# see model_03a_Forecast_settings.R, model_03b_Forecast_runs.R, 
#     model_03c_Forecast_summary.R, model_03d_Forecast_interpolation.R

interimyear_GM      <- read.csv(stfiy_GM.file)
catchoptiontable_GM <- read.csv(stfop_GM.file)
advTab_GM           <- read.csv(adv_advTab_GM.csv)

# contribution_ages_msy_GM <- CatchOptTb_GM$contribution_ages_msy
#
# contribution_ages_msy_GM <- rbind(contribution_ages_msy_GM$catch*contribution_ages_msy_GM$pland,
#                                contribution_ages_msy_GM$catch*(1-contribution_ages_msy_GM$pland))
# rownames(contribution_ages_msy_GM) <- paste(rep(c('landings', 'discards'), each = 2), (dtyr+1):(dtyr+2), sep = "-")
#
# par(mfrow = c(2,1))
# barplot(contribution_ages_msy_GM[c(1,3),], main = dtyr+1, col = c('darkcyan', 'tomato'))
# barplot(contribution_ages_msy_GM[-c(1,3),], main = dtyr+2, col = c('darkcyan', 'tomato'))

save(catchoptiontable_GM, interimyear_GM, advTab_GM, file = out_catchOptTab_GM) #, contribution_ages_msy_GM


# gmrec <- exp(mean(log(recr[paste('Recr_', 1990:years[length(years)-2], sep = ""),1])))
# # Replace the recruitment in dtyr-1 and dtyr and age 1 in dtyr in natage.
# natage_gmrec <- replace_recruitment(dtyr, ssMats, M = 0.4, recruitment = gmrec)
# # Replace natage in ssMats by the new natage matrix.
# ssMats_gm <- ssMats
# ssMats_gm$natage <- natage_gmrec
# # Save natage_gmrec in case it needs to be used afterwards.
# save(natage_gmrec, file = 'model/final/output/natage_gmrec.RData')
# #save(natage_gmrec, file = 'model/wg20upd_RD/output/natage_gmrec.RData')
# 
# # To obtain ssb22, we need to run first CatchOptTable and take it from interim 
# # years values. so what I do is first use the wrong value in 'Btable' and then
# # update it with the correct one
# fsq   <- mean(ssMats$fbarlen[as.character((dtyr-2):dtyr)])
# 
# # Call to catch option table function here.
# CatchOptTb_GM <- catch_option_table(lmin = 15, lmax = 80, 
#                                  fmult = c(seq(0,4, 0.0001), seq(5,7, 0.0005)), fmult.tab = seq(0,2,0.1),
#                                  Ftable = c(0,0.26, 0.18,0.40,0.6,0.84, 1.02, 0.73, fsq),
#                                  Btable = c(40000,56000,ssbiy_gm), 
#                                  Ctable = c(102888, 102888), 
#                                  advice.wgyr = 102888, tac.wgyr = 98657,
#                                  Fmsy = 0.26, nseasons = 4, nmorphs = 3, ages = 0:15,
#                                  ssMats = ssMats_gm, years = years, dtyr = dtyr,
#                                  aveyrs = as.character((dtyr-2):dtyr), output = output, 
#                                  scaleFbartofinalyear = FALSE,
#                                  file.csv = rep_cathOptTab_GM.csv)
# save(CatchOptTb_GM, file=out_catchOptTab_GM)
# 
# 
# # Put the table in the correct order, correct files, to copy-paste in the advice.
# # Round F-s, not the statu quo, not to be confounded with Fmsy.
# advTab_gm <- CatchOptTb_GM$advice_table[c(2,2:4,1,5:8, 10:11,11,12,9,13:14),c(5:7,2:4,8:11)]
# advTab_gm[c(1:13,15:16),4:6] <- as.numeric(icesRound(advTab_gm[c(1:13,15:16),4:6]))
# advTab_gm[,8:10] <- icesRound(advTab_gm[,8:10]*100, percent = TRUE) 
# write.csv(advTab_gm,  file = adv_advTab_GM.csv, quote = FALSE, row.names = FALSE)
# 
# catchoptiontable_gm <- CatchOptTb_GM$catchoptionstable
# interimyear_gm      <- CatchOptTb_GM$interimyear
# 
# contribution_ages_msy_gm <- CatchOptTb_GM$contribution_ages_msy
# 
# 
# contribution_ages_msy_gm <- rbind(contribution_ages_msy_gm$catch*contribution_ages_msy_gm$pland,
#                                contribution_ages_msy_gm$catch*(1-contribution_ages_msy_gm$pland))
# rownames(contribution_ages_msy_gm) <- paste(rep(c('landings', 'discards'), each = 2), (dtyr+1):(dtyr+2), sep = "-")
# 
# par(mfrow = c(2,1))
# barplot(contribution_ages_msy_gm[c(1,3),], main = dtyr+1, col = c('darkcyan', 'tomato'))
# barplot(contribution_ages_msy_gm[-c(1,3),], main = dtyr+2, col = c('darkcyan', 'tomato'))
# 
# 
# barplot(contribution_ages_msy[-c(1,3),], main = 'Est. Recruitment', col = c('darkcyan', 'tomato'))
# barplot(contribution_ages_msy_gm[-c(1,3),], main = 'Geometric Mean Recr.', col = c('darkcyan', 'tomato'))
# 
# 
# save(catchoptiontable_gm, interimyear_gm, advTab_gm, contribution_ages_msy, contribution_ages_msy_gm, file = out_catchOptTab_GM)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### YPR  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

fsq <- interimyear[[paste0("F",dtyr+1)]]

equil_yield <- output[["equil_yield"]] %>% 
  arrange(F_report) %>% 
  mutate(slope = (YPR - lag(YPR)) / (F_report - lag(F_report)), 
         slope = ifelse(is.na(slope), 0, slope), 
         SSBpr = SSB/Recruits)

fref <- equil_yield %>% filter(Fmult == 1) %>% .$F_report

fval <- function(x, ref, tgt=NULL) {
  
  if(!ref %in% c("F01", "SPR", "Fmax")) stop("ref must take value 'Depletion', 'SPR' or 'Fmax'")
  
  if (ref == 'F01') {
    out <- x[abs(x$slope - 0.1) == min(abs(x$slope - 0.1)), "F_report"]
  } else if (ref == 'SPR') {
    if (is.null(tgt)) stop("'tgt' value is required")
    out <- x[abs(x[ref] - tgt) == min(abs(x[ref] - tgt)), "F_report"]
  } else
    out <- x[x$YPR == max(x$YPR), "F_report"]
  
}

F01  <- fval(equil_yield, ref = "F01")
F35  <- fval(equil_yield, ref = "SPR", tgt = 0.35)
F30  <- fval(equil_yield, ref = "SPR", tgt = 0.30)
Fsq  <- fsq
Fmax <- fval(equil_yield, ref = "Fmax")

# YPR plot
plot(0, type = "n", xlab = "F(1-7)", ylab = "YPR (t)", 
     xlim = c(0, 0.5), 
     ylim = c(0, max(equil_yield[["YPR"]],  na.rm = TRUE)*1.05))
lines(equil_yield[["F_report"]], equil_yield[["YPR"]], lwd = 2, lty = 1)
abline(v = F01, col = "green", lwd = 2)
abline(v = F35, col = "blue", lwd = 2)
abline(v = F30, col = "lightblue", lwd = 2, lty = 2)
abline(v = Fsq, col = "darkorange", lwd = 2)
abline(v = Fmax, col = "red", lwd = 2)
legend("bottomright", bty = "n", lwd = 2, lty = c(rep(1,2),2, rep(1,2)), 
       col = c("green", "blue", "lightblue", "darkorange", "red"), legend = c("F0.1","F35%","F30%","Fsq","Fmax"))

# SPR plot
plot(0, type = "n", xlab = "F(1-7)", ylab = "SPR (t)", 
     xlim = c(0, 0.5), 
     ylim = c(0, max(equil_yield[["SPR"]],  na.rm = TRUE)*1.05))
lines(equil_yield[["F_report"]], equil_yield[["SPR"]], lwd = 2, lty = 1)
abline(v = fval(equil_yield, ref = "F01"), col = "green", lwd = 2)
abline(v = fval(equil_yield, ref = "SPR", tgt = 0.35), col = "blue", lwd = 2)
abline(v = fval(equil_yield, ref = "SPR", tgt = 0.30), col = "lightblue", lwd = 2, lty = 2)
abline(v = fsq, col = "darkorange", lwd = 2)
abline(v = fval(equil_yield, ref = "Fmax"), col = "red", lwd = 2)
legend("topright", bty = "n", lwd = 2, lty = c(rep(1,2),2, rep(1,2)), 
       col = c("green", "blue", "lightblue", "darkorange", "red"), legend = c("F0.1","F35%","F30%","Fsq","Fmax"))

save( equil_yield, F01, F35, F30, Fsq, Fmax, fref, file = rep_YPR)


yprtab <- equil_yield %>%
  select(SPR, Fmult, F_report, YPR, SSBpr) %>%
  mutate( SPR = icesRound(SPR), Fmult = icesRound(Fmult), F_report = icesRound(as.numeric(F_report)),
          YPR = icesRound(YPR), SSBpr = icesRound(SSBpr)) %>% 
  mutate(Fmult = round(as.numeric(Fmult),1)) %>% 
  rename('F(1-7)' = F_report)

pos <- c()
for (f in seq(0, 2, 0.1))
  pos <- c(pos, which(round(yprtab$Fmult,1) == round(f,1))[1])
# pos <- c(pos, which(abs(equil_yield$Fmult - f)  == min(abs(equil_yield$Fmult - f))))

pos <- pos[!is.na(pos)]

yprtab[pos,]

prp <- c()
for (f in c(Fsq, F01, F35, F30, Fmsy)) #c(Fmsy, Flow, Fupp, Fpa)
  prp <- c(prp, which(abs(as.numeric(yprtab[,'F(1-7)']) - f)  == min(abs(as.numeric(yprtab[,'F(1-7)']) - f)))[1])

yprtab[prp,]

length(pos); length(prp)

write.csv(yprtab[c(pos,prp),], file=rep_YPRtable, row.names = FALSE)

