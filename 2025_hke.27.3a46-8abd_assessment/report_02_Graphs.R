#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FORMATTING OF THE TABLES FOR THE REPORT
# Dorleta Garcia 
# 2021/05/10
#  modified:  2022-05-16 (ssanchez@azti.es) - adapt given WKANGHAKE2022 (2 sexes + F from SS? not Camen's code)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(ggridges)
library(tidyverse)
library(icesTAF)
library(icesAdvice)

source(file.path(taf.boot.path("software"),'mohn_plot.R'))
source(file.path(taf.boot.path("software"),'summary_plot.R'))

nyears <- dtyr-1978+1

run <- paste0("_", run)

rep_wd <- file.path("report","wg_report")
if (!dir.exists(rep_wd)) dir.create(rep_wd)

# Load data
load(file.path("data","LFDs.RData"))
load(file.path("data","surveys_biomass.RData"))
load(file.path("data","report","ss3dat.RData"))

SS3_R.name  <- file.path("model", "final", 'post', 'ss3_R_output.RData')

rep_estCatch   <- file.path("output", "report", paste0('estimated_catch',   run, '.RData'))
rep_estLFD     <- file.path("output", "report", paste0('estimated_LFD',     run, '.RData'))
rep_estSurveys <- file.path("output", "report", paste0('estimated_surveys', run, '.RData'))
rep_retroInd   <- file.path("output", "report", paste0('retro',             run, '.RData'))
rep_sumTab.csv <- file.path("output", "report", paste0('summary_table',     run, '.csv'))
rep_cathOptTab.csv <- file.path("model", "sum_advice", paste0('catchoptiontable', run, '.csv'))
rep_cathOptTab_GM.csv <- file.path("model", "sum_advice", paste0('catchoptiontable_GM', run, '.csv')) #! PENDING
rep_YPR        <- file.path("output", "report", paste0('YPR', run, '.RData'))

out_sumTab   <- file.path('output', paste0('summary_table',   run, '.RData'))
out_selatlen <- file.path('output', paste0('sel_ret_atlen',   run,'.RData'))

load(rep_estSurveys)
load(rep_estLFD) 
load(rep_retroInd)
load(rep_YPR)
load(out_selatlen)
load(out_sumTab)

load(SS3_R.name) # output

# Commercial fleets
fltnms <- setNames(output$definitions$Fleet_name, output$fleet_ID)[which(output$IsFishFleet)]
# fltnms <- c('SPTR7', 'TROTH', 'FRNEP8', 'SPTR8', 'GILLNET', 'LONGLINE', 'OTHHIST', 'NSTRAWL', 'OTHER')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Survey Obs. biomass ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,paste0("surveys_biomass", run, ".png")))
  p <- ggplot(subset(surveys.id, year < dtyr+1), aes(x = year, y = obs, fill = index)) + 
    facet_grid(index~., scales = 'free') +
    geom_ribbon(aes(ymin = ll, ymax = ul, colour = index), alpha = 0.3) +
    geom_line(aes(colour = index)) + geom_point(aes(colour = index)) +
    theme_bw() + theme(legend.position="none")
  print(p) 
dev.off()

# Some calculations
evhoe <- surveys.id %>% filter(year < dtyr+1, index == "EVHOE-WIBTS-Q4") %>%
  mutate(year = floor(year)) %>% select(year, obs) %>% 
  mutate(pct_change = (obs - lag(obs))/lag(obs) * 100, 
         pcr = round(pct_change))
evhoe %>% arrange(obs)

igfs <- surveys.id %>% filter(year < dtyr+1, index == "IGFS-WIBTS-Q4") %>%
  mutate(year = floor(year)) %>% select(year, obs) %>% 
  mutate(pct_change = (obs - lag(obs))/lag(obs) * 100, 
         pcr = round(pct_change))
igfs %>% arrange(obs)

ibts <- surveys.id %>% filter(year < dtyr+1, index == "SpPGFS-WIBTS-Q4") %>%
  mutate(year = floor(year)) %>% select(year, obs) %>% 
  mutate(pct_change = (obs - lag(obs))/lag(obs) * 100, 
         pcr = round(pct_change))
ibts %>% arrange(obs)

iams <- surveys.id %>% filter(year < dtyr+1, index == "IAMS-WIBTS-Q4") %>%
  mutate(year = floor(year)) %>% select(year, obs) %>% 
  mutate(pct_change = (obs - lag(obs))/lag(obs) * 100, 
         pcr = round(pct_change))
iams %>% arrange(obs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Surveys Obs. LFD ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,paste0("surveys_LFD", run, ".png")))
  p_lfd_surv<- ggplot(subset(lfd.surv, survnms %in% c('EVHOE', 'SP-PORC', 'IR-IGFS', 'IR-IAMS') & year %in% c((dtyr-4):dtyr)), 
                      aes(x=length, height=value, y=factor(survnms),  fill = year)) +
    geom_density_ridges2(stat="identity", aes(fill = year, colour = year), scale=1.2,  alpha=0.3) + 
    ylab("survey")+theme_bw()
  print(p_lfd_surv)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Total catch over time by SS3 fleet ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cyr <- as_tibble(ss3dat$catch) %>% group_by(year, fleet) %>% 
  filter(year > 1979 & year <= dtyr) %>% mutate(fleet = fltnms[fleet])
  
png(file.path(rep_wd,paste0("catch_ss3_fleet", run, ".png")), width = 600, height = 480)
  p_ct_ss3fl_lines <-ggplot(cyr, aes(year, catch, group = fleet, fill = fleet)) + geom_bar(stat = 'identity')
  print(p_ct_ss3fl_lines + theme_bw())
dev.off()

# Some calculations
ctrw <- cyr %>% 
  mutate(fl = case_when(fleet %in% grep("TR|NEP", fleet, value = TRUE) ~ "TRW", 
                        fleet %in% grep("^OTH", fleet, value = TRUE) ~ "OTH", 
                        TRUE ~ fleet)) %>% 
  group_by(year, fl) %>% summarise(catch = sum(catch)) %>% 
  pivot_wider(names_from = fl, values_from = catch) %>% 
  mutate(ptrw = TRW/(GILLNET+LONGLINE+TRW+OTH), poth = (GILLNET+LONGLINE)/(GILLNET+LONGLINE+TRW+OTH))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Fleets LFD observed ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,paste0("Fleets_LFD", run, ".png")), width = 600, height = 800)
  p_lfd <- ggplot(subset(lfd.flt,  year %in% (dtyr-2):dtyr), 
                  aes(x=length, height=value, y=factor(fltnm), fill = year)) + facet_grid(.~category*season)+
    geom_density_ridges2(stat="identity", scale=1.2, aes(fill = year, colour = year), alpha=0.3, size = 0.2) +
    ylab("fleet") + ggtitle(paste('Length frequency distribution - ', dtyr)) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
  print(p_lfd + theme_bw())
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Survery biomass Pearson Residuals  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,paste0("Pearson_surveys", run, ".png")),  width = 600, height = 700)
  p_survp <- ggplot(surveys %>% filter(Yr <= dtyr)) + geom_line(aes(Yr, residuals)) + 
    geom_point(aes(Yr, residuals), col = 'blue') +
    facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y')+
    geom_hline(yintercept = 0) + theme_bw()
  print(p_survp)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Surveys bubble plots  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,paste0("Bubbles_surveys", run, ".png")))
  p_bub <- ggplot(LFD %>% filter(Fleet %in% c(10,15:17), variable == "Pearson", CatchComponent == 'cat')) + 
    geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
    scale_color_manual(values = c('blue','red')) + facet_grid(FleetNm~Sex) + ggtitle("SURVEYS") + theme_bw()
  print(p_bub)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Length-frequency distributions  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# lfd.flt <- lfd.flt %>% mutate(fltnm = factor(fltnms[fleet])) 

fleetlut <- output$definitions %>% select(Fleet ,Fleet_name)
sexlut   <- data.frame(Sexes=c(0,3,3), Sex=c(1,1,2), SexName=c('U','F','M'))
partlut  <- data.frame(Part=0:2, CatchCat=c('Cat','Dis','Lan'))

lablev <- expand.grid(category = partlut$CatchCat, sex = sexlut$SexName, fleet = fleetlut$Fleet_name) %>% 
  mutate(levels = paste(fleet, category, sex)) %>% .$levels

a1 <- output$lendbase %>% left_join(fleetlut) %>% left_join(sexlut) %>% left_join(partlut) %>%
  mutate(Label = factor(paste(Fleet_name,CatchCat,SexName), levels = lablev))
a2 <- a1 %>% group_by(Label,Bin) %>%
  summarise(Obs=sum(Obs),Exp=sum(Exp)) %>%
  group_by(Label) %>% mutate(Obs=Obs/sum(Obs),Exp=Exp/sum(Exp))

png(file.path(rep_wd,paste0("LenFit", run, ".png")))
  p_lfit <- ggplot(a2) + geom_line(aes(Bin,Exp),col=4) + geom_point(aes(Bin,Obs),size=0.5) + 
    facet_wrap(~Label,ncol=4) + xlab('Length bin (cm)') + ylab('Relative frequency')
  print(p_lfit)
dev.off()
# ggsave('LenFit.png',width=6,height=6,dpi=600,scale=1.5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Selectivity and Retention curves  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

selatlen <- selatlen %>% filter(Fleet %in% names(fltnms)) %>% 
  mutate(FleetNm = fltnms[Fleet], 
         Sex = factor(Sex, levels = 1:2, labels = c('F','M'))) %>% 
  filter(Sex == 'F') # same selectivity for M and F

retatlen <- retatlen %>% filter(Fleet %in% names(fltnms)) %>% 
  mutate(FleetNm = fltnms[Fleet], 
         Sex = factor(Sex, levels = 1:2, labels = c('F','M'))) %>% 
  filter(Sex == 'F') # same retention for M and F

png(file.path(rep_wd,paste0("selectivities", run, ".png")), width = 600, height = 800)
  p_sel <- ggplot(selatlen %>% filter(Yr %in% 2000:dtyr), aes(lng, value, group = Yr, color = Yr)) + 
    facet_wrap(FleetNm~.) + #facet_wrap(FleetNm~Sex, ncol = 4) + 
    geom_line(lwd = 1) + theme_bw() +
    geom_line(data = selatlen %>% filter(Yr == dtyr), aes(lng, value, group = Yr, color = Yr), lwd = 1.5, color = 'black') + 
    geom_line(data = selatlen %>% filter(Yr == dtyr-1), aes(lng, value, group = Yr, color = Yr), lwd = 1.5, color = 'black', linetype = "longdash") 
  print(p_sel)
dev.off()

png(file.path(rep_wd,paste0("retentions", run, ".png")), width = 600, height = 800)
  p_ret <- ggplot(retatlen %>% filter(Yr %in% 2000:dtyr), aes(lng, value, group = Yr, color = Yr)) + 
    facet_wrap(FleetNm~.) + #facet_wrap(FleetNm~Sex, ncol = 4) + 
    geom_line(lwd = 1) + theme_bw() +
    geom_line(data = retatlen %>% filter(Yr == dtyr), aes(lng, value, group = Yr, color = Yr), lwd = 1.5, color = 'black') + 
    geom_line(data = retatlen %>% filter(Yr == dtyr-1), aes(lng, value, group = Yr, color = Yr), lwd = 1.5, color = 'black', linetype = "longdash") 
  print(p_ret)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Retrospective plots: time series + peels  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,paste0("retro_CI", run, ".png")), width = 700, height = 500)
  p_retroCI <- ggplot(retro_indicators %>% filter(retro %in% as.character(0:-5)), aes(year, value, group = retro, colour = retro)) + 
   geom_line(lwd = 1) + facet_grid(indicator~., scales = 'free') + theme_bw()+
   geom_ribbon(data = sumTab4retro %>% filter(indicator %in% c('ssb', 'rec', 'f')) %>% mutate(retro = '0'), 
               aes(year, ymin = lower, ymax = upper), col = 'black', alpha = 0.3)
  print(p_retroCI)
dev.off()
 
mohn_plot(retro_indicators, file = file.path(rep_wd,paste0("retro_peels", run, ".png")))
 
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summary plots:   ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (any(is.na(sumTab[nrow(sumTab),c("lowerrec","upperrec")]))) # for avoiding error
  sumTab[nrow(sumTab),c("lowerrec","upperrec")] <- sumTab[nrow(sumTab),"rec"]

png(file.path(rep_wd,paste0("summaryPlot", run, ".png")), width = 700, height = 500)
  summary_plot(sumTab)
dev.off()

 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #### Summary plots:   ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# png(file.path(rep_wd,paste0("ageClass_Contribution", run, ".png")), width = 500, height = 700)
#   par(mfrow = c(2,1))
#   barplot(contribution_ages_msy[-c(1,3),], main = 'Est. Recruitment', col = c('darkcyan', 'tomato'))
#   barplot(contribution_ages_msy_gm[-c(1,3),], main = 'Geometric Mean Recr.', col = c('darkcyan', 'tomato'))
# dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### YPR plots:   ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file.path(rep_wd,"YPRequilibrium.png"), width = 500, height = 700)
  par(mfrow = c(2,1))
  # YPR plot
  plot(0, type = "n", xlab = "F (1-7)", ylab = "YPR (t)", 
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
  plot(0, type = "n", xlab = "F (1-7)", ylab = "SPR (t)", 
       xlim = c(0, 0.5), 
       ylim = c(0, max(equil_yield[["SPR"]],  na.rm = TRUE)*1.05))
  lines(equil_yield[["F_report"]], equil_yield[["SPR"]], lwd = 2, lty = 1)
  abline(v = F01, col = "green", lwd = 2)
  abline(v = F35, col = "blue", lwd = 2)
  abline(v = F30, col = "lightblue", lwd = 2, lty = 2)
  abline(v = Fsq, col = "darkorange", lwd = 2)
  abline(v = Fmax, col = "red", lwd = 2)
  # legend("topright", bty = "n", lwd = 2, lty = c(rep(1,2),2, rep(1,2)), 
  #        col = c("green", "blue", "lightblue", "darkorange", "red"), legend = c("F0.1","F35%","F30%","Fsq","Fmax"))
dev.off()

