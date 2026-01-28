##----------------------------------------------------------------------
#     Write the necessary files to run SS3
#
#     The output is written to 'data' file: 
#         * starter.ss
#         * forecast.ss
#         * nhake-wg2X.ctl
#         * nhake-wg2X.dat
#   
# The control file need to be specified manually.
#
# Dorleta Garcia
# 2021/04/22
#   modified: 2022-04-21 13:19:30 (ssanchez@azti.es) - adapt for wgbie2022
#             2023-04-24 12:10:46 (ssanchez@azti.es) - adapt for wgbie2023
#-----------------------------------------------------------------------

require(r4ss)
require(dplyr)
require(ggplot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input & Output data files ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs <- 2013:dtyr
ass.yr <- as.numeric(substr(dtyr,3,4)) + 1

## SS3 input files names --
ss3_prev_dir  <- file.path(taf.boot.path("data"),"ss3_previous")
inp_ctrl_file <- paste0("nhake-wg",ass.yr-1,".ctl")
inp_data_file <- paste0("nhake-wg",ass.yr-1,".dat")
inp_fore_file <- "forecast.ss"
inp_star_file <- "starter.ss"

## Global indices input files names --
surv_dir         <- file.path(taf.boot.path("data"),"indices")
inp_evhoe_global <- paste0("fr_EVHOE_",dtyr,".csv")
inp_porc_global  <- paste0("sp_IBTS_",dtyr,".csv")
inp_igfs_global  <- paste0("ir_IGFS_",dtyr,".csv") 
inp_iams_global  <- paste0("ir_IAMS_",dtyr,".csv") 

## LFD indices input files names --
inp_evhoe_lfd  <- paste0("fr_EVHOE_lfdSex_",dtyr,".csv")
inp_porc_lfd   <- paste0("sp_IBTS_lfdSex_",dtyr,".csv")
inp_igfs_lfd   <- paste0("ir_IGFS_lfdSex_",dtyr,".csv")
inp_iams_lfd   <- paste0("ir_IAMS_lfdSex_",dtyr,".csv")
  
## SS3 output files names --
ss3_saly_dir <- file.path("data","ss3_saly")
ctrl_file    <- paste0("nhake-wg",ass.yr,".ctl")
data_file    <- paste0("nhake-wg",ass.yr,".dat")
fore_file    <- "forecast.ss"
star_file    <- "starter.ss"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Start Creating the files     ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### STARTER FILE ####
star <- SS_readstarter(file.path(ss3_prev_dir, inp_star_file))
star[['datfile']] <- data_file
star[['ctlfile']] <- ctrl_file
star[['init_values_src']] <- 0  # not using ss.par (in case it was set for selecting a specific jitter)
SS_writestarter(star, file = file.path(ss3_saly_dir, star_file), overwrite = TRUE)

#### FORECAST FILE ####
# No changes needed.
fore <- SS_readforecast(file.path(ss3_prev_dir, inp_fore_file))
SS_writeforecast(fore, file = file.path(ss3_saly_dir, fore_file), overwrite = TRUE)

#### CONTROL FILE ####
ctrl <- readLines(file.path(ss3_prev_dir, inp_ctrl_file))
ctrl_new <- ctrl
# The lines that have a year dependent input
ctrl_lines_yr <- which(apply(sapply(1978:(dtyr+5), function(x) grepl(x, ctrl)),1, any))
#edit(ctrl[ctrl_lines_yr])
# Lines that need to be edited:ctrl_lines_yr[c(2, 4:6, 20:27, 30:31)]
#edit(ctrl[ctrl_lines_yr[c(2, 4:6, 20:27, 30:31)]])
ctrl_edit <- ctrl_lines_yr[c(2, 4:6, 20:27, 30:31)]

#! Same as in retros function
ctrl_new[[ctrl_edit[1]]]  <- paste0(" -12 12 -1.82923 -0.56 0 0 6 0 23 1978 ",dtyr," 4 0 0 # RecrDist_GP_1_area_1_month_7")
ctrl_new[[ctrl_edit[2]]]  <- paste0(dtyr," # last year of main recr_devs; forecast devs start in following year")
ctrl_new[[ctrl_edit[3]]]  <- paste0(dtyr-1," #_last_yr_fullbias_adj_in_MPD")
ctrl_new[[ctrl_edit[4]]]  <- paste0(dtyr," #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)")
ctrl_new[[ctrl_edit[5]]]  <- paste0("             5            60       43.1905            15          0.01             0          2          0         23       1998       ",dtyr,"          3          0          0  #  Size_DblN_peak_SPTRAWL7(1)")
ctrl_new[[ctrl_edit[6]]]  <- paste0("         0.001            60       21.5299            27          0.01             0          4          0         23       1998       ",dtyr,"          3          0          0  #  Retain_L_infl_SPTRAWL7(1)") 
ctrl_new[[ctrl_edit[7]]]  <- paste0("             5            60       26.0866            35          0.01             0          2          0         23       1998       ",dtyr,"          3          0          0  #  Size_inflection_TRAWLOTH(2)")
ctrl_new[[ctrl_edit[8]]]  <- paste0("             1            50        14.649            27          0.01             0          4          0         23       1998       ",dtyr,"          3          0          0  #  Retain_L_infl_TRAWLOTH(2)")
ctrl_new[[ctrl_edit[9]]]  <- paste0("             5            30       15.6367            15          0.01             0          2          0         23       1998       ",dtyr,"          3          0          0  #  Size_DblN_peak_FRNEP8(3)")
ctrl_new[[ctrl_edit[10]]] <- paste0("            10            40       23.2954            27          0.01             0          4          0         23       1998       ",dtyr,"          3          0          0  #  Retain_L_infl_FRNEP8(3)")
ctrl_new[[ctrl_edit[11]]] <- paste0("             5            30       19.6159            15          0.01             0          2          0         23       1998       ",dtyr,"          3          0          0  #  Size_DblN_peak_SPTRAWL8(4)")
ctrl_new[[ctrl_edit[12]]] <- paste0("             1            40       12.5067            27          0.01             0          4          0         23       1998       ",dtyr,"          3          0          0  #  Retain_L_infl_SPTRAWL8(4)") 
ctrl_new[[ctrl_edit[13]]] <- paste0("             5            70        55.397            35          0.01             0          2          0         23       2013       ",dtyr,"          3          0          0  #  Size_inflection_NSTRAWL(8)") 
ctrl_new[[ctrl_edit[14]]] <- paste0("            10            60       55.6092            27          0.01             0          4          0         23       2013       ",dtyr,"          3          0          0  #  Retain_L_infl_NSTRAWL(8)")

#write the new control file, it overwrites the old one
writeLines(ctrl_new, con= file.path(ss3_saly_dir, ctrl_file))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA FILE ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(file.path("data","allocations","Total_Disc_ss3.RData"))
load(file.path("data","allocations","Total_Land_ss3.RData"))
load(file.path("data","allocations","ss3dat_LFD.RData"))

data_past <- SS_readdat_3.30(file.path(ss3_prev_dir, inp_data_file))

data <- data_past

data[["endyr"]] <- dtyr

## Landings observations and dimension ----
chist <- setNames(data_past[['catch']],c('year', 'seas', 'fleet', 'catch', 'catch_se'))
clast <- setNames(TotalLand_ss3, c('year', 'seas', 'fleet', 'catch', 'catch_se'))

data[['catch']] <- rbind( chist,
                          # dtyr (including also zeros)
                          left_join( expand.grid(year = dtyr, seas = 1:4, fleet = 1:9), clast %>% filter(year == dtyr)) %>% 
                            mutate(catch = ifelse(is.na(catch), 0, catch), 
                                   catch_se = ifelse(is.na(catch_se), 0.1, catch_se)) )

# #! CHECK consistency with historical data
# for (yr in unique(TotalLand_ss3$year)) {
#   d1 <- chist %>% filter(year == yr) %>% arrange(year, seas, fleet)
#   d2 <- clast %>% filter(year == yr) %>% arrange(year, seas, fleet)
#   d <- full_join(d1,d2,by=c('year','seas','fleet')) %>%
#     mutate(cdif = catch.x - catch.y, sedif = catch_se.x - catch_se.x)
#   d <- d %>% filter(!is.na(cdif) & cdif != 0)
#   if (nrow(d)>0) {
#     print(paste('------------------', yr, '------------------/n'))
#     print(d)
#     print('------------------------------------------/n')
#   }
# }
# NOTE: historically observed differences in 2013


## DISCARDS ---- 
#---------------
dhist <- setNames(data_past[['discard_data']],c('year', 'seas', 'fleet', 'discard', 'std_in')) # CV to Std_in (ok as mean = 0)
dlast <- setNames(TotalDisc_ss3, c('year', 'seas', 'fleet', 'discard', 'std_in'))

data[['discard_data']] <- rbind( dhist, 
                                 dlast %>% filter(year == dtyr)) # dtyr (not including zeros)

# #! CHECK consistency with historical data
# for (yr in unique(TotalDisc_ss3$year)) {
#   d1 <- dhist %>% filter(year == yr) %>% arrange(year, seas, fleet)
#   d2 <- dlast %>% filter(year == yr) %>% arrange(year, seas, fleet)
#   d <- full_join(d1,d2,by=c('year','seas','fleet')) %>%
#     mutate(ddif = round(discard.x - discard.y,0), sedif = round(std_in.x - std_in.x,0))
#   d <- d %>% filter(!is.na(ddif) & ddif != 0)
#   if (nrow(d)>0) {
#     print(paste('------------------', yr, '------------------/n'))
#     print(d)
#     print('------------------------------------------/n')
#   }
# }


## Abundance indices and number of observations ----
#---------------------------------------------------

### *EVHOE* ----
ev <- subset(data_past[['CPUE']], index == 10)
ev_past <- ev
ev <- rbind(ev, ev[1,])
ev[dim(ev)[1],'year']    <- dtyr

dt <- subset(read.csv(file.path(surv_dir, inp_evhoe_global),
                      sep = ";", dec = ","), Area == 'EVHOE' & Year %in% c(1997:2016, 2018:dtyr))
ev[,'obs']     <- dt$Average_number
ev[,'se_log']  <- dt$SD_Average_number/dt$Average_number # CV

# Compare the old and new indices.
df <- rbind(cbind(ev_past, type = 'old'), cbind(ev, type = 'new'))
p_ev <- ggplot(df, aes(year, obs, group = type, col = type)) + geom_line() +
  geom_ribbon(aes(ymin = obs*(1-2*se_log), ymax = obs*(1+2*se_log), fill = type), alpha = 0.3) + 
  ggtitle('EVHOE')
# => The indices are the same

## *RESGASQ Q1, Q2, Q3, Q4* ----
r1 <- subset(data_past[['CPUE']], index == 11)
r2 <- subset(data_past[['CPUE']], index == 12)
r3 <- subset(data_past[['CPUE']], index == 13)
r4 <- subset(data_past[['CPUE']], index == 14)

## *SP-PORCUPINE* ----
po <- subset(data_past[['CPUE']], index == 15)
po_past <- po
po <- rbind(po, po[1,])
po[dim(po)[1],'year']    <- dtyr 

dt <- read.csv(file.path(surv_dir, inp_porc_global)) %>% 
  mutate(Year = stringr::str_replace(Year, '\\*', ''))

po[,'obs']     <- dt[, 'Yst'] 
po[,'se_log']  <- dt[, 'SE']/dt[, 'Yst'] 

# Compare the old and new indices.
df <- rbind(cbind(po_past, type = 'old'), cbind(po, type = 'new'))
p_po <- ggplot(df, aes(year, obs, group = type, col = type)) + geom_line() +
  geom_ribbon(aes(ymin = obs*(1-2*se_log), ymax = obs*(1+2*se_log), fill = type), alpha = 0.3) + 
  ggtitle('PORCUPINE')
# => The indices are the same


# *IR-IGFS*
ig <- subset(data_past[['CPUE']], index == 16)
ig_past <- ig
ig <- rbind(ig, ig[1,])
ig[dim(ig)[1],'year']    <- dtyr 

dt <- read.csv(file.path(surv_dir, inp_igfs_global))

ig[,'obs']     <- dt[,'CatchNoskm2'] 
ig[,'se_log']  <- dt[,'CatchNosSe']/dt[,'CatchNoskm2']

# Compare the old and new indices.
df <- rbind(cbind(ig_past, type = 'old'), cbind(ig, type = 'new'))
p_ig <- ggplot(df, aes(year, obs, group = type, col = type)) + geom_line() +
  geom_ribbon(aes(ymin = obs*(1-2*se_log), ymax = obs*(1+2*se_log), fill = type), alpha = 0.3) + 
  ggtitle('IGFS')
# => The indices are the same


# *IR-IAMS*
ia <- subset(data_past[['CPUE']], index == 17)
ia_past <- ia
ia <- rbind(ia, ia[1,])
ia[dim(ia)[1],'year']    <- dtyr 

dt <- read.csv(file.path(surv_dir, inp_iams_global))

ia[,'obs']     <- dt[,'WtKgHkeKm2'] 
ia[,'se_log']  <- dt[,'se']/dt[,'WtKgHkeKm2']

# Compare the old and new indices.
df <- rbind(cbind(ia_past, type = 'old'), cbind(ia, type = 'new'))
p_ia <- ggplot(df, aes(year, obs, group = type, col = type)) + geom_line() +
  geom_ribbon(aes(ymin = obs*(1-2*se_log), ymax = obs*(1+2*se_log), fill = type, alpha = 0.3)) + 
  ggtitle('IAMS')
# => The indices are the same

# Join the 7 indices in one table and add them to the data list.
data[['CPUE']] <- rbind(ev, r1, r2, r3, r4, po, ig, ia) 


# **Length frequency data** ----
#-------------------------------
# Length classes:  data[['lbin_vector']]
#   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  42
#  44  46  48  50  52  54  56  58  60  62  64  66  68  70  72  74  76  78  80  82  84  86  88  90  92  94  96  98 100

lfd_hist <- data_past[['lencomp']] 

# Assumed same number of samples in all the years

## 1. Compile the LFD for surveys ----

### * evhoe: index:10 * ----
ev_lfd <- read.csv(file.path(surv_dir,inp_evhoe_lfd), sep = ";", dec = ",")
ev_lfd <- ev_lfd %>% filter(Year %in% c(1997:2016,2018:dtyr) & Area == 'EVHOE') %>% 
  select(Year, contains("_mm"))
ev_lfd <- ev_lfd %>% group_by(Year) %>% summarize_all(sum)
names(ev_lfd)[-1] <- as.numeric(substr(names(ev_lfd)[-1], 2, nchar(names(ev_lfd)[-1])-3))/10
ev_lfd <- ev_lfd[,-1]

lfd <- matrix(0, dim(ev_lfd)[1], data[["N_lbins"]]*2, 
              dimnames = list(c(1997:2016,2018:dtyr), paste0(rep(c('f','m'),each=data[["N_lbins"]]),data[["lbin_vector"]])))

lfd[,'f4'] <- rowSums(ev_lfd[,which(as.numeric(names(ev_lfd))<= 4)])
for(lc in 5:40) 
  lfd[,paste0('f',lc)] <- rowSums(ev_lfd[,which(as.numeric(names(ev_lfd))> lc-1 & as.numeric(names(ev_lfd))<= lc)])
for(lc in seq(42,98,2)) 
  lfd[,paste0('f',lc)] <- rowSums(ev_lfd[,which(as.numeric(names(ev_lfd))> lc-2 & as.numeric(names(ev_lfd))<= lc)])
lfd[,'f100'] <- rowSums(ev_lfd[,which(as.numeric(names(ev_lfd))> 98)])

# Check the total sum.
all(round(rowSums(lfd)/rowSums(ev_lfd),3) == 1)

ev_lfd_yr <- c(dtyr, as.numeric(subset(data[['lencomp']], FltSvy == 10)[1,2:6]), lfd[as.character(dtyr),])


### * spporc: index:15 * ---
po_lfd <-  read.csv(file.path(surv_dir,inp_porc_lfd))
names(po_lfd)[1] <- 'lng'
names(po_lfd)[-c(1:2)] <- gsub('X','',names(po_lfd[-c(1:2)]))

po_lfd_f <- po_lfd %>% filter(sex == "F") %>% select(-sex)
po_lfd_m <- po_lfd %>% filter(sex == "M") %>% select(-sex)

lfd <- matrix(0, dtyr-2001+1, data[["N_lbins"]]*2, 
              dimnames = list(2001:dtyr, paste0(rep(c('f','m'),each=data[["N_lbins"]]),data[["lbin_vector"]])))

lfd[,'f4'] <- colSums(po_lfd_f[which(po_lfd_f$lng <= 4),-1])
for(lc in 5:40) lfd[,paste0('f',lc)] <- colSums(po_lfd_f[which(po_lfd_f$lng > lc-1 &  po_lfd_f$lng <= lc),-1])
for(lc in seq(42,98,2))  lfd[,paste0('f',lc)] <- colSums(po_lfd_f[which(po_lfd_f$lng > lc-2 &  po_lfd_f$lng <= lc),-1])
lfd[,'f100'] <- colSums(po_lfd_f[which(po_lfd_f$lng > 98),-1])

lfd[,'m4'] <- colSums(po_lfd_m[which(po_lfd_m$lng <= 4),-1])
for(lc in 5:40) lfd[,paste0('m',lc)] <- colSums(po_lfd_m[which(po_lfd_m$lng > lc-1 &  po_lfd_m$lng <= lc),-1])
for(lc in seq(42,98,2))  lfd[,paste0('m',lc)] <- colSums(po_lfd_m[which(po_lfd_m$lng > lc-2 &  po_lfd_m$lng <= lc),-1])
lfd[,'m100'] <- colSums(po_lfd_m[which(po_lfd_m$lng > 98),-1])

# Check the total sum.
all(round(rowSums(lfd)/colSums(po_lfd %>% select(all_of(as.character(2001:dtyr)))),3) == 1)

po_lfd_yr <- c(Yr = dtyr, as.numeric(subset(data[['lencomp']], FltSvy == 15)[1,2:6]), lfd[dim(lfd)[1],])


## * irigfs: index:16 * ----
ig_lfd <- read.csv(file.path(surv_dir,inp_igfs_lfd))
ig_lfd <- ig_lfd %>% select(-se) %>% 
  group_by(LngtClassCm, Year) %>% summarise(freq = sum(CatchNoAtSex)) %>%
  tidyr::pivot_wider(names_from = LngtClassCm, values_from = freq) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(Year) %>% filter(Year %in% c(2003:dtyr))
ig_lfd <- ig_lfd[,-1]

lfd <- matrix(0, dtyr-2003+1, data[["N_lbins"]]*2, 
              dimnames = list(c(2003:dtyr), paste0(rep(c('f','m'),each=data[["N_lbins"]]),data[["lbin_vector"]])))


lfd[,paste0('f',4:40)] <- as.matrix(ig_lfd[,as.character(4:40)])
for(lc in seq(42,98,2)) 
  lfd[,paste0('f',lc)] <- rowSums(ig_lfd[,which(as.numeric(names(ig_lfd))> lc-2 & as.numeric(names(ig_lfd))<= lc)], na.rm = TRUE)
lfd[,'f100'] <- rowSums(ig_lfd[,which(as.numeric(names(ig_lfd))> 98)], na.rm = TRUE)

lfd[is.na(lfd)] <- 0

# Check the total sum.
all(round(rowSums(lfd)/rowSums(ig_lfd),3) == 1)

ig_lfd_yr <- c(Yr = dtyr, as.numeric(subset(data[['lencomp']], FltSvy == 16)[1,2:6]), lfd[as.character(dtyr),])


## * iriams: index:17 * ----
ia_lfd <- read.csv(file.path(surv_dir,inp_iams_lfd))
ia_lfd <- ia_lfd %>% select(-se) %>% 
  arrange(LenCm) %>% 
  tidyr::pivot_wider(names_from = LenCm, values_from = NumHkeKm2) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(Year) %>% filter(Year %in% c(2016:dtyr))

ia_lfd_f <- ia_lfd %>% filter(Sex == "F") %>% select(-(CruiseName:Sex))
ia_lfd_m <- ia_lfd %>% filter(Sex == "M") %>% select(-(CruiseName:Sex))

lfd <- matrix(0, dtyr-2016+1, data[["N_lbins"]]*2, 
              dimnames = list(c(2016:dtyr), paste0(rep(c('f','m'),each=data[["N_lbins"]]),data[["lbin_vector"]])))

for (lc in 4:40) if(lc %in% as.numeric(names(ia_lfd_f)))
  lfd[,paste0('f',lc)] <- t(ia_lfd_f[,which(as.numeric(names(ia_lfd_f)) == lc)])
for(lc in seq(42,98,2)) if(any(c(lc:(lc-1)) %in% as.numeric(names(ia_lfd_f))))
  lfd[,paste0('f',lc)] <- rowSums(ia_lfd_f[,which(as.numeric(names(ia_lfd_f))> lc-2 & as.numeric(names(ia_lfd_f))<= lc)], na.rm = TRUE)
lfd[,'f100'] <- rowSums(ia_lfd_f[,which(as.numeric(names(ia_lfd_f))> 98)], na.rm = TRUE)

for (lc in 4:40) if(lc %in% as.numeric(names(ia_lfd_m)))
  lfd[,paste0('m',lc)] <- t(ia_lfd_m[,which(as.numeric(names(ia_lfd_m)) == lc)])
for(lc in seq(42,98,2)) if(any(c(lc:(lc-1)) %in% as.numeric(names(ia_lfd_m))))
  lfd[,paste0('m',lc)] <- rowSums(ia_lfd_m[,which(as.numeric(names(ia_lfd_m))> lc-2 & as.numeric(names(ia_lfd_m))<= lc)], na.rm = TRUE)
lfd[,'m100'] <- rowSums(ia_lfd_m[,which(as.numeric(names(ia_lfd_m))> 98)], na.rm = TRUE)

# Check the total sum.
all(round(rowSums(lfd)/rowSums(ia_lfd_f + ia_lfd_m, na.rm=T),3) == 1)

ia_lfd_yr <- c(Yr = dtyr, as.numeric(subset(data[['lencomp']], FltSvy == 17)[1,2:6]), lfd[as.character(dtyr),])


## 2. LFD for fleets ----

# By SS3 definition: fleets = 6, 9 must have Part = 0 --> those with Part = 1 or 2 are set to Part = 0
lbs <- colnames(ss3dat)[-c(1:6)]
ss3dat_yr <- as_tibble(ss3dat) %>% filter(Yr == dtyr) %>%
  mutate(Part = ifelse(Yr %in% yrs & FltSvy %in% c(6,9), 0, Part)) %>%
  group_by(Yr, Seas, FltSvy, Gender, Part, Nsamp) %>% summarise_at(vars(one_of(lbs)), sum) %>%
  ungroup()

# ss3dat_yr %>% # CHECK for NAs
#   filter_at(vars(-Yr,-Seas,-FltSvy,-Gender,-Part,-Nsamp), any_vars(is.na(.)))

ss3dat_yr <- ss3dat_yr %>% arrange(Yr, FltSvy, Part, Seas)

colnames(ss3dat_yr) <- names(ev_lfd_yr) <- names(po_lfd_yr) <- names(ig_lfd_yr) <- names(ia_lfd_yr) <- names(data_past[['lencomp']])

data[['lencomp']] <- rbind(data_past[['lencomp']], ss3dat_yr, ev_lfd_yr, po_lfd_yr, ig_lfd_yr, ia_lfd_yr)

# change the season to the new coding 2.5, 5.5, 8.5 and 11.5.
data$lencomp$Seas <- recode(data$lencomp$Seas, `1`= 2.5, `2` = 5.5, `3` = 8.5, `4` = 11.5)

SS_writedat_3.30(data, outfile = file.path(ss3_saly_dir,data_file) , overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONSISTENCY OF INDICES ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

indices <- data[['CPUE']][,c(1,3:5)]
indices$index <- recode(indices$index, `10` = 'EVHOE', `11` = 'RESSGASCQ1', `12` = 'RESSGASCQ2', 
                        `13` = 'RESSGASCQ3', `14` = 'RESSGASCQ4', `15` = 'PORCUPINE', `16` = 'IGFS', `17` = 'IAMS')
names(indices)[4] <- 'cv'
indices <- as_tibble(indices) %>% group_by(index) %>% mutate(index_std = obs/mean(obs))

p_all <- ggplot(indices, aes(year, index_std, group = index, color = index)) + geom_line(linewidth = 1) +
  geom_point() + ggtitle('Abudance Indices')
  
pdf(file.path("data","plots","02_indices.pdf"), width = 10)
  print(p_ev); print(p_po); print(p_ig); print(p_ia)
  print(p_all)
dev.off()
  
