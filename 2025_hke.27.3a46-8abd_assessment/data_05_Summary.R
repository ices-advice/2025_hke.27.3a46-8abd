################################################################################
#  hke.27.3a46-8abd_assessment : summarise input data                          #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# data_05_Summarise.R - Summarise and save input data
# ~/*_hke.27.3a46-8abd_assessment/catch_indices.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

# Based on code from: Dorleta Garcia (dgarcia@azti.es) - AZTI - 09/05/2021


#==============================================================================
# DATA                                                                     ----
#==============================================================================

ass.yr <- as.numeric(substr(dtyr,3,4))+1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the ss3 data file using r4ss:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ss3dat <- SS_readdat_3.30(file.path("data", "ss3_saly", paste0("nhake-wg",ass.yr,".dat")))

lfd      <- as_tibble(ss3dat$lencomp) 
landings <- as_tibble(ss3dat$catch)
surveys  <- as_tibble(ss3dat$CPUE)
discards <- as_tibble(ss3dat$discard_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Length frequency distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Convert into length format and filter those with integer Nsamp that are the ones with 
# true sampling.
lbins <- substr( grep('^f[0-9]', names(lfd), value = TRUE), 2, 5)
lfd <- lfd %>% 
  pivot_longer(cols = c(paste0('f',lbins),paste0('m',lbins)), names_to = 'length', values_to = 'number') %>% 
  filter(Nsamp %in% c(50, 80, 125, 200)) %>% 
  mutate(Part = ifelse(Part == 2, 'landings', 'discards'), 
         FltSvy = plyr::mapvalues(FltSvy, 1:17, ss3dat$fleetnames[1:17]), 
         Gender = ifelse(grepl('m',length), 'M', 'F'), length = substr(length, 2, 5))

names(lfd) <- c('year', 'season', 'fleet', 'gender', 'category', 'weight', 'length', 'number')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Landings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#landings <- landings %>% pivot_longer(cols= SPTRAWL7:OTHERS, names_to = 'fleet', values_to = 'tons') 
names(landings)[1:4] <- c('year', 'season', 'fleet', 'tons')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Discards
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
discards <- discards %>% mutate(Flt = plyr::mapvalues(Flt, c(1:9), 
                                                      ss3dat$fleetnames[c(1:9)])) %>% filter(Yr > 0)
names(discards) <- c('year', 'season', 'fleet', 'tons', 'std')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Surveys
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
surveys <- surveys %>%  mutate(index = plyr::mapvalues(index, 10:17, ss3dat$fleetnames[10:17])) 

write.taf(list(surveys = surveys, discards = discards, landings = landings, lfd = lfd), dir = 'data')

