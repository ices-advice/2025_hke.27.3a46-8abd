#------------------------------------------------------------------------------------------------------------------------
#
#       ----------------------------------------------------------------------------------------------    
#       |   Script used to import the data from intercatch and create the input data file for SS3.   |
#       |   INPUT :: 'Table 1' and 'Table 2' from intercatch in csv format.                          |
#       |   OUTPUT::                                                                                 |  
#       |       - 'zaux_Allocations_info.txt' some info with the allocations.                        |
#       |       - 'zaux_ICData_Allocations.RData' RData file with intermediate data to creat the SS3 |
#       |               input data.                                                                  |
#       |                                                                                            |
#       ----------------------------------------------------------------------------------------------  
#
#
# Dorleta Garcia
#  * 11/04/2019
#  * 18/02/2020 - prepare for TAF
#  * 28/04/2020 - TAF philosophy to prepare WGBIE dta.
#  * 19/04/2021 - 
#   modified: 2022-04-21 13:19:30 (ssanchez@azti.es) - adapt given WKANGHAKE2022 for wgbie2022
#             2023-04-25 08:36:18 (ssanchez@azti.es) - update for WGBIE2023
#
# ** From 2016 the allocation of LFD and the raising is done in this script. The allocations and raisings in 
#    Table 2 are useless, they've been done using a fake allocation.
#
# ** From 2018 (2017 data) we consider SSC as trawl, before it was as MSC, we have taken this decision because now there is some SSC in BoB and we 
#   need to include in TRW, GLN or LLN. The best match is TRW,. In 2017 overall the catch was 1200tons, almost all in BoB.
#   In 2016 data there were ~800t of SSC in VII, so it maybe not accounted!! Need to revise 2017 and previous years!!!
#
# ORIGINAL FILE: "Export Intercatch - WG14.R" stored in 'C:\use\ICES\WGBIE\WG14\NHKE\Data\InterCatch'.
#
#   * table1 :: original table with relevant columns and all years.
#   * table1a :: catch by year/country/category.
#   * table1b :: catch by year/country/area/category.
#   * table1c :: catch by year/country/area/fleet/category.
#   * table1d :: catch by year/season/area/fleet/category. (fleet aggregated in OUT to export into IC)
#
#   * table2 ::  original table with relevant columns and all years. lng/fleet/area aggregated to the levels defined.
#   * table2a ::
#-------------------------------------------------------------------------------
require(ggplot2)
require(dplyr)
# require(gtools) # odd fun
require(ggridges)
require(r4ss)

odd          <- gtools::odd
pivot_longer <- tidyr::pivot_longer

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


#~~~~~~~~~~~~~~~~~~
####   INPUT   #### 
#~~~~~~~~~~~~~~~~~~

yrs  <- 2013:dtyr
nyr  <- length(yrs)

# length-weight relationship parameters (SS3: weight = kg, length = cm). UPDATE THESE VALUES!!
a_vec <- setNames(rep(3.77e-06*1000, nyr), yrs) # in grams -> grams/1e+06 = tons
b_vec <- setNames(rep(3.16826, nyr), yrs) 

# Previous wg data
wg0_data_file <-  file.path(taf.data.path(), "ss3_previous", paste0("nhake-wg",substr(dtyr,3,4),".dat"))
dat_wg0 <- SS_readdat_3.30(wg0_data_file)

L_wg0   <- as_tibble(dat_wg0$catch) %>% filter(year %in% yrs)
D_wg0   <- as_tibble(dat_wg0$discard_data) %>% filter(Yr %in% yrs)
names(L_wg0) <- names(D_wg0) <- c('year', 'season', 'fleet', 'catch', 'sd')
lfd_wg0 <- as_tibble(dat_wg0$lencomp) 

# Previous year data
tab1 <- file.path(catd_wd, "Table_1.RData") # It has all the years.
tab2 <- file.path(catd_wd, "Table_2.RData")

load(tab1)
load(tab2)

# In some fleets we have annual samples, that represent a low percentage (higher in the case of the OTHERS fleet), 
# we don't consider these samples.
tab2 <- tab2 %>% filter(type == 'sampled', season %in% 1:4)


# In tab1; 
# * Total landings are pretty much the same we have in the advice sheet => we need to ensure both are the same!! 
# * Total discards: Big difference in 2018: 7038 vs 6493 in the advice sheet!! The difference seems general in all the areas!!
#      But in the SS data the number is 7034, so we assume something is wrong in the script that generates de table!
# Check through the script that we don't loose catch in comparison with tab1
#  Do the conversion to SS fleet here so it can be use semi-directly with IC output.
# Start comparing tab1 total landings and discards with data used in WG.
Ltot_wg0 <- L_wg0 %>% group_by(year) %>% summarize(catch = sum(catch))
Dtot_wg0 <- D_wg0 %>% group_by(year) %>% summarize(catch = sum(catch))
Ltot     <- tab1 %>% filter(category == 'Landings') %>% group_by(year) %>% summarize(caton = sum(caton))
Dtot     <- tab1 %>% filter(category == 'Discards') %>% group_by(year) %>% summarize(caton = sum(caton))
Ctot     <- tab1 %>% group_by(year) %>% summarize(caton = sum(caton))

# comparisons
Ltot_prev <- Ltot %>% filter(year != dtyr)
round(Ltot_prev$caton/Ltot_wg0$catch,2)
# The difference in 2013 is due to Spanish unallocated.

# Discard raising information
dat.disc_file <- file.path( taf.data.path(), "discard_raising", "caton_summary.csv")


#~~~~~~~~~~~~~~~~~~
####   OUTPUT  #### 
#~~~~~~~~~~~~~~~~~~
allocations_file      <- file.path( aloc_wd, "zaux_Allocations_info.txt")
rdata                 <- file.path( aloc_wd, "LFD_totC.RData")
dif_caton_canum_file  <- file.path( aloc_wd, "zaux_Check_SOP.csv")
Total_Disc_ss3        <- file.path( aloc_wd, "Total_Disc_ss3.RData")
Total_Land_ss3        <- file.path( aloc_wd, "Total_Land_ss3.RData")
ss3dat_LFD            <- file.path( aloc_wd, "ss3dat_LFD.RData")
IC_to_SS3             <- file.path( aloc_wd, "zaux_IC_to_SS3.RData")
discr_file            <- file.path( aloc_wd, "All_Disc_ss3.RData")


# Save all plots in a pdf file
pdf(file.path("data","plots","01_allocations.pdf"), width = 10)

# LFD -  How do we raise the LFD when the fleet is heterogeneous, like TRAWLOTH and NSTRAWL & OTHER fleet???
# If we assume that the number of samples in a metier/strata is proportional to the 
# importance of this strata in the fishery. 
# We can combine the samples in an heterogeneous fleet summing up all of them and calculating proportions.
# In TRAWLOTH we do the raising as if they were 2 fleet, TRAWLOTH_CRU & TRAWLOTH_DEF and afterwards we merge the two fleets.

# When in Tab1 we have annual catches (i.e. season == "all"), we divide them by season using the mean in the corresponding fleet, so we can automate the process.

# # Some data in area 8,7 wrong assigned to OTHERS => assign it to TRAWLOTH
# tab1 <- tab1 %>% mutate(ss3_fl = ifelse(substr(ss3_fl, 1,6) == 'OTHERS' & substr(area, 4,4)>6, 'TRAWLOTH_DEF', ss3_fl)) %>% filter(country != 'Third unknown country')
# tab2 <- tab2 %>% mutate(ss3_fl = ifelse(substr(ss3_fl, 1,6) == 'OTHERS' & substr(area, 4,4)>6, 'TRAWLOTH_DEF', ss3_fl))

LFD  <- NULL
totC <- NULL

for(yr in yrs){ 
  cat('\n ************************************************************************\n *** YEAR = ', yr,'\n')
  a <- a_vec[as.character(yr)]
  b <- b_vec[as.character(yr)]
  
  table1 <- tab1 %>% filter(year == yr, caton > 0) %>% 
    select('country', 'year', 'category', 'repcategory', 'div', 'season', 'fleet', 'caton', 'type', "ss3_fl", "NLengthSamples", "NLengthMeasured") 
  
  names(table1)[c(3,4,5,7,9)] <- c('category', 'reported', 'div', 'metier', 'type')
  
  table1 <- table1 %>% mutate(area = ifelse(!div %in% c("27.7", "27.8"), 'out', as.numeric(substr(div, 4,4))))
  
  # year-country-category
  table1a <- aggregate(list(caton = table1$caton), list(year = table1$year, country = table1$country, category = table1$category), sum)
  # year-country-area-category
  table1b <- aggregate(list(caton = table1$caton), list(year = table1$year, country = table1$country, area = table1$area, category = table1$category), sum)
  # year-country-area-fleet-category
  table1c <- aggregate(list(caton = table1$caton), list(year = table1$year, country = table1$country, area = table1$area, metier = table1$metier, category = table1$category), sum)
  # year-season-area-ss3_fl-category
  table1d <- aggregate(list(caton = table1$caton), list(year = table1$year, season = table1$season, area = table1$area, ss3_fl = table1$ss3_fl, category = table1$category), sum)
  
  # There is some data (spanish 'unallocated' among them without quarter data), to assign a quarter we divide the 
  # data in the same proportion observed in the corresponding group.
  table1da <- table1d[table1d$season != "all",]
  table1db <- table1d[table1d$season == "all",]
  table1dc <- table1da
  table1dc[, 'caton'] <- 0
  
  for(category in c('Landings', 'Discards')){
    for(area in unique(table1db$area)){
      for(fleet in unique(table1db$ss3_fl)){
        p <- table1da[table1da$area == area & table1da$category == category & table1da$ss3_fl == fleet, 'caton']
        p <- p/sum(p)
        
        agg <- table1db[table1db$area == area & table1db$category == category & table1db$ss3_fl == fleet, 'caton']
        
        if(length(agg)==0) next
        
        table1dc[table1dc$area == area & table1dc$category == category & table1dc$ss3_fl == fleet, 'caton'] <- p*agg
      }}}
  
  flag1 <- round(sum(table1dc$caton),10) == round(sum(table1db$caton), 10)
  flag2 <- round(sum(table1dc$caton) - sum(table1db$caton), 10)
  
  if(flag1 == TRUE & flag2 == 0) cat('---------------------------------------------------\n Check: Up to line 164 everything OK! \n ---------------------------------------------------\n')
  if(flag1 != TRUE | flag2 != 0) stop('Something went wrong: Line 151!')
  
  # Add a column 'included', if TRUE the row has been included in a SS3 fleet if not, it has not and will not go into the assessment!
  table1 <- table1 %>%  mutate(included = FALSE) 
  
  aggregate(caton~category,table1,sum)
  
  
  tC <- table1 %>% group_by(category, metier) %>% summarise(caton = sum(caton), dplyr.summarise.inform = FALSE)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####  LOAD TABLE 2  :::  Landings and discards at length #### 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  table2 <- as_tibble(read.csv(tab2_nm)[,c(2:4,8:9,11:12,16,18, 20)])
  table2 <- tab2 %>% filter(year == yr)
  names(table2) <- tolower(names(table2))
  names(table2)[c(11,18)] <- c('metier', 'lng')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### ADD LFD in weight (tons)  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if weight at length is available we use the value otherwise we use the modeled value.
  
  table2 <- table2 %>% mutate(canum_w =  ifelse(!is.na(weca), weca*canum/1e6, (a*(lng/10)^b)*canum/1e6)) # 1000 gr => kg, 1000 numb => miles
  
  cat('Percentage sampled: ', round(sum(table2$canum_w)/sum(table1$caton),2)*100, '%\n')       
  
  # Differences between caton and canum in weight.
  if(yr == 2013){
    cat('------------------------------------------------------------------------\n', file = dif_caton_canum_file)
    cat('----------------------- ', yr, ' ------------------------------------\n', file = dif_caton_canum_file, append = TRUE)
    cat('------------------------------------------------------------------------\n', file = dif_caton_canum_file, append = TRUE)
  } else {
    cat('------------------------------------------------------------------------\n', file = dif_caton_canum_file, append = TRUE)
    cat('----------------------- ', yr, ' ------------------------------------\n', file = dif_caton_canum_file, append = TRUE)
    cat('------------------------------------------------------------------------\n', file = dif_caton_canum_file, append = TRUE)
  }
  
  cat('Differences between caton and canum in weight.\n\n', file = dif_caton_canum_file, append = TRUE)
  cat('caton: ', sum(table2$canum_w), '\n',file = dif_caton_canum_file, append = TRUE)
  cat('canum in weight useing LW rel.: ', sum(table1$caton),'\n', file = dif_caton_canum_file, append = TRUE)
  cat('Ratio: ', sum(table2$canum_w)/sum(table1$caton), '\n\n',file = dif_caton_canum_file, append = TRUE)
  
  sum(table2$canum_w)/sum(table1$caton)
  
  # By country and metier check if the information stored as sampled in tab1 is the same as in tab2.
  table1_sam <- subset(table1, type == 'sampled')
  for(ct in unique(table1_sam$country)){
    x1 <- subset(table1_sam, country == ct)
    x2 <- subset(table2, country == ct)
    for(mt in unique(as.character(x1$metier))){
      x11 <- subset(x1, metier == mt)
      x21 <- subset(x2, metier == mt)
      cat(ct, ', ', mt, ', ', round(sum(x21$canum_w)/sum(x11$caton),2), '\n',file = dif_caton_canum_file, append = TRUE)
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### TABLE 2: LFD ALLOCATION & RAISING   ####
  #
  #          o allocations_file: file with information on allocation scheme and data.
  #         o SPTRAWL7, SPTRAWL8 & FRNEP8: Use only 'season' to perfrom the raising, 
  #         o Use all the data available and weighting factor: caton.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(yr == 2013){
    cat('File with information on the allocation scheme and data\n', file = allocations_file)
    cat('------------------------------------------------------------------------\n', file = allocations_file)
    cat('----------------------- ', yr, ' ------------------------------------\n',    file = allocations_file, append = TRUE)
    cat('------------------------------------------------------------------------\n', file = allocations_file, append = TRUE)
  } else {
    cat('------------------------------------------------------------------------\n', file = allocations_file, append = TRUE)
    cat('----------------------- ', yr, ' ------------------------------------\n',    file = allocations_file, append = TRUE)
    cat('------------------------------------------------------------------------\n', file = allocations_file, append = TRUE)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####  CALCULATE LFD-s #### 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  for(fl in unique(tab1$ss3_fl)){
    
    cat('\n** ', fl, '**\n', file = allocations_file,  append = TRUE )
    
    fl_tot <- table1 %>% filter(ss3_fl == fl)
    fl_lfd <- table2 %>% filter(ss3_fl == fl)
    
    
    # Remove the observations with season equal to year or all and divide it along the seasons 
    # according to the observed proportion. 
    # totC will be the data frame used to generate SS total Landings and discards file
    totC.aux <- fl_tot %>% filter(season %in% 1:4) %>% group_by(year, category, season, ss3_fl) %>% 
                              summarize_at(c('caton', "NLengthSamples", "NLengthMeasured"), sum) %>% 
                              ungroup() %>% group_by(year, category,  ss3_fl) %>% 
                              mutate(prop = caton/sum(caton))
    # The data with season = all or yr 
    totC.all  <- fl_tot %>% filter(!(season %in% 1:4)) %>% group_by(year, category, season, ss3_fl) %>% 
      summarize_at(c('caton', "NLengthSamples", "NLengthMeasured"), sum) 
    
    if(dim(totC.all)[1] > 0){
      for(cat in unique(totC.all$category)){
        totC.aux[totC.aux$category == cat, 'caton'] <- totC.aux[totC.aux$category == cat, 'caton'] + 
                                                       totC.aux[totC.aux$category == cat, 'prop']*totC.all[totC.all$category == cat, ][['caton']] 
      }
    }
    
    # Anotate that this fleet has been processed. 
    table1 <- table1 %>%  mutate(included = ifelse(ss3_fl == fl, TRUE, included))
    
    # Proportion of sampled landings and discards.
    pLandSampled <- sum(filter(fl_tot, type == 'sampled', category == 'Landings')$caton)/sum(filter(fl_tot, category == 'Landings')$caton)
    pDiscSampled <- sum(filter(fl_tot, type == 'sampled', category == 'Discards')$caton)/sum(filter(fl_tot, category == 'Discards')$caton)
    pLandSampled
    pDiscSampled
    
    samplingInfo <- table(fl_tot$season, fl_tot$type, fl_tot$category)
    samplingInfo
    
    if("Discards" %in% dimnames(samplingInfo)[[3]]){
      nonsamp_seasons <- list(Discards = which(samplingInfo[,1,'Discards']!=0), Landings = which(samplingInfo[,1,'Landings']!=0))
    } else {
      nonsamp_seasons <- list(Discards = NA, Landings = which(samplingInfo[,1,'Landings']!=0))
    }
    
    cat(round(pLandSampled*100),'% of the landings and ', round(pDiscSampled*100), '% of the discards are sampled\n
      ', file = allocations_file, append = TRUE)
    write.table(samplingInfo,  quote = FALSE, row.names = FALSE, col.names = FALSE, file = allocations_file, append = TRUE )
    
    fl_lfd <- fl_lfd  %>%  group_by(category, season, lng) %>% 
                           summarise(canum_w = sum(canum_w), canum = sum(canum)) %>% 
                           ungroup() %>% group_by(season, category) %>% 
                           mutate(canum_p = canum/sum(canum), type = 'sampled')
    
    # Checks
    sum(fl_lfd$canum_w)/sum(fl_tot$caton)
    
    aux1 <- fl_tot %>% group_by(category) %>% summarise(caton = sum(caton))
    aux2 <- table1 %>% filter(ss3_fl == fl) %>% group_by(category) %>% summarise(caton = sum(caton))
    
    if(any(round(aux1$caton/aux2$caton,2) != 1)) stop('Something went wrong!!')
    
    
    fl_lfd <- fl_lfd %>% mutate(fleet = fl, year = yr)
    LFD <- rbind(LFD, fl_lfd)
    totC <- rbind(totC, totC.aux)
    
    totD.aux <- sum((totC.aux %>% filter(category == 'Discards'))$caton)
    totL.aux <- sum((totC.aux %>% filter(category == 'Landings'))$caton)
    fl_Dtot <- sum((fl_tot %>% filter(category == 'Discards'))$caton)
    fl_Ltot <- sum((fl_tot %>% filter(category == 'Landings'))$caton)
    
    aux <- ifelse(is.na(round(totD.aux/fl_Dtot,2)), 1, round(totD.aux/fl_Dtot,2))
    
    # Print a message if the discards at the beginning for the fleet fl are not the same as in the end after the calculations.
    # If nothing is printed they are equal.
    if(aux != 1 | round(totL.aux/fl_Ltot,2) != 1){
      cat('********************************************************\n')
      cat('Year: ', yr, ', fleet: ', fl, '\n')
      cat('final check: discards', round(totD.aux/fl_Dtot,2), '\n')
      cat('final check: landings', round(totL.aux/fl_Ltot,2), '\n')
      cat('********************************************************\n')
    }
    
  }
}
names(totC)[4] <- 'fleet'


#### Convert into SS format ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~
## OUTPUT ----
#~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------------
# Join TRAWLOTH_DEF & TRAWLOTH_CRU fleets data generated in the previous scripts.
# We join them using a weighted mean of the total landing and total discards in 
# each fleet.
#-------------------------------------------------------------------------------
# TRAWLOTH
p1 <- ggplot(LFD %>% filter(fleet == 'TRAWLOTH_DEF', category == 'Discards'& type == 'sampled'), aes(lng/10, canum_p, group = (year), colour = (year))) +
  geom_line() + facet_wrap(~season,ncol = 2)
p2 <- ggplot(LFD %>% filter(fleet == 'TRAWLOTH_CRU', category == 'Discards'& type == 'sampled'), aes(lng/10, canum_p, group = (year), colour = (year))) +
  geom_line() + facet_wrap(~season,ncol = 2)
p3 <- ggplot(LFD %>% filter(fleet == 'TRAWLOTH_DEF', category == 'Landings'& type == 'sampled'), aes(lng/10, canum_p, group = (year), colour = (year))) +
  geom_line() + facet_wrap(~season,ncol = 2)
p4 <- ggplot(LFD %>% filter(fleet == 'TRAWLOTH_CRU', category == 'Landings'& type == 'sampled'), aes(lng/10, canum_p, group = (year), colour = (year))) +
  geom_line() + facet_wrap(~season,ncol = 2)

print(p1); print(p2); print(p3); print(p4)

totC %>% filter(fleet %in% c('TRAWLOTH_DEF', 'TRAWLOTH_CRU'))
LFD  %>% filter(fleet %in% c('TRAWLOTH_DEF', 'TRAWLOTH_CRU'))


# Add caton in totC to LFD to do a weighted mean later in TRAWLOTH fleet.
totC <- totC %>% mutate(season = as.numeric(season)) # to join variables need to be of the same class.

nfl <- totC %>% filter(fleet %in% c('TRAWLOTH_DEF', 'TRAWLOTH_CRU')) %>%
  mutate(nfl = ifelse(round(caton,0) > 0, 1, 0)) %>%
  mutate(fleet = 'TRAWLOTH') %>%
  group_by(year, category, season, fleet) %>% summarise(nfl = sum(nfl)) %>%
  # correction: as no LFD distribution available for 'TRAWLOTH_CRU' landings in 2014_4
  mutate(nfl = ifelse(fleet =='TRAWLOTH' & category == 'Landings' & year == 2014 & season == 4, 1, nfl))

LFD <- LFD %>% group_by(category, season, fleet, year) %>% left_join(totC[,1:5]) %>%
  mutate(canump_caton = canum_p*caton,
         fleet = ifelse(fleet %in% c('TRAWLOTH_DEF', 'TRAWLOTH_CRU'), 'TRAWLOTH', fleet),
         canum_p = canump_caton/sum(canump_caton)) %>%
  left_join(nfl, by = c("year","category","season",'fleet')) %>%
  mutate(nfl = ifelse(is.na(nfl), 1, nfl), 
         nfl = ifelse(fleet == 'TRAWLOTH', nfl/2, nfl), # NOTE: correction for sum(canum_p) == 1 by fleet, yr, ss, cat
         canum_p = canum_p/nfl) %>%
  select(year, season, fleet, category, lng, canum_p)
names(LFD)[6] <- 'prop'


LFD <- LFD %>% mutate(lng = round(lng/10)) %>% mutate(lng =  ifelse(lng < 3, 4, ifelse(lng > 100, 100, lng))) %>% 
  mutate(lng = ifelse(lng <= 40, lng, ifelse(odd(lng), lng + 1, lng))) %>% 
  group_by(year, category, season, fleet, lng) %>% summarise(prop = sum(prop))

# CHECK if any not 1
LFD %>% group_by(year, category, season, fleet) %>% summarise(prop = sum(prop)) %>% filter(round(prop,8) != 1)

save(LFD, totC, file = rdata)

p1 <- ggplot(totC, aes(year + (season-1)*0.25, NLengthSamples, fill = factor(season))) + 
  geom_bar(stat = 'identity') + facet_grid(fleet~category)
p2 <- ggplot(totC, aes(year + (season-1)*0.25, NLengthMeasured, fill = factor(season))) + 
  geom_bar(stat = 'identity') + facet_grid(fleet~category)
p3 <- ggplot(totC, aes(year + (season-1)*0.25, NLengthSamples, fill = factor(season))) + 
  geom_bar(stat = 'identity') + facet_grid(fleet~category, scales = 'free')
p4 <- ggplot(totC, aes(year + (season-1)*0.25, NLengthMeasured, fill = factor(season))) + 
  geom_bar(stat = 'identity') + facet_grid(fleet~category, scales = 'free')

print(p1); print(p2); print(p3); print(p4)

# Check that we have not lost catch from tab1 to totC.
# In 2013 is normal to have differences due to the Spanish non allocated 'Third unknown country'.
# For the rest it should be the same.
Ltot_comp <- totC %>% filter(category == 'Landings') %>% group_by(year) %>%  summarize(caton = sum(caton))
Dtot_comp <- totC %>% filter(category == 'Discards') %>% group_by(year) %>%  summarize(caton = sum(caton))

# Compare initial tab1 values with values after calculations.
round(Ltot$caton/Ltot_comp$caton,2) # 2013: > 1
round(Dtot$caton/Dtot_comp$caton,2)

# Compare values after calculations with values in the assessment. Diffs in 2013 not comparable.
round(Ltot_wg0$catch/(Ltot_comp %>% filter(year != dtyr) %>% .$caton),2) # NOTE: SS3 catches correspond to landings



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  CONVERT lfd INTO SS3 LFD FORMAT  ####
#  ss3dat <- matrix(0, 4*nf+4*4, 6 +  length(c(4:40, seq(42,100,2))), dimnames = list(NULL, c('yr', 'seas', 'flt', 'zero', 'cat', 'sampsize', c(4:40, seq(42,100,2)))))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ss3dat <- NULL

sampsize <- paste(rep(sort(unique(LFD$fleet)),2), rep(c('Discards', 'Landings'), each = nyr), sep = "_")
fleetNumb <- setNames(c(1:6,8,9), c("SPTRAWL7", "TRAWLOTH", "FRNEP8", "SPTRAWL8", "GILLNET", "LONGLINE",  "NSTRAWL", "OTHERS"))

lenc <- c(4:40, seq(42,100,2))
nlen <- length(lenc)

for(fl in unique(LFD$fleet)){
  for(cat in c('Landings', 'Discards')){
    for(yr in yrs){
      
      aux <- LFD %>%  filter(category == cat, fleet == fl, year == yr)
      
      seas <- unique(aux$season)
      
      ss3dat_temp <- matrix(0, length(seas), 6 + nlen * 2, 
                            dimnames = list(NULL, c('Yr', 'Seas', 'FltSvy', 'Gender', 'Part', 'Nsamp', #'yr', 'seas', 'flt', 'zero', 'cat', 'sampsize', 
                                                    paste0(rep(c('f','m'),each=nlen),lenc))))
      
      nsamp <-  subset(dat_wg0$lencomp, FltSvy == ifelse(fleetNumb[fl] < 10, fleetNumb[fl], 10)  & 
                         Part == ifelse(cat == 'Landings', 2, 1) & 
                         Yr == yr)$Nsamp[1]
      
      ss3dat_temp[, 'Yr']     <- yr 
      ss3dat_temp[, 'Seas']   <- seas
      ss3dat_temp[, 'FltSvy'] <- fleetNumb[fl]
      ss3dat_temp[, 'Gender'] <- 0
      ss3dat_temp[, 'Part']   <- ifelse(cat == 'Landings', 2, 1)
      ss3dat_temp[, 'Nsamp']  <- ifelse(is.na(nsamp), 125, nsamp) #! fixed at the moment at 125 for all cases
      
      for(ss in seas){
        aux <- LFD %>%  filter(category == cat, fleet == fl, year == yr, season ==ss)
        lng  <- aux[['lng']][aux[['lng']] >3]
        prop <- aux[['prop']][aux[['lng']] >3]
        ss3dat_temp[ss3dat_temp[,'Seas'] == as.numeric(ss), paste0('f',lng)] <- prop
      }
      ss3dat <- rbind(ss3dat, ss3dat_temp)
    }
  }
}

save(ss3dat, file = ss3dat_LFD)

save(LFD, totC, ss3dat, file = IC_to_SS3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total Landings by fleet ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
totC <- totC[,1:5] # remove one of the 'fleet' columns.
TotalLand_ss3 <- totC %>% group_by(year, season, fleet, category) %>% 
                          mutate(fleet = ifelse(fleet %in% c("TRAWLOTH_DEF", "TRAWLOTH_CRU"),"TRAWLOTH", fleet)) %>% 
                          summarise(caton = round(sum(caton))) %>% #! rounding issue in sumTab --> TO DELETE rounding
                          mutate(fleet = recode(fleet, SPTRAWL7 = 1, TRAWLOTH = 2, FRNEP8 = 3, SPTRAWL8 = 4, GILLNET = 5, LONGLINE = 6, 
                                                NSTRAWL = 8, OTHERS = 9), CV = 0.1) %>% 
                          filter(category == 'Landings') %>% select(year, season, fleet, caton, CV) %>% 
                          arrange(year, fleet, season)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####                       *** Total Discards ***    ####
# In 2019 data:
# No discards in sp7-trawlers in season 2 & 4. 
# No discards in gillnetters in season 3. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TotalDisc_ss3 <- totC %>% group_by(year, season, fleet, category) %>% 
                 mutate(fleet = ifelse(fleet %in% c("TRAWLOTH_DEF", "TRAWLOTH_CRU"),"TRAWLOTH", fleet)) %>% 
                 summarise(caton = sum(caton)) %>% 
                 ungroup() %>% 
                 mutate(fleet =recode(fleet, SPTRAWL7 = 1, TRAWLOTH = 2, FRNEP8 = 3, SPTRAWL8 = 4, GILLNET = 5, LONGLINE = 6, 
                                      NSTRAWL = 7, OTHERS = 8),  CV = 0.5, 
                        month = (as.numeric(season)-1)*3 + 2.5) %>% 
                 filter(category == 'Discards') %>% select(year, month, fleet, caton, CV) %>% arrange(year, fleet, month)

TotalDisc_ss3_ini <- TotalDisc_ss3

# Do the raising of discards following Claire Moore's approach

dat.disc <- as_tibble(read.csv(dat.disc_file))

# Check if the landings in Intercatch before 2012 are lower than in the model because the datacall for those years was only done
# for discards to reconstruct them in 2014 benchmark that was specially done for raising discards.
tot_dt <- dat.disc %>% group_by(year) %>% summarize_at(c('landings', 'discards'), sum)

# Add SS fleet to dat.disc.
dat.disc <- dat.disc %>% mutate(area = as.numeric(substr(subArea, 4,4)),
                                fl_ss = case_when((substr(fleet, 1,3) %in% c('OTB', 'PTB') & country == 'Spain' & area == 8) ~ 'SPTRAWL8',
                                                  (substr(fleet, 1,3) %in% c('OTB', 'PTB') & country == 'Spain' & area == 7) ~ 'SPTRAWL7',
                                                  (fleet %in% c('OTB_CRU', 'OTT_CRU', 'TBB_CRU') & country == 'France' & area == 8) ~ 'FRNEP8',
                                                  (substr(fleet, 1,3) %in% c('OTB', 'PTB', 'OTT', 'OTM', 'PTM', 'TBB') & area %in% 7:8) ~ 'TRAWLOTH',
                                                  (substr(fleet, 1,3) %in% c('OTB', 'PTB', 'OTT', 'OTM', 'PTM', 'TBB') & !(area %in% 7:8)) ~ 'NSTRAWL',
                                                  (substr(fleet, 1,3) %in% c("GNS", "GTR") & area %in% 7:8) ~ 'GILLNET',
                                                  (substr(fleet, 1,3) %in% c("LLS") & area %in% 7:8) ~ 'LONGLINE',
                                                  TRUE ~ 'OTHERS'),
                                fl_ss_numb = case_when(fl_ss == 'SPTRAWL7' ~ 1, fl_ss == 'TRAWLOTH' ~ 2, fl_ss == 'FRNEP8' ~ 3, 
                                                       fl_ss == 'SPTRAWL8' ~ 4, fl_ss == 'GILLNET' ~ 5, fl_ss == 'LONGLINE' ~ 6,
                                                       fl_ss == 'NSTRAWL' ~ 8, fl_ss == 'OTHERS' ~ 9),
                                season = case_when(Season == 1 ~ 2.5, Season == 2 ~ 5.5, Season == 3 ~ 8.5, Season == 4 ~ 11.5))

discr_SS3 <- dat.disc %>% filter(fl_ss_numb %in% c(1:5,8))
# discr_SS3 %>% group_by(year) %>% summarise(landings = sum(landings))
save(discr_SS3, file = discr_file)

dat.disc <- dat.disc %>% group_by(year, season, fl_ss_numb) %>% summarize(discards = sum(discards)) %>% filter(year > 2013)

for(yr in yrs[-1]){
  for(ss in unique(dat.disc$season)){
    for(fl in c(1:5,8)){
      aux0 <- dat.disc %>% filter(year == yr, season == ss, fl_ss_numb == fl)
      if(dim(aux0)[1] == 0) next
      aux1 <- dat_wg0$discard_data[dat_wg0$discard_data$Yr == yr & dat_wg0$discard_data$Seas == ss & dat_wg0$discard_data$Flt == fl, ]
      if(dim(aux1)[1] == 0){
        cat(yr, '- ', ss, ' - ', fl, '\n')
        dat_wg0$discard_data <- rbind(dat_wg0$discard_data, data.frame(Yr = yr, Seas = ss, Flt = fl, Discard = aux0$discards, Std_in = 0.5))
      } else {
        cat(yr, '- ', ss, ' - ', fl, '\n')
        dat_wg0$discard_data[dat_wg0$discard_data$Yr == yr & dat_wg0$discard_data$Seas == ss & dat_wg0$discard_data$Flt == fl, 'Discard'] <- aux0$discards
      }
      
    }
  }
}

Dtot_raised <- dat_wg0$discard_data %>% group_by(Yr) %>% summarise(discards = sum(Discard)) %>% filter(Yr %in% yrs)

# comparisons
Dtot_raised_prev <- Dtot_raised %>% filter(Yr != dtyr)
round(Dtot_raised_prev$discards/Dtot_wg0$catch,2)


TotalDisc_ss3 <- dat_wg0$discard_data %>% filter(Yr %in% yrs)
names(TotalDisc_ss3) <- names(TotalLand_ss3)

save(TotalDisc_ss3, file = Total_Disc_ss3, row.names = F)
save(TotalLand_ss3, file = Total_Land_ss3, row.names = F)

# From Tab1
full_join( Dtot %>% rename(discards=caton), 
           Ltot %>% rename(landings = caton), by = "year")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~# TOTAL LANDINGS AND DISCARDS IN THE SS3 FILES. ---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat('-------------------------------------------------------- \n')
cat('Total *landings* in ss3 files:', sum(TotalLand_ss3$caton), '\n')
cat('Total *discards* in ss3 files:', sum(TotalDisc_ss3$caton), '\n')
cat('Total *catch* in ss3 files:',    sum(TotalDisc_ss3$caton) + sum(TotalLand_ss3$caton), '\n')
cat('-------------------------------------------------------- \n')

cat('-------------------------------------------------------- \n')
cat(dtyr, ' *landings* in ss3 files:', sum(TotalLand_ss3 %>% filter(year == dtyr) %>% .$caton), '\n')
cat(dtyr, ' *discards* in ss3 files:', sum(TotalDisc_ss3 %>% filter(year == dtyr) %>% .$caton), '\n')
cat(dtyr, ' *catch* in ss3 files:',    sum(TotalDisc_ss3 %>% filter(year == dtyr) %>% .$caton) + 
                                        sum(TotalLand_ss3 %>% filter(year == dtyr) %>% .$caton), '\n')
cat('-------------------------------------------------------- \n')


#------------------------------------------------------------------------------------
# PRODUCE LFD PLOTS
#------------------------------------------------------------------------------------
# convert ss3dat into length format.
ss3dat <- as_tibble(as.data.frame(ss3dat))
lfd    <- ss3dat %>% pivot_longer(names(ss3dat)[-(1:6)], names_to = "length")

# Join both sexes for plotting
lfd <- lfd %>% 
  mutate(length = as.numeric(gsub('f|m','',length))) %>% 
  group_by(Yr, Seas, FltSvy, Gender, Part, Nsamp, length) %>% 
  summarise(value = sum(value)) %>% 
  mutate(seascat = paste("Seas",Seas, ifelse(Part == 1, 'Disc', 'Land')), 
                      value = as.numeric(value), Yr = factor(Yr))

p1 <- ggplot(subset(lfd, FltSvy == 1), aes(x=length, height=value, y=factor(seascat), group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.2)+
  ylab("seasoncat") + ggtitle('SPTRAWL7')
p2 <- ggplot(subset(lfd, FltSvy == 2), aes(x=length, height=value, y=factor(seascat), group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.2)+
  ylab("seasoncat") + ggtitle('TRAWLOTH')
p3 <- ggplot(subset(lfd, FltSvy == 3), aes(x=length, height=value, y=factor(seascat),  group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.2)+
  ylab("seasoncat") + ggtitle('FRNEP8')
p4 <- ggplot(subset(lfd, FltSvy == 4), aes(x=length, height=value, y=factor(seascat),  group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+
  ylab("seasoncat") + ggtitle('SPTRAWL8')
p5 <- ggplot(subset(lfd, FltSvy == 5), aes(x=length, height=value, y=factor(seascat),  group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+
  ylab("seasoncat") + ggtitle('GILLNET')
p6 <- ggplot(subset(lfd, FltSvy == 6), aes(x=length, height=value, y=factor(seascat), group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+
  ylab("seasoncat") + ggtitle('LONGLINE')
p7 <- ggplot(subset(lfd, FltSvy == 8), aes(x=length, height=value, y=factor(seascat),  group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+
  ylab("seasoncat") + ggtitle('NSTRAWL')
p8 <- ggplot(subset(lfd, FltSvy == 9), aes(x=length, height=value, y=factor(seascat),  group=paste(seascat, Yr, sep = "_"), fill= Yr))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+
  ylab("seasoncat") + ggtitle('OTHERS')

print(p1); print(p2); print(p3); print(p4); print(p5); print(p6); print(p7); print(p8)


#------------------------------------------------------------------------------------
#### Compare with original SS data. ####
#------------------------------------------------------------------------------------

# ** Landings **.
L_wg0 <- L_wg0  %>%  filter(year %in% yrs)   
L_now <- TotalLand_ss3

comp <- L_wg0 %>% group_by(year, season, fleet) %>% 
  left_join(L_now) %>% mutate(rat = catch/caton - 1, time = year + 0.25*(season-1)) %>% 
  mutate(caton = ifelse(is.na(caton), 0, caton))
# comp %>% filter(rat != 0)                       # only differences in 2013
# comp %>% filter(is.na(rat)) %>% as.data.frame() # fleet 7: 0 in L_wg0 and NAs in L_now (as not included)

# In 2013 the problems in the "Third unknown" => Perfect!!
comp_yr <- comp %>% ungroup() %>% group_by(year)  %>% 
  summarize(catch = sum(catch, na.rm = TRUE), caton = sum(caton))  %>%  mutate(rat = catch/caton - 1)
# In 2013 the problems in the "Third unknown" => Perfect!!
comp_fl <- comp %>% ungroup() %>% group_by(year, fleet)  %>% 
  summarize(catch = sum(catch), caton = sum(caton))  %>%  mutate(rat = catch/caton - 1)

p1 <- ggplot(comp_fl, aes(year, rat)) + geom_line() + facet_wrap(.~fleet)  

p2 <- ggplot(comp, aes(time, rat)) + geom_line() + facet_wrap(.~fleet)  

print(p1); print(p2)

## ** LFD **
LFD_wg0 <- lfd_wg0 %>% 
  pivot_longer(names(lfd_wg0)[grepl('^f[0-9]+$',names(lfd_wg0))], names_to = 'length', names_prefix = 'f') %>% 
  filter(Yr > 2012, FltSvy < 10) %>% select(Yr,  Part,Seas, FltSvy, length, value) %>% 
  group_by(Yr, Part, Seas, FltSvy) %>% mutate(value = value/sum(value))
names(LFD_wg0) <- c("year", "category","season", "fleet", "lng", "prop")

LFD_now <- lfd %>% ungroup() %>% 
  select(Yr, Part, Seas, FltSvy, length, value) %>% group_by(Yr, Part, Seas, FltSvy) %>% mutate(value = value/sum(value))
names(LFD_now) <- c("year", "category","season", "fleet", "lng", "prop")

#LFD <- LFD %>% group_by(year, category, season, fleet) #%>% mutate(prop = prop/sum(prop))


both_lfd <- rbind(cbind(source = 'wg0', LFD_wg0), cbind(source = dtyr, LFD_now)) %>% 
  mutate(fleet = ifelse(fleet %in% as.character(1:9), fleet, fleetNumb[fleet]),
         lng = as.numeric(lng), 
         category = as.character(category),
         season = as.character(season),
         category = case_when(category == '1' ~ 'Discards',
                              category == '2' ~ 'Landings',
                              TRUE ~ category),
         season = case_when(season == '2.5' ~ '1', season == '5.5' ~ '2',
                            season == '8.5' ~ '3', season == '11.5' ~ '4',
                            TRUE ~ season),
         y = paste(year,season,  sep = "_"),
         prop = ifelse(is.na(prop), 0, prop))


p1 <- ggplot(subset(both_lfd, fleet ==1), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('SPTRAWL7')
p2 <- ggplot(subset(both_lfd, fleet ==2), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('TRAWLOTH')
p3 <- ggplot(subset(both_lfd, fleet ==3), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category)+
  ylab("seasoncat") + ggtitle('FRNEP8')
p4 <- ggplot(subset(both_lfd, fleet ==4), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category)+
  ylab("seasoncat") + ggtitle('SPTRAWL8')
p5 <- ggplot(subset(both_lfd, fleet ==5), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category)+
  ylab("seasoncat") + ggtitle('GILLNET')
p6 <- ggplot(subset(both_lfd, fleet ==6), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('LONGLINE') # will be joined Landings and Discards 
p7 <- ggplot(subset(both_lfd, fleet ==8), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('NSTRAWL')
p8 <- ggplot(subset(both_lfd, fleet ==9), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('OTHERS') # will be joined Landings and Discards 

print(p1); print(p2); print(p3); print(p4); print(p5); print(p6); print(p7); print(p8)

dev.off()  

# If joined?
both_lfd2 <- both_lfd %>% filter(fleet %in% c(6,9)) %>% 
  mutate(category = ifelse(source == "2022" & category == "Landings", 0, category))
p <- ggplot(subset(both_lfd2, fleet ==6), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('LONGLINE')
p <- ggplot(subset(both_lfd2, fleet ==9), aes(x=lng, height=prop, y=factor(y), group=paste(y, source, sep = "_"), fill= source))+
  geom_density_ridges2(stat="identity", scale=1.2,  alpha=0.2)+ facet_grid(~category) +
  ylab("seasoncat") + ggtitle('OTHERS')

