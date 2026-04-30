##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Data Analysis
#
# Dorleta Garcia
# 2021/04/27
#   modified: 2023-04-24 13:10:36 (ssanchez@azti.es) - adapt for wgbie2023
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
require(tidyr)
require(dplyr)
require(r4ss)
require(ggplot2)
require(ggridges)

rm(list = ls()[ls() != "dtyr"])
# rm(list = ls()[!ls() %in% c("dtyr","adv_dtyr","tac_dtyr")])

ac  <- as.character 
myr <- function(x) {x %>% mutate(year = as.factor(year))}

ass.yr <- as.numeric(substr(dtyr,3,4))+1

### Input data ----
# allocations <- 'bootstrap/initial/data/zaux_IC_Allocations.RData'
IC2SS3      <- file.path("data","allocations","zaux_IC_to_SS3.RData")
disc_raised <- file.path("data","allocations", "All_Disc_ss3.RData")
ss3_dat     <- file.path("data","ss3_saly",paste0("nhake-wg",ass.yr,".dat"))
hist_catch  <- file.path(taf.boot.path("data"),"hist_catch_by_area.csv")
table1.csv  <- file.path("data","catch","Table_1.csv")
table1.R    <- file.path("data","catch","Table_1.RData")
table2.R    <- file.path("data","catch","Table_2.RData")
# historic advice and tacs
tacadv_file <- file.path(taf.boot.path("data"),"advice_tac.csv")

if(!dir.exists(file.path("data","report"))) dir.create(file.path("data","report"))
if(!dir.exists(file.path("data","advice"))) dir.create(file.path("data","advice"))
if(!dir.exists(file.path("data","plots"))) dir.create(file.path("data","plots"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### LOAD DATA ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

# load(allocations)
load(IC2SS3)

load(table1.R)
load(table2.R)

ss3dat <- SS_readdat_3.30(ss3_dat)
save(ss3dat, file = file.path("data","report","ss3dat.RData"))


hist_data <- read.csv(hist_catch) %>% rename(year = 1) %>% filter(year < 2013)

tac_adv <- read.csv(tacadv_file)

ny <- dtyr - 2013+1


#================================================================================
#### Calculate sampled proportions ----
#===============================================================================
table1.sample <- tab1 %>% group_by(year, season, div, gear, category, type) %>% summarise(caton = sum(caton)) %>% 
                          ungroup() %>% group_by(year, season, div, gear, category) %>% 
                          mutate(prop_samp = caton/sum(caton)) %>% filter(type == 'sampled')
  
write.csv(table1.sample, file = file.path("data","Samples_Allocation_TotalC.csv"), row.names = F)


#================================================================================
#### Calculate some values to fill the report tables. ----
#===============================================================================

# raised discards (issues with 2013)
load(disc_raised) # discr_SS3

discr_SS3.area <- discr_SS3 %>% 
  mutate(div = as.character(subArea), category = "Discards") %>% #, div = ifelse(div == "27.5", "27.6", div)
  group_by(year, div, category) %>% summarise(discards = sum(discards)) %>% 
  ungroup()

discr_SS3.gear <- discr_SS3 %>% 
  mutate(gear = substr(fleet, 1, 3), category = "Discards", 
         gearc = case_when(gear %in% c("OTB","TBB","OTM","PTB","PTM","OTT") ~ "trawl",
                           gear %in% c("GNS", "GTR") ~ "gillnet",
                           gear == "LLS" ~ "longline",
                           TRUE ~ "other")) %>% 
  group_by(year, gearc, category) %>% summarise(discards = sum(discards)) %>% 
  ungroup()

discr_SS3.ctry <- discr_SS3 %>% 
  mutate(div = as.character(subArea), category = "Discards") %>%
  group_by(year, country, category) %>% summarise(discards = sum(discards)) %>% 
  ungroup()

# discr_SS3.tot <- discr_SS3 %>%
#   group_by(year) %>% summarise(discards = sum(discards)) %>%
#   ungroup()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
### Table 3.1: Total Landings and Discards by area ----
# we calculate it for years 2014 and 2013.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

catch_a3_yr <- tab1 %>% group_by(a3, year, category) %>% summarise(caton = sum(caton))
p_ct_a3_zoom <- ggplot(catch_a3_yr %>% myr(), aes(x=year, y=caton, group=a3)) + 
  facet_grid(category~., scales = "free") + 
  geom_line(aes(colour = a3), lwd = 2) + ggtitle('Landings & Discards by area')
p_ct_a3_zoom

catch_div_yr <- tab1 %>% group_by(div, year, category) %>% summarise(caton = sum(caton))
p_ct_div_zoom <- ggplot(catch_div_yr %>% myr(), aes(x=year, y=caton, group=div)) + 
  facet_grid(category~., scales = "free") + 
  geom_line(aes(colour = div), lwd = 2) + ggtitle('Landings & Discards by division')
p_ct_div_zoom

aa <- data.frame(year = rep(1961:2012,5), a3 = rep(c('oth',7,'8','unn','tot'), each = 2012-1961+1), category = c(rep('landings', each = (2012-1961+1)*4), rep('discards', each = 2012-1961+1)), 
                 caton = c(hist_data[,2], hist_data[,6], hist_data[,7], hist_data[,8], hist_data[,15]))

catch_area_yr <- catch_a3_yr[,c(2,1,3,4)] %>% mutate(category = tolower(category), caton = caton/1000) %>% bind_rows(aa)

tot_catch  <- catch_area_yr %>% group_by(year, category) %>% summarise(caton = sum(caton))

catch_area_yr <-  catch_area_yr %>% bind_rows(cbind(year = tot_catch[,1], a3 = 'total', tot_catch[,2:3])) %>% 
                        filter(a3 != 'tot') %>%  filter(!(year < 1971 & a3 != 'unn')) %>% mutate(a3 = ifelse(year < 1971, 'total', a3))

tot_catch  <- catch_area_yr %>% group_by(year, a3) %>% summarise(caton = sum(caton, na.rm = TRUE))

catch_area_yr <- catch_area_yr %>% bind_rows(cbind(tot_catch[,1:2], category = 'total', caton = tot_catch[,3]))


p_ct_area <- ggplot(catch_area_yr, aes(x=year, y=caton, group=a3, color = a3)) + 
  facet_grid(category~., scales = "free") + 
  geom_line(lwd = 2) 
p_ct_area


# Compare catch and tac
tac <- cbind(rbind(data.frame(year = 1961:1986, tac = NA), tac_adv[,c(1,3)]), 
             catch = c(subset(catch_area_yr, category == 'total' & a3 == 'total')$caton*1000,NA))

p_tac <- ggplot(subset(tac, year > 1986), aes(x=year)) + 
  geom_bar(aes(y=catch), stat="identity") +
  geom_line(aes(y=tac), color="red", lwd=2) + ggtitle('Catch vs TAC')
p_tac


table31 <- tab1 %>% group_by(div, year, category) %>% summarise(caton = sum(caton))

# full_join(table31, discr_SS3.area) %>% print(n =nrow(.))

table31 <- table31 %>% full_join(discr_SS3.area) %>% 
  mutate(caton = ifelse(category == "Discards" & year != 2013, discards, caton)) %>% 
  select(-discards)

table31 <- table31 %>% pivot_wider(id_cols = year, names_from = c(category, div), values_from = caton)

table31 <- table31[,names(table31)[c(1,(1:6)*2 + 1,(1:6)*2)]]
write.csv(table31, file = file.path("data","advice","adv_table8.csv"), row.names = F) # NOT USED used "rep_table1.RData" instead


# Prepare the report table 3.1

land_cols <- c("Landings_27.1", "Landings_27.2", "Landings_27.3", "Landings_27.4", "Landings_27.5", 
               "Landings_27.6", "Landings_27.7", "Landings_27.8")

new_data <- tab1 %>% mutate(area = substr(area,1,4)) %>%  group_by(area, year, category) %>% summarise(caton = sum(caton)) 

# full_join(new_data, discr_SS3.area %>% rename(area = div)) %>% print(n =nrow(.))

new_data <- new_data %>% full_join(discr_SS3.area %>% rename(area = div)) %>% 
  mutate(caton = ifelse(category == "Discards" & year != 2013, discards, caton)) %>% 
  select(-discards)

new_data <- new_data %>% 
  pivot_wider(id_cols = year, names_from = c(category, area), values_from = caton) %>% arrange(year) %>% 
  mutate(Unn. = 0, L_Total = Landings_27.1 + Landings_27.2 + Landings_27.3 + Landings_27.4 + Landings_27.5 + 
                             Landings_27.6 + Landings_27.7 + Landings_27.8, 
        D_Total = Discards_27.3 + Discards_27.4 + Discards_27.5 + Discards_27.6 + Discards_27.7 + Discards_27.8,
        Total = L_Total + D_Total) %>% 
  select("year", "Landings_27.1", "Landings_27.2", "Landings_27.3", "Landings_27.4", "Landings_27.5", 
                 "Landings_27.6", "Landings_27.7", "Landings_27.8", 'Unn.', 'L_Total', 
                 "Discards_27.3", "Discards_27.4", "Discards_27.5", "Discards_27.6", "Discards_27.7", 
                 "Discards_27.8", 'D_Total', 'Total')  
  
names(new_data) <- c('Year', paste('L', 1:8, sep = "_"), 'Unallocated', 'L_Total', paste('D',3:8, sep = "_"), 'D_Total', 'Total')

new_data[,11] <- rowSums(new_data[,2:10], na.rm= TRUE)
new_data[,18] <- rowSums(new_data[,12:17], na.rm= TRUE)
new_data[,19] <- rowSums(new_data[,c(11,18)], na.rm= TRUE)
  
hist_data[,-1] <- hist_data[,-1]*1000

save(hist_data, new_data,  file = file.path("data","report","rep_table1.RData")) #! different to advice table (here only from sampling programs)
# write.csv(table31, file = file.path("data","report","Table31_Disc&Land_by_Area.csv"), row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Table 3.2: DISCARDS & LANDINGS in weight and numbers by SS3 fleet ----
# + Total weight
# + Number of individuals sampled/measured, so we have and idea of the 
# + sampling intensity by fleet and category.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
table32 <- tab1 %>% group_by(ss3_fl, category, year) %>% 
                    summarise(caton = sum(caton), NLengthMeasured = sum(NLengthMeasured), NLengthSamples = sum(NLengthSamples)) %>% 
                    pivot_wider(id_cols = c('ss3_fl', 'year'), names_from = 'category',values_from = c('caton',  'NLengthSamples', 'NLengthMeasured',))
save(table32, file = file.path("data","report","rep_table2.RData"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Table REPORT 1st SECTION: SAMPLES    ----
# + Number of individuals sampled/measured,  by country  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

table14a <- tab1 %>% group_by(country) %>% filter(year == dtyr & category == 'Landings') %>%  
  summarise(NLengthMeasured = sum(NLengthMeasured), NLengthSamples = sum(NLengthSamples)) 

Numb_in_Landings <- sum(filter(tab2, year == dtyr & category == 'Landings')$canum)
Numb_measured <- sum(table14a$NLengthMeasured)
prop_measured <- Numb_measured/Numb_in_Landings

save(table14a, Numb_in_Landings, Numb_measured, prop_measured, file = file.path("data","report","rep_Sec1_table4a.RData"))

table14b <- tab1 %>% group_by(country) %>% filter(year == dtyr & category == 'Discards') %>%  
  summarise(NLengthMeasured = sum(NLengthMeasured), NLengthSamples = sum(NLengthSamples)) 

Numb_in_Discards <- sum(filter(tab2, year == dtyr & category == 'Discards')$canum)
Numb_measured <- sum(table14b$NLengthMeasured)
prop_measured <- Numb_measured/Numb_in_Discards

save(table14b, Numb_in_Discards, Numb_measured, prop_measured, file = file.path("data","report","rep_Sec1_table4b.RData"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Table 3.3: Data available by fleet and country. ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
table31 <- tab1
table32 <- tab2

tab1 <- tab1 %>% mutate(country = ifelse(!(country %in% c('Spain', "Belgium", 'France', 'Denmark', "Germany", "Netherlands", "Norway", 'Ireland','UK (England)',"UK(Northern Ireland)" ,'UK(Scotland)', "Sweden")), 'Others', as.character(country)), 
                               fleet = as.character(fleet)) %>% 
                 mutate(FU = ifelse(a3 == '0', 'FU16', 
                               ifelse(a3 == '8' & ss3_fl == 'LONGLINE', 'FU12', ifelse(a3 == '7' & ss3_fl == 'LONGLINE', 'FU1-2', 
                                 ifelse(a3 == '8' & ss3_fl == 'GILLNET', 'FU13', ifelse(a3 == '7' & ss3_fl == 'GILLNET', 'FU03', 
                                   ifelse(a3 == '8' & substr(fleet,1,7) == 'OTB_CRU', 'FU9',
                                     ifelse(a3 == '7' & gear == 'TRW_CRU', 'FU8', ifelse(gear == 'MIS', 'FU15',
                                       ifelse(a3 == '8' & substr(fleet,1,3) %in%  c('OTB', 'TBB', 'PTB', 'OTM', 'OTT', 'OTM', 'PTM'), 'FU10+14',# The rest in VIII is trawl.
                                         ifelse(a3 == '7' & gear == 'TRW', 'FU4-6', 'FU15'))))))))))) # The rest in VII is trawl.

tab2 <- tab2 %>% mutate(country = ifelse(!(country %in% c('Spain', "Belgium", 'France', 'Denmark', "Germany", "Norway", "Netherlands", 'Ireland','UK (England)',"UK(Northern Ireland)", 'UK(Scotland)', "Sweden")), 'Others', as.character(country)), 
                        fleet = as.character(fleet)) %>% 
                 mutate(FU = ifelse(a3 == '0', 'FU16', 
                               ifelse(a3 == '8' & ss3_fl == 'LONGLINE', 'FU12', ifelse(a3 == '7' & ss3_fl == 'LONGLINE', 'FU1-2', 
                                 ifelse(a3 == '8' & ss3_fl == 'GILLNET', 'FU13', ifelse(a3 == '7' & ss3_fl == 'GILLNET', 'FU03', 
                                   ifelse(a3 == '8' & substr(fleet,1,7) == 'OTB_CRU', 'FU9',
                                     ifelse(a3 == '7' & gear == 'TRW_CRU', 'FU8', ifelse(gear == 'MIS', 'FU15',
                                       ifelse(a3 == '8' & substr(fleet,1,3) %in% c('OTB', 'TBB', 'PTB', 'OTM', 'OTT', 'OTM', 'PTM'), 'FU10+14',# The rest in VIII is trawl.
                                          ifelse(a3 == '7' & gear == 'TRW', 'FU4-6', 'FU15'))))))))))) # The rest in VII is trawl.

table(tab1$FU,tab1$country)

aux <- tab1
aux$country <- ifelse(aux$country %in% c("Denmark","France","Ireland", "Spain", "UK (England)", "UK(Scotland)"),
                      aux$country, 'Others')

aux1 <- subset(aux, type == 'sampled')

# Are there samples?
tab33 <- table(aux1$season, aux1$FU,aux1$country, aux1$year)[,,,as.character(dtyr)]
tab33 <- ifelse(tab33!=0, 1,0)

# Are there catches?
tab33. <- table(aux$season, aux$FU,aux$country, aux$year)[,,,as.character(dtyr)]
tab33. <- ifelse(tab33.!=0, 1,0)

tab33 <- (tab33+tab33.)[1:4,,]
tab33
t33 <- data.frame(FU = rep(c("FU1-2", "FU03", "FU4-6", "FU8", "FU9", "FU10+14", "FU12", 
                             "FU13", "FU15", "FU16"), each = 4), Quarter = rep(1:4, dim(tab33)[2]))
for(i in dimnames(tab33)[[2]]){
  for(j in dimnames(tab33)[[3]]){
    t33[t33$FU == i, j] <- tab33[,i,j]
  }
}

t33[,-(1:2)] <- ifelse(t33[,-(1:2)] == 2, 'C+LFD', ifelse(t33[,-(1:2)] == 1,'C', 0))
tab33 <- t33

save(tab33, file = file.path("data","report","rep_table3.RData"))


#~~~~~~~~~~~~~~~~~~~~~~~~
### CATCH BY COUNTRY ----
#~~~~~~~~~~~~~~~~~~~~~~~~

tab1_cntryd <- tab1 %>% group_by(year, country, category) %>% summarise(caton = sum(caton))

# full_join(tab1_cntryd, discr_SS3.ctry) %>% print(n =nrow(.))
  
tab1_cntryd <- tab1_cntryd %>% full_join(discr_SS3.ctry) %>% 
  mutate(caton = ifelse(category == "Discards" & year != 2013, discards, caton)) %>% 
  select(-discards)
  
tab1_cntry <- tab1_cntryd %>% 
  pivot_wider(id_cols = 'year', names_from = c('country', 'category'), values_from = 'caton')

save(tab1_cntry, file = file.path("data","advice","catch_cntry.RData"))

tab1_adv_cntry <- tab1_cntryd %>% group_by(year, country) %>% summarise(caton = sum(caton)) %>% 
  pivot_wider(id_cols = 'year', names_from = c('country'), values_from = 'caton')

write.csv(tab1_adv_cntry, file = file.path("data","advice","adv_table9.csv"), row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Catch by country ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux  <- tab1 %>% group_by(year) %>% summarize_at('caton', sum)
aux2 <- unlist(aux$caton)
aux2 <- paste(100*round(aux2[-1]/aux2[-length(aux2)] -1,2), "%", sep = "")
aux <- aux %>% mutate(perc = c(NA, aux2), country = 'all')

pCaC <- ggplot(data = tab1 %>% myr(), aes(year, caton, fill = country)) +   
  geom_bar(stat="identity") + 
  geom_text(aes(year, caton*1.03, label = perc), data = aux %>% myr()) +
  #        annotate("text", x = 2014:2020, y = aux1$caton[-1]*1.03, label = aux2) + 
  ylab('tons') + ggtitle('Catch by country')
pCaC


aux  <- tab1 %>% group_by(category, year) %>% summarize_at('caton', sum)
aux2 <- unlist(aux$caton)
aux2 <- paste(100*round(aux2[-1]/aux2[-length(aux2)] -1,2), "%", sep = "")
aux <- aux %>% ungroup() %>% mutate(perc = c(NA, aux2[1:(ny-1)], NA, aux2[-(1:ny)]), country = 'all')

pLaDiC <- ggplot(tab1 %>% myr(), aes(year, caton, fill = country)) +   
  geom_bar(stat="identity") + facet_grid(category~., scales = 'free') +
  geom_text(aes(year, caton*1.05, label = perc), data = aux %>% myr()) +
  ylab('tons') + ggtitle('Landings & Discards by country')
pLaDiC


# catch by season
pCaS <- ggplot(tab1 %>% myr(), aes(year, caton, fill = season)) +   
  geom_bar(stat="identity") + 
  ylab('tons') + ggtitle('Catch by season')
pCaS

pLaDiS <- ggplot(tab1 %>% myr(), aes(year, caton, fill = season)) +   
  geom_bar(stat="identity") + facet_grid(category~., scales = 'free') +
  ylab('tons') + ggtitle('Landings & Discards by season')
pLaDiS

# catch by gear
tab1 %>% group_by(gear) %>% summarize_at('caton', sum)
pCaG <- ggplot(tab1 %>% myr(), aes(year, caton, fill = gear)) +   
  geom_bar(stat="identity") + 
  ylab('tons') + ggtitle('Catch by gear')
pCaG

pLaDiG <- ggplot(tab1 %>% myr(), aes(year, caton, fill = gear)) +   
  geom_bar(stat="identity") + facet_grid(category~., scales = 'free') +
  ylab('tons') + ggtitle('Landings & Discards by gear')
pLaDiG

# catch by ss3 fleet
tab1 %>% group_by(ss3_fl) %>% summarize_at('caton', sum)
pCaSS3FL <- ggplot(tab1 %>% myr(), aes(year, caton, fill = ss3_fl)) +   
  geom_bar(stat="identity") + 
  ylab('tons') + ggtitle('Catch by SS3 fleet')
pCaSS3FL

# NEW PLOTS (REVISE)

# landings and disc by ss3 fleet
pLDSS3FL <- ggplot(tab1 %>% myr(), aes(year, caton, fill = ss3_fl)) +   
  geom_bar(stat="identity") + facet_grid(category~., scales = 'free') +
  ylab('tons') + ggtitle('Landings & Discards by SS3 fleet')
pLDSS3FL

# with percentages  
aux  <- tab1 %>% group_by(ss3_fl, year) %>% summarize_at('caton', sum)
aux2 <- unlist(aux$caton)
aux2 <- paste(100*round(aux2[-1]/aux2[-length(aux2)] -1,2), "%", sep = "")
perc <- c(NA, aux2[1:(ny-1)])
for(x in 2:length(unique(aux$ss3_fl))) perc <- c(perc,  NA, aux2[((x-1)*ny+1):(x*ny-1)])
aux <- aux %>% ungroup() %>% mutate(perc = perc, country = 'all')

pLDSS3FLperc <- ggplot(tab1 %>% myr(), aes(year, caton, fill = ss3_fl)) +   
  geom_bar(stat="identity") +facet_grid(ss3_fl~., scales = 'free') +
  geom_text(aes(year, caton*1.05, label = perc), data = aux %>% myr()) +
  ylab('tons') + ggtitle('Catch by area')
pLDSS3FLperc

aux  <- tab1 %>% group_by(category, year) %>% summarize_at('caton', sum)
aux2 <- unlist(aux$caton)
aux2 <- paste(100*round(aux2[-1]/aux2[-length(aux2)] -1,2), "%", sep = "")
perc <- c(NA, aux2[1:(ny-1)])
for(x in 2:length(unique(aux$category))) perc <- c(perc,  NA, aux2[((x-1)*ny+1):(x*ny-1)])
aux <- aux %>% ungroup() %>% mutate(perc = perc, country = 'all')

pCatSS3FLperc <- ggplot(tab1 %>% myr(), aes(year, caton, fill = category)) +   
  geom_bar(stat="identity") +facet_grid(category~., scales = 'free') +
  geom_text(aes(year, caton*1.05, label = perc), data = aux %>% myr()) +
  ylab('tons') + ggtitle('Catch by area')
pCatSS3FLperc


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### catch by area ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tab1 %>% group_by(div) %>% summarize_at('caton', sum)

pCaA <- ggplot(tab1 %>% myr(), aes(year, caton, fill = div)) +   
  geom_bar(stat="identity") + 
  ylab('tons') + ggtitle('Catch by division')
pCaA


pLaDiA <- ggplot(tab1 %>% myr(), aes(year, caton, fill = div)) +   
  geom_bar(stat="identity") + facet_grid(category~., scales = 'free') +
  ylab('tons') + ggtitle('Landings & Discards by division')
pLaDiA

aux  <- tab1 %>% group_by(div, year) %>% summarize_at('caton', sum) %>% filter(div != '27.5')
aux2 <- unlist(aux$caton)
aux2 <- paste(100*round(aux2[-1]/aux2[-length(aux2)] -1,2), "%", sep = "")
perc <- c(NA, aux2[1:(ny-1)])
for(x in 2:length(unique(aux$div))) perc <- c(perc,  NA, aux2[((x-1)*ny+1):(x*ny-1)])
aux <- aux %>% ungroup() %>% mutate(perc = perc, country = 'all')

pCaAr <- ggplot(tab1 %>% filter(div != '27.5') %>% myr(), aes(year, caton, fill = div)) +   
  geom_bar(stat="identity") +facet_grid(div~., scales = 'free') +
  geom_text(aes(year, caton*1.05, label = perc), data = aux %>% myr()) +
  ylab('tons') + ggtitle('Catch by area')
pCaAr

pCaAr2 <- ggplot(tab1 %>% filter(div != '27.5') %>% myr(), aes(year, caton, fill = div)) +   
  geom_bar(stat="identity") +facet_grid(div~.) +
  # geom_text(aes(year, caton*1.05, label = perc), data = aux %>% myr()) +
  ylab('tons') + ggtitle('Catch by area')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### catch by fleet group ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cgear <- tab1 %>% filter(year == dtyr) %>% 
  mutate(gearc = case_when(gear %in% c("TRW_CRU", "TRW") ~ "trawl",
                           gear == "GLN" ~ "gillnet",
                           gear == "LLS" ~ "longline",
                           TRUE ~ "other")) %>% 
  group_by(category, gearc) %>% summarize_at('caton', sum)

cgear <- cgear %>% 
  full_join(discr_SS3.gear %>% filter(year == dtyr) %>% select(-year)) %>% 
  mutate(caton = ifelse(category == "Discards", discards, caton)) %>% 
  select(-discards) %>% 
  replace(is.na(.), 0)

cgear <- cgear %>% group_by(category) %>% mutate(prop = caton/sum(caton)*100, tld = sum(caton)) %>% 
  ungroup() %>% mutate(tcat = sum(caton))

write.csv(cgear, file = file.path("data","advice","adv_table7.csv"), row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Proportion Sampled Landings/Discards ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sampling <- tab1 %>% group_by(year, category, type) %>% 
  summarise(caton = sum(caton)) %>% 
  reframe(perc = caton/sum(caton), Sampled = ifelse(type == 'estimated', FALSE, TRUE))

p_samp <- ggplot(sampling %>% filter(Sampled == TRUE) %>% myr(), aes(year, perc, group = category, color = category)) + 
  geom_line(lwd = 1) + ylab("Sampled (perc)")


sampling <- tab1 %>% group_by(year, category, type, country) %>% 
  summarise(caton = sum(caton))  %>% 
  group_by(year, category, country) %>% 
  reframe(perc = caton/sum(caton), sampled = ifelse(type == 'estimated', FALSE, TRUE))

p_rais <- ggplot(sampling %>% filter(sampled == FALSE) %>% myr(), aes(year, perc, group = category, color = category)) + 
  facet_wrap(country~., ncol = 2) + geom_line(lwd = 1) + ylab("Raised (perc)")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LFD in SURVEYS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lfd.surv <- subset(ss3dat$lencomp,Yr %in% ac(2010:dtyr) & FltSvy %in% c(10,15:17))[,-c(2,4:6)] %>% 
  rename(year = Yr, survey = FltSvy) %>% 
  mutate(year = factor(year))
lfd.surv[,-(1:2)] <- as_tibble(data.frame(sweep(as.matrix(lfd.surv[,-(1:2)] ), 1, 
                                                 apply(as.matrix(lfd.surv[,-(1:2)] ),1,sum), "/")))
lfd.surv.b <- lfd.surv %>% filter(survey %in% c(10,16)) %>% 
  pivot_longer(-(1:2), names_to = "length") %>% 
  mutate(length = as.numeric(substr(length, 2, nchar(length)))) %>% 
  group_by(year, survey, length) %>% summarise(value = sum(value)) %>% 
  mutate(sex = "both")
lfd.surv.f <- lfd.surv %>% filter(!survey %in% c(10,16)) %>% 
  pivot_longer(names(lfd.surv)[grepl('^f[0-9]+$',names(lfd.surv))], names_to = 'length', names_prefix = 'f') %>% 
  select(year, survey, length, value) %>% mutate(sex = "F", length = as.numeric(length))
lfd.surv.m <- lfd.surv %>% filter(!survey %in% c(10,16)) %>% 
  pivot_longer(names(lfd.surv)[grepl('^m[0-9]+$',names(lfd.surv))], names_to = 'length', names_prefix = 'm') %>% 
  select(year, survey, length, value) %>% mutate(sex = "M", length = as.numeric(length))
lfd.surv <- bind_rows(lfd.surv.b, lfd.surv.f, lfd.surv.m) %>% mutate(sex = factor(sex))

p_lfd_ev <- ggplot(subset(lfd.surv, survey == 10), aes(x=length, height=value, y=year, group=year))+
              geom_density_ridges2(stat="identity", scale=1.2, fill="red", colour="red", alpha=0.3) + ggtitle('EVHOE')
p_lfd_po <- ggplot(subset(lfd.surv, survey == 15), aes(x=length, height=value, y=year))+
              geom_density_ridges2(stat="identity", scale=1.2, aes(colour=sex, fill = sex), alpha=0.3) + 
              ggtitle('SP-PORC')
p_lfd_ig <- ggplot(subset(lfd.surv, survey == 16), aes(x=length, height=value, y=year, group= year))+
              geom_density_ridges2(stat="identity", scale=1.2, fill="red",colour="red", alpha=0.3) + ggtitle('IR-IGFS')
p_lfd_ia <- ggplot(subset(lfd.surv, survey == 17), aes(x=length, height=value, y=year))+
              geom_density_ridges2(stat="identity", scale=1.2, aes(colour=sex, fill = sex), alpha=0.3) + 
              ggtitle('IR-IAMS')

p_lfd_ev_ig <- ggplot(subset(lfd.surv, survey %in% c(10,16)), aes(x=length, height=value, y=year))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(colour=factor(survey), fill = factor(survey)), alpha=0.3) + ggtitle('EVHOE')
p_lfd_ev_ig

# RESGASQ survey
lfd.resg <- subset(ss3dat$lencomp, Yr %in% 1985:1997 & FltSvy %in% 11:14)[,-c(2,4:6)] %>% 
  rename(year = Yr, survey = FltSvy) %>% 
  mutate(survey = as.character(survey), 
         survey = as.factor(case_when(survey == 11 ~ "Q1", survey == 12 ~ "Q2", 
                                      survey == 13 ~ "Q3", survey == 14 ~ "Q4", TRUE ~ survey)), 
         year = factor(year))
lfd.resg[,-(1:2)] <- as_tibble(data.frame(sweep(as.matrix(lfd.resg[,-(1:2)] ), 1, 
                                                apply(as.matrix(lfd.resg[,-(1:2)] ),1,sum), "/")))
lfd.resg <- lfd.resg %>% pivot_longer(-(1:2), names_to = "length") %>% 
  mutate(length = as.numeric(substr(length, 2, nchar(length)))) %>% 
  group_by(year, survey, length) %>% summarise(value = sum(value))

p_lfd_re <- ggplot(lfd.resg, aes(x=length, height=value, y=year, group= paste(survey, year)))+
              geom_density_ridges2(stat="identity", scale=1.2, aes(fill=survey, colour=survey), alpha=0.3) + ggtitle('RESG-1')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LFD in FLEETS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lfd.flt <- subset(ss3dat$lencomp,Yr %in% ac(2010:dtyr) & FltSvy %in% 1:9)[,-c(4,6)]
lfd.flt[,-(1:4)] <- as_tibble( data.frame(sweep(as.matrix(lfd.flt[,-(1:4)] ), 1, 
                                                apply(as.matrix(lfd.flt[,-(1:4)] ),1,sum), "/")))
lfd.flt <- lfd.flt %>% pivot_longer(-(1:4), names_to = "length") %>% 
                       mutate(length = as.numeric(substr(length, 2, nchar(length))), Yr = factor(Yr), Seas = factor(Seas),
                       Part = ifelse(Part == 1, 'Discards', 'Landings')) %>% 
  rename(year = Yr, season = Seas, fleet = FltSvy, category = Part) %>% 
  group_by(year, season, fleet, category, length) %>% summarise(value = sum(value)) 
  
lfd.flt <- lfd.flt %>% mutate(seascat = paste("S",season, "_", substr(category, 1,4),sep= "" ),
                              yearcat = paste(year, "_", substr(category, 1,4),sep= "" ))

ss <- paste0("S",levels(lfd.flt$season))
ct <- sort(unique(substr(lfd.flt$category, 1,4)))
lfd.flt$seascat <- factor(lfd.flt$seascat, 
                          levels = paste(rep(ss, each=length(ct)),
                                         rep(ct, length(ss)), sep="_"))

# LFD WITHIN YEAR
p_lfd_sp7 <- ggplot(subset(lfd.flt, fleet == 1 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('SPTRAWL7 - ', dtyr))
p_lfd_tro <- ggplot(subset(lfd.flt, fleet == 2 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('TRAWLOTH - ', dtyr))
p_lfd_fr8 <- ggplot(subset(lfd.flt, fleet == 3 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('FRNEP8 - ', dtyr))
p_lfd_sp8 <- ggplot(subset(lfd.flt, fleet == 4 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('SPTRAWL8 - ', dtyr))
p_lfd_gln <- ggplot(subset(lfd.flt, fleet == 5 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('GILLNET - ', dtyr))
p_lfd_lln <- ggplot(subset(lfd.flt, fleet == 6 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('LONGLINE - ', dtyr))
p_lfd_nst <- ggplot(subset(lfd.flt, fleet == 8 & year == dtyr), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('NSTRAWL - ', dtyr))
p_lfd_oth <- ggplot(subset(lfd.flt, fleet == 9 & year == dtyr),
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle(paste('OTHERS - ', dtyr))


# LFD ALONG YEARS
p_lfd_yr_sp7 <- ggplot(subset(lfd.flt, fleet == 1), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('SPTRAWL7')
p_lfd_yr_tro <- ggplot(subset(lfd.flt, fleet == 2), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('TRAWLOTH')
p_lfd_yr_fr8 <- ggplot(subset(lfd.flt, fleet == 3), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('FRNEP8')
p_lfd_yr_sp8 <- ggplot(subset(lfd.flt, fleet == 4), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2)+ 
  facet_grid(~season) + ggtitle('SPTRAWL8')
p_lfd_yr_gln <- ggplot(subset(lfd.flt, fleet == 5), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('GILLNET')
p_lfd_yr_lln <- ggplot(subset(lfd.flt, fleet == 6), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('LONGLINE')
p_lfd_yr_nst <- ggplot(subset(lfd.flt, fleet == 8), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) + ggtitle('NSTRAWL')
p_lfd_yr_oth <- ggplot(subset(lfd.flt, fleet == 9), aes(x=length, height=value, y=year, fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) + ggtitle('OTHERS')


# ALL TOGETHER FOR REPORT
fltnms <- c('SPTR7', 'TROTH', 'FRNEP8', 'SPTR8', 'GILLNET', 'LONGLINE', 'OTHHIST', 'NSTRAWL', 'OTHER')
names(fltnms) <- 1:9

lfd.flt <- lfd.flt %>% mutate(fltnm = factor(fltnms[fleet])) 

p_lfd <- ggplot(subset(lfd.flt,  year %in% (dtyr-2):dtyr), 
                aes(x=length, height=value, y=factor(fltnm), fill = year))+ facet_grid(.~category*season)+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = year, colour = year), alpha=0.3, size = 0.2) +
  ylab("fleet") + ggtitle(paste('Length frequency distribution - ', dtyr))


survnms <- c(paste('RES', 1:4, sep = "_"), 'EVHOE', 'SP-PORC', 'IR-IGFS', 'IR-IAMS')
names(survnms) <- c(11:14, 10, 15:17)

lfd.surv <- lfd.surv %>% mutate(survnms = (survnms[as.character(survey)])) %>% 
  group_by(year, survey, survnms, length) %>% summarise(value = sum(value))

p_lfd_surv <- ggplot(subset(lfd.surv, survnms %in% c('EVHOE', 'SP-PORC', 'IR-IGFS', 'IR-IAMS') & year %in% c((dtyr-4):dtyr)), 
                     aes(x=length, height=value, y=factor(survnms),  fill = year))+
  geom_density_ridges2(stat="identity", aes(fill = year, colour = year), scale=1.2,  alpha=0.3) + 
  ylab("survey")

save(lfd.flt, lfd.surv, file = file.path("data","LFDs.RData"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Indices Time Series
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
surveys.id <- ss3dat$CPUE

surveys.id$index <- ifelse(surveys.id$index %in% 11:14, "RESSGASC", 
                           ifelse(surveys.id$index == 10, "EVHOE-WIBTS-Q4",
                                  ifelse(surveys.id$index == 15, "SpPGFS-WIBTS-Q4", 
                                         ifelse(surveys.id$index == 16, "IGFS-WIBTS-Q4", "IAMS-WIBTS-Q4"))))
# surveys.id$ll <- surveys.id$obs - 1.96*surveys.id$se_log*surveys.id$obs
# surveys.id$ul <- surveys.id$obs + 1.96*surveys.id$se_log*surveys.id$obs #! 95% confidence interval from normal distribution
surveys.id$ll <- exp(log(surveys.id$obs) - sqrt(log(1+(1.96*surveys.id$se_log)^2)))
surveys.id$ul <- exp(log(surveys.id$obs) + sqrt(log(1+(1.96*surveys.id$se_log)^2))) #! 95% confidence interval from LOGnormal distribution
surveys.id$year <- surveys.id$year + surveys.id$seas/12

p_surveys <- ggplot(surveys.id, aes(x = year, y = obs, fill = index))  + facet_grid(index~., scales = 'free') +
  geom_ribbon(aes(ymin = ll, ymax = ul, colour = index), alpha = 0.3) +
  geom_line(aes(colour = index)) + geom_point(aes(colour = index)) +
  theme(legend.position="none")

save(surveys.id, file = file.path("data","surveys_biomass.RData"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REPORTR: TABLE 1.3 NUMBER OF SAMPLES, NO. OF LENGTHS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tab1Samp <- read.csv(table1.csv)[,c(2,4,16,18:19)]
names(tab1Samp) <- tolower(names(tab1Samp))
tab1Samp <- subset(tab1Samp,sampledorestimated == 'Sampled_Distribution')
names(tab1Samp)[4:5] <- c('no.samples', 'no.length')

# replace -9 NO SAMPLE with 0
tab1Samp <- tab1Samp %>% 
  mutate(no.samples = ifelse(no.samples == -9, 0, no.samples),
         no.length = ifelse(no.length == -9, 0, no.length))

tab1Samp.ctry <- aggregate(list(no.samples = tab1Samp$no.samples, no.length = tab1Samp$no.length), list(country = tab1Samp$country, cat = tab1Samp$catchcategory), sum)

write.csv(tab1Samp.ctry, file = file.path("data","report","SamplingIntensity_by_Country.csv"), row.names = FALSE)

tab1Samp.Tot <- tab1Samp.ctry %>% group_by(cat) %>% 
  summarise(no.samples = sum(no.samples), no.length = sum(no.length))

write.csv(tab1Samp.Tot, file = file.path("data","report","SamplingIntensity_total.csv"), row.names = FALSE)

# PENDING: assessment estimates in reporting code
# - Total No. in international landings ('000)
# - Nb. meas. as % of annual nb. caught
# - Total no. in international discards ('000)
# - Nb. meas. as % of annual nb. Discarded


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
# Landings and Discards data by country
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
yrs <- 2013:dtyr

tab1.cty <- aggregate(caton/1000~year+country+area+category, tab1, sum)
names(tab1.cty)[5] <- 'caton'

tab1.cty$country <- ifelse(tab1.cty$country == 'Denmark', 'DK', ifelse(tab1.cty$country == 'Germany', 'DE',
                  ifelse(tab1.cty$country == 'Sweden', 'SE', ifelse(tab1.cty$country == 'Netherlands', 'NE',
                    ifelse(tab1.cty$country == 'Norway', 'NO', ifelse(tab1.cty$country == 'Belgium', 'BE',
                       ifelse(tab1.cty$country ==  'France', 'FR',ifelse(tab1.cty$country %in% c("UK (England)",  "UK(Scotland)","UK(Northern Ireland)"), 'UK',
                          ifelse(tab1.cty$country == 'Ireland', 'IR', ifelse(tab1.cty$country %in% c('Spain', 'Third unknown country'), 'SP', 
                            ifelse(tab1.cty$country == 'Poland', 'PO', tab1.cty$country)))))))))))                                         

tab1.cty <- aggregate(caton~year+country+area+category, tab1.cty, sum)

p_ct_cnty <- ggplot(tab1.cty, aes(year, caton, fill = country)) + geom_bar(stat= 'identity') + 
  facet_grid(category~.) + ggtitle('Catch by country')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#   Make a bundle with all the plots generated.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List of produced plots
ls(pattern = 'p_')

#  [1] "p_ct_a3_zoom"  "p_ct_area"     "p_ct_cnty"     "p_ct_div_zoom" "p_lfd"         "p_lfd_ev"      "p_lfd_ev_ig"  
#  [8] "p_lfd_fr8"     "p_lfd_gln"     "p_lfd_ia"      "p_lfd_ig"      "p_lfd_lln"     "p_lfd_nst"     "p_lfd_oth"    
# [15] "p_lfd_po"      "p_lfd_re"      "p_lfd_sp7"     "p_lfd_sp8"     "p_lfd_surv"    "p_lfd_tro"     "p_lfd_yr_fr8" 
# [22] "p_lfd_yr_gln"  "p_lfd_yr_lln"  "p_lfd_yr_nst"  "p_lfd_yr_oth"  "p_lfd_yr_sp7"  "p_lfd_yr_sp8"  "p_lfd_yr_tro" 
# [29] "p_surveys"     "p_tac"         "prop_measured" 


# - For presentation
#------------------------------

# Input data EDA

png(file.path("data","plots","Pres_EDA_01_catch&tac.png"), width = 800, height = 480)
  p <- p_tac + 
    geom_bar(aes(y=catch, fill = "Reported Catches"), stat="identity") +
    geom_line(aes(y=tac, color = "TAC"), lwd=2) + ggtitle('Catch vs TAC') + 
    scale_colour_manual(" ", values=c("TAC" = "red")) + 
    scale_fill_manual("", values="grey50")+
    ylab('tonnes') + 
    theme(legend.key = element_blank(),
          legend.title = element_blank()) +
    ggtitle("")
  print(p)
dev.off()

png(file.path("data","plots","Pres_EDA_02a_landDisc_area.png"), width = 800, height = 480)
  print(p_ct_area)
dev.off()
png(file.path("data","plots","Pres_EDA_02b_catch_area.png"), width = 800, height = 480)
  print(pCaAr)
dev.off()
png(file.path("data","plots","Pres_EDA_02b_catch_area_scaled.png"), width = 800, height = 480)
  print(pCaAr2)
dev.off()

png(file.path("data","plots","Pres_EDA_03a_landDisc_gear.png"), width = 800, height = 480)
  print(pLaDiG)
dev.off()
png(file.path("data","plots","Pres_EDA_03b_catch_gear.png"), width = 800, height = 480)
  print(pCaSS3FL + ggtitle('Catch by SS3 fleet') + ylab('tonnes'))
dev.off()

png(file.path("data","plots","Pres_EDA_04_Surveys.png"), width = 800, height = 480)
  print(p_surveys)
dev.off()

pdf(file.path("data","plots","Pres_EDA_Commercial_Catch.pdf"), width = 10)
  print(p_tac + ylab('tonnes'))
  print(p_ct_area + ggtitle('Landings & Discards by area'))
  print(p_ct_div_zoom)
  print(pCaAr)
  print(pCaG )
  print(pLaDiG)
  print(pCaSS3FL + ggtitle('Catch by SS3 fleet') + ylab('tonnes'))
dev.off()

pdf(file.path("data","plots","Pres_EDA_Surveys_LFD.pdf"))
  print(p_lfd_ev)
  print(p_lfd_po)
  print(p_lfd_ig)
  print(p_lfd_ia)
dev.off()

pdf(file.path("data","plots","Pres_EDA_Commercial_LFD.pdf"))
  print(p_lfd_sp7)
  print(p_lfd_yr_sp7)
  print(p_lfd_tro)
  print(p_lfd_yr_tro)
  print(p_lfd_fr8)
  print(p_lfd_yr_fr8)
  print(p_lfd_sp8)
  print(p_lfd_yr_sp8)
  print(p_lfd_gln)
  print(p_lfd_yr_gln)
  print(p_lfd_lln)
  print(p_lfd_yr_lln)
  print(p_lfd_nst)
  print(p_lfd_yr_nst)
  print(p_lfd_oth)
  print(p_lfd_yr_oth)
dev.off()


# - For report
#------------------------------

print(p_lfd)
ggsave(file.path("data","plots","Rep_Figure_9.5_FleetsLFD.png"), units = 'cm', width = 15, height = 20, scale = 1.3)
#dev.off()
print(p_lfd_surv)
ggsave(file.path("data","plots","Rep_Figure_9.6_SurveysLFD.png"), units = 'cm', width = 6, height = 4, scale = 2)
#dev.off()

pdf(file.path("data","plots","03_analysis.pdf"), width = 10)
  print(p_ct_a3_zoom)
  print(pCaC); print(pLaDiC)
  print(pCaS); print(pLaDiS)
  print(pLDSS3FL); print(pLDSS3FLperc); print(pCatSS3FLperc)
  print(pCaA); print(pLaDiA)
  print(p_samp); print(p_rais)
  print(p_lfd_ev_ig); print(p_lfd_re)
  print(p_ct_cnty)
dev.off()

