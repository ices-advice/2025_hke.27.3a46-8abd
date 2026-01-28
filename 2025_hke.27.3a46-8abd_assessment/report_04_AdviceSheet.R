################################################################################
#  WGBIE northern hake - advice tables and figures                             #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  27/06/2022                                                       #
#   modified:                                                                  #
################################################################################

# report_04_AdviceSheet.R - compiling tables and figures for the ICES Advice Sheet
# ~/report_04_AdviceSheet.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


rm(list = ls()[!ls() %in% c("dtyr","stockcode")])


#==============================================================================
# WORKING DIRECTORIES                                                      ----
#==============================================================================

save_wd <- file.path("report","advice")

if(!dir.exists(save_wd)) dir.create(save_wd)


intyr.tab     <- file.path(save_wd, "tab01_intermediate_year.csv")
cat_opt.tab   <- file.path(save_wd, "tab02_catch_option.csv")
cat_tot.tab   <- file.path(save_wd, "tab06_catch_advice.csv")
cat_fleet.tab <- file.path(save_wd, "tab07_catch_fleet.csv")
cat_area.tab  <- file.path(save_wd, "tab08_catch_area.csv")
cat_ctry.tab  <- file.path(save_wd, "tab09_catch_country.csv")
ass_sum.tab   <- file.path(save_wd, "tab10_assessment_summary.csv")


#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

library(dplyr)
library(icesAdvice)


#==============================================================================
# Catch scenarios                                                          ----
#==============================================================================

# Interim year 

iyTab <- read.csv(file.path("model","sum_advice","table_intermediate_year_icesRound.csv"))
iyTab

write.csv(iyTab, file = intyr.tab, row.names = FALSE)

# Catch option table

coptTab <- read.csv(file.path("model","sum_advice","catchoptiontable_final_ICESround.csv"))
coptTab

write.csv(coptTab, file = cat_opt.tab, row.names = FALSE)


#==============================================================================
# HISTORY                                                                  ----
#==============================================================================

# History of the catch and landings

# - by fleet

gearTab <- read.csv(file.path("data","advice","adv_table7.csv")) %>% 
  mutate(prop_round = round(prop,1)) # mutate(prop_round = icesRound(prop))

gearTab.sum <- gearTab %>% group_by(category) %>% summarise(ptot = sum(prop_round))

if (any(round(gearTab.sum$ptot) != 100)) stop('Check rounding') # check summing 1

write.csv(gearTab, file = cat_fleet.tab, row.names = FALSE)

# - by area

careaTab <- R.utils::loadToEnv(file.path("data","report","rep_table1.RData"))[["new_data"]] %>% 
  mutate(L_6 = sum(c(L_5, L_6), na.rm = TRUE), D_6 = sum(c(D_5, D_6), na.rm = TRUE)) %>% 
  select(-L_5, -D_5) %>% 
  round()

# careaTab <- read.csv(file.path("data","advice","adv_table8.csv"))

careaTab %>% as.data.frame()

careaTab.sum <- careaTab %>% mutate(Ltot = sum(c(L_1, L_2, L_3, L_4, L_6, L_7, L_8, Unallocated), na.rm = TRUE), 
                                    Dtot = sum(c(D_3, D_4, D_6, D_7, D_8), na.rm = TRUE), 
                                    Tot = sum(c(L_Total, D_Total), na.rm = TRUE)) %>% 
  mutate(Ldif = Ltot - L_Total,
         Ddif = Dtot - D_Total,
         Tdif = Tot - Total) %>% 
  select(Year, Ltot, Dtot, Tot, Ldif, Ddif, Tdif)

if (any(careaTab.sum$Ldif != 0) | any(careaTab.sum$Ddif != 0) | any(careaTab.sum$Ddif != 0)) 
  warning('Check rounding in careaTab') # check summing > 0

write.csv(careaTab, file = cat_area.tab, row.names = FALSE)


# - by country

cctryTab <- read.csv(file.path("data","advice","adv_table9.csv"))

write.csv(cctryTab, file = cat_ctry.tab, row.names = FALSE)


# History of the advice, catch, and management

ctotTab <- careaTab %>% select(Year, L_Total, D_Total, Total)
ctotTab

write.csv(ctotTab, file = cat_tot.tab, row.names = FALSE)



#==============================================================================
# Summary of the assessment                                                ----
#==============================================================================

sumTab <- read.csv(file.path("output","advice","summary_table_final.csv"))

ass_sum <- sumTab %>% 
  select( year, rec, upperrec, lowerrec, ssb, upperssb, lowerssb, biomass, 
          LanObs, DisObs, f, upperf, lowerf)

for (i in c(2:10))  ass_sum[,i] <- round(ass_sum[,i])
for (i in c(11:13)) ass_sum[,i] <- icesRound(ass_sum[,i])

write.csv(ass_sum, file = ass_sum.tab, row.names = FALSE)



#==============================================================================
# CONSISTENCY AMONG TABLES                                                 ----
#==============================================================================

# Tables 6, 8 & 9

# ctotTab - "tab06_catch_advice.csv"

# gearTab - "tab07_catch_fleet.csv"

clast <- ctotTab %>% filter(Year == dtyr)

if (round(clast$L_Total) != round(gearTab %>% filter(category == "Landings") %>% select(caton) %>% sum()) |
    round(clast$D_Total) != round(gearTab %>% filter(category == "Discards") %>% select(caton) %>% sum())) {
  warning("Differences between advice tab06 and tab07")
  bind_cols(clast, 
            L_total_gear = gearTab %>% filter(category == "Landings") %>% select(caton) %>% sum(), 
            D_total_gear = gearTab %>% filter(category == "Discards") %>% select(caton) %>% sum())
  stop("Differences between advice tab06 and tab07")
}


# careaTab - "tab08_catch_area.csv"

if (any(round(ctotTab$L_Total) != round(careaTab$L_Total)) | 
    any(round(ctotTab$D_Total) != round(careaTab$D_Total)) | 
    any(round(ctotTab$Total) != round(careaTab$Total))) {
  print("Differences between advice tab06 and tab08")
  full_join(ctotTab, 
            careaTab %>% select(Year, L_Total, D_Total) %>% rename(L_Total_area = L_Total, D_Total_area = D_Total))
  stop("Differences between advice tab06 and tab08")
}
  

# cctryTab - "tab09_catch_country.csv"

cctry <- cctryTab %>% 
  tidyr::pivot_longer(-year, names_to = "country", values_to = "Total") %>% 
  group_by(year) %>% summarise(Total = sum(Total, na.rm = TRUE)) %>% filter(year > 2013)

ctotTabl <- ctotTab %>% filter(Year >2013)

if (any(round(ctotTabl$Total) != round(cctry$Total))) {
  print("Differences between advice tab06 and tab09")
  full_join(ctotTabl %>% select(Year, Total), 
            cctry %>% rename(Year = year, Total_ctry = Total)) %>% print(n=nrow(.))
  stop("Differences between advice tab06 and tab09")
}

# ass_sum - "tab10_assessment_summary.csv")

ass_sum <- ass_sum %>% filter(year %in% ctotTab$Year)

if (any(round(ctotTab$L_Total) != round(ass_sum$LanObs)) | 
    any(round(ctotTab$D_Total) != round(ass_sum$DisObs))) {
  print("Differences between advice tab06 and tab10")
  full_join(ctotTab %>% select(Year, L_Total, D_Total), 
            ass_sum %>% select(year, LanObs, DisObs), by = join_by(Year == year)) %>% print()
  stop("Differences between advice tab06 and tab10")
}
  
#     Year L_Total D_Total LanObs DisObs
#  1  2013   86148   15450  77343  11098 #! issue in 2013 --> changed
#  2  2014   89940   12131  89940  12131
#  3  2015   95041   14446  95043  14446
#  4  2016  107546   16041 107547  16041
#  5  2017  104671   10488 104670  10488
#  6  2018   89695    9934  89695   9934
#  7  2019   82298    6966  82298   6966
#  8  2020   72579    6946  72579   6946
#  9  2021   68058    6738  68061   6738
# 10  2022   67431    3241  67433   3241
# 11  2023   59381    2990  59380   2990
# 12  2024   49569    4164  49572   4164

#! Force to totals in previous tables? (see lines 475-483 in output.R)

# ass_sum <- ass_sum %>% filter(year %in% ctotTab$Year & year > 2013)
# 
# if (any(round(ctotTabl$L_Total) != round(ass_sum$LanObs)) | 
#     any(round(ctotTabl$D_Total) != round(ass_sum$DisObs))) {
#   print("Differences between advice tab06 and tab10")
#   full_join(ctotTabl %>% select(Year, L_Total, D_Total), 
#             ass_sum %>% select(year, LanObs, DisObs), by = join_by(Year == year)) %>% print()
#   stop("Differences between advice tab06 and tab10")
# }
