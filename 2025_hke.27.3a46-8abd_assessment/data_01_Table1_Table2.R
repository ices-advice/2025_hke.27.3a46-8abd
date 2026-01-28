#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Join Historical Tables with total catch (TABLE 1)
#    Tune Table 2.
# 
# Dorleta Garc?a 
# 2022/04/08
#   modified: 2022-04-20 13:19:30 (ssanchez@azti.es) - adapt given WKANGHAKE2022 + correction of 2020 data and new 2021
#             2023-04-25 08:16:00 (ssanchez@azti.es) - update for WGBIE2023
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(dplyr)

# New tables
table1 <- file.path(catd_wd, "Table_1.csv")
table2 <- file.path(catd_wd, "Table_2.csv")

# Historical tables
hist_tab1 <- file.path(taf.data.path(), "Table_1.RData")
hist_tab2 <- file.path(taf.data.path(), "Table_2.RData")

# Output tables
out_tab1 <- file.path(catd_wd, "Table_1.RData")
out_tab2 <- file.path(catd_wd, "Table_2.RData")

# load historical data and change name
# tab1
load(hist_tab1)
tab1_hist <- tab1
# tab2
load(hist_tab2)
tab2_hist <- tab2



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### JOIN HISTORICAL TABLE 1 & TUNE ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nms <- c('stock', 'country', 'year', 'category', 'repcategory', 'raisedorimported', 
         'misreportedarea', 'area', 'season', 'seasontype', 'fleet', 'caton', 
         'effort', 'uniteffort', 'officiallandings', 'sampledorestimated', 
         'sampledcatch', 'NLengthSamples', 'NLengthMeasured')

# read last year data.
tab1 <- as_tibble(read.csv(file = table1))[,1:19]
names(tab1) <- nms

tab1$category <- recode(tab1$category, `BMS landing` = 'Landings', `Logbook Registered Discard` = 'Discards') 
#! PENDING ICES response on how to proceed

# Recode season
tab1$season <- ifelse(tab1$season>4, 'all', tab1$season)

# Recode area from the all roman numbers to the official code, and 
names(tab1)[16] <- 'type'
tab1 <- tab1 %>% mutate(area = ifelse(nchar(area) == 5, substr(area, 1,4), substr(area, 1,6))) %>%  # do not distinguish between sub-divisions.
                 mutate(div = substr(area, 1,4)) %>% 
                 mutate(div = ifelse(div %in% c('27.1', '27.2'), '27.3', div)) %>% 
                 mutate(gear = substr(fleet, 1,7)) %>% mutate(gear = sub('-' , '_', gear)) %>% 
                 mutate(gear = ifelse(substr(gear,1,3) %in% c("MIS","SDN","SSC","FPO","DRB","PS_"), 'MIS', gear)) %>% 
                 mutate(gear = ifelse(substr(gear,1,3) %in% c("OTB","TBB","OTM","PTB","PTM","OTT"), paste('TRW', substr(gear,4,7), sep =""), gear)) %>% 
                 mutate(gear = ifelse(substr(gear,5,7) != 'CRU', substr(gear, 1,3), gear)) %>% 
                 mutate(gear = ifelse(substr(gear,1,3) %in% c("GNS", "GTR"), 'GLN', gear)) %>% 
                 mutate(a3 = ifelse(substr(area, 4,4)<7, 0, substr(area, 4,4))) %>% 
                 mutate(ss3_fl = case_when((a3 < 7 | gear == 'MIS') ~ 'OTHER', 
                                           (a3 > 6 & gear == 'TRW' & country %in% 'Spain') ~ paste('SPTRAWL', a3, sep = ""), 
                                           (a3 == 8 & country == 'France' & gear == 'TRW_CRU') ~ 'FRNEP8',
                                           (a3 > 6 & substr(gear,1,3) == 'TRW') ~ 'TRAWLOTH', TRUE ~ gear)) %>% 
                mutate(ss3_fl = recode(ss3_fl, GLN = 'GILLNET', LLS = 'LONGLINE'),
                       type   = recode(type, Estimated_Distribution = 'estimated', Sampled_Distribution = 'sampled')) 

# dga, 2022/04/08 in ss3_fl disaggregate the fleet OTHER in two fleets.
tab1 <- tab1 %>% mutate(ss3_fl = case_when(ss3_fl == 'OTHER' & substr(fleet,1,3) %in% c("OTB", "OTM", "OTT", "PTB", "PTM", "TBB")    ~ 'NSTRAWL',
                                           ss3_fl == 'OTHER' & !(substr(fleet,1,3) %in% c("OTB", "OTM", "OTT", "PTB", "PTM", "TBB")) ~ 'OTHERS',
                                           ss3_fl == 'TRAWLOTH' & substr(fleet,5,7) == 'CRU' ~ 'TRAWLOTH_CRU',
                                           ss3_fl == 'TRAWLOTH' & substr(fleet,5,7) != 'CRU' ~ 'TRAWLOTH_DEF',                        
                                           TRUE ~ ss3_fl))

# Join last year to historical data
tab1 <- rbind(tab1_hist, tab1)

save(tab1, file = out_tab1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Tune Table 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nms <- c('stock', 'country', 'year', 'category', 'reportingcategory', 
         'catonraisedorimported', 'misreportedarea', 'area', 'season', 
         'seasontype', 'fleet', 'caton', 'effort', 'uniteffort', 'officiallandings', 
         'type', 'sex', 'ageorlength', 'ageorlengthtype', 'canum', 'weca', 'leca', 
         'sampledcatch', 'NLengthSamples', 'NLengthMeasured')

# read last year data
tab2 <- as_tibble(read.csv(file = table2))[,1:25]
names(tab2) <- tolower(names(tab2))
names(tab2) <- nms

tab2$category <- recode(tab2$category, `BMS landing` = 'Landings', `Logbook Registered Discard` = 'Discards')
#! PENDING ICES response on how to proceed


# Recode season : If the line below is executed changes must be made in 02 script when detecting observations with season = year or all.
#tab2$season <- ifelse(tab2$season>4, 'all', tab1$season)

# Recoding
tab2 <- tab2 %>% mutate(area = ifelse(nchar(area) == 5, substr(area, 1,4), substr(area, 1,6))) %>%  # do not distinguish between sub-divisions.
                 mutate(div = substr(area, 1,4)) %>% 
                 mutate(div = ifelse(div %in% c('27.1', '27.2'), '27.3', div)) %>% 
                 mutate(gear = substr(fleet, 1,7)) %>% mutate(gear = sub('-' , '_', gear)) %>% 
                 mutate(gear = ifelse(substr(gear,1,3) %in% c("MIS","SDN","SSC","FPO","DRB","PS_"), 'MIS', gear)) %>% 
                 mutate(gear = ifelse(substr(gear,1,3) %in% c("OTB","TBB","OTM","PTB","PTM","OTT"), paste('TRW', substr(gear,4,7), sep =""), gear)) %>% 
                 mutate(gear = ifelse(substr(gear,5,7) != 'CRU', substr(gear, 1,3), gear)) %>% 
                 mutate(gear = ifelse(substr(gear,1,3) %in% c("GNS", "GTR"), 'GLN', gear)) %>% 
                 mutate(a3 = ifelse(substr(area, 4,4)<7, 0, substr(area, 4,4))) %>% 
                 mutate(ss3_fl = case_when((a3 < 7 | gear == 'MIS') ~ 'OTHER', 
                            (a3 > 6 & gear == 'TRW' & country %in% 'Spain') ~ paste('SPTRAWL', a3, sep = ""), 
                            (a3 == 8 & country == 'France' & gear == 'TRW_CRU') ~ 'FRNEP8',
                            (a3 > 6 & substr(gear,1,3) == 'TRW') ~ 'TRAWLOTH', TRUE ~ gear)) %>% 
                 mutate(ss3_fl = recode(ss3_fl,GLN = 'GILLNET', LLS = 'LONGLINE'),
                        type   = recode(type, Estimated_Distribution = 'estimated', Sampled_Distribution = 'sampled')) 


# dga, 2022/04/08 in ss3_fl dissaggregate the fleet OTHER in two fleets.
tab2 <- tab2 %>% mutate(ss3_fl = case_when(ss3_fl == 'OTHER' & substr(fleet,1,3) %in% c("OTB", "OTM", "OTT", "PTB", "PTM", "TBB")    ~ 'NSTRAWL',
                                           ss3_fl == 'OTHER' & !(substr(fleet,1,3) %in% c("OTB", "OTM", "OTT", "PTB", "PTM", "TBB")) ~ 'OTHERS',
                                           ss3_fl == 'TRAWLOTH' & substr(fleet,5,7) == 'CRU' ~ 'TRAWLOTH_CRU',
                                           ss3_fl == 'TRAWLOTH' & substr(fleet,5,7) != 'CRU' ~ 'TRAWLOTH_DEF', 
                                           TRUE ~ ss3_fl))

# Join last year to historical data
tab2 <- rbind(tab2_hist, tab2)

save(tab2, file = out_tab2)

