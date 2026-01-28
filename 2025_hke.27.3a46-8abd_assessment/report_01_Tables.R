#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FORMATTING OF THE TABLES FOR THE REPORT
# Dorleta Garcia 
# 2021/05/10
#  modified:  2022-05-16 (ssanchez@azti.es) - adapt given WKANGHAKE2022 (2 sexes + F from SS? not Carmen's code)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(flextable)
library(icesAdvice)
library(tidyverse)

rep_wd <- file.path("report","wg_report")
if (!dir.exists(rep_wd)) dir.create(rep_wd)

SS3_R.name  <- file.path("model", "final", 'post', 'ss3_R_output.RData')
rep_tables  <- file.path(rep_wd, paste0('tables_', run, '.RData'))
rep_sampTab <- file.path(rep_wd, "SamplingIntensity_by_Country.csv")

rep_sumTab     <- file.path("output", paste0("summary_table_", run, ".RData"))
rep_catOptTab  <- file.path("output", paste0("catchoptiontable_", run, ".RData"))
rep_catOptTabf <- file.path("model", "sum_advice", "catchoptiontable_runs.csv")
adv_catOptTabf <- file.path("model", "sum_advice", paste0("catchoptiontable_", run, ".csv"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REPORTR: TABLE 0.1 NUMBER OF SAMPLES, NO. OF LENGTHS BY COUNTRY AND STOCK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tab1Samp <- read.csv(file.path("data","catch","Table_1.csv")) %>% 
  rename_with(tolower, .cols = everything())  %>% 
  rename(no.samples = no..of.length.samples, no.length = no..of.length.measured) %>% 
  select(country, catchcategory, sampledorestimated, no.samples, no.length) %>% 
  subset(sampledorestimated == 'Sampled_Distribution')

# replace -9 NO SAMPLE with 0
tab1Samp <- tab1Samp %>% 
  mutate(no.samples = ifelse(no.samples == -9, 0, no.samples),
         no.length = ifelse(no.length == -9, 0, no.length))

tab1Samp.ctry <- aggregate(list(no.samples = tab1Samp$no.samples, no.length = tab1Samp$no.length), 
                           list(country = tab1Samp$country, cat = tab1Samp$catchcategory), sum)

write.csv(tab1Samp.ctry, file = file.path("data","report","SamplingIntensity_by_Country.csv"), row.names = FALSE)

tab1Samp.Tot <- tab1Samp.ctry %>% group_by(cat) %>% 
  summarise(no.samples = sum(no.samples), no.length = sum(no.length))

load(SS3_R.name) # output

cnum <- as_tibble(output$timeseries) %>% filter(Era == 'TIME' | (Era == 'FORE' & Yr == dtyr+1)) %>% 
  select(Yr, Seas, starts_with("retain(N)") | starts_with("dead(N)") | starts_with("sel(N)")) %>% 
  tidyr::pivot_longer(cols = -c(Yr,Seas), names_to = "id") %>% 
  mutate(indicator = sapply(strsplit(id, split = ":_"), function(x) gsub("[(]N[)]","",x[[1]])), 
         fleet     = sapply(strsplit(id, split = ":_"), function(x) x[[2]])) %>% 
  select(-id) %>% 
  replace(is.na(.),0) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value)

if (nrow(cnum %>% mutate(ds = dead - sel) %>% filter(ds != 0))==0)
  cnum <- cnum %>% select(-sel)
  
cnum <- cnum %>% 
  mutate(disc = dead - retain) %>% 
  group_by(Yr) %>% summarise(no.land = sum(retain), no.disc = sum(disc))

# - Total No. in international landings and discards ('000)
cnum_last <- cnum %>% filter(Yr == dtyr)

# - Nb. meas. as % of annual nb. caught
tab1Samp.Tot <- tab1Samp.Tot %>% 
  mutate(no.cat = ifelse(cat == "Landings", cnum_last$no.land, 
                         ifelse(cat == "Discards", cnum_last$no.disc, NA)), 
         samp.perc = no.samples/no.cat)

write.csv(tab1Samp.Tot, file = file.path("data","report","SamplingIntensity_total.csv"), row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table 3.1: Landings & Discards by area ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(file.path("data","report","rep_table1.RData"))

# hist_data: a bit tuning
# remove land_3_4_6 and move the values to land_3. Then we will combine cells.
# add land1, land2, land_5
hist_data <- as_tibble(hist_data)  %>% mutate(land_3 = land_3_4_6, land_1 = NA, land_2 = NA, land_5 = NA, disc_5 = NA) %>%
              select(1,paste('land', 1:7,sep = "_"), 'land_8abd','land_unn', 'land_total', 
                     paste('disc', 3:7,sep = "_"), 'disc_8abd', 'disc_total', 'total')
names(hist_data) <- names(new_data)    
  
# rename columns to match those in 
t31 <- bind_rows(hist_data, new_data)
#names(t31) <- c('Year', 1:8, 'Unnallocated', 'Total', 3:8, 'Total', 'Total')
t31[,-1] <- round(t31[,-1]/1000,1)
# Set up general table properties and formatting
# cell_p = cellProperties(padding.right=3, padding.left=3)
# par_p = parProperties(text.align="right")

# convert it to character to be printed correctly.
t31[,1] <- as.character(unlist(t31[,1]))

t31 <- subset(t31, Year <= dtyr)

# Create table
ft31 <- flextable(t31)

# Add a header 
ft31 <- add_header_row(ft31, values = c("", "Landings (t)", "Discards (t)", "Catches(t)"), colwidths = c(1,10,7,1)) 
  

# Combine L3-L6 from 1961 to 2012.
ft31 <- merge_h_range(ft31, 1:(2012-1961+1), j1 = "L_3", j2 = "L_6")
ft31 <- flextable::align(ft31,  align = "center")
ft31


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table 3.2 Number of samples by fleet ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(file.path("data","report","rep_table2.RData"))
names(table32) <- c('ss_fleet', 'Year', 'Discards', 'Landings', 'NLgSp_D', 'NLgSp_L', 'NLgMs_D', 'NLgMs_L')
table32 <- table32 %>% arrange(ss_fleet, Year) %>% select(c(2:1,3:8))
table32[,-(1:2)] <- round(table32[,-(1:2)])
for(j in 1:dim(table32)[2]) table32[,j] <- as.character(unlist(table32[,j]))
t32 <- table32
t32 <- subset(t32, Year <= dtyr)

ft32 <- flextable(t32)
ft32


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table 3.3 Samples by FU and country ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(file.path("data","report","rep_table3.RData"))
t33 <- tab33
ft33 <- flextable(tab33)
ft33 <- autofit(ft33)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table 3.4 Summary Table ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(rep_sumTab)

t34 <- sumTab[,c('year', 'rec', 'biomass', 'ssb', 'LanObs', 'DisObs', 'CatObs', 'yield.ssb', 'f')]
names(t34) <- c("Year", "Recruits Age 0 ('000')", "Total Biomass ('000')", "Female-only SSB (t)", "Landings (t)", "Discards (t)", "Catch (t)", "Yield/SSB (%)", "F_{age(1-7)}")

t34 <- rbind(t34, colMeans(t34, na.rm=T))

for(j in 1:7) t34[,j] <- round(unlist(t34[,j]))
for(j in 8:9) t34[,j] <- icesRound(unlist(t34[,j]))

for(j in 1:9) t34[,j] <- as.character(unlist(t34[,j]))
t34[dim(t34)[1],1]  <- "Arithmetic mean" # for year we don't want mean   
ft34 <- flextable(t34)
ft34 <- hrule(ft34, i = dim(t34)[1]-1)
ft34 <- hline(ft34, i = dim(t34)[1]-1)
ft34 <- autofit(ft34)
ft34 <- add_footer_row(ft34, top = FALSE, values =c("Units", "Thousands
 of individuals",	"Thousands", "Tonnes",	"Tonnes",	"Tonnes",	"Tonnes",	"Percentage",""), 
 colwidths = rep(1,9))
ft34 <- hline(ft34, i = 1, part = 'footer')
ft34


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table 3.5 Catch Option Table ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(rep_catOptTab)

t35a <- interimyear

# reorder and rename
t35a <- t35a %>% select(paste0("Rec",dtyr+1), paste0("F",dtyr+1), paste0("Catches",dtyr+1), paste0("Landings",dtyr+1), 
                        paste0("SSB",dtyr+2), paste0("Rec",dtyr+2))
names(t35a) <- c(paste0("Rec ",dtyr+1), paste0("F_{1-7} ",dtyr+1), paste0("Catch ",dtyr+1), paste0("Land ",dtyr+1), 
                 paste0("SSB ",dtyr+2), paste0("Rec ",dtyr+2))

# Apply ICES rounding
t35a[,2]  <- icesRound(t35a[,2])
t35a[,-2] <- round(t35a[,-2])

ft35a <- autofit(flextable(t35a))
ft35a


coptTab <- read.csv(rep_catOptTabf)
fmultv <- seq(0,2,0.1)
pos <- which(coptTab$scenario != "") # Adv.Sheet scenarios
for (i in 1:length(fmultv))
  pos <- c(pos, which(round(coptTab$Fmult,1) == round(fmultv[i],1))[1])
pos <- sort(unique(pos))

t35b <- coptTab[pos,] %>%
  select(Fmult, paste0("F",dtyr+2), paste0("Fland",dtyr+2),  paste0("Fdisc",dtyr+2), 
         paste0("Catches",dtyr+2), paste0("Landings",dtyr+2), paste0("Discards",dtyr+2), paste0("SSB",dtyr+3), 
         scenario)
names(t35b) <- c("F_{multiplier}", paste0("F_{1-7} catch (",dtyr+2,")"), paste0("F_{1-7} landings (",dtyr+2,")"), paste0("F_{1-7} discards (",dtyr+2,")"),
                 paste0("Catch (",dtyr+2,")"), paste0("Landings (",dtyr+2,")"), paste0("Discards (",dtyr+2,")"), paste0("SSB (",dtyr+3,")"), 
                 "Scenario")

# Apply ICES rounding
for (i in 2:4) t35b[,i] <- icesRound(t35b[,i])

ft35b <- autofit(flextable(as.data.frame(t35b)))
ft35b


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table 3.6 YPR Table ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nrskip <- nrow(read.csv(file.path("output","report",paste0("YPRtable_", run, ".csv"))))-5
t36a <- read.csv(file.path("output","report",paste0("YPRtable_", run, ".csv")), nrows = nrskip)
t36b <- read.csv(file.path("output","report",paste0("YPRtable_", run, ".csv")), skip = nrskip, nrows = 5)
names(t36b) <- names(t36a)
# t36b <- read.csv(paste0("report/YPRtable_", run, ".csv"), skip = 16, nrows = 4, header = 16)

# renaming
names(t36a) <- c("SPR-level","F_{multiplier}","F_{1-7}","YPR-catch","SSB-PR")
names(t36b) <- c("SPR-level","F_{multiplier}","F_{1-7}","YPR-catch","SSB-PR")

# for(j in 1:6) t36a[,j] <- icesRound(t36a[,j])
ft36a <- flextable(t36a)
ft36b <- flextable(t36b)

ft36a
ft36b

save(t32, t31, t33, t34, t35a, t35b, t36a, t36b, 
     ft32, ft31, ft33, ft34, ft35a, ft35b, ft36a, ft36b,  file = rep_tables)

