################################################################################
#  WGBIE northern hake - STF comparison with previous year                     #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  12/05/2023                                                       #
#   modified:                                                                  #
################################################################################

# report_05_STF_comparison.R - comparing STF settings with previous year ones
# ~/report_05_STF_comparison.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


rm(list = ls()[!ls() %in% c("dtyr","run","Fmsy","stockcode")])

# stockcode <- 'hke.27.3a46-8abd'
# run <- 'final'
# Fmsy <- 0.243


#==============================================================================
# WORKING DIRECTORIES                                                      ----
#==============================================================================

save_wd <- file.path("report","STF")

if(!dir.exists(save_wd)) dir.create(save_wd)


# Input data (dirs and files)

stftab_cy.dir <- file.path("model", "final", "table_Fmult.csv")
fmult_cy      <- 1.15703960595326 # F2026 = 0.241254 #! TO BE estimated given table_Fmult.csv
stf_cy.dir    <- file.path(".","model", "stf", paste0("Fmult_",fmult_cy))
ass_py        <- file.path( "..", paste0(dtyr,"_",stockcode,"_assessment"))
stftab_py.dir <- file.path(ass_py, "model", "final", "table_Fmult.csv")
fmult_py      <- 1.07970594780237 # F2025 = 0.239888 #! TO BE estimated given table_Fmult.csv 
stf_py.dir <- file.path( ass_py, "model", "stf", paste0("Fmult_",fmult_py))


# - Summary tables
sumtab_cy.file <- file.path("output","advice","summary_table_final.csv")
sumtab_py.file <- file.path(ass_py, "output", "report", "summary_table_final.csv")

# - STF settings
iytab_cy.file <- file.path("model","stf","table","table_intermediate_year.csv")
iytab_py.file <- file.path(ass_py, "output", "catchoptiontable_final.RData")

# - Catch option table
coptab_cy.file <- file.path("model","sum_advice","catchoptiontable_final.csv")
coptab_py.file <- file.path(ass_py, "output", "catchoptiontable_final.RData")

# - Selectivity and retention
selretatlen_cy.file <- file.path("output","sel_ret_atlen_final.RData")
selretatlen_py.file <- file.path(ass_py, "output", "sel_ret_atlen_final.RData")

# - Assessment wd
ass_cy.wd <- file.path("model","stf","Fmult_1")
ass_py.wd <- file.path(ass_py, "model","stf","Fmult_1")

# - Assessment output
SS3_R.name  <- file.path("model", "final", 'post', 'ss3_R_output.RData')


#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

library(dplyr)
library(icesAdvice)
library(ggplot2)
library(ss3om) # readFLSss3


#==============================================================================
# CATCH + REC + F + SSB                                                    ----
#==============================================================================

# Current STF information

sumtab_cy <- read.csv(sumtab_cy.file) %>% 
  rename(cat = CatEst, land = LanEst, disc = DisEst) %>% 
  select(year, ssb, rec, f, cat, land, disc) %>% 
  tidyr::pivot_longer(!year, names_to = "indicator", values_to = "value") %>% 
  filter(!(indicator == "rec" & year == dtyr+1)) %>% 
  mutate(forecast = FALSE)

iytab_cy  <- read.csv(iytab_cy.file)
coptab_cy <- read.csv(coptab_cy.file) %>% filter(X == "MSY approach = FMSY") %>% select(-X)

stf_cy <- bind_cols(iytab_cy, coptab_cy) %>% 
  tidyr::pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
  mutate(year = as.numeric(gsub("\\D", "", var)), 
         indicator = case_when( grepl("^ssb",tolower(var))    ~ "ssb", 
                                grepl("^rec",tolower(var))    ~ "rec", 
                                grepl("^f[0-9]",tolower(var)) ~ "f", # to avid Fland and Fdisc
                                grepl("^cat",tolower(var))    ~ "cat", 
                                grepl("^lan",tolower(var))    ~ "land", 
                                grepl("^dis",tolower(var))    ~ "disc")) %>% 
  filter(!is.na(year), !is.na(indicator)) %>% 
  select(year, indicator, value) %>% mutate(forecast = TRUE)

datcy <- bind_rows(sumtab_cy, stf_cy) %>% mutate(wg = paste0("wgbie",dtyr+1))

  
# Previous year STF information

sumtab_py <- read.csv(sumtab_py.file) %>% 
  rename(cat = CatEst, land = LanEst, disc = DisEst) %>% 
  select(year, ssb, rec, f, cat, land, disc) %>% 
  tidyr::pivot_longer(!year, names_to = "indicator", values_to = "value") %>% 
  filter(!(indicator == "rec" & year == dtyr)) %>% 
  mutate(forecast = FALSE)

iytab_py  <- R.utils::loadToEnv(iytab_py.file)[["interimyear"]]
coptab_py <- R.utils::loadToEnv(coptab_py.file)[["advTab"]] %>% 
  filter(X == "MSY approach = FMSY") %>% select(-X)

stf_py <- bind_cols(iytab_py, coptab_py) %>% 
  tidyr::pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
  mutate(year = as.numeric(gsub("\\D", "", var)), 
         indicator = case_when( grepl("^ssb",tolower(var))    ~ "ssb", 
                                grepl("^rec",tolower(var))    ~ "rec", 
                                grepl("^f[0-9]",tolower(var)) ~ "f", # to avid Fland and Fdisc
                                grepl("^cat",tolower(var))    ~ "cat", 
                                grepl("^lan",tolower(var))    ~ "land", 
                                grepl("^dis",tolower(var))    ~ "disc")) %>% 
  filter(!is.na(year), !is.na(indicator)) %>% 
  select(year, indicator, value) %>% mutate(forecast = TRUE)

datpy <- bind_rows(sumtab_py, stf_py) %>% mutate(wg = paste0("wgbie",dtyr))
  #                                                %>% 
  # # add missing value for SSB_dtyr #!! NOT NECESSARY?
  # bind_rows(data.frame(year = dtyr, indicator = "ssb", value = 186358, forecast = FALSE, wg = paste0("wgbie",dtyr)))

# join both

dat <- bind_rows(datcy, datpy)


#==============================================================================
# SELECTIVITY                                                              ----
#==============================================================================

# Commercial fleets
load(SS3_R.name) # output
fltnms <- setNames(output$definitions$Fleet_name, output$fleet_ID)[which(output$IsFishFleet)]

# Function to rename fleet and sex

rn_flsex <- function(x) {
  
  
  # fltnms <- c('SPTR7', 'TROTH', 'FRNEP8', 'SPTR8', 'GILLNET', 'LONGLINE', 'OTHHIST', 'NSTRAWL', 'OTHER')
  # names(fltnms) <- 1:9
  
  out <- x %>% filter(Fleet %in% names(fltnms)) %>% 
    mutate(FleetNm = fltnms[Fleet], 
           Sex = factor(Sex, levels = 1:2, labels = c('F','M')))
  
  return(out)
  
}


# Selectivity

selatlen_cy <- R.utils::loadToEnv(selretatlen_cy.file)[["selatlen"]] %>% 
  rn_flsex() %>% 
  mutate(wg = paste0("wgbie",dtyr+1), 
         forecast = case_when(Yr == dtyr+1 ~ TRUE, TRUE ~ FALSE)) %>% 
  filter(Yr %in% c(dtyr + (-2):1))

selatlen_py <- R.utils::loadToEnv(selretatlen_py.file)[["selatlen"]] %>% 
  rn_flsex() %>% 
  mutate(wg = paste0("wgbie",dtyr), 
         forecast = case_when(Yr == dtyr ~ TRUE, TRUE ~ FALSE)) %>% 
  filter(Yr %in% c((dtyr-1) + (-2):1))

selatlen <- bind_rows(selatlen_cy, selatlen_py)


# Retention

retatlen_cy <- R.utils::loadToEnv(selretatlen_cy.file)[["retatlen"]] %>% 
  rn_flsex() %>% 
  mutate(wg = paste0("wgbie",dtyr+1), 
         forecast = case_when(Yr == dtyr+1 ~ TRUE, TRUE ~ FALSE)) %>% 
  filter(Yr %in% c(dtyr + (-2):1))

retatlen_py <- R.utils::loadToEnv(selretatlen_py.file)[["retatlen"]] %>% 
  rn_flsex() %>% 
  mutate(wg = paste0("wgbie",dtyr), 
         forecast = case_when(Yr == dtyr ~ TRUE, TRUE ~ FALSE)) %>% 
  filter(Yr %in% c((dtyr-1) + (-2):1))

retatlen <- bind_rows(retatlen_cy, retatlen_py)


#==============================================================================
# Mage                                                                     ----
#==============================================================================

hke.stk_cy <- readOutputss3(stf_cy.dir, repfile = "Report.sso", compfile = "CompReport.sso")
hke.stk_py <- readOutputss3(stf_py.dir, repfile = "Report.sso", compfile = "CompReport.sso")


ages <- 0:15
yrs  <- dtyr + 0:2


for (ass in c("cy","py")) {
  
  if (ass == "cy") {  out <- hke.stk_cy
  } else           {  out <- hke.stk_py }
  
  # Nage (1st jan)
  
  nage_all <- out$natage %>% tidyr::pivot_longer(all_of(as.character(ages)), names_to = "age", values_to = "value") %>% 
    filter(Yr %in% yrs, `Beg/Mid` == "B")
  
  nageR <- nage_all %>% filter(age == 0) %>% group_by(Yr, age) %>% summarise(value = sum(value))
  nageA <- nage_all %>% filter(Seas == 1) %>% group_by(Yr, age) %>% summarise(value = sum(value))
  
  nage <- bind_rows(nageR, nageA) %>% group_by(Yr, age) %>% summarise(value = sum(value))
  
  # Cage
  
  cage <- out$catage %>% tidyr::pivot_longer(all_of(as.character(ages)), names_to = "age", values_to = "value") %>% 
    filter(Yr %in% yrs) %>% group_by(Yr, age) %>% summarise(value = sum(value))
  
  # Sel
  
  zage <- out$Z_at_age %>% mutate_if(is.character,as.numeric) %>% 
    tidyr::pivot_longer(all_of(as.character(ages)), names_to = "age", values_to = "value") %>% 
    group_by(Yr, age) %>% summarise(tot_mort = sum(value)) %>% filter(Yr %in% yrs)
  
  mage_all <- out$M_at_age 
  if ("NoName" %in% names(mage_all)) mage_all <- mage_all %>% select(-NoName) 
  
  mage <- mage_all %>% mutate_if(is.character,as.numeric) %>% 
    tidyr::pivot_longer(all_of(as.character(ages)), names_to = "age", values_to = "value") %>% 
    group_by(Yr, age) %>% summarise(m = sum(value)) %>% filter(Yr %in% yrs)
  
  fbar <- out$exploitation %>% filter(!is.na(F_std)) %>% select(Yr, F_std) %>% filter(Yr %in% yrs)
  
  fage <- left_join(zage, mage) %>% left_join(fbar) %>% mutate(f = tot_mort - m, sel = f/F_std)
  
  selage <- fage %>% rename(value = sel) %>% select(Yr, age, value)
  
  
  assign( paste0("dat",ass),
          bind_rows(nage %>% mutate(indicator = "nage"), 
                    cage %>% mutate(indicator = "cage"), 
                    selage %>% mutate(indicator = "selage")) %>% 
            mutate(wg = ifelse(ass == "cy", paste0("wgbie",dtyr+1), paste0("wgbie",dtyr))))
  
}

dage <- bind_rows(datcy, datpy)


#==============================================================================
# FIGURES                                                                  ----
#==============================================================================

# catcj + rec + f + ssb

dd <- dat %>% filter(year > 2018, indicator %in% c("cat","rec","f","ssb")) %>% 
  mutate(#year = as.factor(year), 
         value = case_when( indicator %in% c("ssb","cat","rec") ~ value/1000, 
                            indicator == "f" ~ value), 
         indicator = factor(indicator, levels = c("cat","rec","f","ssb"), 
                            labels = c("Catches (1000 t)","Recruitment - age 0 (millions)",
                                       "Fishing mortality","Spawning Stock Biomass"), 
                            ordered = TRUE))
p <- ggplot(dd, aes(year, value, col = wg, lty = forecast)) +
  geom_line(lwd = 1.25) + geom_point(size = 2) +
  facet_wrap(indicator~., scales = 'free') +
  theme_bw() + theme(text = element_text(size = 15)) +
  ylab("")

png(file.path(save_wd,"STF_summaryPlot.png"), width = 700, height = 500)
  print(p)
dev.off()


# Selectivity & retention

selretatlen <- bind_rows(selatlen, retatlen)

psr <- ggplot(selretatlen%>% filter(Sex == "F"), aes(lng, value, col = Yr, lty = wg)) +
  geom_line(lwd = 1.25) + 
  facet_wrap(FleetNm ~ Factor, scales = 'free', ncol = 6) +
  theme_bw() + theme(text = element_text(size = 15)) +
  ylab("")

png(file.path(save_wd,"STF_select_retent.png"), width = 1200, height = 800)
  print(psr)
dev.off()


# Selectivity

psel <- ggplot(selatlen %>% filter(Sex == "F"), aes(lng, value, col = Yr, lty = wg)) +
  geom_line(lwd = 1.25) + 
  facet_wrap(FleetNm ~ ., scales = 'free') +
  theme_bw() + theme(text = element_text(size = 15)) +
  ylab("")

png(file.path(save_wd,"STF_selectivities.png"), width = 600, height = 800)
  print(psel)
dev.off()


# Retention

pret <- ggplot(retatlen %>% filter(Sex == "F"), aes(lng, value, col = Yr, lty = wg)) +
  geom_line(lwd = 1.25) + 
  facet_wrap(FleetNm ~ ., scales = 'free') +
  theme_bw() + theme(text = element_text(size = 15)) +
  ylab("")

png(file.path(save_wd,"STF_retentions.png"), width = 600, height = 800)
  print(pret)
dev.off()


# Na - Wa, Sa (Fa/Fbar)

dage <- dage %>% mutate(age = as.numeric(age)) %>% #%>% filter(age > 3) #%>% filter(age != 0)
  filter(!(wg == paste0("wgbie",dtyr) & indicator == "cage" & Yr == dtyr+2))

page <- ggplot(dage, aes(age, value, col = wg)) +
  geom_line(lwd = 1.25) + geom_point(size = 2) +
  facet_grid(indicator ~ Yr, scales = 'free') +
  theme_bw() + theme(text = element_text(size = 15)) +
  ylab("")

png(file.path(save_wd,"STF_ages.png"), width = 600, height = 800)
  print(page)
dev.off()

dd <- dage %>% mutate(age = as.numeric(age)) %>% 
  tidyr::pivot_wider(names_from = "wg", values_from = "value") %>% 
  mutate(rel_chg = get(paste0("wgbie",dtyr+1))/get(paste0("wgbie",dtyr)))

qage <- ggplot(dd, aes(age, rel_chg)) +
  geom_line(lwd = 1.25) + geom_point(size = 2) +
  geom_hline(yintercept = 1, lty = 2, col = 2) +
  facet_grid(indicator ~ Yr, scales = 'free_y') +
  theme_bw() + theme(text = element_text(size = 15)) +
  ylab(paste0("wgbie",dtyr+1,"/wgbie",dtyr))

png(file.path(save_wd,"STF_ages_rel.png"), width = 600, height = 800)
  print(qage)
dev.off()

dd <- dage %>% mutate(age = as.numeric(age)) %>% 
  tidyr::pivot_wider(names_from = wg, values_from = value) %>% 
  mutate(chg_relpy = get(paste0("wgbie",dtyr+1))/get(paste0("wgbie",dtyr)) - 1)

qage_chg <- ggplot(dd, aes(age, chg_relpy)) +
  geom_line(lwd = 1.25) + geom_point(size = 2) +
  geom_hline(yintercept = 0, lty = 2, col = 2) +
  facet_grid(indicator ~ Yr, scales = 'free_y') +
  theme_bw() + theme(text = element_text(size = 15))

png(file.path(save_wd,"STF_ages_relchg.png"), width = 600, height = 800)
  print(qage_chg)
dev.off()


