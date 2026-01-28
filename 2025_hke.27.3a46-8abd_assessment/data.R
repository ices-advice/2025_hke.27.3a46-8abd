##
## Dorleta Garcia (dgarcia@azti.es)
## AZTI
## 09/05/2021
##  modified:  2023-04-20 12:05:44 (ssanchez@azti.es) - 2023 update
##
## Preprocess data, write TAF data tables
##
## Before:
## After:

# remotes::install_github("r4ss/r4ss", ref = "523f45a") # v1.48.1 (19 September 2023)
# remotes::install_github("r4ss/r4ss", ref = "523f45a", lib="/home/ssanchez/rlibs") # v1.48.1 (19 September 2023)

library(icesTAF)
library(ggplot2)
library(tidyr)
library(dplyr)
library(r4ss)

mkdir("data")

ss3d_wd <- file.path("data", "ss3_saly") 
catd_wd <- file.path("data", "catch")
aloc_wd <- file.path("data", "allocations")
datp_wd <- file.path("data", "plots")

mkdir(ss3d_wd)
mkdir(catd_wd)
mkdir(aloc_wd)
mkdir(datp_wd)

dtyr <- 2024

ac <- as.character


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create the SS input files ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## From "CatchAndSampleDataTables.txt" file in InterCatch create 'Table_1.csv' and 
## 'Table_2.csv' with total catch data and length frequency samples.
source('data_00_Create_Tables_from_IC.R')

## Create 'Table1.RData' and 'Table2.RData' with the historical tables.
source('data_01_Table1_Table2.R')

## Do the allocations. Only really needed for TRAWLOTHER and OTHER fleet to account for the 
## difference in selectivity of different segments.
## And create the necessary data pieces to generate the SS input files.
source('data_02_Allocations.R')

## Generate the SS input files.
source('data_03_Create_SS_Files.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Analyse the data ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
source('data_04_Analysis.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Summarise and save the data ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
source('data_05_Summary.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### Surveys info from DATRAS  ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
source('data_06a_surveys_dat.R')
source('data_06b_surveys_plot.R')

