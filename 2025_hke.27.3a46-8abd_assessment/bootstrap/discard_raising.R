################################################################################
#  hke.27.3a46-8abd_assessment : discard data raising                          #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# ss3_previous.R - Load previous assessment files
# ~/*_hke.27.3a46-8abd_assessment/bootstrap/ss3_previous.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


# setwd("./bootstrap/data/discard_raising") # for testing

#==============================================================================
# DATA                                                                     ----
#==============================================================================

dtyr <- 2024
yy   <- substr(dtyr,3,4)

# print(getwd()) #"~/*_hke.27.3a46-8abd_assessment/bootstrap/data/discard_raising"

foln   <- "discard_raising"

inp_wd <- file.path("..","..","initial/data",foln)
dat_wd <- file.path("..",foln,"dat")

# Next year read from previous year data
# # lastdat_wd  <- "https://github.com/ices-taf/2023_hke.27.3a46-8abd_assessment/blob/main/bootstrap/data/discard_raising"
# lastdat_wd <- paste0("c:/use/GitHub/ICES_taf/",dtyr,"_hke.27.3a46-8abd_assessment/bootstrap/data/discard_raising/dat")
lastdat_wd <- file.path("..", "..", "initial", "ass_prev", "discard_raising_dat")

# Create directories

dir.create(dat_wd, recursive = TRUE)


# Import historical data files

disc_files <- dir(lastdat_wd)
for (filen in disc_files)
  file.copy(file.path(lastdat_wd, filen), 
            file.path(dat_wd, filen), copy.date = TRUE)


# Files listing

files   <- list.files(inp_wd)
rmdfile <- grep(".Rmd$", files, value = TRUE)
flcltab <- "fleets_cleanup_table_new.csv"


# Copy 

# - data files
for (filen in files[!files %in% c(rmdfile,flcltab)])
  file.copy(file.path(inp_wd, filen), file.path(dat_wd, gsub("\\.", paste0("_",dtyr,"."), filen)), copy.date = TRUE)

if (file.exists(file.path(inp_wd, flcltab))) {
  flcl  <- dplyr::bind_rows(read.csv(file.path(lastdat_wd, "fleets_cleanup_table.csv")), 
                            read.csv(file.path(inp_wd, flcltab)))
  write.csv(dplyr::arrange(flcl, fleet_old), file.path(dat_wd, "fleets_cleanup_table.csv"), row.names = FALSE)
}
#   file.remove(file.path(dat_wd, gsub("\\.", paste0("_",dtyr,"."), rmdfile)))

# - code for processing data
file.copy(file.path(inp_wd, rmdfile), file.path("..",foln, rmdfile), copy.date = TRUE)


#==============================================================================
# REMOVE                                                                   ----
#==============================================================================

rm(foln, inp_wd, dat_wd, lastdat_wd, files, rmdfile, flcltab, filen)


#==============================================================================
# SUMMARY                                                                  ----
#==============================================================================

# print(getwd())
rmarkdown::render(file.path(".",paste0("aggregate_IC_data_hke_",dtyr+1,".Rmd")), output_format='html_document', clean=TRUE)

