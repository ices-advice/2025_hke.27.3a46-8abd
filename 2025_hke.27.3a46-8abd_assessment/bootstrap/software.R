################################################################################
#  hke.27.3a46-8abd_assessment : available functions                           #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# software.R - Load previously used functions
# ~/*_hke.27.3a46-8abd_assessment/bootstrap/software.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


# setwd("./bootstrap/data/software") # for testing

#==============================================================================
# DATA                                                                     ----
#==============================================================================

dtyr <- 2024


#==============================================================================
# DATA - new software                                                      ----
#==============================================================================

# Is there new software

for (bf in c("SOFTWARE_new.bib","software_new.bib")) {
  bibf <- file.path("..","..",bf)
  if (file.exists(bibf)) {
    bib_ini <- readLines(bibf)
    file.rename(bibf, file.path("..","..","SOFTWARE_new.bib"))
  }
}
  

#==============================================================================
# DATA - previous year software                                            ----
#==============================================================================

# # fun_wd <- "https://github.com/ices-taf/2023_hke.27.3a46-8abd_assessment/blob/main/bootstrap/software"
# ref_wd <- paste0("c:/use/GitHub/ICES_taf/",dtyr,"_hke.27.3a46-8abd_assessment/bootstrap")
lastdat_wd <- file.path("..", "..", "initial", "ass_prev")

fun_wd <- file.path(lastdat_wd, "software")

# Copy bib file
file.copy(file.path(lastdat_wd,"SOFTWARE.bib"), file.path("..","..","SOFTWARE.bib"), overwrite = TRUE, copy.date = TRUE)

# Create directory ./bootstrap/initial/software
soft_wd <- file.path("..","..","initial","software")
if (!dir.exists(soft_wd)) dir.create(soft_wd)

# Download functions available from previous year assessment folder into this new directory
files <- dir(fun_wd)
# except one that has a newer version
files <- files[!files == "retro_function.R"]
for (filen in files) {
  # download(file.path(lastdat_wd, filen), dir = file.path("..", filen)) #! NEEDS TOKEN??
  file.copy(file.path(fun_wd, filen), file.path("..","..","initial","software",filen), copy.date = TRUE)
}

# # Replace file : reason correction
# file.copy(file.path("..","..","initial","software","aggregate_IC_functions_CORRECT.R"), 
#           file.path("..","..","initial","software","aggregate_IC_functions.R"), overwrite = TRUE, copy.date = TRUE)


#==============================================================================
# Combine SOFTWARE.bib                                                     ----
#==============================================================================

# In case of new software available, we should add extra code to join all SOFTWARE.bib old and new information
if (exists("bib_ini")) {
  bib  <- file.path("..","..","SOFTWARE.bib")
  writeLines(c(readLines(bib), "", bib_ini), bib)
}
# Not new but changed
bib  <- file.path("..","..","SOFTWARE.bib")
bibd <- readLines(bib)
# - retro_function
rl <- grep("@Misc[{]retro_function", bibd)
bibd[rl+0:6] <- c("@Misc{retro_function,",                                                                                        
                  "  author  = {Sonia Sanchez-Maroño},",                                                                                
                  "  year    = {2024},",                                                                                          
                  "  title   = {R function to run the SS retrospective analysis, based on r4ss::retro},",                                               
                  "  version = {1.0},",                                                                                           
                  "  source  = {initial/software/retro_function.R},",                                                             
                  "}")
# - ss (both win and linux)
ssl <- grep("@Misc[{]ss", bibd)
bibd[ssl+0:6] <- c("@Misc{ss,",                                                                                        
                   "  author  = {Rick Methot et al.},",                                                                                
                   "  year    = {2020},",                                                                                          
                   "  title   = {Stock Synthesis executable},",                                               
                   "  version = {3.30.18},",                                                                                           
                   "  source  = {initial/software/ss*},",                                                             
                   "}")
writeLines(bibd, con = bib)



#==============================================================================
# Process SOFTWARE.bib                                                     ----
#==============================================================================

setwd(file.path("..","..",".."))
taf.boot(software = TRUE, data = FALSE, clean = FALSE, force = FALSE)

# unlink(file.path(taf.boot.path("data"),"software"))


#==============================================================================
# REMOVE                                                                   ----
#==============================================================================

rm(bf, bibf, lastdat_wd, fun_wd, soft_wd, filen)

