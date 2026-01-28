##----------------------------------------------------------------------------------------
## Run analysis, write model results
##
## Dorleta Garcia (AZTI) 
##
##  2021/05/07
##
## Before: data.R
## After:
##
##----------------------------------------------------------------------------------------

library(icesTAF)
library(r4ss)


#==============================================================================
# PREVIOUS ASSESSMENT                                                      ----
#==============================================================================

# directories and files

mkdir("model/last_final")

file.copy(file.path(taf.boot.path("data"),"ss3_previous",datp_file), file.path("model","last_final",datp_file), overwrite = TRUE)
file.copy(file.path(taf.boot.path("data"),"ss3_previous",ctlp_file), file.path("model","last_final",ctlp_file), overwrite = TRUE)
file.copy(file.path(taf.boot.path("data"),"ss3_previous","starter.ss"),     file.path("model","last_final","starter.ss"), overwrite = TRUE)
file.copy(file.path(taf.boot.path("data"),"ss3_previous","forecast.ss"),    file.path("model","last_final","forecast.ss"), overwrite = TRUE)
file.copy(file.path(taf.boot.path("software"),"ss.exe"), file.path("model","last_final","ss.exe"), overwrite = TRUE)

# run SS3 (no hessian)

setwd('model/last_final')

t1 <- Sys.time()
system(paste('ss.exe', '-nohess'), intern = TRUE)
t2 <- Sys.time()
t2-t1

setwd(file.path("..",".."))


#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

#------------------------------------------------------------------------------------
# Create the directory structure where the different model runs will be executed
#------------------------------------------------------------------------------------

# Scenarios:
# - saly: same as last year

run     <- "final"
dat_wd  <- file.path("data","ss3_saly")

run_wd  <- file.path("model",run)

mkdir(run_wd)


#----------------------------------------------------
# Copy the input files on those directories
#----------------------------------------------------

file.copy(file.path(dat_wd,data_file),     file.path(run_wd,data_file), overwrite = TRUE)
file.copy(file.path(dat_wd,ctrl_file),     file.path(run_wd,ctrl_file), overwrite = TRUE)
file.copy(file.path(dat_wd,"starter.ss"),  file.path(run_wd,"starter.ss"), overwrite = TRUE)
file.copy(file.path(dat_wd,"forecast.ss"), file.path(run_wd,"forecast.ss"), overwrite = TRUE)
file.copy(file.path(taf.boot.path("software"),"ss.exe"), file.path(run_wd,"ss.exe"), overwrite = TRUE)


#-------------------------------------------------------------------------
# Run SS3 without Hessian for a preliminary analysis of the results.
#-------------------------------------------------------------------------

nohess_wd <- file.path(run_wd,"nohess")
mkdir(nohess_wd)

file.copy(file.path(dat_wd,data_file),     file.path(nohess_wd,data_file), overwrite = TRUE)
file.copy(file.path(dat_wd,ctrl_file),     file.path(nohess_wd,ctrl_file), overwrite = TRUE)
file.copy(file.path(dat_wd,"starter.ss"),  file.path(nohess_wd,"starter.ss"), overwrite = TRUE)
file.copy(file.path(dat_wd,"forecast.ss"), file.path(nohess_wd,"forecast.ss"), overwrite = TRUE)
file.copy(file.path(taf.boot.path("software"),"ss.exe"), file.path(nohess_wd,"ss.exe"), overwrite = TRUE)

setwd(nohess_wd)
t1 <- Sys.time()
system(paste('ss.exe', '-nohess'), intern = TRUE)
t2 <- Sys.time()
t2-t1
setwd(file.path("..",".."))


#-------------------------------------------------------------------------
# Run SS3 with Hessian for complete results.
#-------------------------------------------------------------------------

hess_wd <- file.path(run_wd,"hess")
mkdir(hess_wd)

file.copy(file.path(dat_wd,data_file),     file.path(hess_wd,data_file), overwrite = TRUE)
file.copy(file.path(dat_wd,ctrl_file),     file.path(hess_wd,ctrl_file), overwrite = TRUE)
file.copy(file.path(dat_wd,"starter.ss"),  file.path(hess_wd,"starter.ss"), overwrite = TRUE)
file.copy(file.path(dat_wd,"forecast.ss"), file.path(hess_wd,"forecast.ss"), overwrite = TRUE)
file.copy(file.path(taf.boot.path("software"),"ss.exe"), file.path(hess_wd,"ss.exe"), overwrite = TRUE)

setwd(hess_wd)
t1 <- Sys.time()
system('ss.exe', intern = TRUE)
t2 <- Sys.time()
t2-t1
setwd(file.path("..",".."))


#-------------------------------------------------------------------------
#  
#     RUN THE RETROSPECTIVE
#
# Export the results and run the analysis: save the output within 'post' folder
#   USES: "model_02_assessment_results_retro.R" script. 
#
#-------------------------------------------------------------------------

retro <- 1:5

source(file.path(taf.boot.path("software"),"retro_function.R"), echo=TRUE)

t1 <- Sys.time()

retros(dir = run_wd, oldsubdir = "", newsubdir = "retros", subdirstart = "retro", years = -(1:5), 
       exe = "ss", extras = "-nox -nohess")

# retros(run_wd, '', extras = '-nohess -nox', years = -1)
# retros(run_wd, '', extras = '-nohess -nox', years = -2)
# retros(run_wd, '', extras = '-nohess -nox', years = -3)
# retros(run_wd, '', extras = '-nohess -nox', years = -4)
# retros(run_wd, '', extras = '-nohess -nox', years = -5)

t2 <- Sys.time()
t2-t1


#-------------------------------------------------------------------------
# Run R0 profile.
#-------------------------------------------------------------------------

LNR0 <- 12.7803
R0.vec <- LNR0*seq(0.94,1.06,0.02)

setwd(run_wd)

root_R0profile <- 'LN_R0'
mkdir(root_R0profile)

for (iter in 1:length(R0.vec)) {
  
  r0_wd <- file.path(root_R0profile, paste0('prof-', iter))
  mkdir(r0_wd)
  
  copy_SS_inputs(dir.old = getwd(), dir.new = r0_wd, create.dir = TRUE,
                 overwrite = TRUE, copy_par = FALSE, verbose = TRUE)
  file.copy(ctrl_file, file.path(r0_wd,'control_modified.ss'), overwrite = TRUE)
  file.copy("ss.exe", file.path(r0_wd,"ss.exe"), overwrite = TRUE)
  
  # Modify starter
  starter <- SS_readstarter(file.path(r0_wd, 'starter.ss'))
  # - change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  # - make sure the prior likelihood is calculated for non-estimated quantities
  starter$prior_like <- 1
  # - write modified starter file
  SS_writestarter(starter, dir=r0_wd, overwrite=TRUE)
  
  t1 <- Sys.time()
  
  setwd(r0_wd) #! PROVISIONAL
  
  R0profile <- profile(dir        = getwd(),   # not working with r0_wd
                       oldctlfile = ctrl_file, 
                       newctlfile = "control_modified.ss", 
                       string     = "SR_LN(R0)",
                       profilevec = R0.vec, 
                       whichruns  = iter, # In each task of the cluster only the run h.vec[iter] is done.
                       exe        = "ss.exe", 
                       extras     = '-nox -nohess', 
                       verbose    = FALSE)
  
  save(R0profile, file = file.path("..", paste0('LN_R0_profile', iter, '.RData')))
  
  setwd(file.path("..",".."))
  
  t2 <- Sys.time()
  t2-t1
  
}

iter <- 2

r0_wd          <- file.path(root_R0profile, paste0('prof-', iter))
mkdir(r0_wd)

copy_SS_inputs(dir.old = dat_wd, dir.new = r0_wd, create.dir = TRUE,
               overwrite = TRUE, copy_par = FALSE, verbose = TRUE)
# starter, forecast, data, ctl
file.copy(file.path(r0_wd,ctrl_file), file.path(r0_wd,'control_modified.ss'), overwrite = TRUE)
file.copy(file.path(taf.boot.path("software"),"ss.exe"), file.path(r0_wd,"ss.exe"), overwrite = TRUE)

# Modify starter
starter_file <- file.path(r0_wd, 'starter.ss')
starter <- SS_readstarter(starter_file)
# - change control file name in the starter file
starter$ctlfile <- "control_modified.ss"
# - make sure the prior likelihood is calculated for non-estimated quantities
starter$prior_like <- 1
# - Use ss.par
mrun <- ifelse('hess' %in% dir(run_wd), 'hess', 'nohess')
file.copy(file.path(run_wd, mrun, 'ss.par'), file.path(r0_wd, 'ss.par'))
starter$init_values_src <- 1
# - write modified starter file
SS_writestarter(starter, dir=r0_wd, overwrite=TRUE)

t1 <- Sys.time()

setwd(r0_wd)

R0profile <- profile(dir        = getwd(), 
                     oldctlfile = ctrl_file, # masterctlfile = ctrl_file, 
                     newctlfile = "control_modified.ss", 
                     string     = "SR_LN(R0)",
                     profilevec = R0.vec, 
                     usepar     = TRUE, # to use ss.par values as initial
                     whichruns  = iter, # In each task of the cluster only the run h.vec[iter] is done.
                     exe        = "ss.exe", 
                     extras     = '-nox -nohess', 
                     verbose    = FALSE)

save(R0profile, file = file.path(root_R0profile, paste0('LN_R0_profile', iter, '.RData')))

setwd(file.path("..","..","..",".."))

t2 <- Sys.time()
t2-t1



#-------------------------------------------------------------------------
# Run jitter.
#-------------------------------------------------------------------------


