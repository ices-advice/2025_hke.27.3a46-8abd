################################################################################
#  hke.27.3a46-8abd_assessment : short-term forecast final values              #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model_02d_Forecast_interpolation.R - STF final values (after interpolation)
# ~/*_hke.27.3a46-8abd_assessment/model_02d_Forecast_interpolation.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

# Code based on previous works by: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#                                                                                                        
#   Authors:                                       #                                                                     
#   Francesco Masnadi (CNR-IRBIM & UNIBO, Ancona)  #                                                                   
#   Massimiliano Cardinale (SLU Aqua, Lysekil)     #
#   Christopher Griffiths (SLU Aqua, Lysekil)      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Modified 05/04/2022 : Marta Cousido, Francisco Izquierdo & Santiago Cervino
# Modified 10/05/2022 : Dorleta Garcia


library(dplyr)
library(icesAdvice)


#==============================================================================
# DIRECTORIES                                                              ----
#==============================================================================

tabledir_stf <- file.path(stf_path, "table")

dir.create(file.path("model","sum_advice"))
dir.create(file.path("model","sum_report"))


#==============================================================================
# CATCH OPTION TABLE (runs output)                                         ----
#==============================================================================

stTab     <- read.csv( file.path(tabledir_stf, "table_Fmult.csv"))
stTab_int <- read.csv( file.path(tabledir_stf, "table_intermediate_year.csv"))


#==============================================================================
# FUNCTIONS                                                                ----
#==============================================================================

## Function to interpolate -----------------------------------------------------

# Interpolate relevant figures for ST
#colName<-"Catch"
#varVal<-TAC*0.85
#rowName<-"Rec. Plan TAC constraint (-15%)"

interpolateStTab <- function (colName, varVal, rowName, stTab){
  # colName <- valid variable names ("Fmult", "F", "Yield", "Catch", "Bio", "SSB")
  # varVal <- amount to interpolate
  # rowName <- text with reference to identify (e.g. "Fmsy")
  # stTab is the tab produced with rscript with a grid of Fmult
  # It returns a line with the df strulture with all the variables interpolated
  varNames <- names(stTab)
  stopifnot(colName %in% varNames)
  
  # Identify upper and lower row indices regarding varValue
  vec <- stTab[,colName]
  stopifnot(varVal>min(vec) & varVal < max(vec))
  if (which(vec>varVal)[1] == 1) iLow <- max(which(vec>varVal)) 
  else iLow <- max(which(vec<varVal))
  iUp <- iLow + 1
  
  # variabble to estimate (all but varName)
  x1 <- stTab[iLow, colName]
  x2 <- varVal
  x3 <- stTab[iUp, colName]
  y2List <- list()
  for (i in varNames) {
    if (i == colName) y2List[i] <- varVal else {
      y1 <- stTab[iLow, i]
      y3 <- stTab[iUp, i]
      y2List[i] <- y1 + (x2-x1) * (y3-y1) / (x3 - x1)  # function to interpolate
    }
  }
  newLine <- as.data.frame(y2List)
  row.names(newLine) <- rowName
  return(newLine)  # data frame same structure than stTab with the line asked
}


#==============================================================================
# CATCH OPTION TABLE (with interpolation - for Advice Sheet)               ----
#==============================================================================

## Text of Ices template ------------------------------------------------------
# "MSY approach = FMSY"
# "EU MAP: FMSY"
# "F = MAP FMSY lower"
# "F = MAP FMSY upper"
# "F = 0"
# "F = Fpa"
# "SSB (year_inter+2) = Blim"
# "SSB (year_inter+2) = Bpa"
# "SSB (year_inter+2) = MSY Btrigger"
# "SSB (year_inter+2) = SSB(year_inter+1)"
# "F = F(year_inter+1)"


## Set years for the text in Cat. Opt. Tab. basis
intYr <- year_inter
ssbYr <- intYr + 2
catYr <- intYr + 1

### Fmsy ----------------------------------------------------------------------
basis <- "MSY approach = FMSY"
df1 <- interpolateStTab(colName=paste0("F",year_inter+1), 
                        varVal=Fmsy, rowName=basis, stTab)

### EU MAP ----------------------------------------------------------------------
df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), 
                                   varVal=Fmsy, rowName="EU MAP: FMSY", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), 
                                   FmsyLower, "F = FMSY lower", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), 
                                   FmsyUpper, "F = FMSY upper", stTab))


### F = 0 ----------------------------------------------------------------------
xx <- stTab[1,]
#xx[, 1:4] <- 0
rownames(xx) <- "F = 0"
df1 <- rbind(df1, xx)

### PA -----------------------------------------------------------------------------
df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), 
                                   Fpa, "F = Fpa", stTab))
# df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), 
#                                    Flim, "F = Flim", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("SSB",year_inter+2), 
                                   Blim, paste0("SSB (", ssbYr, ") = Blim"), stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("SSB",year_inter+2), 
                                   Bpa, paste0("SSB (", ssbYr, ") = Bpa"), stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("SSB",year_inter+2), 
                                   Bpa, paste0("SSB (", ssbYr, ") = MSY Btrigger"), stTab))

# ### F range -------------------------------------------------------------------
# df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), FmsyLower, "F = FMSY lower", stTab))
# frng <- seq(FmsyLower+0.01, FmsyUpper-0.01, by=0.01)
# for(f in frng){
#   basis <- paste0("F = FMSY lower differing by ", round(f - FmsyLower, 2))
#   df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), f, basis, stTab))
# }
# df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), FmsyUpper, "F = FMSY upper", stTab))

### Equal things ---------------------------------------------------------------
SSBint <- stTab_int[,paste0("SSB",year_inter+1)]
Fint   <- stTab_int[,paste0("F",year_inter)]

df1 <- rbind(df1, interpolateStTab(colName=paste0("SSB",year_inter+2), 
                                   SSBint, paste0("SSB (", ssbYr, ") = SSB (", ssbYr-1, ")"), stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("F",year_inter+1), 
                                   Fint, paste0("F = F (", catYr-1, ")"), stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste0("Catches",year_inter+1), 
                                   TAC, paste0("Catch (", catYr, ") = TAC (", catYr-1, ")"), stTab))

# ### Management plan -------------------------------------------------------------
# 
# df1 <- rbind(df1, interpolateStTab(colName= paste0("Catches",year_inter+1), TAC, "equal TAC", stTab))
# #ForeCatch_2022 10809.5 1264.79
# ForeCatch_2022=10809.5
# catches_fmsy=df1[1,4]
# if(catches_fmsy>ForeCatch_2022*1.2){
#   df1 <- rbind(df1, interpolateStTab(colName= paste0("Catches",year_inter+1), ForeCatch_2022*1.2, paste0("MP Fmsy",year_inter), stTab))
# } else {
#   
# }
# 
# TAC=7836
# if(catches_fmsy>TAC*1.2){
#   df1 <- rbind(df1, interpolateStTab(colName= paste0("Catches",year_inter+1), TAC*1.2, "MP TAC*1.2 (7836)", stTab))
# }

df1 <- df1 %>% mutate(Fland = !!as.name(paste0("F",catYr)) * !!as.name(paste0("Landings",catYr))/!!as.name(paste0("Catches",catYr)),
                      Fdisc = !!as.name(paste0("F",catYr)) * !!as.name(paste0("Discards",catYr))/!!as.name(paste0("Catches",catYr))) %>% 
  rename(!!paste0("Fland",year_inter+1) := Fland, !!paste0("Fdisc",year_inter+1) := Fdisc) %>% 
  mutate_at(vars(-paste0("pBlim_",year_inter+2)), ~replace(., is.na(.), 0)) # keep NA for cases without Hessian runs (i.e. only values for sc.prob)
                      
### Reformat for Advice Sheet ----------------------------------------------------------------------
df2 <- df1 %>% mutate(SSBchg = (!!as.name(paste0("SSB",ssbYr))/SSBint-1)*100, 
                      ADVchg = (!!as.name(paste0("Catches",catYr))/TACadvice-1)*100, 
                      TACchg = (!!as.name(paste0("Catches",catYr))/TAC-1)*100) %>% 
  mutate_at(vars(-paste0("pBlim_",year_inter+2)), ~replace(., is.na(.), 0)) %>% # keep NA for cases without Hessian runs (i.e. only values for sc.prob)
  select(paste0("Catches",catYr), paste0("Landings",catYr), paste0("Discards",catYr), 
         paste0("F",catYr), paste0("Fland",catYr), paste0("Fdisc",catYr), 
         paste0("SSB",ssbYr), SSBchg, ADVchg, TACchg, paste0("pBlim_",ssbYr))

write.csv (df2, file.path("model","sum_advice","catchoptiontable_final.csv")) # keep row.names, as correspondo to scenarios

for (v in names(df2))
  if (v %in% c(paste0(c("F","Fland","Fdisc"),year_inter+1), paste0("pBlim_",ssbYr))) {
    df2[,v] <- icesRound(df2[,v])
  } else
    df2[,v] <- round(df2[,v])

write.csv (df2, file.path("model","sum_advice","catchoptiontable_final_ICESround.csv")) # keep row.names, as correspondo to scenarios

# ## Catch Option Table - COT (ICES format) -----------------------------------------
# cot <- as.data.frame(matrix(nrow=dim(df1)[1], ncol=11))
# names(cot) <- c("basis", "tcat", "wcat", "ucat", "ft", "fw", "fu", "ssb", "schang", "adchang", "tchang")
# cot$basis <- rownames(df1)
# cot$tcat <- round(df1[,4], 0) 
# cot$wcat <- round(df1[,5], 0) 
# cot$ucat <- cot$tcat - cot$wcat
# cot$ft <- round(df1[,3], 2)
# cot$fw <- round(cot$ft * cot$wcat / cot$tcat, 2)
# cot$fw[cot$ft==0] <- 0
# cot$fu <- round(cot$ft * cot$ucat / cot$tcat, 2)
# cot$fu[cot$ft==0] <- 0
# cot$ssb <- round(df1[,2], 0) 
# cot$tchang <- as.character(paste0(round(100*(cot$tcat-TAC)/TAC, 0), " %"))
# cot$adchang <- as.character(paste0(round(100*(cot$tcat-TACadvice)/TACadvice, 0), " %"))
# cot$schang <- as.character(paste0(round(100*(cot$ssb-SSBint)/SSBint, 0), " %"))
# 
# write.csv (cot, file.path(tabledir_stf, "tables.prj", "catOptionsTab.csv"), row.names=FALSE)
# write.table (df1, file.path(tabledir_stf, "tables.prj", "tab_10-st.txt"), row.names=TRUE)


#==============================================================================
# CATCH OPTION TABLE (runs output - for Report with Advice Sheet values)   ----
#==============================================================================

# Reformat for report - rename columns

stfTab <- stTab %>% 
  mutate(Fland = !!as.name(paste0("F",year_inter+1)) * 
           !!as.name(paste0("Landings",year_inter+1))/!!as.name(paste0("Catches",year_inter+1)), 
         Fdisc = !!as.name(paste0("F",year_inter+1)) * 
           !!as.name(paste0("Discards",year_inter+1))/!!as.name(paste0("Catches",year_inter+1))) %>% 
  mutate_at(vars(-paste0("pBlim_",year_inter+2)), ~replace(., is.na(.), 0)) %>% # keep NA for cases without Hessian runs (i.e. only values for sc.prob)
  select(Fmult, paste0("F",year_inter+1), Fland, Fdisc, 
         paste0("Catches",year_inter+1), paste0("Landings",year_inter+1), paste0("Discards",year_inter+1), 
         paste0("SSB",year_inter+2), paste0("pBlim_",year_inter+2)) %>% 
  rename(!!paste0("Fland",year_inter+1) := Fland, !!paste0("Fdisc",year_inter+1) := Fdisc)


# Incorporate Advice Sheet values

stfTab <- stfTab %>% mutate(scenario = "") %>% 
  filter(Fmult != 0) %>% # already in df1
  bind_rows(df1 %>% select(-paste0("Catches",year_inter+2)) %>% mutate(scenario = rownames(df1))) %>% 
  arrange(Fmult)


# Reformat for report - rounding

for (v in names(stfTab)[sapply(stfTab,class) != "character"])
  if (v %in% c(paste0(c("F","Fland","Fdisc"),year_inter+1), paste0("pBlim_",year_inter+2))) {
    stfTab[,v] <- icesRound(stfTab[,v])
  } else if (v %in% c("Fmult")) {
    stfTab[,v] <- round(stfTab[,v],2)
  } else
    stfTab[,v] <- round(stfTab[,v])

write.csv (stfTab, file.path("model", "sum_advice", "catchoptiontable_runs.csv"), row.names = FALSE)



#==============================================================================
# CATCH OPTION TABLE - Plots                                               ----
#==============================================================================

png(height=600, width=600,  file=file.path("model", "sum_report", "ShortTermProj.png")) #file.path(plotdir_stf, "plot_stf.png")

  par(mfrow=c(2,1), mar=c(3,3,2,2), mgp=c(2,0.8,0), oma=c(2,1.5,1.5,1.5))
  
  stTab <- stTab %>% 
    rename(SSB =  matches(paste0("SSB",catYr+1)), F =  matches(paste0("F",catYr)), 
           Catch = matches(paste0("Catches",catYr)), Yield =  matches(paste0("Landings",catYr)))
  
  # stTab <- stTab %>% filter(Fmult < 4)
  
  ### Plot F-Yield ---------------------------------------------------------------
  plot(stTab$Fmult, stTab$Yield, xlab="F mult", ylab=paste0("Yield ", intYr), type="l",lty=2, lwd= 1.5)
  lines (stTab$Fmult, stTab$Catch)
  abline(v=Fmsy/Fint, lty=3, col=2)
  abline(v=Fint/Fint, lty=3, col=4)
  text(x=Fmsy/Fint*1.5, y=150000, labels=paste0("Fmsy=", round(Fmsy,2)), col=2, cex=0.9)
  text(x=0.5, y=150000, labels=paste0("Fsq=", round(Fint, 2)), col=4, cex=0.9)
  legend (5, 100000, c("Catch", "Land"), col=1, cex=1.2, lty=c(1, 2), bty="n", lwd= 2)
  
  ### Plot F-SSB -----------------------------------------------------------------
  plot(stTab$Fmult, stTab$SSB, xlab="F mult", ylab=paste0("SSB ", intYr+1), type="l", ylim=c(0, max(stTab$SSB)))
  abline(v=Fmsy/Fint, lty=3, col=2)
  abline(v=Fint/Fint, lty=3, col=4)
  abline(h=Blim, lty=3, col=2)
  text(x=2.5, y=Blim*1.1, labels=paste0("Blim=", round(Blim, 0)), col=2, cex=0.9)
  abline(h=Bpa, lty=3, col=2)
  text(x=2.5, y=Bpa*1.1, labels=paste0("Bpa=", round(Bpa, 0)), col=2, cex=0.9)
  
  mtext("Short Term Projections",  outer=T, cex=1.3)
dev.off()

