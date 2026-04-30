################################################################################
#  WGBIE : plot survey indices info from ICES DATRAS                           # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI)                                                       #
#   created:  24/05/2022                                                       #
#   modified: 2023-04-24 12:10:46 (ssanchez@azti.es) - adapt for wgbie2023     #
################################################################################

# data_06_surveys_plot.R - plotting hake survey information from Datras
# ~/data_06_surveys_plot.R

# Copyright: AZTI, 2022
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#         Based on code by Hans Gerritsen
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================


require(dplyr)
require(ggplot2)
require(maps)
# library(mapdata)
require(gridExtra)
require(survey)


#==============================================================================
# INPUT FILES                                                              ----
#==============================================================================

datras_wd          <- file.path("data","datras")

hkeSurv_file       <- file.path(datras_wd, "surv_data.RData")
hkeDATRAS_len_file <- file.path(datras_wd, "DatrasHkeLen.csv")
# hkeDATRAS_cat_file <- file.path(datras_wd, "DatrasHkeCatch.csv")


#==============================================================================
# OUTPUT FILES                                                             ----
#==============================================================================

dplots_wd <- file.path("data", "plots")
drep_wd   <- file.path("data", "report")

if(!dir.exists(dplots_wd)) dir.create(dplots_wd, recursive = TRUE)
if(!dir.exists(drep_wd)) dir.create(drep_wd, recursive = TRUE)

idx_catchwt_file     <- file.path(dplots_wd, "DATRAS_CatchWt.png")
idx_catchnum_file    <- file.path(dplots_wd, "DATRAS_CatchNum.png")
idx_catchwt_repfile  <- file.path(drep_wd, "surveys_distr.png")
idx_LFD_file         <- file.path(dplots_wd, "DATRAS_lfd.png")


#==============================================================================
# DATA                                                                     ----
#==============================================================================

load(hkeSurv_file)
len <- read.csv(hkeDATRAS_len_file)


#==============================================================================
# CATCH                                                                    ----
#==============================================================================

Catch <- Catch %>% mutate(CatchWgtKg=ifelse(is.na(CatchWgtKg),0,CatchWgtKg)
                          ,CatchNos=ifelse(is.na(CatchNos),0,CatchNos)
                          ,CatchNoS=ifelse(is.na(CatchNoS),0,CatchNoS)
#                          ,CatchNoM=ifelse(is.na(CatchNoM),0,CatchNoM)
#                          ,CatchNoL=ifelse(is.na(CatchNoL),0,CatchNoL)
                          ,CatchWgtKg1=ifelse(is.na(CatchWgtKg1),0,CatchWgtKg1)
                          )

Surveys  <- c("EVHOE","SP-PORC","IE-IGFS","IE-IAMS")
Quarters <- c(3,4,1)

ylim <- c(42.5,56)

Catch1 <- subset(Catch, Survey %in% Surveys & Quarter %in% Quarters & MidLat>ylim[1] & MidLat<ylim[2])
Catch1 <- subset(Catch1, MidLat<52 | MidLong< -8) # remove Irish sea igfs

with(droplevels(Catch1),table(Survey,Gear))

Survey <- droplevels(Catch1) %>% ungroup() %>% 
  group_by(Year, Survey) %>% 
  summarise(Size = length(unique(paste(round(MidLong/.5)*.5, round(MidLat/0.25)*0.25))), 
            NumHauls = length(MidLong), 
            SizeInterCal = ifelse(Survey=='SP-PORC', 1/1, 1) * Size)

fun <- function(x) x/sum(x)

# Survey <- Survey %>% mutate_each(list(~ fun(.)), SizeProp=SizeInterCal) # mutate_each depreciated
Survey <- Survey %>% group_by(Year) %>% mutate(SizeProp = fun(SizeInterCal)) # Grouped by Year

png(file.path(datras_wd, "surv_areaCovered.png"), 6, 6, 'in', res = 600)
  boxplot(SizeProp ~ Survey, data = Survey, las=3, ylab='Area covered (proportion of total)')
dev.off()

png(file.path(datras_wd, "surv_stationsNum.png"), 6, 6, 'in', res = 600)
  boxplot(NumHauls~Survey, data = Survey, las=3, ylab='Num stations')  
dev.off()

Catch1 <- merge(Catch1, Survey) %>% mutate(SampleWeight = SizeProp*60/HaulDur/NumHauls)

png(file.path(datras_wd, "surv_sample_weight.png"))
  with(droplevels(Catch1), boxplot(SampleWeight~Survey,ylab='Sample weights'), 10, 4, 'in', res = 600)
dev.off()

index <- Catch1 %>%
  group_by(Year, Survey) %>% 
  summarise(Biomass = sum(CatchWgtKg*SampleWeight), 
            Abundance = sum(CatchNos*SampleWeight))

p1 <- ggplot(index,aes(Year,Abundance,colour=Survey)) + 
  geom_line() + geom_point()
p2 <- ggplot(index,aes(Year,Biomass,colour=Survey)) + 
  geom_line() + geom_point()
grid.arrange(p1, p2)

index <- Catch1 %>%
  group_by(Year) %>% 
  summarise(Biomass = sum(CatchWgtKg*SampleWeight), 
            Abundance = sum(CatchNos*SampleWeight))

p1 <- ggplot(index,aes(Year,Abundance)) + geom_line() + geom_point() + ylim(0,max(index$Abundance))
p2 <- ggplot(index,aes(Year,Biomass)) + geom_line() + geom_point() + ylim(0,max(index$Biomass))
grid.arrange(p1, p2)

#### same but using survey package

design <- svydesign(id=~1, strata=~Survey, weights=~SampleWeight, data=Catch1)
index  <- svyby(~CatchWgtKg+CatchNos, by=~Year, design=design, vartype="ci", svytotal)

p1 <- ggplot(index,aes(Year,CatchNos,ymin=ci_l.CatchNos,ymax=ci_u.CatchNos)) + 
  geom_ribbon(fill='lightgrey') +
  geom_line() +
  geom_point() +
  ylab('Abundance') + 
  ylim(0,max(index$ci_u.CatchNos))

p2 <- ggplot(index,aes(Year,CatchWgtKg,ymin=ci_l.CatchWgtKg,ymax=ci_u.CatchWgtKg)) + 
  geom_ribbon(fill='lightgrey') +
  geom_line() +
  geom_point() +
  ylab('Biomass') + 
  ylim(0,max(index$ci_u.CatchWgtKg))

png(file.path(datras_wd, "surv_globalIndex_biom&num.png"), 6, 6, 'in', res = 600)
  grid.arrange(p1, p2)
dev.off()

index  <- svyby(~CatchWgtKg+CatchNos,by=~Year+Survey,design=design,vartype="ci",svytotal)

q1 <- ggplot(index,aes(Year,CatchNos,colour=Survey)) + 
  geom_ribbon(aes(ymin=ci_l.CatchNos,ymax=ci_u.CatchNos,fill=Survey,colour=NULL),alpha=0.25) +
  geom_line() +
  geom_point() +
  ylab('Abundance') + 
  ylim(min(c(0,index$ci_l.CatchNos)),max(index$ci_u.CatchNos))
q2 <- ggplot(index,aes(Year,CatchWgtKg,colour=Survey)) + 
  geom_ribbon(aes(ymin=ci_l.CatchWgtKg,ymax=ci_u.CatchWgtKg,fill=Survey,colour=NULL),alpha=0.25) +
  geom_line() +
  geom_point() +
  ylab('Biomass') + 
  ylim(min(c(0,index$ci_l.CatchWgtKg)),max(index$ci_u.CatchWgtKg))

png(file.path(datras_wd, "surv_indices_biom&num.png"), 6, 6, 'in', res = 600)
  grid.arrange(q1, q2)
dev.off()

mapfun <- function(lon,lat,z,year,survey,col,title,size){
  z <- ifelse(z<0,0,z)
  maxz <- max(z,na.rm=T)
  cex <- size*sqrt(z/maxz)*5
  cex <- ifelse(cex<0.1*size,0.1*size,cex)
  par(mfrow=c(4,4))
  surveys <- c("EVHOE", "SP-PORC", "IE-IGFS", "IE-IAMS") #levels(factor(as.character(survey))) #!! setting manually the order
  for(y in (dtyr-15):dtyr){
    cat(y)
    cat(" ")
    map('world', xlim=c(-16,1),ylim=c(43,56),fill=2,col='grey',
        border=NA,mar=c(0,0,2,0),resolution=0)
    for(s in 1:length(survey)){
      i <- which(year==y & z>0 & survey==surveys[s])        
      points(lon[i],lat[i],cex=cex[i],pch=16,col=col[1+s])
      j <- which(year==y & z==0 & survey==surveys[s])
      points(lon[j],lat[j],cex=0.1*size,pch=4,col=col[1],lwd=0.5)
      title(y,line=0.2)
    }
    if(y==dtyr-3) legend('bottomleft',legend=c(0,signif(c(0.5,1)*maxz,3))
                         ,pt.cex=c(0.1*size,sqrt(c(0.25,1))*size),pch=c(4,16,16)
                         ,col=col[c(1,2,2)],title=title)
    if(y==dtyr-2) legend('bottomleft',legend=surveys,pt.cex=2,pch=16,col=col[-1],title='Survey')
  }
  # plot.new()
  # legend('top',legend=c(0,signif(c(0.5,1)*maxz,3))
  #    ,pt.cex=c(0.1*size,sqrt(c(0.25,1))*size),pch=c(4,16,16)
  #    ,col=col[c(1,2,2)],title=title)
  # legend('center',legend=surveys,pt.cex=2,pch=16,col=col[-1],title='Survey')
}

Catch1$InterCal <- ifelse(Catch1$Survey=='SP-PORC',1/1,1)
Catch2 <- Catch1 %>% 
  group_by(Year, Survey, MidLong=round(MidLong/0.05)*0.05, MidLat=round(MidLat/0.025)*0.025) %>% 
  summarise(CpueWgtKg=mean(InterCal*60*ifelse(is.na(CatchWgtKg),0,CatchWgtKg)/HaulDur)
            ,CpueNos=mean(InterCal*60*ifelse(is.na(CatchNos),0,CatchNos)/HaulDur)
            ,CpueNoS=mean(InterCal*60*ifelse(is.na(CatchNoS),0,CatchNoS)/HaulDur)
            )

pcnum <- with(Catch2,mapfun(MidLong,MidLat,CpueWgtKg,Year,Survey,
                            c("#e41a1c","#377eb890","#4daf4a90","#984ea390", "#FED9A690"),'Biomass (kg/hr)',5))

png(idx_catchwt_file,8,10,'in',10,res=600)
  with(Catch2,mapfun(MidLong,MidLat,CpueWgtKg,Year,Survey,
                     c("#e41a1c","#377eb890","#4daf4a90","#984ea390", "#FED9A690"),'Biomass (kg/hr)',5))
dev.off()

png(idx_catchwt_repfile,8,10,'in',10,res=600)
  with(Catch2,mapfun(MidLong,MidLat,CpueWgtKg,Year,Survey,
                     c("#e41a1c","#377eb890","#4daf4a90","#984ea390", "#FED9A690"),'Biomass (kg/hr)',5))
dev.off()

png(idx_catchnum_file,8,10,'in',10,res=600)
  with(Catch2,mapfun(MidLong,MidLat,CpueNoS,Year,Survey,
                     c("#e41a1c","#377eb890","#4daf4a90","#984ea390", "#FED9A690"),'Recruits <20cm (n/hr)',5))
dev.off()


rm(list = ls()[!ls() %in% c("len","Catch1","idx_LFD_file")])


#==============================================================================
# LENGTH                                                                   ----
#==============================================================================

len1 <- merge(len,Catch1[,c('Survey','Year','Quarter','Country','Ship','Gear','HaulNo','SampleWeight')])

rm(len,Catch1) # to free memory
gc()

minLen <- 10; maxLen <- 100; int  <- 2
len1$LenFac <- factor(round(0.001+len1$LngtClassCm/int)*int,levels=seq(minLen,maxLen,int))

len1$Frequency <- len1$HLNoAtLngt * len1$SubFactorReal

design <- svydesign(id=~1, strata=~Survey, weights=~SampleWeight, data=len1)
index  <- svyby(~Frequency, by=~Year+LenFac, design=design, vartype="ci", svytotal)
#write.csv(index,'HkeCombinedLf.csv',row.names = F)


p1 <- 
  ggplot(index,aes(LenFac,log(Frequency))) + geom_bar(stat = "identity") + facet_grid(Year~.) + xlab('Length class (cm)')

png(idx_LFD_file,4,8,'in',10,res=600)
  p1
dev.off()

# Linf=130 cm, K = 0.177319 and mean length-at-age 0.75 = 15.8392
linf <- 130
k    <- 0.177319
t0   <- 0.0175
linf * (1-exp(-k*(0.75-t0)))

a <- 0:10 + 0.75
la <- linf * (1-exp(-k*(a-t0)))

png(idx_LFD_file,4,8,'in',10,res=600)
  t1 <- with(index,tapply(Frequency,list(Year,LenFac),sum))
  t1 <- ifelse(is.na(t1),0,t1)
  t1 <- t1[,1:45]
  t2 <- (t(t1)-apply(t1,2,mean))/apply(t1,2,mean)
  l <- as.numeric(rownames(t2))
  par(mfrow=c(ncol(t2),1),mar=c(0,0,0,0),oma=c(2,0,1,0))
  for(i in 1:ncol(t2)){  b<-barplot(t2[,i],col=as.numeric(t2[,i]<0)+1,names=NA,axes=F)
    b <- as.numeric(b)
    abline(v=predict(lm(b~l),newdata=data.frame(l=la)),col=4)
    }
  axis(1,b,as.numeric(rownames(t2)))
dev.off()

b <- barplot(log(colSums(t1)))
b <- as.numeric(b)
abline(v=predict(lm(b~l),newdata=data.frame(l=la)),col=4)

b <- barplot(sqrt(apply(t1,2,sd)))
b <- as.numeric(b)
abline(v=predict(lm(b~l),newdata=data.frame(l=la)),col=4)

