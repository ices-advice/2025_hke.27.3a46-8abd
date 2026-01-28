################################################################################
#  WGBIE : get survey indces info from ICES DATRAS                             # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTIa)                                                      #
#   created:  24/05/2022                                                       #
#   modified: 2023-04-24 12:10:46 (ssanchez@azti.es) - adapt for wgbie2023     #
################################################################################

# data_05_datSurveys.R - getting hake survey information from Datras
# ~/data_05_datSurveys.R

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
# devtools::install_github("ices-tools-prod/icesDatras")
require(icesDatras)


#==============================================================================
# OUTPUT FILES                                                             ----
#==============================================================================

datras_wd          <- file.path("data","datras")
if (!dir.exists(datras_wd)) dir.create(datras_wd)

hkeSurv_file       <- file.path(datras_wd, "surv_data.RData")
hkeDATRAS_len_file <- file.path(datras_wd, "DatrasHkeLen.csv")
hkeDATRAS_cat_file <- file.path(datras_wd, "DatrasHkeCatch.csv")


#==============================================================================
# DATA                                                                     ----
#==============================================================================

# DATRAS: Database of Trawl Surveys

# - HH record type
# Represent the haul characteristics (e,g, date, time, coordinates, gear specifications, 
# haul duration and distance, and environmental conditions). 

hhevhoe <- getDATRAS(record = "HH", 'EVHOE', 1997:dtyr, 4)
hhporc  <- getDATRAS(record = "HH", 'SP-PORC', 2001:dtyr, 3)
hhigfs  <- getDATRAS(record = "HH", "IE-IGFS", 2003:dtyr, 4)
hhiams  <- getDATRAS(record = "HH", "IE-IAMS", 2016:dtyr, 1)

# - HL record type
# Recording subsampling and categorisation of the catch, length measurements and 
# the measurement units, weight recording, numbers caught.

hlevhoe <- getDATRAS(record = "HL", 'EVHOE', 1997:dtyr, 4)
hlporc  <- getDATRAS(record = "HL", 'SP-PORC', 2001:dtyr, 3)
hligfs  <- getDATRAS(record = "HL", "IE-IGFS", 2003:dtyr, 4)
hliams  <- getDATRAS(record = "HL", "IE-IAMS", 2016:dtyr, 1)


HH <- rbind(hhevhoe, hhporc, hhigfs, hhiams)
HL <- rbind(hlevhoe, hlporc, hligfs, hliams)


save(HH, HL, file = file.path(datras_wd, "surv_data_imported.RData"))


#==============================================================================
# DATA EXPLORATION                                                         ----
#==============================================================================

### some of the fields are padded with spaces, fix this

HH$Survey   <- factor(trimws(HH$Survey,'right'))
HH$Ship     <- factor(trimws(HH$Ship,'right'))
HH$Gear     <- factor(trimws(HH$Gear,'right'))
HH$HaulVal  <- factor(trimws(HH$HaulVal,'right'))
HH$DataType <- factor(trimws(HH$DataType,'right'))
HH$MidLong  <- (HH$ShootLong+HH$HaulLong)/2
HH$MidLat   <- (HH$ShootLat+HH$HaulLat)/2
Haul        <- select(HH,Survey, Year, Quarter, Country, Ship, Gear, HaulNo, HaulDur, HaulVal, GroundSpeed, MidLong, MidLat,DataType)

HL$Survey      <- factor(trimws(HL$Survey,'right'))
HL$Ship        <- factor(trimws(HL$Ship,'right'))
HL$Gear        <- factor(trimws(HL$Gear,'right'))
HL$LngtCode    <- factor(trimws(HL$LngtCode,'right'))
HL$CatCatchWgt <- ifelse(HL$CatCatchWgt<0,NA,HL$CatCatchWgt)
HL$HLNoAtLngt  <- ifelse(HL$HLNoAtLngt<0,NA,HL$HLNoAtLngt)
HL$SubFactor   <- ifelse(HL$SubFactor<0,NA,HL$SubFactor)
HL$LngtClass   <- ifelse(HL$LngtClass<0,NA,HL$LngtClass)
HL$NoMeas      <- ifelse(HL$NoMeas<0,NA,HL$NoMeas)

# only HAKE
HL <- HL %>% filter(SpecCode == 126484)

### what have we got?
with(HH,table(Survey,Gear))

### sort out length classes
hl1 <- HL %>% group_by(LngtClass,LngtCode) %>% summarise(Freq=sum(HLNoAtLngt*SubFactor,na.rm=T))
ggplot(hl1,aes(x=LngtClass,y=Freq,fill=LngtCode)) + geom_bar(stat = "identity")

HL$LngtClassCm <- ifelse(HL$LngtCode%in%c('.','0'),HL$LngtClass/10,HL$LngtClass)
hl1 <- HL %>% group_by(LngtClassCm,LngtCode) %>% summarise(Freq=sum(HLNoAtLngt*SubFactor,na.rm=T))
ggplot(HL,aes(x=LngtClassCm,y=HLNoAtLngt*SubFactor,fill=LngtCode)) + geom_bar(stat = "identity")

table(subset(HL,HLNoAtLngt<=0)$HLNoAtLngt)
HL <- subset(HL,HLNoAtLngt>0)
hl1 <- HL %>% group_by(LngtClassCm,LngtCode) %>% summarise(Freq=sum(HLNoAtLngt*SubFactor))
ggplot(HL,aes(x=LngtClassCm,y=HLNoAtLngt*SubFactor,fill=LngtCode)) + geom_bar(stat = "identity")

table(HL$LngtClassCm)


# and for the CA (not needed for hake)
#hl1 <- CA %>% group_by(LngtClass,LngtCode) %>% summarise(Freq=length(LngtClass))
#ggplot(hl1,aes(x=LngtClass,y=Freq,fill=LngtCode)) + geom_bar(stat = "identity")

#CA$LngtClassCm <- ifelse(CA$LngtCode%in%c('.','0'),CA$LngtClass/10,CA$LngtClass)
#hl1 <- CA %>% group_by(LngtClassCm,LngtCode) %>% summarise(Freq=length(LngtClass))
#ggplot(hl1,aes(x=LngtClassCm,y=Freq,fill=LngtCode)) + geom_bar(stat = "identity")

#table(CA$LngtClassCm)
#CA <- subset(CA,LngtClassCm>0)


# check raising factors for extremely high values
table(HL$SubFactor)
plot(as.numeric(table(HL$SubFactor)),as.numeric(names(table(HL$SubFactor))),log='xy')

subset(HL,SubFactor>10)
#HH <- subset(HH,!(Survey=="EVHOE" & Year==2008 & HaulNo==75))
#HL <- subset(HL,!(Survey=="EVHOE" & Year==2008 & HaulNo==75))


# Code that specifies the data type in HL-record. 
# C - category catch weight is adjusted per hour;
# R - weight of the category catch in the haul;
# S - weight of the category catch in the subsample of the total catch.
# Note: if subsampling was performed per species, but the whole catch was not sub-sampled, R should be reported.


with(HH,table(Survey,DataType))
# evhoe should not be 'C' this was a mistake in 2018
HH$DataType[HH$Survey=='EVHOE'] <- 'R'
Haul$DataType[Haul$Survey=='EVHOE'] <- 'R'

# so where datatype = 'C' the catch weights (and numbers) are already adjusted to 1 hour tows
nrow(HL); nrow(merge(HL,Haul))
HL <- merge(HL,Haul)
HL <- mutate(HL,CatCatchWgtReal=ifelse(DataType=='C',CatCatchWgt*HaulDur/60,CatCatchWgt))
HL <- mutate(HL,SubFactorReal=ifelse(DataType=='C',SubFactor*HaulDur/60,SubFactor))

# check catch weights
CatCatch <- HL %>% 
  group_by(Survey, Year, Quarter, Country, Ship, Gear, HaulNo, CatIdentifier) %>% 
  summarise(CatCatchWgtKg=mean(CatCatchWgtReal/1000)
            ,NumCatCatch=length(unique(CatCatchWgtReal))
            ,CatCatchWgtKg1=sum(HLNoAtLngt*SubFactorReal*0.000022 *LngtClassCm^2.89)
            )

with(CatCatch,plot(CatCatchWgtKg,CatCatchWgtKg1,pch=16,cex=0.5,col='#00000050',log='xy'))
curve(1*x,col=2,add=T)
curve(2*x,col=2,add=T)
curve(10*x,col=2,add=T)
curve(100*x,col=2,add=T)

# our EVHOE friends provide sample weights by sex
# other countries also have sex in HH but they provide a single sample weight
with(CatCatch,table(Survey,NumCatCatch))
with(subset(CatCatch,NumCatCatch==2),points(CatCatchWgtKg,CatCatchWgtKg1,pch=16,cex=0.5,col='#FF000050'))
with(subset(CatCatch,NumCatCatch==3),points(CatCatchWgtKg,CatCatchWgtKg1,pch=16,cex=0.5,col='#00FF0050'))
HL$Sex1 <- factor(ifelse(HL$Survey=="EVHOE",as.character(HL$Sex),'-9'))

CatCatch <- HL %>% 
  group_by(Survey, Year, Quarter, Country, Ship, Gear, HaulNo, CatIdentifier,Sex1) %>% 
  summarise(CatCatchWgtKg=mean(CatCatchWgtReal/1000)
            ,NumCatCatch=length(unique(CatCatchWgtReal))
            ,CatCatchWgtKg1=sum(HLNoAtLngt*SubFactorReal*0.000022 *LngtClassCm^2.89)
            )
table(CatCatch$NumCatCatch) # that's sorted

# now we still have catch weights that are out by a factor of 10 and 100 (not many though)
with(CatCatch,plot(CatCatchWgtKg,CatCatchWgtKg1,pch=16,cex=0.5,col='#00000050',log='xy'))
curve(1*x,col=2,add=T)
curve(2*x,col=3,add=T)
curve(10*x,col=2,add=T)
curve(100*x,col=2,add=T)
curve(25*x,col=3,add=T)


a <- subset(CatCatch,CatCatchWgtKg1>50*CatCatchWgtKg)
table(a$Survey,a$Year)
table(a$Survey,a$Country)
# only north sea

a <- subset(CatCatch,CatCatchWgtKg1<=50*CatCatchWgtKg & CatCatchWgtKg1>5*CatCatchWgtKg)
table(a$Survey,a$Year)
table(a$Survey,a$Country)
# mostly north sea, bit of evhoe

# just fix it
CatCatch$CatCatchWgtKg <- with(CatCatch,ifelse(CatCatchWgtKg1>50*CatCatchWgtKg,CatCatchWgtKg*100,CatCatchWgtKg))
CatCatch$CatCatchWgtKg <- with(CatCatch,ifelse(CatCatchWgtKg1>5*CatCatchWgtKg,CatCatchWgtKg*10,CatCatchWgtKg))

with(CatCatch,plot(CatCatchWgtKg,CatCatchWgtKg1,pch=16,cex=0.5,col='#00000050',log='xy'))
with(CatCatch,plot(CatCatchWgtKg,CatCatchWgtKg1,pch=16,cex=0.5,col='#00000050'))

# still quite a few missing and one zero catch weight
sum(is.na(CatCatch$CatCatchWgtKg))
sum(CatCatch$CatCatchWgtKg==0,na.rm=T)
sum(CatCatch$CatCatchWgtKg<0,na.rm=T)
with(CatCatch,table(is.na(CatCatch$CatCatchWgtKg),Survey))

# Create catch table
Catch <- CatCatch %>% 
  group_by(Survey, Year, Quarter, Country, Ship, Gear,  HaulNo) %>% 
  summarise(CatchWgtKg=sum(ifelse(is.na(CatCatchWgtKg)|CatCatchWgtKg==0,CatCatchWgtKg1,CatCatchWgtKg))
            )

# add in catch numbers
CatchLen <- HL %>% 
  group_by(Survey, Year, Quarter, Country, Ship, Gear, HaulNo, SpecVal) %>% 
  summarise(CatchNos=sum(HLNoAtLngt*SubFactorReal)
            ,CatchNoS=sum(HLNoAtLngt*SubFactorReal*(LngtClassCm<20))
#            ,CatchNoM=sum(HLNoAtLngt*SubFactorReal*(LngtClassCm>=20 & LngtClassCm<=41))
#            ,CatchNoL=sum(HLNoAtLngt*SubFactorReal*(LngtClassCm>41))
            ,CatchWgtKg1=sum(HLNoAtLngt*SubFactorReal*0.000006812474 *LngtClassCm^2.97)
            )

nrow(Catch); nrow(merge(Catch,CatchLen,all.x=T))
Catch <- merge(Catch,CatchLen,all.x=T)
with(Catch,plot(CatchWgtKg,CatchWgtKg1,log='xy'))
curve(1*x,col=2,add=T)
curve(0.5*x,col=2,add=T)

#not right!
with(subset(Catch,CatchWgtKg1<CatchWgtKg*0.5),table(Survey,Year))
# but only NS and SWC IBTS     
     
# and add in haul data
nrow(Catch); nrow(merge(Catch,Haul)); nrow(merge(Catch,Haul,all.y=T))
Catch <- merge(Catch,Haul,all.y=T)
Catch <- subset(Catch,HaulDur>=15);nrow(Catch)
Catch <- subset(Catch,SpecVal==1 | is.na(SpecVal));nrow(Catch)
Catch <- subset(Catch,HaulVal%in%c('A','C','V','F'));nrow(Catch)

Catch$SpecVal <- NULL
Catch$DataType <- NULL


with(Catch,table(is.na(CatchWgtKg),is.na(CatchWgtKg1)))
abline(0,1,col=2)


#==============================================================================
# SAVE                                                                     ----
#==============================================================================

save(Catch,file=hkeSurv_file)

Len <- select(HL,Survey, Year, Quarter, Country, Ship, Gear, HaulNo, LngtClassCm, HLNoAtLngt, SubFactorReal)

#write.csv(Bio,'DatrasWafBio.csv',row.names=F)
write.csv(Len,hkeDATRAS_len_file,row.names=F)
write.csv(Catch,hkeDATRAS_cat_file,row.names=F)

