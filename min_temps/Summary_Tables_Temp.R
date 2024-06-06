## *************** Summarizing temperature data ********************************
## For specific weather stations (towns) in Montana
## Look for trends in average monthly minimum and maximum teperatures.
## *****************************************************************************

library(readxl)
DD <- read_excel("MTCombinedTempData.xlsm",sheet="Combined_Sites")
head(DD,1)

Townlist <- as.matrix(unique(DD[,"TOWN"]))
TotSites = dim(Townlist)[1]  #total number of sites in the imported data file
cnt = rep(1:(3*TotSites), each=4, times=1)
ct=0

Table1 <- matrix(0,3*TotSites,6)
colnames(Table1) <- c("Town", "Month","Min.Temp.Slope.all","Min.Temp.Slope.1981:2019","Max.Temp.Slope.all",
                      "Max.Temp.Slope.1981:2019")
Table1[,1]<-rep(Townlist,each=3, times=1)
Table1[,2]<-rep(c("June","July","August"))

## Subset the data for each weather station to do trend analysis
## by month (JUN, JUL, AUG)
for(site in 1:TotSites){
  Sta <- subset(DD, TOWN==Townlist[site])
  Sta1 <- data.frame(YEAR=Sta$YEAR, JAN=Sta$JAN.min, FEB=Sta$FEB.min, MAR=Sta$MAR.min, APR=Sta$APR.min, MAY=Sta$MAY.min, 
                     JUN=Sta$JUN.min, JUL=Sta$JUL.min, AUG=Sta$AUG.min, SEP=Sta$SEP.min, OCT=Sta$OCT.min, 
                     NOV=Sta$NOV.min, DEC=Sta$DEC.min)
  Stamin <- na.omit(Sta1)
  Stamin$Sum=rowMeans(cbind(Stamin$JUN,Stamin$JUL,Stamin$AUG))
  Sta2 <- data.frame(YEAR=Sta$YEAR.max, JAN=Sta$JAN.max, FEB=Sta$FEB.max, MAR=Sta$MAR.max, APR=Sta$APR.max, MAY=Sta$MAY.max, 
                     JUN=Sta$JUN.max, JUL=Sta$JUL.max, AUG=Sta$AUG.max, SEP=Sta$SEP.max, OCT=Sta$OCT.max, 
                     NOV=Sta$NOV.max, DEC=Sta$DEC.max)
  Stamax <- na.omit(Sta2)
  Stamax$Sum=rowMeans(cbind(Stamax$JUN,Stamax$JUL,Stamax$AUG))
  lateStamin <- subset(Stamin, YEAR>1980)
  lateStamax <- subset(Stamax, YEAR>1980)
  
  mod1 <- lm(JUN~YEAR, data=Stamin)
  Table1[cnt[ct+1],3] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(JUN~YEAR, data=lateStamin)
  Table1[cnt[ct+2],4] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  mod1 <- lm(JUN~YEAR, data=Stamax)
  Table1[cnt[ct+3],5] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(JUN~YEAR, data=lateStamax)
  Table1[cnt[ct+4],6] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  
  mod1 <- lm(JUL~YEAR,data=Stamin)
  Table1[cnt[ct+5],3] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(JUL~YEAR,data=lateStamin)
  Table1[cnt[ct+6],4] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  mod1 <- lm(JUL~YEAR,data=Stamax)
  Table1[cnt[ct+7],5] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(JUL~YEAR,data=lateStamax)
  Table1[cnt[ct+8],6] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  
  mod1 <- lm(AUG~YEAR,data=Stamin)
  Table1[cnt[ct+9],3] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(AUG~YEAR,data=lateStamin)
  Table1[cnt[ct+10],4] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  mod1 <- lm(AUG~YEAR,data=Stamax)
  Table1[cnt[ct+11],5] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(AUG~YEAR,data=lateStamax)
  Table1[cnt[ct+12],6] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  ct=ct+12
}
Table1

##*****************************************************
## Show a map of the weather station locations
##*****************************************************
library(maps)
library(mapproj)
D <- read_excel("Montana_weather_temps.xlsm",sheet="Station_Location")
head(D,1)
DTown <- data.frame(Town=D$Town,long=D$Long/100*-1, lat=D$Lat/100)

## Maximum Temperatures in summer months
map("county", "Montana", fill=TRUE, col=7)
points(DTown$long, DTown$lat, col = "red", pch=19, cex = .6)
T2 <-as.data.frame(subset(Table1, Table1[,'Max.Temp.Slope.all']>0))
for(nn in 1:length(T2$Town)){
  for(zz in 1:length(DTown$Town)){
    if(T2$Town[nn]==DTown$Town[zz]){
      T2$long[nn] = DTown$long[zz]
      T2$lat[nn] = DTown$lat[zz]
    }   
  }
}
# Proportion of summer months showing positive trend in max temp with all years
dim(T2)[1]/dim(Table1)[1] 
points(T2$long, T2$lat, col=3, pch=1, cex = 1)
T3 <-as.data.frame(subset(Table1, Table1[,'Max.Temp.Slope.1981:2019']>0))
for(nn in 1:length(T3$Town)){
  for(zz in 1:length(DTown$Town)){
    if(T3$Town[nn]==DTown$Town[zz]){
      T3$long[nn] = DTown$long[zz]
      T3$lat[nn] = DTown$lat[zz]
    }   
  }
}
# Proportion of summer months showing positive trend in max temp from 1981-2019
dim(T3)[1]/dim(Table1)[1] 
points(T3$long, T3$lat, col=4, pch=1, cex = 1.4)

## Minimm Temperatures in summer months
map("county", "Montana", fill=TRUE, col=8)
points(DTown$long, DTown$lat, col = "red", pch=19, cex = .6)
T2 <-as.data.frame(subset(Table1, Table1[,'Min.Temp.Slope.all']>0))
for(nn in 1:length(T2$Town)){
  for(zz in 1:length(DTown$Town)){
    if(T2$Town[nn]==DTown$Town[zz]){
      T2$long[nn] = DTown$long[zz]
      T2$lat[nn] = DTown$lat[zz]
    }   
  }
}
# Proportion of summer months showing positive trend in max temp with all years
dim(T2)[1]/dim(Table1)[1] 
points(T2$long, T2$lat, col=3, pch=1, cex = 1)
T3 <-as.data.frame(subset(Table1, Table1[,'Min.Temp.Slope.1981:2019']>0))
for(nn in 1:length(T3$Town)){
  for(zz in 1:length(DTown$Town)){
    if(T3$Town[nn]==DTown$Town[zz]){
      T3$long[nn] = DTown$long[zz]
      T3$lat[nn] = DTown$lat[zz]
    }   
  }
}
# Proportion of summer months showing positive trend in max temp from 1981-2019
dim(T3)[1]/dim(Table1)[1] 
points(T3$long, T3$lat, col=4, pch=1, cex = 1.4)

## Combine the summer months to do trend analysis
## Subset the data for each weather station and use 
## the mean of summer months to detect trend
Table2 <- matrix(0,TotSites,5)
colnames(Table2) <- c("Town", "Min.Temp.Slope.all","Min.Temp.Slope.1981:2019",
                      "Max.Temp.Slope.all","Max.Temp.Slope.1981:2019")
Table2[,1]<-Townlist

for(site in 1:TotSites){
  Sta <- subset(DD, TOWN==Townlist[site])
  Sta1 <- data.frame(YEAR=Sta$YEAR, JAN=Sta$JAN.min, FEB=Sta$FEB.min, MAR=Sta$MAR.min, APR=Sta$APR.min, MAY=Sta$MAY.min, 
                     JUN=Sta$JUN.min, JUL=Sta$JUL.min, AUG=Sta$AUG.min, SEP=Sta$SEP.min, OCT=Sta$OCT.min, 
                     NOV=Sta$NOV.min, DEC=Sta$DEC.min)
  Stamin <- na.omit(Sta1)
  Stamin$Summ=rowMeans(cbind(Stamin$JUN,Stamin$JUL,Stamin$AUG))
  Sta2 <- data.frame(YEAR=Sta$YEAR.max, JAN=Sta$JAN.max, FEB=Sta$FEB.max, MAR=Sta$MAR.max, APR=Sta$APR.max, MAY=Sta$MAY.max, 
                     JUN=Sta$JUN.max, JUL=Sta$JUL.max, AUG=Sta$AUG.max, SEP=Sta$SEP.max, OCT=Sta$OCT.max, 
                     NOV=Sta$NOV.max, DEC=Sta$DEC.max)
  Stamax <- na.omit(Sta2)
  Stamax$Summ=rowMeans(cbind(Stamax$JUN,Stamax$JUL,Stamax$AUG))
  lateStamin <- subset(Stamin, YEAR>1980)
  lateStamax <- subset(Stamax, YEAR>1980)
  
  mod1 <- lm(Summ~YEAR, data=Stamin)
  Table2[site,2] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(JUN~YEAR, data=lateStamin)
  Table2[site,3] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
  mod1 <- lm(JUN~YEAR, data=Stamax)
  Table2[site,4] <- ifelse(summary(mod1)$coefficients[,4][2]<0.1,coef(mod1)[2],NA)
  modlateSta <- lm(JUN~YEAR, data=lateStamax)
  Table2[site,5] <- ifelse(summary(modlateSta)$coefficients[,4][2]<0.1,coef(modlateSta)[2],NA)
}
Table2
## Proportion of weather stations that have summer monthly average maximum teperatures
## increasing using all years of data
dim(subset(Table2,Table2[,'Max.Temp.Slope.all']>0))[1]/TotSites 
## Proportion of weather stations that have summer monthly average maximum teperatures
## increasing using 1981-2019 data
dim(subset(Table2,Table2[,'Max.Temp.Slope.1981:2019']>0))[1]/TotSites

## Proportion of weather stations that have summer monthly average minimum teperatures
## increasing using all years of data
dim(subset(Table2,Table2[,'Min.Temp.Slope.all']>0))[1]/TotSites 
## Proportion of weather stations that have summer monthly average minamum teperatures
## increasing using 1981-2019 data
dim(subset(Table2,Table2[,'Min.Temp.Slope.1981:2019']>0))[1]/TotSites

## Show maps
