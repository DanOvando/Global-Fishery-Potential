#################################################################
# Script for Obtaining Life History data from Fishbase for species in dataset
# Original code by Jorge Cornejo
# Adapted by Tyler Clavelle
#
# Purpose: Function takes Fishbase codes found using GetFbIds.R function and creates data table of Life History Values for each species.
#          Output data table will be used by FindFishbase function for finding
#
#################################################################

# LhFishbase

## Load data frame with Fishbase codes found using GetFbIds.R

ScNames <- read.csv(paste(ResultFolder,'FB_IDS.csv',sep=''))

### VON BERT and TEMP - Loop to scrape the growth parameters 
pb <- txtProgressBar(min = 0, max = 5, style = 3)
for (i in 1:5 ) #length(spList))
{
  temp <-  fb_growth(idFB=ScNames$idFB[i], Genus= ScNames$Genus[i], Species = ScNames$Species[i])
  if (i == 1) 
  {Growth <- temp}
  else {Growth <- rbind(Growth, temp)}
  
  setTxtProgressBar(pb, i)
}

colnames(Growth)[6]<-"VonBertK" # change colname so R function will recognize variable

Growth$VonBertK<-as.numeric(Growth$VonBertK) # make numeric
Growth$Temp<-as.numeric(Growth$Temp)

VBK<-aggregate(VonBertK~idFB+Genus+Species,data=Growth,mean) # find mean VBK
Temp<-aggregate(Temp~idFB+Genus+Species,data=Growth,mean) # find mean Temp

### MATURITY -  loop to scrape the maturity parameters
pb <- txtProgressBar(min = 0, max = 5, style = 3)
for (i in 1:5) #length(spList))
{
  temp <-  fb_maturity(idFB=ScNames$idFB[i], Genus= ScNames$Genus[i], Species = ScNames$Species[i], server= 'http://www.fishbase.us/')
  if (i == 1) { Maturity <- temp }  else {Maturity <- rbind(Maturity, temp)}
  
  setTxtProgressBar(pb, i)
}

Maturity$tm<-as.numeric(levels(Maturity$tm))[Maturity$tm] # convert from factor to numeric

AgeMat<-aggregate(tm~idFB+Genus+Species,data=Maturity,mean) # take mean of tm variable

### MAX LENGTH
pb <- txtProgressBar(min = 0, max = 5, style = 3)
for (i in 1:5) #length(spList))
{
  temp <-  fb_agesize(idFB=ScNames$idFB[i], Genus= ScNames$Genus[i], Species = ScNames$Species[i], server= 'http://www.fishbase.us/')
  if (i == 1) { Agesize <- temp }  else {Agesize <- rbind(Agesize, temp)}
  
  setTxtProgressBar(pb, i)
}

Agesize$Lmax<-as.numeric(levels(Agesize$Lmax))[Agesize$Lmax]

MaxLength<-aggregate(Lmax~idFB+Genus+Species,data=Agesize,mean)

### Generate Final Life History Data table

FBLifeHistory<-merge(VBK,Temp, by = c("idFB","Genus","Species"), all.x=T)
FBLifeHistory<-merge(FBLifeHistory,AgeMat, by = c("idFB","Genus","Species"), all.x=T)
FBLifeHistory<-merge(FBLifeHistory,MaxLength, by = c("idFB","Genus","Species"), all.x=T)
