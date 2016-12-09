#################################################################
# Script for Obtaining Life History data from Fishbase for species in dataset
# Original code by Jorge Cornejo
# Adapted by Tyler Clavelle
#
# Purpose: Function takes Fishbase codes found using GetFbIds.R function and creates data table of Life History Values for each species.
#          Output data table will be used by FindFishbase function for inserting lh values
#
#################################################################

# Currently getting errors if running on UCSB wireless. Runs error-free otherwise

## Load data frame with Fishbase codes found using GetFbIds.R

# For testing:
# TestData<-as.character(sample(unique(RawData$IdOrig),500,replace=FALSE))
# TestData<-RawData[(RawData$IdOrig %in% TestData),]

ScrapeFishbase<-function(Data,FbIds)
{

if(exists("ScNames")==F){ScNames <- read.csv(paste(ResultFolder,'FB_IDS.csv',sep=''),stringsAsFactors=F)}

ScNames<-subset(ScNames,is.na(idFB)==F)

### VON BERT and TEMP - Loop to scrape the growth parameters 
pb <- txtProgressBar(min = 0, max = nrow(ScNames), style = 3)
for (i in 1:nrow(ScNames))
{
  temp <-  fb_growth(idFB=ScNames$idFB[i], Genus= ScNames$Genus[i], Species = ScNames$Species[i])
  if (i == 1) 
  {Growth <- temp}
  else {Growth <- rbind(Growth, temp)}
  
  setTxtProgressBar(pb, i)
  print(i)
}

colnames(Growth)[6]<-"VonBertK" # change colname so R function will recognize variable

Growth$VonBertK<-as.numeric(Growth$VonBertK) # make numeric
Growth$Temp<-as.numeric(Growth$Temp)

VBK<-aggregate(VonBertK~idFB+Genus+Species,data=Growth,mean) # find mean VBK
Temp<-aggregate(Temp~idFB+Genus+Species,data=Growth,mean) # find mean Temp

### MATURITY -  loop to scrape the maturity parameters
pb <- txtProgressBar(min = 0, max = nrow(ScNames), style = 3)
for (i in 1:nrow(ScNames))
{
  temp <-  fb_maturity(idFB=ScNames$idFB[i], Genus= ScNames$Genus[i], Species = ScNames$Species[i], server= 'http://www.fishbase.us/')
  if (i == 1) { Maturity <- temp }  else {Maturity <- rbind(Maturity, temp)}
  
  setTxtProgressBar(pb, i)
  print(i)
}

Maturity$tm<-as.numeric(levels(Maturity$tm))[Maturity$tm] # convert from factor to numeric

AgeMat<-aggregate(tm~idFB+Genus+Species,data=Maturity,mean) # take mean of tm variable

### MAX LENGTH
pb <- txtProgressBar(min = 0, max = nrow(ScNames), style = 3)
for (i in 1:nrow(ScNames))
{
  temp <-  fb_agesize(idFB=ScNames$idFB[i], Genus= ScNames$Genus[i], Species = ScNames$Species[i], server= 'http://www.fishbase.us/')
  if (i == 1) { Age_Size <- temp }  else {Age_Size <- rbind(Age_Size, temp)}
  
  setTxtProgressBar(pb, i)
}

Age_Size$Lmax<-as.numeric(levels(Age_Size$Lmax))[Age_Size$Lmax]

MaxLength<-aggregate(Lmax~idFB+Genus+Species,data=Age_Size,mean)

### Generate Final Life History Data table

FBLifeHistory<-merge(VBK,Temp, by = c("idFB","Genus","Species"), all.x=T)
FBLifeHistory<-merge(FBLifeHistory,AgeMat, by = c("idFB","Genus","Species"), all.x=T)
FBLifeHistory<-merge(FBLifeHistory,MaxLength, by = c("idFB","Genus","Species"), all.x=T)

FBLifeHistory$SciName<-paste(FBLifeHistory$Genus,FBLifeHistory$Species,sep=" ") # Recreate SciName

write.csv(file=paste(ResultFolder,"FBLifeHistory.csv",sep=""),FBLifeHistory)

return(FBLifeHistory)

} # close function