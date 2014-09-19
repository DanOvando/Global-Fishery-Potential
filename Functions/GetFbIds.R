#################################################################
# Script for Obtaining Fishbase codes for species in dataset
# Original code by Jorge Cornejo
# Adapted by Tyler Clavelle
#
# Purpose: Function takes finds Fishbase codes for all available species in dataset. Writes .csv for use by FindFishBase function
#
#################################################################

GetFbIds<-function(FullData){

data(fishbase)

spList<-unique(FullData$SciName) # get vector with all scientific names in dataset

# clean species names so that HTML parsing doesn't fail
spList<-gsub(".","",spList,fixed= T) # remove periods
spList<-gsub(",","",spList) # remove commas
spList<-gsub("\\(.*\\)","",spList) # delete anything within parentheses
spList<-gsub("  "," ",spList) # convert any double spaces to single spaces
spList<- gsub("^\\s+|\\s+$","",spList) # trim leading and trailing space

spList<-spList[!grepl("spp",spList)] # drop SciNames that are only Genus specific

# write function to recognize SciNames that are two words long, aka, not simply the genus/family/class/order
IfSpeciesLevel<-function(x)
{l<-length(unlist(str_split(string=x,pattern=" ")))==2 
 return(l)}

Species<-lapply(spList,IfSpeciesLevel) # find species level entries in spList

spList<-spList[!Species==FALSE] # drop non species level SciNames from spList

temp <- str_split(string=spList, pattern=" ")
n <- length(spList)
ScNames <- data.frame(Genus = rep(NA, n), Species= rep(NA, n))
ScNames$idFB <- NA
ScNames$StockCode <- NA 

## This loop is to obtain the number used by the FB to identify the fish. This is the number
## that is used to call all the links to get the data!
pb <- txtProgressBar(min = 0, max = 1356, style = 3)
 
for (i in 1:length(spList))
{
  ScNames$Genus[i] <- Genus <-  temp[[i]][1]
  ScNames$Species[i] <- Species <- temp[[i]][2]
  ScNames$Species[i]<-tolower(Species) # make sure all species names are lower case
  
#   if (is.na(temp[[i]][2])){
#     ScNames$Species[i] <-"spp"
#     temp[[i]][2]<-"spp"}
#   
#   if (temp[[i]][1]==""){
#     ScNames$Genus[i] <-"Blank"
#     temp[[i]][1]<-"Blank"}
  
  temp2 <- fb_ids(ScNames$Genus[i], ScNames$Species[i])
  ScNames$idFB[i] <- temp2$idFB
  ScNames$StockCode[i] <- temp2$StockCode
  setTxtProgressBar(pb, i)
}

write.csv(file=paste(ResultFolder,"FB_IDS.csv",sep=""),ScNames) # write csv with FB codes so this script only needs to be run once

return (ScNames)
}
