# Adjusting Jorge Conejo's Fishbase code

rm(list=ls())
install <- 1 ## Change this to 0 after the first intalation!!

if (install == 0)
{
  install.packages("rfishbase")
  install.packages("rfishbase")
  install.packages("XML")
  install.packages("stringr")
  install.packages('RCurl')
}

setwd("~/Desktop/scratchWorkspace") ## Chage this....
require(rfishbase)
require(stringr)

## This source command load the files with the Fish Base functions!

source(fb.ids.R)
source(fb_growth.R)
source(fb_maturity.R)
source(fb_agesize.R)

## Now, using the 'rfishbase' package, get the list of species to obtain the data from..

data(fishbase)
## All the species listed in FB that are part of Engraulidae family
# sp <- which_fish("Engraulidae", using="Family", fish.data) 
# spList <- fish_names(fish.data[sp]) ## This is a vector with all the scientific names!

spList<-unique(FullData$SciName) # get vector with all scientific names in dataset
  # clean species names so that HTML parsing doesn't fail
  spList<-gsub(".","",spList,fixed= T) # remove periods
  spList<-gsub(",","",spList) # remove commas
  spList<-gsub("\\(.*\\)","",spList) # delete anything within parentheses
  spList<-gsub("  "," ",spList) # convert any double spaces to single spaces
  spList<- gsub("^\\s+|\\s+$","",spList) # trim leading and trailing space

temp <- str_split(string=spList, pattern=" ")
n <- length(spList)
ScNames <- data.frame(Genus = rep(NA, n), Species= rep(NA, n))
ScNames$idFB <- NA
ScNames$StockCode <- NA 

## This loop is to obtain the number used by the FB to identify the fish. This is the number
## that is used to call all the links to get the data!
pb <- txtProgressBar(min = 0, max = 1356, style = 3)

## I'm just doing this example for the first 5 species! 
for (i in 1:length(spList))
{
  ScNames$Genus[i] <- Genus <-  temp[[i]][1]
  ScNames$Species[i] <- Species <- temp[[i]][2]
  ScNames$Species[i]<-tolower(Species) # make sure all species names are lower case
  
  if (is.na(temp[[i]][2])){
     ScNames$Species[i] <-"spp"
    temp[[i]][2]<-"spp"}
  
  if (temp[[i]][1]==""){
    ScNames$Genus[i] <-"Blank"
    temp[[i]][1]<-"Blank"}
  
  temp2 <- fb_ids(ScNames$Genus[i], ScNames$Species[i])
  ScNames$idFB[i] <- temp2$idFB
  ScNames$StockCode[i] <- temp2$StockCode
  setTxtProgressBar(pb, i)
}

# write.csv(ScNames, file='ScNames_fbIDS.csv')

## Now we use the data from the previous loop to ge the data of interest!
ScNames <- read.csv('ScNames_fbIDS.csv')

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
