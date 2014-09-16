######################################
# Global Fishery Recovery Datbase Filtering --------------------------------------------------
# This code identifys and removes overlap between RAM, SOFIA, and FAO stocks in the Global Fisheries Recovery Database
######################################

RemoveOverlap<- function(Data,OverlapMode,stringsAsFactors=F)
{

#   Data<- RawData
# FilterOverlap<-function(Data, stringsAsFactors=F){

# read in compiled dataset
# Data<-read.csv(paste(ResultFolder,'fulldata.csv',sep=''),stringsAsFactors=F)

#     Data<- FullData
# Data$Country<- as.character(levels(Data$Country))[Data$Country]
  yrs<-c(1950:2013) # filter only years from 1950-2013
  
  for (r in 1:length(yrs))
  {
    
    FilterData<-Data[Data$Year==yrs[r],]
  
  
# subset out the three datasets
ramSubset<-subset(FilterData,Dbase=="RAM" & is.na(Catch)==F)
sofiaSubset<-subset(FilterData,Dbase=="SOFIA" & is.na(Catch)==F)
faoSubset<-subset(FilterData,Dbase=="FAO" & is.na(Catch)==F)

# for RAM stocks across multiple regions, create replicates for each possible FAO region. 
newRam<-data.frame(IdOrig=NA,SciName=NA,Country=NA,RegionFAO=NA)

ramstocks<-unique(ramSubset[c("IdOrig","RegionFAO","SciName","Country")]) # define unique combinations to loop through # Need to break apart countries
sofiastocks<-unique(sofiaSubset[c("IdOrig","RegionFAO","SciName","Country")])
faostocks<-unique(faoSubset[c("IdOrig","RegionFAO","SciName","Country")])

# for ram and sofia, create duplicate stocks for every fao potential fao region for that stock

for (n in 1:length(ramstocks$IdOrig)){ 
  
  RegionFAO<-unlist(strsplit(ramstocks$RegionFAO[n],split=",",fixed=T)) # split apart FAO regions
  num<-length(RegionFAO) # how many?
  
  if(num>0){
    
    IdOrig<-rep(as.character(ramstocks$IdOrig[n],num)) 
    SciName<-rep(as.character(ramstocks$SciName[n],num)) # duplicate info for all columns 
    Country<-rep(ramstocks$Country[n],num) 
    
    newdata<-data.frame(IdOrig,SciName,Country,RegionFAO) # make new data frame with unique rows for each country in the assessment
    newRam<-rbind(newdata,newRam) # add to new ram dataset      
    
  }# close if statement
}# close loop

# convert newRam to character
newRam$IdOrig<-as.character(levels(newRam$IdOrig))[newRam$IdOrig]
newRam$SciName<-as.character(levels(newRam$SciName))[newRam$SciName]
newRam$Country<-as.character(levels(newRam$Country))[newRam$Country]
newRam$RegionFAO<-as.character(levels(newRam$RegionFAO))[newRam$RegionFAO]

newSofia<-data.frame(IdOrig=NA,SciName=NA,Country=NA,RegionFAO=NA)

for (n in 1:length(sofiastocks$IdOrig)){ 
  
  RegionFAO<-unlist(strsplit(sofiastocks$RegionFAO[n],split=",",fixed=T)) # split apart FAO regions
  num<-length(RegionFAO) # how many?
  
  if(num>0){
    
    IdOrig<-rep(as.character(sofiastocks$IdOrig[n],num)) 
    SciName<-rep(as.character(sofiastocks$SciName[n],num)) # duplicate info for all columns 
    Country<-rep(sofiastocks$Country[n],num) 
    
    newdata<-data.frame(IdOrig,SciName,Country,RegionFAO) # make new data frame with unique rows for each country in the assessment
    newSofia<-rbind(newdata,newSofia) # add to new ram dataset      
    
  }# close if statement
}# close loop

newSofia$Country<-as.character(levels(newSofia$Country))[newSofia$Country]
newSofia$IdOrig<-as.character(levels(newSofia$IdOrig))[newSofia$IdOrig]
newSofia$SciName<-as.character(levels(newSofia$SciName))[newSofia$SciName]
newSofia$RegionFAO<-as.character(levels(newSofia$RegionFAO))[newSofia$RegionFAO]

### break up SOFIA so that there is a unique entry for each country
newSofia2<-data.frame(IdOrig=NA,SciName=NA,Country=NA,RegionFAO=NA)

for (n in 1:nrow(newSofia)){ 
  
  # loop currently gives following warnings: In strsplit(SofiaStocks$Country[n], split = ", *", fixed = F) :
  # input string 1 is invalid in this locale  
  
      Country<-unlist(strsplit(newSofia$Country[n],split=", *",fixed=F)) # split apart countries
      num<-length(Country) # how many?
      
      if(num>0){ # if countries are listed...
      
      IdOrig<-rep(newSofia$IdOrig[n],num) 
      SciName<-rep(newSofia$SciName[n],num) # duplicate info for all columns 
      RegionFAO<-rep(newSofia$RegionFAO[n],num) 
      
      newdata<-data.frame(IdOrig,Country,SciName,RegionFAO,stringsAsFactors=F) # make new data frame with unique rows for each country in the assessment
      newSofia2<-rbind(newdata,newSofia2,stringsAsFactors=F) # add to new SOFIA dataset      

}# close if statement
}# close loop

newSofia2$Country<- gsub("^\\s+|\\s+$","",newSofia2$Country) # trim leading and trailing space

# find IDs of SOFIA stocks that are missing SciName and Country, and remove these stocks from dataset
SofiaWithData<-unique(newSofia2$IdOrig)
SofiaWithoutData<-newSofia[newSofia$SciName=="" & newSofia$Country=="",]
SofiaWithoutDataIds<-unique(SofiaWithoutData$IdOrig) 

### Identify SOFIA stocks covered by RAM assessments (may be partial or full overlap)

newRam$SOverlap<-NA
newRam$SOverlapId<-NA

for (i in 1:nrow(newSofia2)){
  
  duplicate<- newSofia2$SciName[i]==newRam$SciName & newSofia2$Country[i]==newRam$Country & newSofia2$RegionFAO[i]==newRam$RegionFAO
  
  newRam$SOverlap[duplicate]<-1
  newRam$SOverlapId[duplicate]<-as.character(newSofia2$IdOrig[i])
}

### Identify FAO stocks covered by RAM assessments (national level)

newRam$FOverlap<-NA
newRam$FOverlapId<-NA

for (i in 1:nrow(faostocks)){
  
  duplicate<- faostocks$SciName[i]==newRam$SciName & faostocks$Country[i]==newRam$Country & faostocks$RegionFAO[i]==newRam$RegionFAO
  
  newRam$FOverlap[duplicate]<-1
  newRam$FOverlapId[duplicate]<-as.character(faostocks$IdOrig[i])
}

### Identify FAO stocks covered by RAM assessments (multinational RAM assessments)

multinational<-subset(newRam,Country=="Multinational")
RamMultiNatOverlap<-NA


for (i in 1:nrow(multinational)){
  
  duplicate<-multinational$SciName[i]==faostocks$SciName & multinational$RegionFAO[i]==faostocks$RegionFAO
  overlapstocks<- (faostocks$IdOrig[duplicate])
  
  num<-length(overlapstocks)
  
  if(num>0){
    
    RamMultiNatOverlap<- (c(RamMultiNatOverlap,as.character(overlapstocks)))
  }
}
### Join FAO stocks that overlap with national and multinational RAM stocks

# natOverlap<-unique(newRam$FOverlapId) # unique FAO stocks that match ram country level assessments
natOverlap<-unique(newRam$FOverlapId,na.rm=T) # unique FAO stocks that match ram country level assessments



multiOverlap<-unique(RamMultiNatOverlap,na.rm=T) # unique FAO stocks that match ram multinational level assessments

RamOverlap<-unique(c(natOverlap,multiOverlap)) # all possible FAO stocks that overlap with RAM

### Identify the FAO stocks that match SOFIA stocks for Scientific name, Country, and FAO region

newSofia2$Overlap<-NA
newSofia2$OverlapId<-NA

for (i in 1:nrow(faostocks)){
  
  duplicate<- faostocks$SciName[i]==newSofia2$SciName & faostocks$Country[i]==newSofia2$Country & faostocks$RegionFAO[i]==newSofia2$RegionFAO
  
  newSofia2$Overlap[duplicate]<-1
  newSofia2$OverlapId[duplicate]<-as.character(faostocks$IdOrig[i])
}

# Identify which Sofia assessments are only partially covered by RAM (e.g., some but not all countries)

newSofia2$ROverlap<-NA
newSofia2$ROverlapId<-NA

for (i in 1:nrow(newRam)){
  
  duplicate<- newRam$SciName[i]==newSofia2$SciName & newRam$Country[i]==newSofia2$Country & newRam$RegionFAO[i]==newSofia2$RegionFAO
  
  newSofia2$ROverlap[duplicate]<-1
  newSofia2$ROverlapId[duplicate]<-as.character(newRam$IdOrig[i])
}

OverlapS<-unique(newRam$SOverlapId) # sofia stocks overlapping with ram stocks in yrs[r]

SofiaOverlap<-newSofia2$OverlapId[is.na(newSofia2$OverlapId)==F] # fao stocks overlapping with sofia in yrs[r]

if (OverlapMode=='SofiaTrumps')
{
AllOverlap<- c(RamOverlap,newSofia2$OverlapId[is.na(newSofia2$OverlapId)==F],OverlapS,SofiaWithoutDataIds)
}

if (OverlapMode=='FaoTrumps')
{
  AllOverlap<- c(RamOverlap,newSofia2$IdOrig[is.na(newSofia2$OverlapId)==F],OverlapS,SofiaWithoutDataIds)
}

FilterData<-FilterData[!(FilterData$IdOrig %in% AllOverlap),]

if(r==1){
  CleanedData<-FilterData
  AllOverlapFinal<-paste(c(AllOverlap),yrs[r],sep="_") # paste using underscore in case want to parse apart ID and year later
  RamOverlapFinal<-paste(c(RamOverlap),yrs[r],sep="_")
  SofiaOverlapFinal<-paste(c(SofiaOverlap),yrs[r],sep="_")
  SofiaRamOverlapFinal<-paste(c(OverlapS),yrs[r],sep="_")
  SofiaWithoutDataIdsFinal<-paste(c(SofiaWithoutDataIds),yrs[r],sep="_")
  } # close if

if(r>1){
  
  CleanedData<-rbind(CleanedData,FilterData) # add year to filtered dataset
  
  AllOverlapYr<-paste(c(AllOverlap),yrs[r],sep="_") # add year to list of overlapping stocks
  RamOverlapYr<-paste(c(RamOverlap),yrs[r],sep="_")
  SofiaOverlapYr<-paste(c(SofiaOverlap),yrs[r],sep="_")
  SofiaRamOverlapYr<-paste(c(OverlapS),yrs[r],sep="_")
  SofiaWithoutDataIdsYr<-paste(c(SofiaWithoutDataIds),yrs[r],sep="_")
  
  
  append(AllOverlapFinal,AllOverlapYr,after=length(AllOverlapFinal)) # add list of IDs and date to final lists
  append(RamOverlapFinal,RamOverlapYr,after=length(RamOverlapFinal))
  append(SofiaOverlapFinal,SofiaOverlapYr,after=length(SofiaOverlapFinal)) 
  append(SofiaRamOverlapFinal,SofiaRamOverlapYr,after=length(SofiaRamOverlapFinal))
  append(SofiaWithoutDataIdsFinal,SofiaWithoutDataIdsYr,after=length(SofiaWithoutDataIdsFinal))
  
  } # close if
show(yrs[r])
} # close loop on yrs

CleanedData<-CleanedData[order(CleanedData$IdOrig, CleanedData$Year),] # sort columns to return to sequential order by id

return(list(FilteredData=CleanedData,AllOverlap=AllOverlapFinal,RamOverlap=RamOverlapFinal,SofiaOverlap=SofiaOverlapFinal,SofiaRamOverlap=SofiaRamOverlapFinal,SofiaWithoutDataIds=SofiaWithoutDataIdsFinal))

}
# FaoOverlap<-subset(FaoStocks,Overlap==1)
# FaoOverlapStocks<-unique(FaoOverlap$IdOrig)

#FilterData<-subset(Data,!(IdOrig %in% c(FaoOverlapStocks))) 
#return(FilterData)
#}

### Some simple aggregations and plots of total catch
# with cleaned data
# FaoTC<-aggregate(Catch~Year+Dbase,data=fao,sum)
# RamTC<-aggregate(Catch~Year+Dbase,data=ram,sum)
# SofiaTC<-aggregate(Catch~Year+Dbase,data=sofia,sum)
# 
# # with cleaned data
# RamClean<-subset(FullData,Dbase=="RAM")
# SofiaClean<-subset(FullData,Dbase=="SOFIA")
# FaoClean<-subset(FullData,Dbase=="FAO")
# 
# FaoTCclean<-aggregate(Catch~Year+Dbase,data=FaoClean,sum)
# RamTCclean<-aggregate(Catch~Year+Dbase,data=RamClean,sum)
# SofiaTCclean<-aggregate(Catch~Year+Dbase,data=SofiaClean,sum)
# 
# 
# TCdf<-rbind(FaoTC,RamTC,SofiaTC)
# TCdfClean<-rbind(FaoTCclean,RamTCclean,SofiaTCclean)
# 
# ggplot(data=TCdf, aes(x=Year,y=Catch)) + geom_line(aes(colour=Dbase))
# ggplot(data=TCdfClean, aes(x=Year,y=Catch)) + geom_line(aes(colour=Dbase))

