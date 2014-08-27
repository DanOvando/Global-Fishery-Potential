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

# subset out the three datasets
ram<-subset(Data,Dbase=="RAM")
sofia<-subset(Data,Dbase=="SOFIA")
fao<-subset(Data,Dbase=="FAO")

# for ram stocks with country but unclear(missing) FAO regions, make a duplicate stock for each possible FAO region
# identify overlap using country/species/region

# for "multinational" ram stocks, make duplicate stock for each possible fao region in the area (Pacific, Atlantic, etc)
# identify overlap using just species/region

Spec_Region_RAM=read.csv("Data/RAM_Regions_72814.csv") # list of RAM Assessed IDs previously matched to species code and FAO Region

ramstocks<-Spec_Region_RAM[,c(2,5,9,13,15)] # subset ram to select only info relevant for filtering
ramstocks$RegionFAO<- gsub("/",",",ramstocks$RegionFAO,fixed=T) # change / to , for use in string parsing later in script
ramstocks$Country<- gsub("multinational","Multinational",ramstocks$Country)

# make data frame with FAO regions for multinational stock possibilities

regionname<-c( "Atlantic Ocean",                    "Central Western Pacific Ocean",     "Eastern Atlantic",                 
               "Eastern Pacific",                   "North Pacific Ocean",               "Northern Atlantic",                
               "Pacific Ocean",                     "South Atlantic",                    "South Pacific Ocean",              
               "Western and Central North Pacific", "Western Atlantic",                  "Western Pacific Ocean") 

regs<-c("21,27,31,34,41,47,48","71","27,34,47","67,77,87","61,67","21,27","61,67,71,77,81,87,88","41,47","81,87","61,67,71,77","21,31,41","61,71,81")

multiFAOdf<-data.frame(regionname,regs,stringsAsFactors=F)

# run for loop to identify multinational stocks with NAs and fill fao regions based on multiFAOdf


for (i in 1:length(ramstocks$assessid)){
  
  if(ramstocks$Country[i]=="Multinational" & is.na(ramstocks$RegionFAO[i])==T){
    
    regmatch<-match(ramstocks$areaname[i],multiFAOdf$regionname)
    
    ramstocks$RegionFAO[i]<-multiFAOdf[regmatch,2]
  }
}

# for RAM stocks across multiple regions, create replicates for each possible FAO region. 
newRam<-data.frame(assessid=NA,scientificname=NA,areaname=NA, Country=NA,RegionFAO=NA)

for (n in 1:length(ramstocks$assessid)){ 
  
  RegionFAO<-unlist(strsplit(ramstocks$RegionFAO[n],split=",",fixed=T)) # split apart FAO regions
  num<-length(RegionFAO) # how many?
  
  if(num>0){
    
    assessid<-rep(as.character(ramstocks$assessid[n],num)) 
    scientificname<-rep(as.character(ramstocks$scientificname[n],num)) # duplicate info for all columns 
    Country<-rep(ramstocks$Country[n],num) 
    areaname<-rep(as.character(ramstocks$areaname[n],num))
    
    newdata<-data.frame(assessid,scientificname,areaname,Country,RegionFAO) # make new data frame with unique rows for each country in the assessment
    newRam<-rbind(newdata,newRam) # add to new ram dataset      
    
  }# close if statement
}# close loop

colnames(newRam)[2]<-"SciName"
colnames(newRam)[1]<-"IdOrig"

# get unique stocks from each subset by aggregating to the stock level and calculating total catch
  # this method will not identify stocks that are missing one or more of the 4 aggregating criteria, need to improve

RamStocks<-aggregate(Catch~IdOrig+Country+SciName+RegionFAO,ram,sum) # RegionFAO needs to be updated with revised FAO Region matches
SofiaStocks<-aggregate(Catch~IdOrig+Country+SciName+RegionFAO,sofia,sum) # Need to break apart countries
FaoStocks<-aggregate(Catch~IdOrig+Country+SciName+RegionFAO,fao,sum) 

### break up SOFIA so that there is a unique entry for each country

newSofia<-data.frame(IdOrig=NA,Country=NA,SciName=NA,RegionFAO=NA)

for (n in 1:length(SofiaStocks$IdOrig)){ 
  
  # loop currently gives following warnings: In strsplit(SofiaStocks$Country[n], split = ", *", fixed = F) :
  # input string 1 is invalid in this locale  
  
      Country<-unlist(strsplit(SofiaStocks$Country[n],split=", *",fixed=F)) # split apart countries
      num<-length(Country) # how many?
      
      if(num>0){ # if countries are listed...
      
      IdOrig<-rep(SofiaStocks$IdOrig[n],num) 
      SciName<-rep(SofiaStocks$SciName[n],num) # duplicate info for all columns 
      RegionFAO<-rep(SofiaStocks$RegionFAO[n],num) 
      
      newdata<-data.frame(IdOrig,Country,SciName,RegionFAO,stringsAsFactors=F) # make new data frame with unique rows for each country in the assessment
      newSofia<-rbind(newdata,newSofia,stringsAsFactors=F) # add to new SOFIA dataset      

}# close if statement
}# close loop

### Clean up Country names to match for SOFIA/RAM/FAO stocks

# general fixes to SOFIA
newSofia$Country<-gsub("\n"," ",newSofia$Country) # sub out "/n"
newSofia$Country<-gsub("  "," ",newSofia$Country) # change double spaces to single spaces
newSofia$Country<-gsub("\\(.*\\)","",newSofia$Country) # delete anything within parentheses
newSofia$Country<- gsub("^\\s+|\\s+$","",newSofia$Country) # trim leading and trailing space

# general fixes to FAO
FaoStocks$Country<-gsub(",.*$","",FaoStocks$Country) # removes everything after a comma, this will combine certain entries like "Korea, Dem. People's Rep" and "Korea, Republic of"
FaoStocks$Country<-gsub("\\(.*\\)","",FaoStocks$Country) # delete anything within parentheses
FaoStocks$Country<- gsub("^\\s+|\\s+$","",FaoStocks$Country) # trim leading and trailing space
FaoStocks$Country<-gsub("  "," ",FaoStocks$Country) # change double spaces to single spaces

# specific fixes to SOFIA
newSofia$Country[newSofia$Country=="Saint Vincent/ Grenadines"]<- "Saint Vincent/Grenadines"
newSofia$Country[newSofia$Country=="United States of America"]<- "USA"
newSofia$Country[newSofia$Country=="the Democratic Republic of the Congo"]<- "Congo"
newSofia$Country[newSofia$Country=="United Republic of Tanzania"]<- "Tanzania"
newSofia$Country[newSofia$Country=="Republic of Korea"]<- "Korea"

# specific FAO country fixes
FaoStocks$Country[FaoStocks$Country=="United States of America"]<- "USA"
FaoStocks$Country[FaoStocks$Country=="Falkland Is.(Malvinas)"]<- "Falkland Islands"


### Identify SOFIA stocks covered by RAM assessments (may be partial or full overlap)

RamStocks$SOverlap<-NA
RamStocks$SOverlapId<-NA

for (i in 1:nrow(newSofia)){
  
  duplicate<- newSofia$SciName[i]==RamStocks$SciName & newSofia$Country[i]==RamStocks$Country & newSofia$RegionFAO[i]==RamStocks$RegionFAO
  
  RamStocks$SOverlap[duplicate]<-1
  RamStocks$SOverlapId[duplicate]<-as.character(newSofia$IdOrig[i])
}

### Identify FAO stocks covered by RAM assessments (national level)

RamStocks$FOverlap<-NA
RamStocks$FOverlapId<-NA

for (i in 1:nrow(FaoStocks)){
  
  duplicate<- FaoStocks$SciName[i]==RamStocks$SciName & FaoStocks$Country[i]==RamStocks$Country & FaoStocks$RegionFAO[i]==RamStocks$RegionFAO
  
  RamStocks$FOverlap[duplicate]<-1
  RamStocks$FOverlapId[duplicate]<-as.character(FaoStocks$IdOrig[i])
}

### Identify FAO stocks covered by RAM assessments (multinational RAM assessments)

multinational<-subset(newRam,Country=="Multinational")
RamMultiNatOverlap<-NA


for (i in 1:nrow(multinational)){
  
  duplicate<-multinational$SciName[i]==FaoStocks$SciName & multinational$RegionFAO[i]==FaoStocks$RegionFAO
  overlapstocks<- (FaoStocks$IdOrig[duplicate])
  
  num<-length(overlapstocks)
  
  if(num>0){
    
    RamMultiNatOverlap<- (c(RamMultiNatOverlap,as.character(overlapstocks)))
  }
}
### Join FAO stocks that overlap with national and multinational RAM stocks

# natOverlap<-unique(newRam$FOverlapId) # unique FAO stocks that match ram country level assessments
natOverlap<-unique(RamStocks$FOverlapId,na.rm=T) # unique FAO stocks that match ram country level assessments



multiOverlap<-unique(RamMultiNatOverlap,na.rm=T) # unique FAO stocks that match ram multinational level assessments

RamOverlap<-unique(c(natOverlap,multiOverlap)) # all possible FAO stocks that overlap with RAM

### Identify the FAO stocks that match SOFIA stocks for Scientific name, Country, and FAO region

newSofia$Overlap<-NA
newSofia$OverlapId<-NA

for (i in 1:nrow(FaoStocks)){
  
  duplicate<- FaoStocks$SciName[i]==newSofia$SciName & FaoStocks$Country[i]==newSofia$Country & FaoStocks$RegionFAO[i]==newSofia$RegionFAO
  
  newSofia$Overlap[duplicate]<-1
  newSofia$OverlapId[duplicate]<-as.character(FaoStocks$IdOrig[i])
}

# Identify which Sofia assessments are only partially covered by RAM (e.g., some but not all countries)

newSofia$ROverlap<-NA
newSofia$ROverlapId<-NA

for (i in 1:nrow(RamStocks)){
  
  duplicate<- RamStocks$SciName[i]==newSofia$SciName & RamStocks$Country[i]==newSofia$Country & RamStocks$RegionFAO[i]==newSofia$RegionFAO
  
  newSofia$ROverlap[duplicate]<-1
  newSofia$ROverlapId[duplicate]<-as.character(RamStocks$IdOrig[i])
}

OverlapS<-unique(RamStocks$SOverlapId)

newSofia$OverlapId[is.na(newSofia$OverlapId)==F]

if (OverlapMode=='SofiaTrumps')
{
AllOverlap<- c(RamOverlap,newSofia$OverlapId[is.na(newSofia$OverlapId)==F],OverlapS)
}

if (OverlapMode=='FaoTrumps')
{
  AllOverlap<- c(RamOverlap,newSofia$IdOrig[is.na(newSofia$OverlapId)==F],OverlapS)
}

return(list(AllOverlap=AllOverlap,RamOverlap=RamOverlap,SofiaOverlap=newSofia,SofiaRamOverlap=OverlapS))

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

