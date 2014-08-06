###########################################
###
### Matching RAM Assessments to FAO Regions
###
###########################################

# Read in RAM metadata

load("Data/DBdata.RData") # Load R data pack)

metadata<-as.data.frame(meta.data, stringsAsFactors=F) # save meta.data matrix as a dataframe

rm(bioparams.views.data,bioparams.views.units,timeseries,timeseries.views.data,timeseries.views.units, bioparams) # remove extra data

# Country Identification - using first word from "areaid" in metadata data frame

for (i in 1:length(metadata$areaid)) # loop adds country variable to metadata data frame
{
  country<-unlist(strsplit(metadata$areaid[i],split="-",fixed=T))[1]
  metadata$Country[i]<-country

if(metadata$Country[i]=="USA"){
  metadata$Country[i]<-"United States of America" # changed to match fao records
}
} 

# Read in FAO data

fao=read.csv("Data/FAO_Capture_1950to2011.csv",header=T,stringsAsFactors=F,na.strings=c("...","-","0 0")) # convert ... and - to NA in catch record

# subset to select only variables useful for identification

fao<-fao[,c(1,2,4:8)]

colnames(fao)<- c("Country","CommName","SciName","SpeciesCatName","SpeciesCat","RegionName","RegionFAO")

# find RAM stocks that match both Country and SciName of an FAO stock
RAMNames<-unique(metadata$assessid)
metadata$SpecCntryMatch<-NA
metadata$RegionFAO<-NA

for (i in 1:length(RAMNames)){
  
  SpMatch<-metadata$scientificname[i]==fao$SciName
  matchdata<-fao[SpMatch,]
  SpCtmatch<-match(metadata$Country[i],matchdata$Country)
  
  if(is.na(SpCtmatch)==F){  
  metadata$SpecCntryMatch[i]<-1
  metadata$RegionFAO[i]<-matchdata[SpCtmatch,7]
  }
}

# based on above species/country matching, find FAO region code for each assessorid

mgmt_areas<-aggregate(RegionFAO~assessorid,data=metadata,mean) # take mean, assessorids with non-integer values operate in multiple FAO regions

# for assessorids with integer means (single regions), apply region code to other assessments with same assessorid

need_fao<-is.na(metadata$RegionFAO) # RAM assessments lacking FAO region

for (l in length(RAMNames[need_fao])){  
    
  match<-match(metadata$assessorid[l],mgmt_areas$assessorid) 
  
  
  if(is.na(match)==F & is.integer(mgmt_areas[match,2])==T) # if in need of FAO region and the assessorid matches an assessorid with a single FAO region
  {
    metadata$RegionFAO<-mgmt_areas[match,2] # adopt the FAO region of that assessorid
  } # close if statement
} # close loop around l

# write .csv of all unique assessorids, and one for matched management bodies


