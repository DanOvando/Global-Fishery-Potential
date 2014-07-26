##############################################
####
#### RAM LEGACY DATA PROCESSING
#### DATA RECEIVED ON: 7/23/14
####
##############################################

load("Data/DBdata.RData")

### DATA SUMMARY ### 

length(unique(bioparams[,1])) # number of stocks w/ biopaRAMeters
length(unique(timeseries[,1])) # number of stocks w/ catch timeseries


#### RAM BUILD 2.0 ####

# build dataset using "timeseries.views.data as base dataset

ColNames = c("IdOrig","Year","Fmort","Catch","FmortUnit","CatchUnit","SciName","CommName",
             "Country","RegionFAO","SpeciesCat","SpeciesCatName","Bmsy","SSBmsy","Umsy","Fmsy","Id","Dbase","Biomass","BiomassMetric",
             "BiomassUnit","FmortMetric","ExploitStatus", "VonBertK","VonBertKUnit","VonBertKSource","Temp","TempUnit",
             "TempSource","MaxLength","MaxLengthUnit", "MaxLengthSource","AgeMat","AgeMatUnit","AgeMatSource",'ReferenceBiomass','ReferenceBiomassUnits',"BvBmsy")

####### Data Formatting and Subsetting ####### 

# convert matrices with column names to dataframes and convert variables to characters/numerics
bioparams<-as.data.frame(bioparams)
bioparams$assessid<-as.character(levels(bioparams$assessid))[bioparams$assessid]
bioparams$bioid<-as.character(levels(bioparams$bioid))[bioparams$bioid]
bioparams$biovalue<-as.character(levels(bioparams$biovalue))[bioparams$biovalue] # leave as.character until after filling in dataset, then identify values that need adjustment

metadata<-as.data.frame(meta.data)
metadata$assessid<-as.character(levels(metadata$assessid))[metadata$assessid]
metadata$scientificname<-as.character(levels(metadata$scientificname))[metadata$scientificname]
metadata$areaid<-as.character(levels(metadata$areaid))[metadata$areaid]

# add column names to data sets missing names
colnames(timeseries.views.data)<-c("IdOrig","stockid","CommName","Year","TB", "SSB", "TN", "R",
 "TC", "TL", "Fmort", "ER", "BvBmsyDrop", "SSBvSSBmsy", "FvFmsy", "UvUmsy", "Biomass", "Catch", "Utouse", "BvBmsy", "UvUmsytouse") 

# made following changes to variable names:
# Btouse -> Biomass
# Ctouse -> Catch
# Bmsytouse -> ReferenceBiomass
# tsyear -> Year
# F -> Fmort
# stocklong -> CommName # temporary
# BvBmsy -> BvBmsyDrop # dropping this variable in favor of Bmsytouse
# BvBmsytouse -> BvBmsy

colnames(timeseries.views.units)<-c("IdOrig","stockid","stocklong","TB", "SSB", "TN", "R","TC", "TL", "F", "ER")

# For datasets missing variable names, convert to data frame, name variables, and convert to character/numeric as neccessary
bioparams.views.units<-as.data.frame(bioparams.views.units)
colnames(bioparams.views.units)<-c("IdOrig","stockid","stocklong","Bmsy", "SSBmsy", "Nmsy", "MSY", "Fmsy", "Umsy", "B0", "SSB0", "M")

bioparams.views.data<-as.data.frame(bioparams.views.data)
colnames(bioparams.views.data)<-c("IdOrig","stockid","stocklong","Bmsy", "SSBmsy", "Nmsy", "MSY", "Fmsy", "Umsy", "B0", "SSB0", "M", "ReferenceBiomass", "Umsytouse")

bioparams.views.data$IdOrig<-as.character(levels(bioparams.views.data$IdOrig))[bioparams.views.data$IdOrig]
bioparams.views.data$ReferenceBiomass<-as.numeric(levels(bioparams.views.data$ReferenceBiomass))[bioparams.views.data$ReferenceBiomass]
bioparams.views.data$SSBmsy<-as.numeric(levels(bioparams.views.data$SSBmsy))[bioparams.views.data$SSBmsy]
bioparams.views.data$Fmsy<-as.numeric(levels(bioparams.views.data$Fmsy))[bioparams.views.data$Fmsy]
bioparams.views.data$Umsytouse<-as.numeric(levels(bioparams.views.data$Umsytouse))[bioparams.views.data$Umsytouse]
bioparams.views.data$Bmsy<-as.numeric(levels(bioparams.views.data$Bmsy))[bioparams.views.data$Bmsy]

# subset out unneeded variables and convert to data frame

RAM<-as.data.frame(timeseries.views.data)

RAM<-(subset(RAM, select=c("IdOrig","CommName","Year","Fmort","BvBmsy","Biomass","Catch")))

# convert variables to character
RAM$IdOrig<-as.character(levels(RAM$IdOrig))[RAM$IdOrig]
RAM$CommName<-as.character(levels(RAM$CommName))[RAM$CommName]                  


# convert variables to numeric
RAM$Fmort<-as.numeric(levels(RAM$Fmort))[RAM$Fmort]
RAM$BvBmsy<-as.numeric(levels(RAM$BvBmsy))[RAM$BvBmsy]
RAM$Biomass<-as.numeric(levels(RAM$Biomass))[RAM$Biomass]
RAM$Catch<-as.numeric(levels(RAM$Catch))[RAM$Catch]
RAM$Year<-as.numeric(levels(RAM$Year))[RAM$Year]

# add missing variables
RAM$Id=rep(0,nrow(RAM)) # fill in once full database is compiled 
RAM$Dbase=rep("RAM",nrow(RAM))
RAM$BiomassMetric=rep("B",nrow(RAM)) # all Biomass values are either original total biomass values or SSB values that have been scaled to TB
RAM$BiomassUnit=rep("MT",nrow(RAM)) # all Biomass values reported in metric tons
RAM$FmortMetric=character(length=nrow(RAM))
RAM$FmortUnit=character(length=nrow(RAM))
RAM$ExploitStatus=character(length=nrow(RAM))
RAM$VonBertK<-as.numeric(rep("",nrow(RAM)))
RAM$VonBertKSource=character(length=nrow(RAM))
RAM$VonBertKUnit=character(length=nrow(RAM))
RAM$Temp<-as.numeric(rep("",nrow(RAM)))
RAM$TempSource=character(length=nrow(RAM))
RAM$TempUnit=character(length=nrow(RAM))
RAM$MaxLength<-as.numeric(rep("",nrow(RAM)))
RAM$MaxLengthSource=character(length=nrow(RAM))
RAM$MaxLengthUnit=character(length=nrow(RAM))
RAM$AgeMat<-as.numeric(rep("",nrow(RAM)))
RAM$AgeMatSource=character(length=nrow(RAM))
RAM$AgeMatUnit=character(length=nrow(RAM))
RAM$SpeciesCatName=character(length=nrow(RAM))
RAM$RegionFAO=as.numeric(rep("",nrow(RAM)))
RAM$Bmsy=as.numeric(rep("",nrow(RAM)))
RAM$SSBmsy=as.numeric(rep("",nrow(RAM)))
RAM$Fmsy=as.numeric(rep("",nrow(RAM)))
RAM$Umsy=as.numeric(rep("",nrow(RAM)))
RAM$SciName=character(length=nrow(RAM))
RAM$SpeciesCat=as.numeric(rep("",nrow(RAM)))
RAM$CatchUnit=rep("MT",nrow(RAM))
RAM$Country<-NA

####### Life History ####### 
# run for loop to match life history paRAMeters by assessid/IdOrig to bioparams .csv
# Von Bert K value and unit
RAMNames<- unique(RAM$IdOrig)

for (i in 1:length(RAMNames)) 
{
  
  Where<- RAM$IdOrig==RAMNames[i]
  
  if (sum(bioparams$assessid==RAMNames[i] & grepl("VB-k",bioparams$bioid))>0)
  {
    
    
    RAM$VonBertK[Where]=bioparams[bioparams$assessid==RAMNames[i] & grepl("VB-k",bioparams$bioid),5]
    RAM$VonBertKUnit[Where]=bioparams[bioparams$assessid==RAMNames[i]& grepl("VB-k",bioparams$bioid),4]
    RAM$VonBertKSource[Where]="RAM"
  }
}

# Max Lenght and Units

for (i in 1:length(RAMNames)) 
{
  
  Where<- RAM$IdOrig==RAMNames[i]
  
  if (sum(bioparams$assessid==RAMNames[i] & grepl("MAX-LEN",bioparams$bioid))>0)
  {
    
    
    RAM$MaxLength[Where]=bioparams[bioparams$assessid==RAMNames[i] & grepl("MAX-LEN",bioparams$bioid),5]
    RAM$MaxLengthUnit[Where]=bioparams[bioparams$assessid==RAMNames[i]& grepl("MAX-LEN",bioparams$bioid),4]
    RAM$MaxLengthSource[Where]="RAM"
  }
}

# Age at 50% maturity

for (i in 1:length(RAMNames)) ## multiple matches for Age at Mat for some studies, pick one?
{
  
  Where<- RAM$IdOrig==RAMNames[i]
  
  if (sum(bioparams$assessid==RAMNames[i] & grepl("A50-yr",bioparams$bioid))>0)
  {
    
    
    RAM$AgeMat[Where]=bioparams[bioparams$assessid==RAMNames[i] & grepl("A50-yr",bioparams$bioid),5]
    RAM$AgeMatUnit[Where]=bioparams[bioparams$assessid==RAMNames[i]& grepl("A50-yr",bioparams$bioid),4]
    RAM$AgeMatSource[Where]="RAM"
  }
}

### Clean up character values in life-history parameters and convert to numeric

# VonBertK
# "varies by region" will be converted to NA, still finding a "-0.36 value", can't resolve from reference

# MaxLength
RAM$MaxLength=gsub("26+","26",RAM$MaxLength, fixed=T) # remove + from end of 26
RAM$MaxLength=gsub("available","",RAM$MaxLength) # remove "available", RAM entry says "varies greatly"

# AgeMat
RAM$AgeMat=gsub("7.7+","7.7",RAM$AgeMat,fixed=T) # remove + from end of 7.7 
RAM$AgeMat=gsub("3+","3",RAM$AgeMat,fixed=T) # remove + from end of 3
RAM$AgeMat=gsub(" yr","",RAM$AgeMat) # remove yr from end of 2.5 
RAM$AgeMat=gsub("0-1","0.5",RAM$AgeMat) # average
# still have "FOUND AS A TIME SERIES IN FIGURE 10", "AVAILABLE", and "3-Apr", and "4-Mar" data points. Will be converted to NA

RAM$VonBertK<-as.numeric(RAM$VonBertK)
RAM$AgeMat<-as.numeric(RAM$AgeMat)
RAM$MaxLength<-as.numeric(RAM$MaxLength)

# Add in SciName

for (i in 1:length(RAMNames))
{
  Where<-RAM$IdOrig==RAMNames[i]
  RAM$SciName[Where]<-metadata[RAMNames[i]==metadata$assessid,4]
}

# Add FmortMetric and Fmort Unit

for (i in 1:length(RAMNames))
{
  Wheref<-(RAM$IdOrig==RAMNames[i] & is.na(RAM$Fmort)==F)
  biop_match<-match(RAMNames[i],timeseries.views.units)
  
  RAM$FmortMetric[Wheref]<-"F"
  RAM$FmortUnit[Wheref]<-timeseries.views.units[biop_match,10]
}

# Add in reference values (Bmsy,Fmsy,SSBmsy,Umsy)

for (i in 1:length(RAMNames)) 
{
  Whereref<-RAM$IdOrig==RAMNames[i]
  lh_match<-match(RAMNames[i],bioparams.views.data$IdOrig)
  
  RAM$Bmsy[Whereref]<-bioparams.views.data[lh_match,13] #Bmsy - taken from Bmsytouse a.k.a ReferenceBiomass
  RAM$Umsy[Whereref]<-bioparams.views.data[lh_match,14] #Umsy - taken from Umsytouse
  RAM$Fmsy[Whereref]<-bioparams.views.data[lh_match,8] #Fmsy
  RAM$SSBmsy[Whereref]<-bioparams.views.data[lh_match,5] #SSBmsy
  # filling Bmsy with Bmsytouse, which was renamed to ReferenceBiomass. Will be duplicated in ReferenceBiomass column
}

# Add reference biomass
RAM$ReferenceBiomass<-RAM$Bmsy # This will be the biomass reference point to use

RAM$ReferenceBiomassUnits<-NA

WhereRefB<-RAM$ReferenceBiomass==RAM$Bmsy

RAM$ReferenceBiomassUnits[WhereRefB]<-"Bmsy"


####### Species, Country, and Region Codes ####### 
# read in .csvs with matched RAM assessids and FAO regions and species scientific names and ISSCAAP codes

Spec_ISSCAAP=read.csv("Data/Species_ASFIS_ISSCAAP.csv") # list of ASFIS scientific names and corressponding ISSCAAP codes 
Spec_Region_RAM=read.csv("Data/Species_Region_RAM.csv") # list of RAM Assessed IDs previously matched to species code and FAO Region

Spec_Region_RAM$Assessed.ID=as.character(levels(Spec_Region_RAM$Assessed.ID))[Spec_Region_RAM$Assessed.ID] # convert ID to character
Spec_ISSCAAP$Species_AFSIS=as.character(levels(Spec_ISSCAAP$Species_AFSIS))[Spec_ISSCAAP$Species_AFSIS] # convert ID to character

# FAO Region matching
RegionFAOMatches=unique(Spec_Region_RAM$Assessed.ID)

for (i in 1:length(RAMNames)) # currently only matching 11 of the 17 unique FAO regions in the Spec_Region.csv. Due to stocks in multiple zones? 
{
  
  Where1<- RAMNames[i]==Spec_Region_RAM$Assessed.ID
  
  Where2<- RAMNames[i]==RAM$IdOrig
  
  
  if (sum(Spec_Region_RAM$Assessed.ID==RAMNames[i])>0)
  {
    RAM$RegionFAO[Where2]<- Spec_Region_RAM[Where1,4][1]
  }
}

# Country Identification - using first word from "areaid" in metadata data frame
for (i in 1:length(metadata$areaid)) # loop adds country variable to metadata data frame
     {
  country<-unlist(strsplit(metadata$areaid[i],split="-",fixed=T))[1]
  metadata$Country[i]<-country
} 

for(i in 1:length(RAMNames))# loop adds country variable to RAM
  {
  whereram<-RAMNames[i]==RAM$IdOrig
  wheremeta<-RAMNames[i]==metadata$assessid
  
  RAM$Country[whereram]<-metadata[wheremeta,12]
}


# ISSCAAP Species Code Matching

GroupNames_ISSCAAP<-read.csv("Data/ISSCAAP Codes.csv",stringsAsFactors=F)
SpecNames<-unique(RAM$SciName)


for (i in 1:length(SpecNames)) # match species name to group code
{
  Where<-RAM$SciName==SpecNames[i]
  
  if (sum(Spec_ISSCAAP$Species_AFSIS==SpecNames[i])>0)
  {
    RAM$SpeciesCat[Where]<-Spec_ISSCAAP[Spec_ISSCAAP$Species_AFSIS==SpecNames[i],2]
  }
}

GroupNums<-unique(na.omit(RAM$SpeciesCat))

for (i in 1:length(GroupNums)) # match group code to group name
{
  Where<-((RAM$SpeciesCat==GroupNums[i]))
  
  if (sum(GroupNames_ISSCAAP$ISSCAAP.code==GroupNums[i])>0)
  {
    RAM$SpeciesCatName[Where]<-GroupNames_ISSCAAP[GroupNames_ISSCAAP$ISSCAAP.code==GroupNums[i],2]
  }
}

####### END RAM ####### 

