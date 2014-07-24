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
             "TempSource","MaxLength","MaxLengthUnit", "MaxLengthSource","AgeMat","AgeMatUnit","AgeMatSource",'ReferenceBiomass','ReferenceBiomassUnits')

# convert matrices with column names to dataframes and convert variables to characters/numerics
bioparams<-as.data.frame(bioparams)
bioparams$assessid<-as.character(levels(bioparams$assessid))[bioparams$assessid]
bioparams$bioid<-as.character(levels(bioparams$bioid))[bioparams$bioid]
bioparams$biovalue<-as.character(levels(bioparams$biovalue))[bioparams$biovalue] # leave as.character until after filling in dataset, then identify values that need adjustment

metadata<-as.data.frame(meta.data)
metadata$assessid<-as.character(levels(metadata$assessid))[metadata$assessid]
metadata$scientificname<-as.character(levels(metadata$scientificname))[metadata$scientificname]

# add column names to data sets missing names
colnames(timeseries.views.data)<-c("IdOrig","stockid","CommName","Year","TB", "SSB", "TN", "R",
 "TC", "TL", "Fmort", "ER", "BvBmsy", "SSBvSSBmsy", "FvFmsy", "UvUmsy", "Biomass", "Catch", "Utouse", "BvBmsytouse", "UvUmsytouse") 

# made following changes to variable names:
# Btouse -> Biomass
# Ctouse -> Catch
# Bmsytouse -> ReferenceBiomass
# tsyear -> Year
# F -> Fmort
# stocklong -> CommName # temporary

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

# subset out unneeded variables and convert to data fRAMe

RAM<-as.data.frame(timeseries.views.data)

RAM<-(subset(RAM, select=c("IdOrig","CommName","Year","TB","SSB","TC","TL", "Fmort","BvBmsy","SSBvSSBmsy", 
                  "BvBmsytouse","Biomass","Catch")))

# convert variables to character
RAM$IdOrig<-as.character(levels(RAM$IdOrig))[RAM$IdOrig]
RAM$CommName<-as.character(levels(RAM$CommName))[RAM$CommName]                  
RAM$Year<-as.character(levels(RAM$Year))[RAM$Year]

# convert variables to numeric
RAM$TB<-as.numeric(levels(RAM$TB))[RAM$TB]
RAM$SSB<-as.numeric(levels(RAM$SSB))[RAM$SSB]
RAM$TC<-as.numeric(levels(RAM$TC))[RAM$TC]
RAM$TL<-as.numeric(levels(RAM$TL))[RAM$TL]
RAM$Fmort<-as.numeric(levels(RAM$Fmort))[RAM$Fmort]
RAM$BvBmsy<-as.numeric(levels(RAM$BvBmsy))[RAM$BvBmsy]
RAM$SSBvSSBmsy<-as.numeric(levels(RAM$SSBvSSBmsy))[RAM$SSBvSSBmsy]
RAM$BvBmsytouse<-as.numeric(levels(RAM$BvBmsytouse))[RAM$BvBmsytouse]
RAM$Biomass<-as.numeric(levels(RAM$Biomass))[RAM$Biomass]
RAM$Catch<-as.numeric(levels(RAM$Catch))[RAM$Catch]

# add missing variables
RAM$Id=rep(0,nrow(RAM)) # fill in once full database is compiled 
RAM$Dbase=rep("RAM",nrow(RAM))
RAM$BiomassMetric=character(length=nrow(RAM))
RAM$BiomassUnit=character(length=nrow(RAM))
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

RAM$ReferenceBiomass<-RAM$Bmsy # This will be the biomass reference point to use

RAM$ReferenceBiomassUnits<-NA

WhereRefB<-RAM$ReferenceBiomass==RAM$Bmsy

WhereRefSSB<-RAM$ReferenceBiomass==RAM$SSBmsy

RAM$ReferenceBiomassUnits[WhereRefB]<-"Bmsy"

RAM$ReferenceBiomassUnits[WhereRefSSB]<-"SSBmsy"

# WhereProxyB <- After identifying which Bmsy values are potential proxies, need to indicate 

# Biomass Metric and Units
# In RAM, it appears that Biomass (taken from Btouse) is either B, SSB, or some proxy. 
# values are rounded in Biomass and will need to use some form of numerical tolerance for determining matches with B or SSB


####### Species and Region Codes ####### 
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




### Before continuing, convert remaining datasets to dataframes and convert relevent variables to characters/numeric

