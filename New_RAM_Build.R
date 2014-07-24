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

ColNames = c("IdOrig","Year","Fmort","Catch","FmortUnit","CatchUnit","SciName","CommName",
             "Country","RegionFAO","SpeciesCat","SpeciesCatName","Bmsy","SSBmsy","Umsy","Fmsy","Id","Dbase","Biomass","BiomassMetric",
             "BiomassUnit","FmortMetric","ExploitStatus", "VonBertK","VonBertKUnit","VonBertKSource","Temp","TempUnit",
             "TempSource","MaxLength","MaxLengthUnit", "MaxLengthSource","AgeMat","AgeMatUnit","AgeMatSource",'ReferenceBiomass','ReferenceBiomassUnits')

# convert to dataframes
bioparams<-as.data.frame(bioparams)
bioparams$assessid<-as.character(levels(bioparams$assessid))[bioparams$assessid]
bioparams$bioid<-as.character(levels(bioparams$bioid))[bioparams$bioid]

metadata<-as.data.frame(meta.data)
metadata$assessid<-as.character(levels(metadata$assessid))[metadata$assessid]
metadata$scientificname<-as.character(levels(metadata$scientificname))[metadata$scientificname]

bioparams<-as.data.frame(bioparams)
bioparams$assessid<-as.character(levels(bioparams$assessid))[bioparams$assessid]
bioparams$bioid<-as.character(levels(bioparams$bioid))[bioparams$bioid]


# build dataset using "timeseries.views.data as base dataset

# add column names to data sets
colnames(timeseries.views.data)<-c("IdOrig","stockid","stocklong","Year","TB", "SSB", "TN", "R",
 "TC", "TL", "Fmort", "ER", "BvBmsy", "SSBvSSBmsy", "FvFmsy", "UvUmsy", "Biomass", "Catch", "Utouse", "BvBmsytouse", "UvUmsytouse") 

# made following changes to variable names:
# Btouse -> Biomass
# Ctouse -> Catch
# Bmsytouse -> ReferenceBiomass
# tsyear -> Year
# F -> Fmort

colnames(timeseries.views.units)<-c("IdOrig","stockid","stocklong","TB", "SSB", "TN", "R","TC", "TL", "F", "ER")

colnames(bioparams.views.data)<-c("IdOrig","stockid","stocklong","Bmsy", "SSBmsy", "Nmsy", "MSY", "Fmsy", "Umsy", "B0", "SSB0", "M", "ReferenceBiomass", "Umsytouse")

colnames(bioparams.views.units)<-c("IdOrig","stockid","stocklong","Bmsy", "SSBmsy", "Nmsy", "MSY", "Fmsy", "Umsy", "B0", "SSB0", "M")


# subset out unneeded variables and convert to data fRAMe


RAM<-as.data.frame(timeseries.views.data)

RAM<-(subset(RAM, select=c("IdOrig","stocklong","Year","TB","SSB","TC","TL", "Fmort","BvBmsy","SSBvSSBmsy", 
                  "BvBmsytouse","Biomass","Catch")))

# convert variables to character
RAM$IdOrig<-as.character(levels(RAM$IdOrig))[RAM$IdOrig]
RAM$stocklong<-as.character(levels(RAM$stocklong))[RAM$stocklong]                  
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
RAM$RegionFAO=rep(0,nrow(RAM))
RAM$Bmsy=as.numeric(rep("",nrow(RAM)))
RAM$SSBmsy=as.numeric(rep("",nrow(RAM)))
RAM$Fmsy=as.numeric(rep("",nrow(RAM)))
RAM$Umsy=as.numeric(rep("",nrow(RAM)))
RAM$SciName=character(length=nrow(RAM))

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

# Add in reference values

for (i in 1:length(RAMNames))
{
  Whereref<-RAM$IdOrig==RAMNames[i]
  RAM$Bmsy
}

# add in reference values (Bmsy,Fmsy,SSBmsy,Umsy)

### Before continuing, convert remaining datasets to dataframes and convert relevent variables to characters/numeric

