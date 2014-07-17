
# Ram Build ---------------------------------------------------------------

############################################################################################################

# Script first creates columns for final database, then formats the RAM, SOFIA, and FAO databases, and finally binds the three databases

############################################################################################################
############ CREATE DATABASE COLUMNS ############

ColNames=c("Id","IdOrig","Dbase","CommName","SciName","SpeciesCat","Country","RegionFAO","Year","Catch","CatchUnit", "Biomass",
           "BiomassUnit","BiomassMetric", "Fmort","FmortUnit","FmortMetric", "ExploitStatus","Bmsy","SSBmsy","Fmsy",
           "Umsy", "VonBertK","VonBertKUnit", "VonBertKSource","Temp","TempUnit", "TempSource","MaxLength","MaxLengthUnit",
           "MaxLengthSource","AgeMat","AgeMatUnit", "AgeMatSource")

############################################################################################################
############ READ IN RAM DATASET ############

# .csvs from RAM Legacy output
RAM<-read.csv("Data/Ram Time Series 6_11_14.csv", stringsAsFactors=F) # Time series .csv
RAM_BioParams<-read.csv("Data/RAM BioParams 6_11_14.csv", stringsAsFactors=F) # Bio parameters .csv

# drop unwanted columns
RAM<-subset(RAM, select=c("assessid","tsyear","ssb","total","f","catch_landings","ssb_unit","total_unit",
                          "f_unit","catch_landings_unit","scientificname","commonname","country",
                          "bmsy" ,"ssbmsy","umsy","fmsy"))

# rename columns to common format (see ColNames above)
names(RAM)[1]<-"IdOrig"
names(RAM)[2]<-"Year"
names(RAM)[3]<-"ssb"
names(RAM)[5]<-"Fmort"
names(RAM)[6]<-"Catch"
names(RAM)[9]<-"FmortUnit"
names(RAM)[10]<-"CatchUnit"
names(RAM)[11]<-"SciName"
names(RAM)[12]<-"CommName"
names(RAM)[13]<-"Country"
names(RAM)[14]<-"Bmsy"
names(RAM)[15]<-"SSBmsy"
names(RAM)[16]<-"Umsy"
names(RAM)[17]<-"Fmsy"

# add columns for missing variables
RAM$Id=rep(0,nrow(RAM)) # fill in once full database is compiled 
RAM$Dbase=rep("RAM",nrow(RAM))
RAM$Biomass=rep(0,nrow(RAM))
RAM$BiomassMetric=character(length=nrow(RAM))
RAM$BiomassUnit=character(length=nrow(RAM))
RAM$FmortMetric=character(length=nrow(RAM))
RAM$FmortUnit=character(length=nrow(RAM))
RAM$VonBertK=rep(0,nrow(RAM))
RAM$VonBertKSource=character(length=nrow(RAM))
RAM$VonBertKUnit=character(length=nrow(RAM))
RAM$Temp=rep(0,nrow(RAM))
RAM$TempSource=character(length=nrow(RAM))
RAM$TempUnit=character(length=nrow(RAM))
RAM$MaxLength=rep(0,nrow(RAM))
RAM$MaxLengthSource=character(length=nrow(RAM))
RAM$MaxLengthUnit=character(length=nrow(RAM))
RAM$AgeMat=rep(0,nrow(RAM))
RAM$AgeMatSource=character(length=nrow(RAM))
RAM$AgeMatUnit=character(length=nrow(RAM))

####### Biomass Calculation ####### 
# calculate biomass variable by either taking "total" if available and ssb otherwise. fill BiomassMetric with B or SSB and BiomassUnit w/ corresponding units
for (i in 1:nrow(RAM))
{
  
  Wheressb<-(is.na(RAM$total) & RAM$ssb[i]>1) # where no biomass but ssb 
  Whereb<-((RAM$total>0) & (is.na(RAM$total)==F))  # where biomass
  Nowhere<-(is.na(RAM$total) & is.na(RAM$ssb)) # where neither biomass or ssb
  
  if (sum(is.na(RAM$total) & RAM$ssb[i]>1)>1)
  {
    RAM$Biomass[Wheressb]<-RAM[is.na(RAM$total) & RAM$ssb[i]>1,3]
    RAM$BiomassMetric[Wheressb]<-"SSB"
    RAM$BiomassUnit[Wheressb]<-RAM[is.na(RAM$total) & RAM$ssb[i]>0,7]
  }
  if (sum((RAM$total[i]>0) & (is.na(RAM$total)==F))>0) 
  {
    RAM$Biomass[Whereb]<-RAM[(RAM$total[i]>0) & (is.na(RAM$total)==F),4]
    RAM$BiomassMetric[Whereb]<-"B"
    RAM$BiomassUnit[Whereb]<-RAM[(RAM$total[i]>0) & (is.na(RAM$total)==F),8]
  }
  if (sum(is.na(RAM$total[i]) & is.na(RAM$ssb[i]))>1) # portion of loop currently not working properly. no errors but not effective
  {
    RAM$Biomass[Nowhere]<-RAM[is.na(RAM$total)[i] & is.na(RAM$ssb)[i],4]
    RAM$BiomassMetric[Nowhere]<-"NA"
    RAM$BiomassUnit[Nowhere]<-"NA"
  } 
}

####### Life History ####### 
# run for loop to match life history parameters by assessid/IdOrig to RAM_BioParams .csv
# Von Bert K value and unit
RamNames<- unique(RAM$IdOrig)

for (i in 1:length(RamNames)) 
{
  
  Where<- RAM$IdOrig==RamNames[i]
  
  if (sum(RAM_BioParams$assessid==RamNames[i] & grepl("VB-k",RAM_BioParams$bioid))>0)
  {
    show(sum(RAM_BioParams$assessid==RamNames[i] & grepl("VB-k",RAM_BioParams$bioid)))
    
    RAM$VonBertK[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i] & grepl("VB-k",RAM_BioParams$bioid),24]
    RAM$VonBertKUnit[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i]& grepl("VB-k",RAM_BioParams$bioid),23]
    RAM$VonBertKSource[Where]="RAM"
  }
}

# Max Lenght and Units

for (i in 1:length(RamNames)) 
{
  
  Where<- RAM$IdOrig==RamNames[i]
  
  if (sum(RAM_BioParams$assessid==RamNames[i] & grepl("MAX-LEN",RAM_BioParams$bioid))>0)
  {
    show(sum(RAM_BioParams$assessid==RamNames[i] & grepl("MAX-LEN",RAM_BioParams$bioid)))
    
    RAM$MaxLength[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i] & grepl("MAX-LEN",RAM_BioParams$bioid),24]
    RAM$MaxLengthUnit[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i]& grepl("MAX-LEN",RAM_BioParams$bioid),23]
    RAM$MaxLengthSource[Where]="RAM"
  }
}

# Age at 50% maturity

for (i in 1:length(RamNames)) ## multiple matches for Age at Mat for some studies, pick one?
{
  
  Where<- RAM$IdOrig==RamNames[i]
  
  if (sum(RAM_BioParams$assessid==RamNames[i] & grepl("A50-yr",RAM_BioParams$bioid))>0)
  {
    show(sum(RAM_BioParams$assessid==RamNames[i] & grepl("A50-yr",RAM_BioParams$bioid)))
    
    RAM$AgeMat[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i] & grepl("A50-yr",RAM_BioParams$bioid),24]
    RAM$AgeMatUnit[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i]& grepl("A50-yr",RAM_BioParams$bioid),23]
    RAM$AgeMatSource[Where]="RAM"
  }
}

####### Species and Region Codes ####### 
# read in .csvs with matched RAM assessids and FAO regions and species scientific names and ISSCAAP codes

Spec_ISSCAAP=read.csv("Data/Species_ASFIS_ISSCAAP.csv") # list of ASFIS scientific names and corressponding ISSCAAP codes 
Spec_Region_RAM=read.csv("Data/Species_Region_RAM.csv") # list of RAM Assessed IDs previously matched to species code and FAO Region

Spec_Region_RAM$Assessed.ID=as.character(levels(Spec_Region_RAM$Assessed.ID))[Spec_Region_RAM$Assessed.ID] # convert ID to character
Spec_ISSCAAP$Species_AFSIS=as.character(levels(Spec_ISSCAAP$Species_AFSIS))[Spec_ISSCAAP$Species_AFSIS] # convert ID to character

# FAO Region matching

for (i in 1:length(RamNames)) 
{
  
  Where<- RAM$IdOrig==RamNames[i]
  
  if (sum(Spec_Region_RAM$Assessed.ID==RamNames[i])>0)
  {
  RAM$RegionFAO[Where]<-Spec_Region_RAM[Spec_Region_RAM$Assessed.ID==RamNames[i],4]
}
} #Warning messages:
# 1: In RAM$RegionFAO[Where] <- Spec_Region_RAM[Spec_Region_RAM$Assessed.ID ==  ... :
# number of items to replace is not a multiple of replacement length

# create variable to check region matches against
RAMfao=aggregate(RAM$RegionFAO~RAM$IdOrig,FUN=mean)

# ISSCAAP Species Code Matching

SpecNames<-unique(RAM$SciName)

for (i in 1:length(SpecNames))
{
  Where<-RAM$SciName==SpecNames[i]
  
  if (sum(Spec_ISSCAAP$Species_AFSIS==SpecNames[i])>0)
  {
    RAM$SpeciesCat[Where]<-Spec_ISSCAAP[Spec_ISSCAAP$Species_AFSIS==SpecNames[i],2]
  }
}
  