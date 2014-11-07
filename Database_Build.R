
# Database Build ----------------------------------------------------------

##############################################
####
#### GLOBAL FISHERY POTENTIAL DATA PROCESSING
#### DATA RECEIVED ON: 
####      RAM: 10/22/14
####      SOFIA: 
####      FAO: 
####
#### BY: TYLER CLAVELLE
#### 
#### Script loads, formats, and compiles the RAM, SOFIA, and FAO databases

############################################################################################################

# load("Data/DBdata.RData") # Load R data pack

load("Data/DBdata_102214.RData") # RData object obtained 10/22/14

############################################################################################################
############ RAM DATABASE ############

# build dataset using "timeseries.views.data as base dataset

ColNames = c("Id","IdOrig","Dbase", "Year","Catch","CatchUnit", "Fmort","FmortUnit","FmortMetric", "SciName","CommName",
             "Country","RegionFAO","SpeciesCat","SpeciesCatName","MSY", "Bmsy","SSBmsy","Umsy","Fmsy","Biomass","BiomassMetric",
             "BiomassUnit","ExploitStatus", "VonBertK","VonBertKUnit","VonBertKSource","Temp","TempUnit",
             "TempSource","MaxLength","MaxLengthUnit", "MaxLengthSource","AgeMat","AgeMatUnit","AgeMatSource",'ReferenceBiomass','ReferenceBiomassUnits',"BvBmsy",'UvUmsytouse')

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
# BvBmsy -> BvBmsyDrop # dropping this variable in favor of BvBmsytouse
# BvBmsytouse -> BvBmsy

timeseries.views.units<-timeseries.views.units[,1:11] # remove newly added 'touse' parameter source columns 10/22/14

colnames(timeseries.views.units)<-c("IdOrig","stockid","stocklong","TB", "SSB", "TN", "R","TC", "TL", "F", "ER")

# For datasets missing variable names, convert to data frame, name variables, and convert to character/numeric as neccessary

bioparams.views.units<-bioparams.views.units[,1:12] # remove newly added 'touse' parameter source colums 10/22/14
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
bioparams.views.data$MSY<-as.numeric(levels(bioparams.views.data$MSY))[bioparams.views.data$MSY]

# subset out unneeded variables and convert to data frame

RAM<-as.data.frame(timeseries.views.data)
UvUmsytouse<- pmax(0,as.numeric(levels(RAM$UvUmsytouse))[RAM$UvUmsytouse])

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
RAM$MSY<-NA
RAM$UvUmsytouse=as.numeric(rep("",nrow(RAM)))
RAM$UvUmsytouse[is.na(UvUmsytouse)==F]<-  UvUmsytouse[is.na(UvUmsytouse)==F]

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
  
  RAM$Bmsy[Whereref]<-bioparams.views.data[lh_match,13] #Bmsy - taken from Bmsytouse 
  RAM$Umsy[Whereref]<-bioparams.views.data[lh_match,14] #Umsy - taken from Umsytouse
  RAM$Fmsy[Whereref]<-bioparams.views.data[lh_match,8] #Fmsy
  RAM$SSBmsy[Whereref]<-bioparams.views.data[lh_match,5] #SSBmsy
  RAM$MSY[Whereref]<-bioparams.views.data[lh_match,7] # MSY
  # filling Bmsy with Bmsytouse, which was renamed to ReferenceBiomass. Will be duplicated in ReferenceBiomass column
}

# Add reference biomass
RAM$ReferenceBiomass<-RAM$Bmsy # This will be the biomass reference point to use, is a duplicate of the Bmsy column (see above)

RAM$ReferenceBiomassUnits<-NA

WhereRefB<-RAM$ReferenceBiomass==RAM$Bmsy

RAM$ReferenceBiomassUnits[WhereRefB]<-"Bmsy"


####### Species, Country, and Region Codes ####### 
# read in .csvs with matched RAM assessids and FAO regions and species scientific names and ISSCAAP codes

Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes 
Spec_Region_RAM=read.csv("Data/Ram_Regions_102814.csv",stringsAsFactors=F) # list of RAM Assessed IDs previously matched to species code and FAO Region
Spec_Region_RAM$RegionFAO<- gsub("/",",",Spec_Region_RAM$RegionFAO,fixed=T) # change / to , for use in string parsing during filtering function

# Spec_Region_RAM$assessid=as.character(levels(Spec_Region_RAM$assessid))[Spec_Region_RAM$assessid] # convert ID to character
# Spec_Region_RAM$areaname<-as.character(levels(Spec_Region_RAM$areaname))[Spec_Region_RAM$areaname]


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

RAM$Country<- gsub("multinational","Multinational",RAM$Country)
Spec_Region_RAM$Country<- gsub("multinational","Multinational",Spec_Region_RAM$Country)
Spec_Region_RAM$Country<- gsub("United States of America","USA",Spec_Region_RAM$Country)

# FAO Region matching

# make data frame with FAO regions for multinational stock possibilities

regionname<-c( "Atlantic Ocean",                    "Central Western Pacific Ocean",     "Eastern Atlantic",                 
               "Eastern Pacific",                   "North Pacific Ocean",               "Northern Atlantic",                
               "Pacific Ocean",                     "South Atlantic",                    "South Pacific Ocean",              
               "Western and Central North Pacific", "Western Atlantic",                  "Western Pacific Ocean", "Indian Ocean") 

regs<-c("21,27,31,34,41,47,48","71","27,34,47","67,77,87","61,67","21,27","61,67,71,77,81,87,88","41,47","81,87","61,67,71,77","21,31,41","61,71,81","57,58")

multiFAOdf<-data.frame(regionname,regs,stringsAsFactors=F)

# run for loop to identify multinational stocks with NAs and fill fao regions based on multiFAOdf

for (i in 1:length(Spec_Region_RAM$assessid)){
  
  if(Spec_Region_RAM$Country[i]=="Multinational" & is.na(Spec_Region_RAM$RegionFAO[i])==T){
    
    regmatch<-match(Spec_Region_RAM$areaname[i],multiFAOdf$regionname)
    
    Spec_Region_RAM$RegionFAO[i]<-multiFAOdf$regs[regmatch]
  }
}

RegionFAOMatches=unique(Spec_Region_RAM$assessid)

for (i in 1:length(RAMNames)) # currently only matching 11 of the 17 unique FAO regions in the Spec_Region.csv. Due to stocks in multiple zones? 
{
  
  Where1<- RAMNames[i]==Spec_Region_RAM$assessid
  
  Where2<- RAMNames[i]==RAM$IdOrig
  
  
  if (sum(Spec_Region_RAM$assessid==RAMNames[i])>0)
  {
    RAM$RegionFAO[Where2]<- Spec_Region_RAM$RegionFAO[Where1]
  }
}

# ISSCAAP Species Code Matching

GroupNames_ISSCAAP<-read.csv("Data/ISSCAAP Codes.csv",stringsAsFactors=F)
SpecNames<-unique(RAM$SciName)


for (i in 1:length(SpecNames)) # match species name to group code
{
  Where<-RAM$SciName==SpecNames[i]
  
  if (sum(Spec_ISSCAAP$Species_AFSIS==SpecNames[i],na.rm=T)>0)
  {
    RAM$SpeciesCat[Where]<-Spec_ISSCAAP$SpeciesCat_ISSCAAP_code[Spec_ISSCAAP$Species_AFSIS==SpecNames[i]]
  }
}

RAM$SpeciesCat[RAM$SciName=='Litopenaeus setiferus']<-45 # add SpeciesCat to missing RAM stock

GroupNums<-unique(na.omit(RAM$SpeciesCat))

for (i in 1:length(GroupNums)) # match group code to group name
{
  Where<-((RAM$SpeciesCat==GroupNums[i]))
  
  if (sum(GroupNames_ISSCAAP$ISSCAAP.code==GroupNums[i])>0)
  {
    RAM$SpeciesCatName[Where]<-GroupNames_ISSCAAP$Definition[GroupNames_ISSCAAP$ISSCAAP.code==GroupNums[i]]
  }
}

# RAM quality control and data checks

RAM$Country[RAM$Country=="Russia"]<-"Russian Federation"

MetaStocks<-unique(metadata$assessid) # unique stocks in metadata 446
RamStocks<-unique(RAM$IdOrig) # unique stocks in RAM 438

missing<-!(MetaStocks %in% RamStocks) # identify which stocks are missing

MissingStocks<-MetaStocks[missing]

MissingStocks %in% timeseries.views.data[,1] # check if missing stocks are in timeseries.views.data. They are not

MissingStocksData<-metadata[(metadata$assessid %in% MissingStocks),] # look at data for missing stocks

# write.csv(MissingStocksData,file='Stocks Missing From Ram TimeSeries Data.csv')

# Subset RAM to only include stocks with all 3 reference values (consider making option in Master)
ThreeRefs<-read.csv('Data/RamStocks_All_Ref_Values.csv',stringsAsFactors=F,col.names=c("IdOrig"))

RAM<-RAM[(RAM$IdOrig %in% ThreeRefs$IdOrig),]

############################################################################################################
############ SOFIA DATABASE ############

SOFIA=read.csv("Data/SOFIA_2011_indiv_stocks_CLEAN.csv", stringsAsFactors=F)

# rename columns to match main database columns as defined by ColNames
names(SOFIA)[1]="RegionFAO"

####### ****DELETE THIS RENAMING SECTION - THESE NUMBERS EXIST FOR INLAND WATERS****
# SOFIA$RegionFAO=gsub("Pacific",1,SOFIA$RegionFAO) # change "Pacific" to 1
# SOFIA$RegionFAO=gsub("Atlantic",2,SOFIA$RegionFAO) # change "Atlantic" to 2
# SOFIA$RegionFAO=gsub("Indian",3,SOFIA$RegionFAO) # change "Indian" to 3

names(SOFIA)[7]="CommName"
names(SOFIA)[12]="SciName"
names(SOFIA)[13]="Country"
names(SOFIA)[7]="ExploitStatus"
names(SOFIA)[4]="SpeciesCat"
names(SOFIA)[5]="CommName"
names(SOFIA)[2]="SpeciesCatName"

# Create "trim" function to trim trailing and leading spaces from data name columns
trim.lead.trail=function (x) gsub("^\\s+|\\s+$","",x)
SOFIA$CommName=trim.lead.trail(SOFIA$CommName) # reduces unique entries to 335
SOFIA$SciName=trim.lead.trail(SOFIA$SciName) # reduced to 276

# sub out "/n" and double spaces
SOFIA$CommName=gsub("\n"," ",SOFIA$CommName) # \n - 321
SOFIA$CommName=gsub("\x90s", "",SOFIA$CommName)

SOFIA$SciName=gsub("\n"," ",SOFIA$SciName) # down to 274

SOFIA$CommName=gsub("  "," ",SOFIA$CommName) # double spaces - 303
SOFIA$SciName=gsub("  "," ",SOFIA$SciName) # - 267 entries

# remove "*" from X1950s variable and convert to numeric
SOFIA$X1950s=gsub("*","",SOFIA$X1950s)
SOFIA$X1950s=as.numeric(levels(SOFIA$X1950s))[SOFIA$X1950s]

# remove unwanted columns
SOFIA$Assessed=NULL
SOFIA$Gt.10k=NULL
SOFIA$Avg.Catch=NULL
SOFIA$Uncertainty=NULL
SOFIA$My..Class=NULL
SOFIA$FAO.Region.1=NULL
SOFIA$state.of.exploitation=NULL
SOFIA$Avg.Catch.1=NULL
SOFIA$Size=NULL
SOFIA$Short.Status=NULL
SOFIA$X1950s=NULL
SOFIA$X1960s=NULL
SOFIA$X1970s=NULL
SOFIA$X1980s=NULL
SOFIA$X1990s=NULL
SOFIA$Region=NULL

# add columns for missing variables
SOFIA$Id=rep(0,nrow(SOFIA)) # fill in once full database is compiled 
SOFIA$Dbase=rep("SOFIA",nrow(SOFIA))
SOFIA$CatchUnit=character(length=nrow(SOFIA))
SOFIA$Biomass=rep(0,nrow(SOFIA))
SOFIA$BiomassMetric=character(length=nrow(SOFIA))
SOFIA$BiomassUnit=character(length=nrow(SOFIA))
SOFIA$Fmort=rep(0,nrow(SOFIA))
SOFIA$FmortMetric=character(length=nrow(SOFIA))
SOFIA$FmortUnit=character(length=nrow(SOFIA))
SOFIA$Bmsy=as.numeric(rep("",nrow(SOFIA)))
SOFIA$SSBmsy=as.numeric(rep("",nrow(SOFIA)))
SOFIA$Fmsy=as.numeric(rep("",nrow(SOFIA)))
SOFIA$Umsy=as.numeric(rep("",nrow(SOFIA)))
SOFIA$VonBertK=as.numeric(rep("",nrow(SOFIA)))
SOFIA$VonBertKSource=character(length=nrow(SOFIA))
SOFIA$Temp=as.numeric(rep("",nrow(SOFIA)))
SOFIA$TempSource=character(length=nrow(SOFIA))
SOFIA$MaxLength=as.numeric(rep("",nrow(SOFIA)))
SOFIA$MaxLengthSource=character(length=nrow(SOFIA))
SOFIA$AgeMat=as.numeric(rep("",nrow(SOFIA)))
SOFIA$AgeMatSource=character(length=nrow(SOFIA))
SOFIA$AgeMatUnit=character(length=nrow(SOFIA))
SOFIA$VonBertKUnit=character(length=nrow(SOFIA))
SOFIA$TempUnit=character(length=nrow(SOFIA))
SOFIA$MaxLengthUnit=character(length=nrow(SOFIA))
SOFIA$ReferenceBiomass<- NA
SOFIA$ReferenceBiomassUnits<- NA
SOFIA$BvBmsy<-NA
SOFIA$MSY<-NA
SOFIA$UvUmsytouse=as.numeric(rep("",nrow(SOFIA)))

# Populate BvBmsy by converting Exploit Status values into numbers, taking the mean of the range of U, F, and O
underexploit<-SOFIA$ExploitStatus=="U"
fullyexploit<-SOFIA$ExploitStatus=="F" # identify which assessments are U,F,O
overexploit<-SOFIA$ExploitStatus=="O"

SOFIA$BvBmsy[underexploit]<-1.6
SOFIA$BvBmsy[fullyexploit]<-1
SOFIA$BvBmsy[overexploit]<-0.4

# *** IdOrig, Catch, and Year to be created next for specific reasons***

# create assessment-specfic ID code for SOFIA by combining Dbase, FAO Region, Species Code, and a Sequential Column
sofiaID=seq(from=1, to=nrow(SOFIA))
SOFIA$IdOrig=paste(sofiaID,SOFIA$Dbase,SOFIA$RegionFAO,SOFIA$SpeciesCat,sep="-") # fill with zeroes, will apply for loop to create unique ID  

# transpose SOFIA dataset to vertical entries where each row represents a years catch, then add year variable

# reshape SOFIA to "long" format while creating "Catch" and "Year" variables
sofia=reshape(SOFIA,varying=8:17,direction="long", v.names="Catch", timevar="Year",times=2000:2009,)

# convert catch from thousands of metric tons to metric tons
sofia$Catch<-sofia$Catch*1000

sofia=subset(sofia, select=c(ColNames)) # subset to remove added column names
sofia=sofia[order(sofia$IdOrig,sofia$Year),] # sort to make catch records sequential
sofia=sofia[,c(ColNames)] # order columns according to ColNames

# apply FAO region possibilities from multiFAOdf for Pacific, Atlantic, and Indian SOFIA stocks
sofia$RegionFAO[sofia$RegionFAO=="Pacific"]<-multiFAOdf[7,2]
sofia$RegionFAO[sofia$RegionFAO=="Pacific "]<-multiFAOdf[7,2]
sofia$RegionFAO[sofia$RegionFAO=="Atlantic"]<-multiFAOdf[1,2]
sofia$RegionFAO[sofia$RegionFAO=="Indian"]<-multiFAOdf[13,2]

# clean up country names
sofia$Country<-gsub("\n"," ",sofia$Country) # sub out "/n"
sofia$Country<-gsub("  "," ",sofia$Country) # change double spaces to single spaces
sofia$Country<-gsub("\\(.*\\)","",sofia$Country) # delete anything within parentheses
sofia$Country<- gsub("^\\s+|\\s+$","",sofia$Country) # trim leading and trailing space

sofia$Country<-gsub("United States of America","USA",sofia$Country) # convert to USA
sofia$Country<-gsub("C\xc8te d\x90Ivoire","Ivory Coast",sofia$Country)
sofia$Country<-gsub("Democratic. People\xfc\xbe\x8c\x83\xa4\xbcs Republic of Korea","Democratic People's Republic of Korea",sofia$Country)
sofia$Country<-gsub("Democratic People\xfc\xbe\x8c\x83\xa4\xbcs Republic of Korea","Democratic People's Republic of Korea",sofia$Country)
sofia$Country<-gsub("Saint Vincent/ Grenadines","Saint Vincent/Grenadines",sofia$Country)
sofia$Country<-gsub("the Democratic Republic of the Congo","Democratic Republic of the Congo",sofia$Country)
sofia$Country<-gsub("Falkland Islands","Falkland Is.",sofia$Country)
sofia$Country<-gsub("China, Hong Kong SAR","China Hong Kong SAR",sofia$Country)
sofia$Country<-gsub("China, Macao SAR","China Macao SAR",sofia$Country)
sofia$Country<-gsub("Other NEI","Other nei",sofia$Country)

# Sci Name fixes

sofia$SciName<-gsub("spp.",'spp',sofia$SciName) # make genus level names same as fao syntax
sofia$SciName<-gsub('Scomberjaponicus','Scomber japonicus',sofia$SciName)
sofia$SciName<-gsub('Ruditapes','Ruditapes spp',sofia$SciName)
sofia$SciName<-gsub('Nephrops norvegicus Palinurus spp','Nephrops norvegicus',sofia$SciName)
sofia$SciName<-gsub('Merluccius capensis, M. paradox.','Merluccius capensis, M.paradoxus',sofia$SciName)
sofia$SciName<-gsub("Haemulidae (= Pomadasyidae)","Haemulidae (=Pomadasyidae)",sofia$SciName,fixed=T)

############################################################################################################
############ FAO DATABASE ############

FAO=read.csv("Data/faotest.csv",header=T,stringsAsFactors=F,na.strings=c("...","-","0 0")) # convert ... and - to NA in catch record
FAO[,10:72]=apply(FAO[,10:72],2,function(y) as.numeric(gsub(" F","",y))) # remove " F" from certain data points and convert catch record to numeric


# rename Country, FAO area, Measure, ASFIS, ASFIS 1, ISSCAAP group 1
names(FAO)[1]="Country"
names(FAO)[2]="CommName"
names(FAO)[3]="SciName"
names(FAO)[5]="SpeciesCat"
names(FAO)[6]="SpeciesCatName"
names(FAO)[8]="RegionFAO"
names(FAO)[9]="CatchUnit"


# remove columns for NEI, Region, Max, CUmSum, 2000-2011 average, Has 2011 landings, ISSCAAP group desc, FAO area desc
FAO$Nei.=NULL
FAO$Max=NULL
FAO$CUmSum=NULL
FAO$Has.2011.Landings=NULL
FAO$X2000.2011..Average=NULL
FAO$Fishing.area..FAO.major.fishing.area.=NULL
FAO$Species..ASFIS.species..2=NULL # delete taxon code column

# add missing columns as per ColNames
FAO$Id=rep(0,nrow(FAO)) # fill in once full database is compiled 
FAO$Dbase=rep("FAO",nrow(FAO))
FAO$Biomass=rep(0,nrow(FAO))
FAO$BiomassMetric=character(length=nrow(FAO))
FAO$BiomassUnit=character(length=nrow(FAO))
FAO$Fmort=rep(0,nrow(FAO))
FAO$FmortMetric=character(length=nrow(FAO))
FAO$FmortUnit=character(length=nrow(FAO))
FAO$Bmsy=as.numeric(rep("",nrow(FAO)))
FAO$SSBmsy=as.numeric(rep("",nrow(FAO)))
FAO$Fmsy=as.numeric(rep("",nrow(FAO)))
FAO$Umsy=as.numeric(rep("",nrow(FAO)))
FAO$VonBertK=as.numeric(rep("",nrow(FAO)))
FAO$VonBertKUnit=character(length=nrow(FAO))
FAO$VonBertKSource=character(length=nrow(FAO))
FAO$Temp=as.numeric(rep("",nrow(FAO)))
FAO$TempSource=character(length=nrow(FAO))
FAO$MaxLength=as.numeric(rep("",nrow(FAO)))
FAO$MaxLengthSource=character(length=nrow(FAO))
FAO$AgeMat=as.numeric(rep("",nrow(FAO)))
FAO$AgeMatSource=character(length=nrow(FAO))
FAO$AgeMatUnit=character(length=nrow(FAO))
FAO$ExploitStatus=character(length=nrow(FAO))
FAO$VonBertKUnit=character(length=nrow(FAO))
FAO$TempUnit=character(length=nrow(FAO))
FAO$MaxLengthUnit=character(length=nrow(FAO))
FAO$ReferenceBiomass<- NA
FAO$ReferenceBiomassUnits<- NA
FAO$BvBmsy<-NA
FAO$MSY<-NA
FAO$UvUmsytouse=as.numeric(rep("",nrow(FAO)))

# create IdOrig for FAO entries. Use same syntax as for SOFIA = 
FAOID=seq(from=1, to=nrow(FAO))
FAO$IdOrig=paste(FAOID,FAO$Dbase,FAO$RegionFAO,FAO$SpeciesCat,sep="-") # fill with zeroes, will apply for loop to create unique ID  

# transpose dataset to "long" format and add variable for Year and Catch
fao=reshape(FAO,varying=8:70,direction="long", v.names="Catch", timevar="Year",times=1950:2012,)

# reorder columns 
fao=subset(fao, select=c(ColNames)) # subset to remove added column names
fao=fao[,c(ColNames)]

# sort to make catch records sequential
fao=fao[order(fao$IdOrig,fao$Year),] # *** currently ordering based only on the first digit of the IdOrig

# general fixes to FAO

fao$Country<-gsub("\\(.*\\)","",fao$Country) # delete anything within parentheses
fao$Country<- gsub("^\\s+|\\s+$","",fao$Country) # trim leading and trailing space
fao$Country<-gsub("  "," ",fao$Country) # change double spaces to single spaces

fao$Country[fao$Country=="United States of America"]<-"USA"
fao$Country<-gsub("R\x8eunion","Reunion",fao$Country)
fao$Country<-gsub("Cura\x8dao","Curacao",fao$Country)
fao$Country<-gsub("C\x99te d'Ivoire","Ivory Coast",fao$Country)
fao$Country<-gsub("Saint Barth\x8elemy","Saint Barthelemy",fao$Country)
fao$Country<-gsub("Korea, Dem. People's Rep", "Democratic People's Republic of Korea",fao$Country)
fao$Country<-gsub("Korea, Republic of", "Republic of Korea",fao$Country,)
fao$Country<-gsub("China, Hong Kong SAR", "China Hong Kong SAR",fao$Country)
fao$Country<-gsub("China, Macao SAR", "China Macao SAR",fao$Country)
fao$Country<-gsub("Congo, Dem. Rep. of the", "Democratic Republic of the Congo",fao$Country)
fao$Country<-gsub("Congo, Republic of", "Congo",fao$Country)
fao$Country<-gsub("Tanzania, United Rep. of","United Republic of Tanzania", fao$Country)

fao$Country<-gsub(",.*$","",fao$Country) # removes everything after a comma, this will shorten names of all countries that only have one entry
# e.g., Fiji, Republic of becomes just Fiji. This change is done after all standardizing of countries between FAO and SOFIA

fao$SciName[fao$SciName=='']<-'Missing'

############################################################################################################
############ BIND AND CLEAN-UP COMPLETE DATABASE ############

# bind
fulldata=rbind(RAM,fao,sofia)

# clean up names/values for Country, SpeciesCatName, SciName, etc.

# SpeciesCatName
fulldata$SpeciesCatName<- gsub("^\\s+|\\s+$","",fulldata$SpeciesCatName) # trim leading and trailing space
fulldata$SpeciesCatName<-gsub("  "," ",fulldata$SpeciesCatName) # change double spaces to single spaces
fulldata$SpeciesCatName[fulldata$SpeciesCatName=="Miscellaneous costal fishes"]<-"Miscellaneous coastal fishes"
fulldata$SpeciesCatName[fulldata$SpeciesCatName=="Micellaneous pelagic fishes"]<-"Miscellaneous pelagic fishes"
fulldata$SpeciesCatName[fulldata$SpeciesCatName=="Flounders halibuts and soles"]<-"Flounders, halibuts, soles"

# convert VonBertK data points recorded in mm/T to cm/T
WhereVbkMM<-fulldata$VonBertKUnit=="VB-k-mm/T"
fulldata$VonBertK[WhereVbkMM]<-fulldata$VonBertK[WhereVbkMM]/10

# write .csv file
write.csv(file=paste(ResultFolder,"fulldata.csv",sep=""),fulldata)
