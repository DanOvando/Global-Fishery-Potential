
# Database Build ----------------------------------------------------------

############################################################################################################

# Script first creates an example database, then formats the RAM, SOFIA, and FAO databases, and finally stacks the databases

############################################################################################################
############ CREATE DATABASE COLUMNS ############

ColNames = c("IdOrig","Year","Fmort","Catch","FmortUnit","CatchUnit","SciName","CommName",
             "Country","RegionFAO","SpeciesCat","SpeciesCatName","Bmsy","SSBmsy","Umsy","Fmsy","Id","Dbase","Biomass","BiomassMetric",
             "BiomassUnit","FmortMetric","ExploitStatus", "VonBertK","VonBertKUnit","VonBertKSource","Temp","TempUnit",
             "TempSource","MaxLength","MaxLengthUnit", "MaxLengthSource","AgeMat","AgeMatUnit","AgeMatSource")

############################################################################################################
############ RAM DATABASE ############

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
RAM$ExploitStatus=character(length=nrow(RAM))
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
RAM$SpeciesCatName=character(length=nrow(RAM))
RAM$RegionFAO=rep(0,nrow(RAM))

####### Biomass Calculation ####### 
# calculate biomass variable by either taking "total" if available and ssb otherwise. fill BiomassMetric with B or SSB and BiomassUnit w/ corresponding units

Wheressb<-(is.na(RAM$total) & is.na(RAM$ssb)==F & RAM$ssb>0) # where no biomass but ssb 
Whereb<-((RAM$total>0) & (is.na(RAM$total)==F))  # where biomass
Nowhere<-(is.na(RAM$total) & is.na(RAM$ssb)) # where neither biomass or ssb

RAM$Biomass[Wheressb]<- RAM$ssb[Wheressb]
RAM$BiomassMetric[Wheressb]<- 'SSB'
RAM$BiomassUnit[Wheressb]<- RAM[Wheressb,7]

RAM$Biomass[Whereb]<-RAM$total[Whereb]
RAM$BiomassMetric[Whereb]<-"B"
RAM$BiomassUnit[Whereb]<-RAM[Whereb,8]

RAM$Biomass[Nowhere]<-"NA"
RAM$BiomassMetric[Nowhere]<-"NA"
RAM$BiomassUnit[Nowhere]<-"NA"


####### Life History ####### 
# run for loop to match life history parameters by assessid/IdOrig to RAM_BioParams .csv
# Von Bert K value and unit
RamNames<- unique(RAM$IdOrig)

for (i in 1:length(RamNames)) 
{
  
  Where<- RAM$IdOrig==RamNames[i]
  
  if (sum(RAM_BioParams$assessid==RamNames[i] & grepl("VB-k",RAM_BioParams$bioid))>0)
  {
    
    
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
    
    
    RAM$AgeMat[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i] & grepl("A50-yr",RAM_BioParams$bioid),24]
    RAM$AgeMatUnit[Where]=RAM_BioParams[RAM_BioParams$assessid==RamNames[i]& grepl("A50-yr",RAM_BioParams$bioid),23]
    RAM$AgeMatSource[Where]="RAM"
  }
}

# clean up life history values that are non-numeric, convert variables to numeric
# Von bert
RAM$VonBertK=gsub("SEE TABLE 5","",RAM$VonBertK)
RAM$VonBertK=gsub("varies by region","",RAM$VonBertK) # remove "varies by region"
RAM$VonBertK=gsub("SEE TABLE 5","",RAM$VonBertK) # remove "SEE TABLE 5"
RAM$VonBertK=gsub("8.30E-02",".083",RAM$VonBertK) # convert "8.30E-02" from scientific notation
RAM$VonBertK=gsub("0.232/0.161","0.161", RAM$VonBertK) # original value indicates "male/female", select only female entry
RAM$VonBertK=gsub("0.261/0.213","0.213", RAM$VonBertK) # original value indicates "male/female", select only female entry

# Max Length
RAM$MaxLength=gsub("26+","26",RAM$MaxLength) # remove + from end of 26
RAM$MaxLength=gsub("available","",RAM$MaxLength) # remove "available", RAM entry says "varies greatly"


####### Species and Region Codes ####### 
# read in .csvs with matched RAM assessids and FAO regions and species scientific names and ISSCAAP codes

Spec_ISSCAAP=read.csv("Data/Species_ASFIS_ISSCAAP.csv") # list of ASFIS scientific names and corressponding ISSCAAP codes 
Spec_Region_RAM=read.csv("Data/Species_Region_RAM.csv") # list of RAM Assessed IDs previously matched to species code and FAO Region

Spec_Region_RAM$Assessed.ID=as.character(levels(Spec_Region_RAM$Assessed.ID))[Spec_Region_RAM$Assessed.ID] # convert ID to character
Spec_ISSCAAP$Species_AFSIS=as.character(levels(Spec_ISSCAAP$Species_AFSIS))[Spec_ISSCAAP$Species_AFSIS] # convert ID to character

# FAO Region matching
RegionFAOMatches=unique(Spec_Region_RAM$Assessed.ID)

for (i in 1:length(RamNames)) 
{
  
  Where1<- RamNames[i]==Spec_Region_RAM$Assessed.ID

  Where2<- RamNames[i]==RAM$IdOrig
  
  
  if (sum(Spec_Region_RAM$Assessed.ID==RamNames[i])>0)
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

# convert Life history columns to numeric *** TEMPORARY - Need to go back and adjust values using references
RAM$VonBertK=as.numeric(levels(RAM$VonBertK))[RAM$VonBertK]
RAM$MaxLength=as.numeric(levels(RAM$MaxLength))[RAM$MaxLength]
RAM$AgeMat=as.numeric(levels(RAM$AgeMat))[RAM$AgeMat]


# delete unneeded columns
RAM<-subset(RAM, select=c(ColNames))
            
############################################################################################################
############ SOFIA DATABASE ############

SOFIA=read.csv("Data/SOFIA_2011_indiv_stocks_CLEAN.csv", stringsAsFactors=F)

# rename columns to match main database columns as defined by ColNames
names(SOFIA)[1]="RegionFAO"
SOFIA$RegionFAO=gsub("Pacific",1,SOFIA$RegionFAO) # change "Pacific" to 1
SOFIA$RegionFAO=gsub("Atlantic",2,SOFIA$RegionFAO) # change "Atlantic" to 2
SOFIA$RegionFAO=gsub("Indian",3,SOFIA$RegionFAO) # change "Indian" to 3

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
SOFIA$Bmsy=rep(0,nrow(SOFIA))
SOFIA$SSBmsy=rep(0,nrow(SOFIA))
SOFIA$Fmsy=rep(0,nrow(SOFIA))
SOFIA$Umsy=rep(0,nrow(SOFIA))
SOFIA$VonBertK=rep(0,nrow(SOFIA))
SOFIA$VonBertKSource=character(length=nrow(SOFIA))
SOFIA$Temp=rep(0,nrow(SOFIA))
SOFIA$TempSource=character(length=nrow(SOFIA))
SOFIA$MaxLength=rep(0,nrow(SOFIA))
SOFIA$MaxLengthSource=character(length=nrow(SOFIA))
SOFIA$AgeMat=rep(0,nrow(SOFIA))
SOFIA$AgeMatSource=character(length=nrow(SOFIA))
SOFIA$AgeMatUnit=character(length=nrow(SOFIA))
SOFIA$VonBertKUnit=character(length=nrow(SOFIA))
SOFIA$TempUnit=character(length=nrow(SOFIA))
SOFIA$MaxLengthUnit=character(length=nrow(SOFIA))

# *** IdOrig, Catch, and Year to be created next for specific reasons***

# create assessment-specfic ID code for SOFIA by combining Dbase, FAO Region, Species Code, and a Sequential Column
sofiaID=seq(from=1, to=nrow(SOFIA))
SOFIA$IdOrig=paste(sofiaID,SOFIA$Dbase,SOFIA$RegionFAO,SOFIA$SpeciesCat,sep="-") # fill with zeroes, will apply for loop to create unique ID  

# transpose SOFIA dataset to vertical entries where each row represents a years catch, then add year variable

# reshape SOFIA to "long" format while creating "Catch" and "Year" variables
sofia=reshape(SOFIA,varying=8:17,direction="long", v.names="Catch", timevar="Year",times=2000:2009,)
sofia=subset(sofia, select=c(ColNames)) # subset to remove added column names
sofia=sofia[order(sofia$IdOrig,sofia$Year),] # sort to make catch records sequential

############################################################################################################
############ FAO DATABASE ############

FAO=read.csv("Data/FAO_Capture_1950to2011.csv",header=T,stringsAsFactors=F,na.strings=c("...","-","0 0")) # convert ... and - to NA in catch record
FAO[,10:71]=apply(FAO[,10:71],2,function(y) as.numeric(gsub(" F","",y))) # remove " F" from certain data points and convert catch record to numeric


# rename Country, FAO area, Measure, ASFIS, ASFIS 1, ISSCAAP group 1
names(FAO)[1]="Country"
names(FAO)[8]="RegionFAO"
names(FAO)[9]="CatchUnit"
names(FAO)[6]="SpeciesCat"
names(FAO)[2]="CommName"
names(FAO)[4]="SciName"
names(FAO)[5]="SpeciesCatName"

# remove columns for NEI, Region, Max, CUmSum, 2000-2011 average, Has 2011 landings, ISSCAAP group desc, FAO area desc
FAO$Nei.=NULL
FAO$Max=NULL
FAO$CUmSum=NULL
FAO$Has.2011.Landings=NULL
FAO$X2000.2011..Average=NULL
FAO$Fishing.area..FAO.major.fishing.area.=NULL

# add missing columns as per ColNames
FAO$Id=rep(0,nrow(FAO)) # fill in once full database is compiled 
FAO$Dbase=rep("FAO",nrow(FAO))
FAO$Biomass=rep(0,nrow(FAO))
FAO$BiomassMetric=character(length=nrow(FAO))
FAO$BiomassUnit=character(length=nrow(FAO))
FAO$Fmort=rep(0,nrow(FAO))
FAO$FmortMetric=character(length=nrow(FAO))
FAO$FmortUnit=character(length=nrow(FAO))
FAO$Bmsy=rep(0,nrow(FAO))
FAO$SSBmsy=rep(0,nrow(FAO))
FAO$Fmsy=rep(0,nrow(FAO))
FAO$Umsy=rep(0,nrow(FAO))
FAO$VonBertK=rep(0,nrow(FAO))
FAO$VonBertKUnit=character(length=nrow(FAO))
FAO$VonBertKSource=character(length=nrow(FAO))
FAO$Temp=rep(0,nrow(FAO))
FAO$TempSource=character(length=nrow(FAO))
FAO$MaxLength=rep(0,nrow(FAO))
FAO$MaxLengthSource=character(length=nrow(FAO))
FAO$AgeMat=rep(0,nrow(FAO))
FAO$AgeMatSource=character(length=nrow(FAO))
FAO$AgeMatUnit=character(length=nrow(FAO))
FAO$ExploitStatus=character(length=nrow(FAO))
FAO$VonBertKUnit=character(length=nrow(FAO))
FAO$TempUnit=character(length=nrow(FAO))
FAO$MaxLengthUnit=character(length=nrow(FAO))

# create IdOrig for FAO entries. Use same syntax as for SOFIA = 
FAOID=seq(from=1, to=nrow(FAO))
FAO$IdOrig=paste(FAOID,FAO$Dbase,FAO$RegionFAO,FAO$SpeciesCat,sep="-") # fill with zeroes, will apply for loop to create unique ID  

# transpose dataset to "long" format and add variable for Year and Catch
fao=reshape(FAO,varying=8:69,direction="long", v.names="Catch", timevar="Year",times=1950:2011,)

# reorder columns 
fao=subset(fao, select=c(ColNames)) # subset to remove added column names
fao=fao[,c(ColNames)]

# sort to make catch records sequential
fao=fao[order(fao$IdOrig,fao$Year),] # *** currently ordering based only on the first digit of the IdOrig

############################################################################################################
############ BIND COMPLETE DATABASE ############

# RAM and fao
fulldata=rbind(RAM,fao)
write.csv(file=paste(ResultFolder,"fulldata.csv",sep=""),fulldata)
