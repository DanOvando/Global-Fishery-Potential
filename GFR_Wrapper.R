
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################

# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

# Read in and process data ------------------------------------------------------------

if (file.exists(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))==F)
{

 source('Database_Build.r') #Build Tyler's database

# fulldata<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))

FullData<- fulldata

rm(fulldata)

CleanedData<- MaidService(FullData)

DroppedStocks<- CleanedData$DroppedStocks

FullData<- CleanedData$CleanedData

rm(CleanedData)

write.csv(file=paste(ResultFolder,'Raw Compiled Database.csv',sep=''),FullData)

write.csv(file=paste(ResultFolder,'Omitted Stocks.csv',sep=''),DroppedStocks)
}
if (file.exists(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))){FullData<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))}

FullData$SpeciesCatName<- as.factor( FullData$SpeciesCatName)

FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA

FullData$Biomass<- as.numeric(FullData$Biomass)

FullData$BvBmsy<- FullData$Biomass/FullData$ReferenceBiomass

ModelNames<- names(Regressions)

for (m in 1:length(ModelNames))
{
  
  
  eval(parse(text=paste('FullData$',ModelNames[m],'Marker<- FALSE',sep='')))

  eval(parse(text=paste('FullData$',ModelNames[m],'Prediction<- NA',sep='')))
  
}

# SofiaData<-  FullData[FullData$Dbase=='SOFIA',]

# FullData[FullData[,LifeHistoryVars]==0 & is.na(FullData[,LifeHistoryVars])==F ,LifeHistoryVars]<- NA
FullData$MaxLength[FullData$MaxLength==0]<- NA
FullData$AgeMat[FullData$AgeMat==0]<- NA
FullData$VonBertK[FullData$VonBertK==0]<- NA

RamData<- FullData[FullData$Dbase=='RAM',]

FaoData<- FullData[FullData$Dbase=='FAO',]

#   FaoIdSample<- sample(unique(FaoData[,IdVar]),500,replace=FALSE)
# # # 
#   FaoData<- FaoData[FaoData[,IdVar] %in% FaoIdSample,]

# Create synthetic stocks -------------------------------------------------

if (GroupMethod=='All')
{
  Groups<- unique(FullData$SpeciesCatName,na.rm=T)
  
  Groups<- Groups[is.na(Groups)==F]
}
if (GroupMethod=='Nei')
{
  Groups<- unique(FaoData$SpeciesCatName[ (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F])
}

SyntheticData<- StitchFish(RamData,IdVar,Groups,GroupSamples,Iterations) 

for (m in 1:length(ModelNames))
{
  
  eval(parse(text=paste('SyntheticData$',ModelNames[m],'Marker<- FALSE',sep='')))
  
  eval(parse(text=paste('SyntheticData$',ModelNames[m],'Prediction<- NA',sep='')))
  
}

# Prepare data for regression ---------------------------------------------

library(proftools)

#  Rprof()

RamData<- FormatForRegression(RamData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)#Add resgression data to database

SyntheticData<- FormatForRegression(SyntheticData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

if (file.exists(paste(ResultFolder,'FaoData.Rdata',sep=''))==F)
{

FaoData<- FormatForRegression(FaoData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

save(file=paste(ResultFolder,'FaoData.Rdata',sep=''),FaoData)
}
if (file.exists(paste(ResultFolder,'FaoData.Rdata',sep='')))
{
  load(paste(ResultFolder,'FaoData.Rdata',sep=''))
}
  
FaoData$MaxLength[FaoData$MaxLength==0]<- NA

FaoData$AgeMat[FaoData$AgeMat==0]<- NA

FaoData$VonBertK[FaoData$VonBertK==0]<- NA

# Rprof(NULL)
#  RProfData<- readProfileData('Rprof.out')
#  flatProfile(RProfData,byTotal=TRUE)

# Run regressions ---------------------------------------------------------

RealModels<- RunRegressions(RamData,Regressions,'Real Stocks')

RealModelFactorLevels<- NULL

Models<- names(Regressions)

# Process Regressions -----------------------------------------------------

## Determine species category levels that were used in each model run

TempOmitted<- NULL
for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))
}

RamData<- InsertFisheryPredictions(RamData,RealModels) #Add fishery predictions back into main dataframe

RealModelSdevs<- CreateSdevBins(RealModels,RamData,TransbiasBin)

NeiRegressions<- list()

NeiRegressions$M6<- Regressions$M6

NeiRegressions$M7<- Regressions$M7

NeiModels<- RunRegressions(SyntheticData,NeiRegressions,'Synthetic Stocks')

NeiModelFactorLevels<- NULL

for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  
  eval(parse(text=paste('NeiModelFactorLevels$',Model,'<- NeiModels$',Model,'$xlevels$SpeciesCatName',sep='')))
  
}

SyntheticData<- InsertFisheryPredictions(SyntheticData,NeiModels) #Add fishery predictions back into main dataframe

NeiModelSdevs<- CreateSdevBins(NeiModels,SyntheticData,TransbiasBin)

# Prepare data for regression application ---------------------------------

WhereFaoNeis<- (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F #Find unassessed NEIs

WhereFaoMarineFish<- grepl('not identified',FaoData$SpeciesCatName)

FaoSpeciesLevel<- FaoData[WhereFaoNeis==F & WhereFaoMarineFish==F ,] #Fao stocks named to the species level

FaoNeiLevel<- FaoData[WhereFaoNeis,] #fao species named to the nei or spp level

FaoMarineFishLevel<- FaoData[WhereFaoMarineFish,] #completely unidentified marine goo

TempLevel<- NULL

TempModel<- NULL

# Apply regressions -------------------------------------------------------

for (m in 1:length(Models))
{
  
TempModelName<- Models[m]

eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))

MatchingSpeciesGroups<- FaoSpeciesLevel$SpeciesCatName %in% TempLevel

eval(parse(text=paste('TempLevel<- NeiModelFactorLevels$',TempModelName,sep='')))

MatchingNeiGroups<- FaoNeiLevel$SpeciesCatName %in% TempLevel

eval(parse(text=paste('TempModel<- RealModels$',TempModelName,sep='')))

Predictions<- predict(TempModel,FaoSpeciesLevel[MatchingSpeciesGroups,])

eval(parse(text=paste('FaoSpeciesLevel$',TempModelName,'Prediction[MatchingSpeciesGroups]<-Predictions',sep='')))

eval(parse(text=paste('TempModel<- NeiModels$',TempModelName,sep='')))

Predictions<- predict(NeiModels$M6,FaoNeiLevel[MatchingNeiGroups,])

eval(parse(text=paste('FaoNeiLevel$',TempModelName,'Prediction[MatchingNeiGroups]<-Predictions',sep='')))

}

NotIdentifiedPredictions<- predict(NeiModels$M7,FaoMarineFishLevel)


# Clean and process predictions and data ---------------------------------------


FaoMarineFishLevel$M7Prediction<- NotIdentifiedPredictions

PredictedData<- rbind(RamData,FaoSpeciesLevel,FaoNeiLevel,FaoMarineFishLevel) #Bind all data back together

BiomassColumns<- grepl('BvBmsy',colnames(PredictedData)) | grepl('Prediction',colnames(PredictedData))

BioNames<- colnames(PredictedData)[BiomassColumns]

HasBiomass<- rowSums(is.na(PredictedData[,BiomassColumns]))<length(BioNames)

BiomassData<- PredictedData[HasBiomass,] #Only store fisheries that have some form of biomass estimates

AvailableBio<- (BiomassData[,BiomassColumns])

AvailableBioMarker<- matrix(rep((1:dim(AvailableBio)[2]),dim(AvailableBio)[1]), dim(AvailableBio)[1],dim(AvailableBio)[2],byrow=TRUE)

AvailableBioMarker<- AvailableBioMarker*(is.na(AvailableBio)==F)

AvailableBioMarker[AvailableBioMarker==0]<- NA

BestModel<- apply(AvailableBioMarker,1,min,na.rm=T)

BestBio<- NULL
for (b in 1:dim(AvailableBio)[1])
{
  BestBio[b]<- AvailableBio[b,BestModel[b]]
}
BestBio[BestModel==1]<- log(BestBio[BestModel==1])

BestModelnames<- c('RAM',ModelNames)

BestModelNames<- BestModelnames[unique(BestModel)]

BestModel<- as.factor((BestModel))
                    
levels(BestModel)<- BestModelNames

BiomassData$BestModel<- BestModel

BiomassData$BestBio<- BestBio

WhereNeis<- (grepl('nei',BiomassData$CommName) | grepl('spp',BiomassData$SciName)) & grepl('not identified',BiomassData$SpeciesCatName)==F #Find unassessed NEIs

WhereUnidentified<- grepl('not identified',BiomassData$SpeciesCatName)

WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level

BiomassData$IdLevel[WhereNeis]<- 'Neis'

BiomassData$IdLevel[WhereUnidentified]<- 'Unidentified'

BiomassData$IdLevel[WhereSpeciesLevel]<- 'Species'
 

# Analyze Current Status --------------------------------------------------

RawSummary<- ddply(BiomassData,c('Dbase','IdLevel','Year'),summarise,MeanBio=mean(exp(BestBio),na.rm=T),MedianBio=median(exp(BestBio),na.rm=T))

FaoSpeciesRetransform<- TransBias(subset(BiomassData,IdLevel=='Species' & Dbase=='FAO'),RealModelSdevs,BinBreak,100)

FaoMiscRetransform<- TransBias(subset(BiomassData,(IdLevel=='Neis' | IdLevel=='Unidentified') & Dbase=='FAO'),NeiModelSdevs,BinBreak,100)

FaoSpeciesDist<- FaoSpeciesRetransform$TotalDistribution

FaoMiscDist<- FaoSpeciesRetransform$TotalDistribution

CatchStatistics<- AnalyzeFisheries(BiomassData,'All Fisheries Catch','ARGH',2009)

CatchStatistics<- AnalyzeFisheries(BiomassData[BiomassData$Country=='USA',],'USA Fisheries Catch','ARGH',2000:2009)


#   Data<- subset(BiomassData,IdLevel=='Species' & Dbase=='FAO')
#   
#   SdevBins<- RealModelSdevs
#   
#   BinBreak<- TransbiasBin
#   
#   J<- 100
#   



# pdf(paste(FigureFolder,'Unassessed Species Level Test Histogram.pdf',sep=''))
# hist(exp(FaoSpeciesLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks')
# abline(v=median(exp(FaoSpeciesLevelPredictions$M6LogBvBmsy),na.rm=T))
# dev.off()
# 
# pdf(paste(FigureFolder,'Unassessed Species Level Test boxplot.pdf',sep=''))
# boxplot(exp(FaoSpeciesLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks',outline=F)
# dev.off()
# 
# pdf(paste(FigureFolder,'Unassessed Species Level Model 1 Test Histogram.pdf',sep=''))
# hist(exp(FaoSpeciesLevelPredictions$M1LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks')
# abline(v=median(exp(FaoSpeciesLevelPredictions$M1LogBvBmsy),na.rm=T))
# dev.off()
# 
# pdf(paste(FigureFolder,'Unassessed Species Level Model 1 Test boxplot.pdf',sep=''))
# boxplot(exp(FaoSpeciesLevelPredictions$M1LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks',outline=F)
# dev.off()
# 
# 
# FaoNeiLevel<- cbind(FaoNeiLevel,FaoNeiLevelPredictions)
# 
# pdf(paste(FigureFolder,'Unassessed Nei Level Test Histogram.pdf',sep=''))
# hist(exp(FaoNeiLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Nei Level Fao Stocks')
# abline(v=median(exp(FaoNeiLevelPredictions$M6LogBvBmsy),na.rm=T))
# dev.off()
# 
# pdf(paste(FigureFolder,'Unassessed Nei Level Test boxplot.pdf',sep=''))
# boxplot(exp(FaoNeiLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Nei Level Fao Stocks',outline=F)
# dev.off()

save.image(file=paste(ResultFolder,'Regression Outputs.rdata',sep=''))

file.exists(paste(ResultFolder,'Regression Outputs.rdata',sep=''))

# Create stitched database ------------------------------------------------

# Estimate MSY ------------------------------------------------------------

# Run projection analysis -------------------------------------------------

# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------


