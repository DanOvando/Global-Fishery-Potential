
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################


# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

# Read in and process data ------------------------------------------------------------

# source('Database_Build.r') #Build Tyler's database

fulldata<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))

FullData<- fulldata

rm(fulldata)

CleanedData<- MaidService(FullData)

DroppedStocks<- CleanedData$DroppedStocks

FullData<- CleanedData$CleanedData

write.csv(file=paste(ResultFolder,'Raw Compiled Database.csv',sep=''),FullData)

write.csv(file=paste(ResultFolder,'Omitted Stocks.csv',sep=''),DroppedStocks)

rm(CleanedData)

FullData$SpeciesCatName<- as.factor( FullData$SpeciesCatName)

FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA

FullData$Biomass<- as.numeric(FullData$Biomass)

FullData$BvBmsy<- FullData$Biomass/FullData$ReferenceBiomass


# Create synthetic stocks -------------------------------------------------



RamData<- FullData[FullData$Dbase=='RAM',]

FaoData<- FullData[FullData$Dbase=='FAO',]

FaoIdSample<- sample(unique(FaoData[,IdVar]),1600,replace=FALSE)

FaoData<- FaoData[FaoData[,IdVar] %in% FaoIdSample,]

if (Groups=='All')
{
  Groups<- unique(FullData$SpeciesCatName,na.rm=T)
  
  Groups<- Groups[is.na(Groups)==F]
}

SyntheticStocks<- StitchFish(RamData,IdVar,Groups,GroupSamples,Iterations)

# Prepare data for regression ---------------------------------------------

library(proftools)

#  Rprof()

RamRegressionData<- FormatForRegression(RamData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

FaoRegressionData<- FormatForRegression(FaoData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

SyntheticRegressionData<- FormatForRegression(SyntheticStocks,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

# Rprof(NULL)
#  RProfData<- readProfileData('Rprof.out')
#  flatProfile(RProfData,byTotal=TRUE)

# Run regressions ---------------------------------------------------------

RealModels<- RunRegressions(RamRegressionData,Regressions,'Real Stocks')

RealModelFactorLevels<- NULL

Models<- names(Regressions)

## Determine species category levels that were used in each model run
for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))
}


NeiModels<- RunRegressions(SyntheticRegressionData,Regressions,'Synthetic Stocks')

NeiModelFactorLevels<- NULL

for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  eval(parse(text=paste('NeiModelFactorLevels$',Model,'<- NeiModels$',Model,'$xlevels$SpeciesCatName',sep='')))
}

# Apply regressions -------------------------------------------------------

FaoNeis<- grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName) #Find unassessed NEIs

FaoSpeciesLevel<- FaoRegressionData[FaoNeis==F,] 

FaoNeiLevel<- FaoRegressionData[FaoNeis,] 

## Only grab fisheries with appropriate species categories at this point

FaoSpeciesLevelPredictions<- as.data.frame(matrix(NA,nrow=dim(FaoSpeciesLevel)[1],ncol=length(Models)))

colnames(FaoSpeciesLevelPredictions)<- paste(Models,'LogBvBmsy',sep='')

FaoNeiLevelPredictions<- as.data.frame(matrix(NA,nrow=dim(FaoNeiLevel)[1],ncol=length(Models)))

colnames(FaoNeiLevelPredictions)<- paste(Models,'LogBvBmsy',sep='')

TempLevel<- NULL

TempModel<- NULL

## Apply each regression to the species level of nei level data
for (m in 1:length(Models))
{
  
TempModelName<- Models[m]

eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))

MatchingSpeciesGroups<- FaoSpeciesLevel$SpeciesCatName %in% TempLevel

eval(parse(text=paste('TempLevel<- NeiModelFactorLevels$',TempModelName,sep='')))

MatchingNeiGroups<- FaoNeiLevel$SpeciesCatName %in% TempLevel

eval(parse(text=paste('TempModel<- RealModels$',TempModelName,sep='')))

FaoSpeciesLevelPredictions[MatchingSpeciesGroups,m]<- predict(TempModel,FaoSpeciesLevel[MatchingSpeciesGroups,])

eval(parse(text=paste('TempModel<- NeiModels$',TempModelName,sep='')))

FaoNeiLevelPredictions[MatchingNeiGroups,m]<- predict(TempModel,FaoNeiLevel[MatchingNeiGroups,])

}

##Bind projections together with regression data

FaoSpeciesLevel<- cbind(FaoSpeciesLevel,FaoSpeciesLevelPredictions) 

FaoNeiLevel<- cbind(FaoNeiLevel,FaoNeiLevelPredictions)

# Create stitched database ------------------------------------------------

# Estimate MSY ------------------------------------------------------------

# Run projection analysis -------------------------------------------------

# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------


