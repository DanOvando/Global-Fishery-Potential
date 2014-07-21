
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

RamData<- FullData[FullData$Dbase=='RAM',]

FaoData<- FullData[FullData$Dbase=='FAO',]

 FaoIdSample<- sample(unique(FaoData[,IdVar]),500,replace=FALSE)
# 
 FaoData<- FaoData[FaoData[,IdVar] %in% FaoIdSample,]

# Create synthetic stocks -------------------------------------------------

if (Groups=='All')
{
  Groups<- unique(FullData$SpeciesCatName,na.rm=T)
  
  Groups<- Groups[is.na(Groups)==F]
}

SyntheticData<- StitchFish(RamData,IdVar,Groups,GroupSamples,Iterations) 

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
  
# Rprof(NULL)
#  RProfData<- readProfileData('Rprof.out')
#  flatProfile(RProfData,byTotal=TRUE)

# Run regressions ---------------------------------------------------------

RealModels<- RunRegressions(RamData,Regressions,'Real Stocks')

RealModelFactorLevels<- NULL

Models<- names(Regressions)

## Determine species category levels that were used in each model run

TempOmitted<- NULL
for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))

  ##While you're in here, figure out what fisheries were used in regression
  
  eval(parse(text=paste('TempOmitted<- na.action(RealModels$',Model,')',sep='')))
  
  KeptEntries<- ((1:dim(RamData)[1]) %in% TempOmitted)==F #Identify fisheries used in each model

  eval(parse(text=paste('RamData$',Model,'Marker[KeptEntries]<- TRUE' ,sep='')))

  eval(parse(text=paste('RamData$',Model,'Prediction[KeptEntries]<- RealModels$',Model,'$fitted.values' ,sep='')))
    
}

NeiRegressions<- list()

NeiRegressions$M6<- Regressions$M6

NeiRegressions$M7<- Regressions$M7

NeiModels<- RunRegressions(SyntheticData,NeiRegressions,'Synthetic Stocks')

NeiModelFactorLevels<- NULL

for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  
  eval(parse(text=paste('NeiModelFactorLevels$',Model,'<- NeiModels$',Model,'$xlevels$SpeciesCatName',sep='')))

  ##While you're in here, figure out what fisheries were run
  
  eval(parse(text=paste('TempOmitted<- na.action(NeiModels$',Model,')',sep='')))
  
  KeptEntries<- ((1:dim(SyntheticData)[1]) %in% TempOmitted)==F #Identify fisheries used in each model
  
  eval(parse(text=paste('SyntheticData$',Model,'Marker[KeptEntries]<- TRUE' ,sep='')))
  
  eval(parse(text=paste('SyntheticData$',Model,'Prediction[KeptEntries]<- NeiModels$',Model,'$fitted.values' ,sep='')))
  

}



# Prepare data for regression application ---------------------------------


WhereFaoNeis<- (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F #Find unassessed NEIs

WhereFaoMarineFish<- grepl('not identified',FaoData$SpeciesCatName)

FaoSpeciesLevel<- FaoData[WhereFaoNeis==F,] #Fao stocks named to the species level

FaoNeiLevel<- FaoData[WhereFaoNeis,] #fao species named to the nei or spp level

FaoMarineFish<- FaoData[WhereFaoMarineFish,] #completely unidentified marine goo

## Only grab fisheries with appropriate species categories at this point

FaoSpeciesLevelPredictions<- as.data.frame(matrix(NA,nrow=dim(FaoSpeciesLevel)[1],ncol=length(Models)))

colnames(FaoSpeciesLevelPredictions)<- paste(Models,'LogBvBmsy',sep='')

FaoNeiLevelPredictions<- as.data.frame(matrix(NA,nrow=dim(FaoNeiLevel)[1],ncol=length(Models)))

colnames(FaoNeiLevelPredictions)<- paste(Models,'LogBvBmsy',sep='')

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

FaoSpeciesLevelPredictions[MatchingSpeciesGroups,m]<- predict(TempModel,FaoSpeciesLevel[MatchingSpeciesGroups,])

eval(parse(text=paste('TempModel<- NeiModels$',TempModelName,sep='')))

FaoNeiLevelPredictions[MatchingNeiGroups,m]<- predict(NeiModels$M6,FaoNeiLevel[MatchingNeiGroups,])

}

NotIdentifiedPredictions<- predict(NeiModels$M7,FaoMarineFish)

##Bind projections together with regression data


FaoSpeciesLevel<- cbind(FaoSpeciesLevel,FaoSpeciesLevelPredictions) 

pdf(paste(FigureFolder,'Unassessed Species Level Test Histogram.pdf',sep=''))
hist(exp(FaoSpeciesLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks')
abline(v=median(exp(FaoSpeciesLevelPredictions$M6LogBvBmsy),na.rm=T))
dev.off()

pdf(paste(FigureFolder,'Unassessed Species Level Test boxplot.pdf',sep=''))
boxplot(exp(FaoSpeciesLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks',outline=F)
dev.off()

pdf(paste(FigureFolder,'Unassessed Species Level Model 1 Test Histogram.pdf',sep=''))
hist(exp(FaoSpeciesLevelPredictions$M1LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks')
abline(v=median(exp(FaoSpeciesLevelPredictions$M1LogBvBmsy),na.rm=T))
dev.off()

pdf(paste(FigureFolder,'Unassessed Species Level Model 1 Test boxplot.pdf',sep=''))
boxplot(exp(FaoSpeciesLevelPredictions$M1LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Species Level Fao Stocks',outline=F)
dev.off()


FaoNeiLevel<- cbind(FaoNeiLevel,FaoNeiLevelPredictions)

pdf(paste(FigureFolder,'Unassessed Nei Level Test Histogram.pdf',sep=''))
hist(exp(FaoNeiLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Nei Level Fao Stocks')
abline(v=median(exp(FaoNeiLevelPredictions$M6LogBvBmsy),na.rm=T))
dev.off()

pdf(paste(FigureFolder,'Unassessed Nei Level Test boxplot.pdf',sep=''))
boxplot(exp(FaoNeiLevelPredictions$M6LogBvBmsy),xlab='Predicted Raw B/Bmsy ',main='Nei Level Fao Stocks',outline=F)
dev.off()

save.image(file=paste(ResultFolder,'Regression Outputs.rdata',sep=''))

file.exists(paste(ResultFolder,'Regression Outputs.rdata',sep=''))

# Create stitched database ------------------------------------------------

# Estimate MSY ------------------------------------------------------------

# Run projection analysis -------------------------------------------------

# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------


