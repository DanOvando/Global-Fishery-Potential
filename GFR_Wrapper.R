
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################


# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source);

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

FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA

FullData$Biomass<- as.numeric(FullData$Biomass)

FullData$BvBmsy<- FullData$Biomass/FullData$ReferenceBiomass


# Create synthetic stocks -------------------------------------------------



RamData<- FullData[FullData$Dbase=='RAM',]

if (Groups=='All')
{
  Groups<- unique(RamData$SpeciesCatName,na.rm=T)
  
  Groups<- Groups[is.na(Groups)==F]
}

SyntheticStocks<- StitchFish(RamData,IdVar,Groups,GroupSamples,Iterations)

# Prepare data for regression ---------------------------------------------

library(proftools)

 Rprof()

RegressionData<- FormatForRegression(RamData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)
 Rprof(NULL)
 RProfData<- readProfileData('Rprof.out')
 flatProfile(RProfData,byTotal=TRUE)


SyntheticRegressionData<- FormatForRegression(SyntheticStocks,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

# Run regressions ---------------------------------------------------------

RealModels<- RunRegressions(RegressionData,Regressions,'Real Stocks')

NeiModels<- RunRegressions(SyntheticRegressionData,Regressions,'Synthetic Stocks')

# Apply regressions -------------------------------------------------------

unassessed<- predict(RealModels$M6,RegressionData)

# Create stitched database ------------------------------------------------

# Estimate MSY ------------------------------------------------------------

# Run projection analysis -------------------------------------------------

# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------


