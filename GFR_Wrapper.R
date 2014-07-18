
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################


# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source);

# Read in and process data ------------------------------------------------------------

source('Database_Build.r') #Build Tyler's database

FullData<- fulldata

rm(fulldata)

FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA

FullData$Biomass<- as.numeric(FullData$Biomass)

FullData$BvBmsy<- FullData$Biomass/FullData$ReferenceBiomass

head(FullData)

# Create synthetic stocks -------------------------------------------------

if (Groups=='All')
{
Groups<- unique(FullData$SpeciesCatName,na.rm=T)

Groups<- Groups[is.na(Groups)==F]
}

SyntheticStocks<- StitchFish(FullData,IdVar,Groups,GroupSamples,Iterations)

# Prepare data for regression ---------------------------------------------

RamData<- FullData[FullData$Dbase=='RAM', ]

RegressionData<- FormatForRegression(RamData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

SyntheticRegressionData<- FormatForRegression(SyntheticStocks,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

# Run regressions ---------------------------------------------------------

RealModels<- RunRegressions(RegressionData,Regressions,'Real Stocks')

NeiModels<- RunRegressions(SyntheticRegressionData,Regressions,'Synthetic Stocks')

# Apply regressions -------------------------------------------------------

# Create stitched database ------------------------------------------------

# Estimate MSY ------------------------------------------------------------

# Run projection analysis -------------------------------------------------

# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------


