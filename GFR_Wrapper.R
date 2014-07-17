
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################


# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source);

# Read in and process data ------------------------------------------------------------

#Call Tyler's code here when its' ready
#source(RAM_Build.R)
RAM<- read.csv('Data/RAM_Complete_TC_071514.csv')

RAM$Bmsy[RAM$Bmsy==0]<- NA

RAM$BvBmsy<- RAM$Biomass/RAM$Bmsy

head(RAM)


# Create synthetic stocks -------------------------------------------------

Groups<- unique(RAM$SpeciesCat,na.rm=T)

Groups<- Groups[is.na(Groups)==F]

SyntheticStocks<- StitchFish(RAM,IdVar,Groups,GroupSamples,Iterations)

# Prepare data for regression ---------------------------------------------

RegressionData<- FormatForRegression(RAM,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

