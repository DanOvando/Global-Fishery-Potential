
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################


# Read in and process data ------------------------------------------------------------

#Call Tyler's code here when its' ready
#source(RAM_Build.R)
RAM<- read.csv('Data/RAM_Complete_TC_071514.csv')


RAM$Bmsy[RAM$Bmsy==0]<- NA

RAM$BvBmsy<- RAM$Biomass/RAM$Bmsy

head(RAM)

# Prepare data for regression ---------------------------------------------

RegressionData<- FormatForRegression(RAM,'BvBmsy',4,c('MaxLength','AgeMat','VonBertK'),TRUE,'IdOrig')

