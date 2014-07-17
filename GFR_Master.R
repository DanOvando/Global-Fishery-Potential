######################################
#Global Fishery Recovery Master File--------------------------------------------------
# This code takes RAM data processed by XXX.r and uses panel regressions similar to those docomented in Costello et al. 2012 
# to estimate B/Bmsy 
######################################
rm(list=ls())

# Basic Controls -------------------------------------------------------------

BatchFolder<- 'Results/Scratch Folder/'

InputFolder<- 'Data/'

FigureFolder<- paste(BatchFolder,'Figures/',sep='')

ResultFolder<- paste(BatchFolder,'Data/',sep='')

dir.create(BatchFolder)

dir.create(FigureFolder)

dir.create(ResultFolder)
  
  
#Output storage

#Sections to run/or try and call

#Iterations

#Storage


# Data Processing ---------------------------------------------------------

#Options for processing RAM, SOFIA, FAO

# Regressions -------------------------------------------------------------

DependentVariable<- 'BvBmsy' #Dependent variable in regression

IsLog<- TRUE #Should dependent variable be logged?
  
CatchLags<- 4 #Number of years of lagged catch to create for regression

LifeHistoryVars<- c('MaxLength','AgeMat','VonBertK') #Life history variables to include for potential regression

IdVar<- 'IdOrig' #Id variable to use in regressions


# Synthetic Stock Settings ------------------------------------------------


GroupSamples<- 10

Iterations<- 10

Data<- RAM

IdVar<- 'IdOrig' #Id variable to use in regressions

#Interpolation etc. 

#Any other basic regression options


# Projections -------------------------------------------------------------

#Time frame

#Discount Rates

#Fishing Rates

# Filters -----------------------------------------------------------------

# Fisheries to include/not include etc. 


# Figures -----------------------------------------------------------------

#Figure fonts, formats etc. 

save.image(file=paste(BatchFolder,'Controlfile Settings.rdata'))

# Run Analysis ------------------------------------------------------------

source('GFR_Wrapper.R')


