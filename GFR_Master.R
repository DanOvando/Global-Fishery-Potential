######################################
#Global Fishery Recovery Master File--------------------------------------------------
# This code takes RAM data processed by XXX.r and uses panel regressions similar to those docomented in Costello et al. 2012 
# to estimate B/Bmsy 
######################################
rm(list=ls())
library(car)
library(plyr)
library(lattice)
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

MinimumCatchYears<- 5 #Minimum length of catch history

FisheriesToOmit<- 'None' #List of fisheries to manually exclude from analysis 

SpeciesCategoriesToOmit<- c('Corals','Frogs and other amphibians','Eared seals, hair seals, walruses',
                            'Turtles','Pearls, mother-of-pearl, shells','Crocodiles and alligators','Miscellaneous aquatic plants'
                            ,'Freshwater crustaceans','Sperm-whales, pilot-whales','Green seaweeds','Red seaweeds',
                            'Brown seaweeds','Sea-squirts and other tunicates','Blue-whales, fin-whales',
                            'Miscellaneous aquatic mammals','Sponges','Krill, planktonic crustaceans')

MissingCatchTolerance<- 0.99 #Maximumum percentage of catch years that can be missing


# Regressions -------------------------------------------------------------

DependentVariable<- 'BvBmsy' #Dependent variable in regression

IsLog<- TRUE #Should dependent variable be logged?

DependentName<- DependentVariable

DependentName<- if (IsLog==T){paste('Log',DependentVariable,sep='')}

CatchLags<- 4 #Number of years of lagged catch to create for regression

LifeHistoryVars<- c('MaxLength','AgeMat','VonBertK') #Life history variables to include for potential regression

IdVar<- 'IdOrig' #Id variable to use in regressions

CatchVariables<- c('YearsBack','ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'TimeToMaxCatch','InitialScaledCatchSlope'
                   ,'MeanScaledCatch','CatchToRollingMax')

# Regressions<- list(M1=c(DependentName,CatchVariables,LifeHistoryVars,'SpeciesCatName'),M6=c(DependentName,CatchVariables,'SpeciesCatName'))  

Regressions<- list(M1=c(DependentName,CatchVariables,LifeHistoryVars,'SpeciesCatName'),M6=c(DependentName,CatchVariables,'SpeciesCatName'),M7=c(DependentName,CatchVariables))  


# Synthetic Stock Settings ------------------------------------------------

Groups<- 'All'

GroupSamples<- 10 #The number of stocks to put in each synthetic nei. This can be a single number, or a vector for each group of interest

Iterations<- 10 #Number of synthetic stocks to create within each group

IdVar<- 'IdOrig' #Id variable to use in creating synthetic stocks

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


