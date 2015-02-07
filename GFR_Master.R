######################################
#Global Fishery Recovery Master File--------------------------------------------------
# This code takes RAM data processed by XXX.r and uses panel regressions similar to those docomented in Costello et al. 2012 
# to estimate B/Bmsy 
######################################
rm(list=ls())
library(car)
library(plyr)
library(lattice)
library(rfishbase)
library(stringr)
library(RCurl)
library(XML)
library(MASS)
library(prettyR)
library(zoo)
library(proftools)
library(snowfall)
library(parallel)
# library(shiny)
library(ggplot2)
library(gridExtra)
library(reshape2)
# library(dplyr)
# Basic Controls -------------------------------------------------------------

RunAnalyses<- TRUE

BatchFolder<- 'Feb 5 MSY NEI Full Run'

BatchFolder<- paste('Results/',BatchFolder,'/',sep='')

InputFolder<- 'Data/'

FigureFolder<- paste(BatchFolder,'Figures/',sep='')

ResultFolder<- paste(BatchFolder,'Data/',sep='')

dir.create(BatchFolder,recursive=T)

dir.create(FigureFolder,recursive=T)

dir.create(ResultFolder,recursive=T)

if (RunAnalyses==FALSE)
{
  load(paste(ResultFolder,'Global Fishery Recovery Results.rdata',sep=''))
 
  BatchFolder<- 'Test Full Run'
    
  BatchFolder<- paste('Results/',BatchFolder,'/',sep='')
  
  InputFolder<- 'Data/'
  
  FigureFolder<- paste(BatchFolder,'Figures/',sep='')
  
  ResultFolder<- paste(BatchFolder,'Data/',sep='')
  
  RunAnalyses<- FALSE
  
}

# Analysis ----------------------------------------------------------------

SubSample<- 0

NumCPUs<- 16 #Number of CPUs to use for parallel computing of CatchMSY

CapRefs<- TRUE

IncludeNEIs<- TRUE

IncludeForageFish<- TRUE

IncludeUnderfished<- FALSE

SaveRDS<- FALSE

PlotFinalFigures<-FALSE

OverlapMode<- 'FaoTrumps' #SofiaTrumps, FaoTrumps

StatusQuoPolicy<-'StatusQuoA' # 'StatusQuoA'

CatchMSYTrumps<- TRUE

CommonFinalYear<- TRUE

BaselineYear<- 2012

CountriesToRun<- c('Global','USA','China','Indonesia','Philippines','Peru','Chile','Mexico','Japan','Myanmar','Viet Nam','EU','Parties to the Nauru Agreement') 

# CountriesToRun<-'All'

EUCountries<- c('Austria',
  'Belgium',
  'Bulgaria',
  'Croatia',
  'Cyprus',
  'Czech Republic',
  'Denmark',
  'Estonia',
  'Finland',
  'France',
  'Germany',
  'Greece',
  'Hungary',
  'Ireland',
  'Italy',
  'Latvia',
  'Lithuania',
  'Luxembourg',
  'Malta',
  'Netherlands',
  'Poland',
  'Portugal',
  'Romania',
  'Slovakia',
  'Slovenia',
  'Spain',
  'Sweden',
  'United Kingdom')

AsianCountries<-c('Afghanistan','Bahrain','Bangladesh', 'Bhutan','Brunei Darussalam','Cambodia','China', 
                  'Timor-Leste','Viet Nam', 'Japan','Tajikistan','Turkmenistan','Iran','Iraq','Israel','Jordan','Kazakhstan',
                  'Kuwait','Myanmar','Indonesia','Thailand', 'India','Philipines','Republic of Korea','Malaysia','Taiwan Province of China',
                  'Maldives','Nepal','Uzbekistan','Kyrgyzstan','Malaysia','Mongolia','Oman','Pakistan','Russian Federation',
                  'Saudi Arabia','Singapore','Syrian Arab Republic','Turkey','United Arab Emirates','Yemen','Sri Lanka','Qatar')


# Data Processing ---------------------------------------------------------

MinimumCatchYears<- 10 #Minimum length of catch history

OutlierBvBmsy<- 40 #Maximum BvBmsy that is allowed in the analysis 

FisheriesToOmit<- 'None' #List of fisheries to manually exclude from analysis 

SpeciesCategoriesToOmit<- c('Corals','Frogs and other amphibians','Eared seals, hair seals, walruses',
                            'Turtles','Pearls, mother-of-pearl, shells','Crocodiles and alligators','Miscellaneous aquatic plants'
                            ,'Freshwater crustaceans','Sperm-whales, pilot-whales','Green seaweeds','Red seaweeds',
                            'Brown seaweeds','Sea-squirts and other tunicates','Blue-whales, fin-whales',
                            'Miscellaneous aquatic mammals','Sponges','Krill, planktonic crustaceans','Miscellaneous freshwater fishes',
                            'River eels','Freshwater molluscs')

SpeciesCategoriesToLump<- c('Miscellaneous pelagic fishes','Tunas, bonitos, billfishes','Cods, hakes, haddocks','Marine fishes not identified')

ForageFish<- c('Herrings, sardines, anchovies')

MissingCatchTolerance<- 0.99 #Maximumum percentage of catch years that can be missing


# Regressions -------------------------------------------------------------

DependentVariable<- 'BvBmsy' #Dependent variable in regression

IsLog<- TRUE #Should dependent variable be logged?

DependentName<- DependentVariable

DependentName<- if (IsLog==T){paste('Log',DependentVariable,sep='')}

CatchLags<- 4 #Number of years of lagged catch to create for regression

LifeHistoryVars<- c('MaxLength','AgeMat','VonBertK','Temp') #Life history variables to include for potential regression

IdVar<- 'IdOrig' #Id variable to use in regressions

CatchVariables<- c('YearsBack','ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'MaxCatch','TimeToMaxCatch','InitialScaledCatchSlope'
                   ,'MeanScaledCatch','CatchToRollingMax')

# Regressions<- list(M1=c(DependentName,'Year',CatchVariables,LifeHistoryVars,'SpeciesCatName'),M2=c(DependentName,'Year',CatchVariables,'MaxLength','AgeMat','VonBertK','SpeciesCatName'),
#                    M3=c(DependentName,'Year',CatchVariables,'MaxLength','VonBertK','SpeciesCatName'),M4=c(DependentName,'Year',CatchVariables,'VonBertK','SpeciesCatName'),M6=c(DependentName,'Year',CatchVariables,'SpeciesCatName'),M7=c(DependentName,CatchVariables))  


Regressions<- list(M1=c(DependentName,CatchVariables,LifeHistoryVars,'SpeciesCatName'),M2=c(DependentName,CatchVariables,'MaxLength','AgeMat','VonBertK','SpeciesCatName'),
                   M3=c(DependentName,CatchVariables,'MaxLength','VonBertK','SpeciesCatName'),M4=c(DependentName,CatchVariables,'VonBertK','SpeciesCatName'),M6=c(DependentName,CatchVariables,'SpeciesCatName'),M7=c(DependentName,CatchVariables))  


TransbiasBin<- 0.9

TransbiasIterations<- 1000

# Synthetic Stock Settings ------------------------------------------------

GroupMethod<- 'All'

GroupSamples<- 10 #The number of stocks to put in each synthetic nei. This can be a single number, or a vector for each group of interest

Iterations<- 10 #Number of synthetic stocks to create within each group

IdVar<- 'IdOrig' #Id variable to use in creating synthetic stocks


# Catch-MSY ---------------------------------------------------------------


ErrorSize<- 0.85 #The amount of error to serach over CatchMSY terms

Smooth<- 0 #Marks whether to smooth catch history

Display<- 0 #Display running outputs

# runCatchMSY<- 0 #run CatchMSY or rely on saved results

BestValues<- 1 # 1 subs in RAM F/Fmsy and MSY values where possible

ManualFinalYear<- 0 #Set year you want to run all analyses for

NumCatchMSYIterations <- 10000  ## number of iterations, e.g. 100000

Parel<- TRUE #Run SNOWFALL in parallel?

# Projections -------------------------------------------------------------

ProjectionTime<- 23

CatchSharePrice<- 1.31

CatchShareCost<- 0.77

beta<- 1.3

Discount<- 0.05

bvec<- seq(0.00000001,2,length.out=50)

tol<- .1

BOAtol<- 0.2

# Figures -----------------------------------------------------------------

#Figure fonts, formats etc. 

save.image(file=paste(BatchFolder,'Controlfile Settings.rdata'))

# Run Analysis ------------------------------------------------------------

source('GFR_Wrapper.R')


