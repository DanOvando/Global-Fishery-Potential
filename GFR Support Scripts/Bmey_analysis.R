################################################################################################
##
##
## BMEY Kobe Plot
##
##
################################################################################################


### Load packages, data, functions, and set options -------------------------------------------------------
library(dplyr)
library(tidyr)
library(pracma)

# Load Projection Data
load(file = 'Global Fishery Potential/Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.rdata')

# Read in ASFIS list to use for matching NEIs
asfis<-read.csv(file = 'Global Fishery Potential/Data/TaxonCodes.csv', stringsAsFactors = F)

## Source or define functions
source(file = 'Global Fishery Potential/GFR Support Scripts/NEI_lookup.R')

DiffF=function(b,bgrid,f0,f1)
{
  #To be zeroed out.  Want to find b* such that f0(b*) = f1(b*) 
  fval0 = spline(bgrid,f0,xout=b,method="natural")
  fval1 = spline(bgrid,f1,xout=b,method="natural")
  difference = fval0$y - fval1$y
  return(difference)
}

## Set options
calcMEY<-F # run Chris's MEY calculating code?
makeNEIlookup<-T # create new NEI lookup table?

## Load MEY data and NEI lookup table if desired
if(calcMEY==F) { mey_data<-read.csv(file = 'MEY-Code/MEY_results.csv', stringsAsFactors = F) }
if(makeNEIlookup==F) { nei_lookup<-read.csv(file = '')}

### Subset Projection Data and run MEY and NEI lookup code if needed --------------------------------------------

## Subset ProjectionData
ProjectionData %>%
  tbl_df(filter())

## Run Chris's MEY code and NEI lookup generator if needed
if(calcMEY==T) {mey_results<-findMEY(df = ) }
if(makeNEIlookup==T) { nei_lookup<-NEI_lookup(df = ProjectionData, asfis = asfis) }
