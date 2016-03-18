###########################################
##
## Script to subset upsides data for Shiny
## Kobe plot
##
##########################################

## Load packages and data ----

# packages
library(dplyr)
library(tidyr)

# set model run data to use and load data
modelrun<-'Results/PNAS Submission - 6.01 global demand common phi/'

load(paste('/Users/Tyler/Documents/Sustainable\ Fisheries\ Group/SFG\ 2014/Global\ Fishery\ Potential/Results/',
           modelrun,'/Data/ProjectionData Data.rdata',sep = ''))

## Subset to data necessary for kobe plot and write csv ----

# global kobe data
ldata<-ProjectionData %>%
  filter(Year==2012 & is.na(BvBmsy)==F & is.na(FvFmsy)==F) %>%
  select(IdOrig,Dbase,Country,SciName,CommName,SpeciesCatName,IdLevel,RegionFAO,BvBmsy,FvFmsy,MSY,Catch) %>%
  ungroup()

# unlumped data 
udata<-UnlumpedProjectionData %>%
  filter(Year==2012 & is.na(BvBmsy)==F & is.na(FvFmsy)==F) %>%
  select(IdOrig,Dbase,Country,SciName,CommName,SpeciesCatName,IdLevel,RegionFAO,BvBmsy,FvFmsy,MSY,Catch) %>%
  ungroup()

# save kobe data as a list
data<-list(lumped=ldata,unlumped=udata)

save(data,file = paste('/Users/Tyler/Documents/Sustainable Fisheries Group/SFG 2014/Global Fishery Potential/KobeAppV2/data/','KobeAppData.rdata',sep=''))
