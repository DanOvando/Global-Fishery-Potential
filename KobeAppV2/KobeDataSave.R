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
modelrun<-'PNAS Submission - 6.01 global demand common phi/'

load('Global Fishery Potential/Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.rdata')

## Subset to data necessary for kobe plot and write csv ----

# global kobe data
ldata<-ProjectionData %>%
  filter(Year==2012 & is.na(BvBmsy)==F & is.na(FvFmsy)==F) %>%
  dplyr::select(IdOrig,Dbase,Country,SciName,CommName,SpeciesCatName,IdLevel,RegionFAO,BvBmsy,FvFmsy,MSY,Catch,g,k) %>%
  ungroup()

# unlumped data 
udata<-UnlumpedProjectionData %>%
  filter(Year==2012 & is.na(BvBmsy)==F & is.na(FvFmsy)==F) %>%
  dplyr::select(IdOrig,Dbase,Country,SciName,CommName,SpeciesCatName,IdLevel,RegionFAO,BvBmsy,FvFmsy,MSY,Catch,g,k) %>%
  ungroup()

# save kobe data as a list
data<-list(lumped=ldata,unlumped=udata)

save(data,file = paste('gfr-kobe/data/','KobeAppData.rdata',sep=''))
