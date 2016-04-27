# A bunch of scripts for analyzing current status from GFR ----------------
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(knitr)

load(
  'Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata'
)


for_chris <- ProjectionData %>%
  filter(Year == 2012)  %>%
  select(
    IdOrig,
    SciName,
    CommName,
    SpeciesCat,
    SpeciesCatName,
    Dbase,
    g,
    k,
    phi,
    MSY,
    MarginalCost,
    Price,
    BvBmsy,
    FvFmsy
  )  %>%
  rename(Cost = MarginalCost)

policies <-
  read.csv(
    'Results/PNAS Submission - 6.01 global demand common phi/Data/PolicyStorage.csv',
    stringsAsFactors = F
  )


for_chris <- policies %>%
  left_join(for_chris, by = 'IdOrig')

dim(for_chris)

save(for_chris, file = 'Policies and Params for Chris.Rdata')