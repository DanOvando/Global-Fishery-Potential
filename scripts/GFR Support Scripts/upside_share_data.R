########################################################################
##
## Script for subsetting Costello et al. 2016 Upsides Data for sharing
##
## By Tyler Clavelle
########################################################################

library(tidyr)
library(dplyr)
library(readr)

# Load Projection Data
load('Results/10.0 No Demand Tyler Prices/Data/ProjectionData Data.rdata')

## Subset both Projection Data and Unlumped Projection Data to neccesary colums

# ProjectionData
upsides_share<-function(data) {
  
  ldata<-data %>%
  tbl_df() %>%
  filter(Policy %in% c('Historic', 'Business As Usual', 'Business As Usual Pessimistic', 'CatchShare','Catch Share Three',
                       'Fmsy', 'Fmsy Three')) %>%
  select(IdOrig,
         Country,
         CommName,
         SciName,
         IdLevel,
         SpeciesCat,
         SpeciesCatName,
         RegionFAO,
         Dbase,
         CatchShare,
         Policy,
         Year,
         Catch,
         Biomass,
         Profits,
         BvBmsy,
         FvFmsy,
         MSY,
         Price,
         g,
         k,
         MarginalCost,
         phi) %>%
  rename( c = MarginalCost)

# Round data
ldata[,13:23]<-round(ldata[,13:23], digits = 3)

# Add CC and All stocks scenario variable
ldata$Scenario <- NA

ldata$Scenario[ldata$Policy=='Historic']<-'Historic'
ldata$Scenario[ldata$Policy=='CatchShare']<-'All Stocks'
ldata$Scenario[ldata$Policy=='Catch Share Three']<-'Con. Concern'
ldata$Scenario[ldata$Policy=='Fmsy']<-'All Stocks'
ldata$Scenario[ldata$Policy=='Fmsy Three']<-'Con. Concern'
ldata$Scenario[ldata$Policy=='Business As Usual'] <- 'Con. Concern'
ldata$Scenario[ldata$Policy=='Business As Usual Pessimistic'] <- 'All Stocks'

# Rename policies to match terms in paper
ldata$Policy[ldata$Policy=='CatchShare'] <- 'RBFM'
ldata$Policy[ldata$Policy=='Catch Share Three'] <- 'RBFM'
ldata$Policy[ldata$Policy=='Fmsy Three'] <- 'Fmsy'
ldata$Policy[ldata$Policy=='Fmsy'] <- 'Fmsy'
ldata$Policy[ldata$Policy=='Business As Usual'] <- 'BAU'
ldata$Policy[ldata$Policy=='Business As Usual Pessimistic'] <- 'BAU'

ldata <- ldata %>%
  select(IdOrig,
         Country,
         CommName,
         SciName,
         IdLevel,
         SpeciesCat,
         SpeciesCatName,
         RegionFAO,
         Dbase,
         CatchShare,
         Policy,
         Scenario,
         Year,
         Catch,
         Biomass,
         Profits,
         BvBmsy,
         FvFmsy,
         MSY,
         Price,
         g,
         k,
         c,
         phi)

return(result = ldata)
}

# run function on both datasets
ldata<-upsides_share(ProjectionData)

udata<-upsides_share(UnlumpedProjectionData)

# subset data for example
exs <- sample(ldata$IdOrig, 100, replace = F)

ex <- subset(ldata, IdOrig %in% exs)

# subset RAM data for B/Bmsy, F/Fmsy, and MSY units
ram_units <- ProjectionData %>%
  filter(Year == 2012 & Dbase == 'RAM') %>%
  select(IdOrig, CommName, BvBmsy, BvBmsyUnit, FvFmsy, FvFmsyUnit, MSY, MSYUnit)

# save result files in folder on desktop to be shared
write_csv(ldata, path = './upside-share/ProjectionData.csv')
write_csv(udata, path = './upside-share/UnlumpedProjectionData.csv')
write_csv(ex, path = './upside-share/example_data.csv')
write_csv(ram_units, path = './Misc Data/Upside outputs/ram_param_units.csv')

