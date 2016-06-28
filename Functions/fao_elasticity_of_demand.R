
# Analyze Price Data ------------------------------------------------------
# Script to consider the elasticity of demend 
# of global marine commodities
# 
#

find_elasticity <- function()
{

rm(list = ls())
library(ggplot2)
# library(plyr)
library(dplyr)
library(tidyr)

pricedat <- read.csv('Data/FAO 2011 SOFIA Price Data.csv', stringsAsFactors = F) 

year_cols <- which(grepl('X',colnames(pricedat)))

pricedat <- pricedat %>%
  gather('year','price',year_cols)

pricedat$year <- as.numeric(gsub('X','',pricedat$year,fixed = T))
  

quartz()
ggplot(pricedat, aes(year,price)) + geom_smooth()

}


