# rm(list=ls())
# set.seed(423)
# library(car)
# library(plyr)
# library(lattice)
# library(rfishbase)
# library(stringr)
# library(RCurl)
# library(XML)
# library(MASS)
# library(prettyR)
# library(zoo)
# library(proftools)
# library(snowfall)
# library(parallel)
# # library(shiny)
# library(ggplot2)
# library(gridExtra)
# library(reshape2)
# library(rfishbase,quietly = T)
# data(fishbase)
# library(dplyr)
# library(broom)
# library(tidyr)
# 
# Runs <- as.data.frame(matrix(NA,nrow = 1,ncol = 8))
# 
# colnames(Runs) <- c('runname','DefaultPhi','custom_phi','elastic_demand','sp_group_demand',
#                     'beta','discount','IncludeNEIs')
# 
# Runs[1,] <- data.frame('5.4 global demand common phi')
# 
# Master_Wrapper <- function(runname,NumCPUs = 1,DefaultPhi = .188, custom_phi = F,elastic_demand = T,
#                            elasticity = -0.9, sp_group_demand = F, beta = 1.3,discount = 0, 
#                            IncludeNEIs = T,SubSample = 0)
# 
