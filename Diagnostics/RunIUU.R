
rm(list=ls())
load('Results/2.5/Data/Global Fishery Recovery Results.rdata')
NumCPUs<- 4
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
sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)


IUULevel<- 1.25

RunIUUDiagnostic(MsyData,RealModels,IUULevel=IUULevel,NumCatchMSYIterations=25000,BatchFolder,SubSample=0)
