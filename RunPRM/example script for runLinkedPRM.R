rm(list=ls())	#clear variables
setwd('/Users/danovando/Desktop/Bren/SFG Work/Global Fisheries Recovery/RunPRM')#set working directory to wherever you are storing packages and data

lifehistory<- read.csv('Data/lifehistory.csv') # read in catch history 
catchhistory<- read.csv('Data/catchhistory_RAM.csv') #read in custom life history

#runPRM takes (landings history, life history, the year you want summary statistics for or 'LAST' to get last available year, and the value of theta (any number from 0-1))

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

FigureFolder<- 'Figures/'

ResultFolder<- 'Results/'

PRMResults<- RunPRM(catchhistory)

head(PRMResults$PredictedBvBmsy)
