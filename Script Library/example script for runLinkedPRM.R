rm(list=ls())	#clear variables
# setwd('/Users/danovando/Desktop/Bren/SFG Work/The Method/Costello et al 2012/runLinkedPRM')#set working directory to wherever you are storing packages and data
source('runLinkedPRM.R') #source the runmethod package
# source('runPRM_FORMETHOD.R') #source the runmethod package

testvariance<- 0 #for reasons to long to explain, you have to include a variable called testvariance=0 in this section
catchhistory<- read.csv('YOURDATA/RAM missing bvbsmy catchhistory.csv') # read in catch history 
lifehistory<- read.csv('YOURDATA/RAM missing bvbmsy lifehistory.csv') #read in custom life history

#runPRM takes (landings history, life history, the year you want summary statistics for or 'LAST' to get last available year, and the value of theta (any number from 0-1))
completeResult<- runLinkedPRM(catchhistory,lifehistory,'LAST',0) #calculate B/Bmsy and summary statistics for the complete dataset. Inputting 999 means you don't have any lifehistory data

head(completeResult$result$ind)

write.csv(completeResult$result$ind,file='PRM Estimated BvBmsy values for RAM stocks missing BvBmsy.csv')