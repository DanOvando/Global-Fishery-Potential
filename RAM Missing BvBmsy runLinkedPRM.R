rm(list=ls())	#clear variables
library('matlab')
setwd('/Users/danovando/Desktop/Bren/SFG Work/The Method/Costello et al 2012/runLinkedPRM')#set working directory to wherever you are storing packages and data
source('runLinkedPRM.R') #source the runmethod package
# source('runPRM_FORMETHOD.R') #source the runmethod package

testvariance<- 0 #for reasons to long to explain, you have to include a variable called testvariance=0 in this section
catchhistory<- read.csv('YOURDATA/catchhistory.csv') # read in catch history 
lifehistory<- read.csv('YOURDATA/lifehistory.csv') #read in custom life history

#runPRM takes (landings history, life history, the year you want summary statistics for or 'LAST' to get last available year, and the value of theta (any number from 0-1))
completeResult<- runLinkedPRM(catchhistory,lifehistory,'LAST',0) #calculate B/Bmsy and summary statistics for the complete dataset. Inputting 999 means you don't have any lifehistory data

completeResult$summary #the key result table

###Calcualte results for each species category ###

data<- completeResult$data #pull out raw data
sps<- unique(data$spcat) #unique species categories
catholder<- as.data.frame(matrix(NA,nrow=0,ncol=size(data)[2]+1))
for (s in 1:length(sps))
{
		ids<- unique(data$id[data$spcat==sps[s]])
		wherec<- catchhistory$id %in% ids		
		wherel<- lifehistory$id %in% ids		
		stemp<- runLinkedPRM(catchhistory[wherec,],lifehistory[wherel,],'LAST',0)
		pass<- stemp$summary
		pass$spcat<- sps[s]
		catholder<- rbind(catholder,pass)
}



