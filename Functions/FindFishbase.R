######################################
#UseFishbase--------------------------------------------------
#This code assigns fishbase based life history variables to any data missing life history variables
######################################

FindFishbase<- function(Data,LifeVars)
{
  
  #Pass FullData
  
 load('Data/mpack.Rdata')
  
 Fishbase<- mpack$lh

 rm(mpack)
}