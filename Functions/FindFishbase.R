######################################
#UseFishbase--------------------------------------------------
#This code assigns fishbase based life history variables to any data missing life history variables
######################################

FindFishbase<- function(Data)
{
    
#    Data<- FullData
  load('Data/mpack.Rdata')
  
 Fishbase<- mpack$lh

 Fishbase[is.na(Fishbase)]= NA
 rm(mpack)
 
 spnames<- unique((Data$SciName))
 
for (i in 1:length(spnames)){
  
  show(paste(100*(round(i/length(spnames),2)),'% Done With Fishbase Matching',sep=''))
  
  lh_match=grepl(spnames[i],Fishbase$sname) #find if species name name matches FB data, locate row
 
  if (sum(lh_match)>0)
  {
  
    WhereSpecies<- spnames[i]==Data$SciName
#   WhereVb<- spnames[i]==Data$SciName & is.na(Data$VonBertK)
  WhereVb<- WhereSpecies & is.na(Data$VonBertK)
  
  WhereAge<-  WhereSpecies & is.na(Data$AgeMat)
  WhereMaxL<- WhereSpecies & is.na(Data$MaxLength)
  WhereTemp<- WhereSpecies & is.na(Data$Temp)
  
    lh_data=Fishbase[lh_match,] #pull out matching data
#     spcat=lh_data$spcat #identify the species category
    
    #Add in life history
    Data$MaxLength[WhereMaxL]<- lh_data$maxl[1]
    Data$MaxLengthSource[WhereMaxL]<-"FishBase"
    
  Data$VonBertK[WhereVb]<-lh_data$vbk[1]
    Data$VonBertKSource[WhereVb]<-"FishBase"
    Data$AgeMat[WhereAge]<-lh_data$agem[1]
    Data$AgeMatSource[WhereAge]<-"FishBase"
    Data$Temp[WhereTemp]<-lh_data$temp[1]
    Data$TempSource[WhereTemp]<-"FishBase" 
 } #close if loop

} #close species loop

return(Data)

}