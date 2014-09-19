

# Function to apply scraped life-history values to FullData

FindFishbase2<-function(Data,FBLifeHistory)
{
spnames<- unique(Data$SciName)

for (i in 1:length(spnames)){
  
  show(paste(100*(round(i/length(spnames),2)),'% Done',sep=''))
  
  lh_match=grepl(spnames[i],FBLifeHistory$SciName) #find if species name name matches FB data, locate row
  
  if (sum(lh_match)>0)
  {
    
    WhereSpecies<- spnames[i]==Data$SciName
    #   WhereVb<- spnames[i]==Data$SciName & is.na(Data$VonBertK)
    WhereVb<- WhereSpecies & is.na(Data$VonBertK)
    
    WhereAge<-  WhereSpecies & is.na(Data$AgeMat)
    WhereMaxL<- WhereSpecies & is.na(Data$MaxLength)
    WhereTemp<- WhereSpecies & is.na(Data$Temp)
    
    lh_data=FBLifeHistory[lh_match,] #pull out matching data
    #     spcat=lh_data$spcat #identify the species category
    
    #Add in life history
    Data$MaxLength[WhereMaxL]<- lh_data$Lmax[1]
    Data$MaxLengthSource[WhereMaxL]<-"FishBase"
    
    Data$VonBertK[WhereVb]<-lh_data$VonBertK[1]
    Data$VonBertKSource[WhereVb]<-"FishBase"
    Data$AgeMat[WhereAge]<-lh_data$tm[1]
    Data$AgeMatSource[WhereAge]<-"FishBase"
    Data$Temp[WhereTemp]<-lh_data$Temp[1]
    Data$TempSource[WhereTemp]<-"FishBase" 
  } #close if loop
  
} #close species loop

return(Data)

} # close function