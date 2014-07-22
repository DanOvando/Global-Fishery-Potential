######################################
#UseFishbase--------------------------------------------------
#This code assigns fishbase based life history variables to any data missing life history variables
######################################

FindFishbase<- function(Data,LifeVars)
{
  
  Data<-FullData
  
 load('Data/mpack.Rdata')
  
 Fishbase<- mpack$lh

 rm(mpack)
 
 spnames<-unique(Data$SciName)

for (i in 1:length(spnames)){
  
  lh_match=match(Data$SciName[i],Fishbase$sname) #find if species name name matches FB data, locate row
  WhereVb<-spnames[i]==Data$SciName & is.na(Data$VonBertK)
  WhereAge<-spnames[i]==Data$SciName & is.na(Data$AgeMat)
  WhereMaxL<-spnames[i]==Data$SciName & is.na(Data$MaxLength)
  WhereTemp<-spnames[i]==Data$SciName & is.na(Data$Temp)
  
  if (is.na(lh_match)==F)
  {
    lh_data=Fishbase[lh_match,] #pull out matching data
    spcat=lh_data$spcat #identify the species category
    
    #Add in life history
    Data$MaxLength[WhereMaxL]=lh_data$maxl
    Data$VonBertK[WhereVb]=lh_data$vbk
    Data$AgeMat[WhereAge]=lh_data$agem
    Data$Temp[WhereTemp]=lh_data$temp
 
  # function taking a VERY long time, not confirmed that it's working
  }
}


}