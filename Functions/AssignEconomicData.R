
AssignEconomicData<- function(Data,BvBmsyOpenAccess)
{
  
#   Data<- FullData
  
#   PriceData<- read.csv('Data/Species Category Prices.csv')
  
  PriceData<-read.csv('Data/Prices_FAO_ExVessel.csv',stringsAsFactors=F)
  
#   BvBmsyOpenAccess<- read.csv('Data/Species Category Open Access Depletion.csv')
  
  SpeciesCats<- unique(Data$SpeciesCatName)
    
  for (s in 1:length(SpeciesCats))
  {
    
    WhereSpecies<- Data$SpeciesCatName==SpeciesCats[s]
        
    WherePrices<- as.character(PriceData$SpeciesCatName)==as.character(SpeciesCats[s])
    
    if (sum(WherePrices)>0)
    {
      Price<- (PriceData$wt_avg_08to12[WherePrices])
      
    }
    else{Price<- mean(PriceData$wt_avg_08to12,na.rm=T)}
    
    WhereBOA<- (BvBmsyOpenAccess$SpeciesCatName)==as.character(SpeciesCats[s])
    
    if (sum(WhereBOA)>0)
    {
      BOA<- (BvBmsyOpenAccess$BvBmsyOpenAccess[WhereBOA])
    }
    else
    {
      BOA<- mean(BvBmsyOpenAccess$BvBmsyOpenAccess,na.rm=T)
    }
    
    Data$Price[WhereSpecies]<- Price
    
    Data$BvBmsyOpenAccess[WhereSpecies]<- BOA
  } #Close species category loop
  
  return(Data)
} #close function
