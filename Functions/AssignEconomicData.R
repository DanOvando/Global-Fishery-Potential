
AssignEconomicData<- function(Data,BvBmsyOpenAccess)
{
  
#   Data<- FullData
  
#   PriceData<- read.csv('Data/Species Category Prices.csv') # Original EDF export prices
  
#   PriceData<-read.csv('Data/Prices_FAO_ExVessel.csv',stringsAsFactors=F) # FAO Ex-vessel prices

  PriceData<-read.csv('Data/ExVessel_PriceDB_0224.csv',stringsAsFactors=F) # FAO Ex-vessel prices
  
  PriceData$Avg07to11<-apply(PriceData[,c('X2007','X2008','X2009','X2010','X2011')],1,function(x) mean(x,na.rm=T))

#   BvBmsyOpenAccess<- read.csv('Data/Species Category Open Access Depletion.csv')
  
### ADD IN PRICES BY SCI NAME-----------------------------------

sci<-unique(Data$SciName)

catprices2011<-PriceData[,c('SpeciesCatName','Species_AFSIS','CommName','X2011')]

catprices2011<- catprices2011 %>% 
  group_by(SpeciesCatName) %>% 
  summarize(MeanPrice=mean(X2011,na.rm=T))

for(l in 1:length(sci))
{
  rm(price)
  
  price<-match(sci[l],PriceData$Species_AFSIS)
  
  if(is.na(price)==F)
  {
    Data$Price[Data$SciName==sci[l]]<-PriceData$Avg07to11[price]
  }
  
  if(is.na(price))
  {
    cat<-unique(Data$SpeciesCatName[Data$SciName==sci[l]])
    
    Data$Price[Data$SciName==sci[l]]<-catprices2011$MeanPrice[catprices2011$SpeciesCatName==cat]
  }
  
}

### ADD IN BIOMASS AT OPEN ACCESS

  SpeciesCats<- unique(Data$SpeciesCatName)
    
  for (s in 1:length(SpeciesCats))
  {
    
    WhereSpecies<- Data$SpeciesCatName==SpeciesCats[s]
        
    WhereBOA<- (BvBmsyOpenAccess$SpeciesCatName)==as.character(SpeciesCats[s])
    
    if (sum(WhereBOA)>0)
    {
      BOA<- (BvBmsyOpenAccess$BvBmsyOpenAccess[WhereBOA])
    }
    else
    {
      BOA<- mean(BvBmsyOpenAccess$BvBmsyOpenAccess,na.rm=T)
    }
    
#     Data$Price[WhereSpecies]<- Price
    
    Data$BvBmsyOpenAccess[WhereSpecies]<- BOA
  } #Close species category loop
  
  return(Data)
} #close function
