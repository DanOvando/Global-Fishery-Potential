##################################################
##
## Adjust MSY of forage fish and RAM stocks with unreliable K estimates
##
##################################################

# Data<-MsyData

AdjustMSY<-function(Data,RawData,ForageFish)
{
  
  ### reduce forage ('Herrings, sardines, anchovies') MSY by 25%
  Data$MSY[Data$SpeciesCatName==ForageFish]<-Data$MSY[Data$SpeciesCatName==ForageFish]*0.75 # reduce forage fish MSY by 25%
    
  Data$MSYUnit[Data$SpeciesCatName==ForageFish]<-'Reduced MSY by 25%'
  
  ### adjust MSY for RAM stocks with unreliable k values
  ToAdjust<-read.csv('Data/RAM_StocksToAdjust_MSY.csv', stringsAsFactors = F)
  
  ids<-unique(ToAdjust$IdOrig)
  
  # Set MSY to equal the average annual lifetime catch of these RAM stocks
  temp<-RawData[RawData$IdOrig %in% ids,]
  
  avgCatch<- temp %>% 
    group_by(IdOrig,CommName) %>% 
    summarize(AvgCatch=mean(Catch,na.rm=T),AvgMsy=mean(MSY,na.rm=T))
  
  avgCatch$PercOfOriginalMSY<-100*(avgCatch$AvgCatch/avgCatch$AvgMsy)
  
  for(b in 1:length(ids))
  {
    Data$MSY[Data$IdOrig==ids[b]]<-avgCatch$AvgCatch[avgCatch$IdOrig==ids[b]]
    
    Data$MSYUnit[Data$IdOrig==ids[b]]<-'Avg Annual Catch of Stock' 
#     show(b)
  }
  
  # Adjust Bmsy and K to reflect new MSY
  adjusted<-Data$MSYUnit %in% c('Reduced MSY by 25%','Avg Annual Catch of Stock') # index
  
  Data$Bmsy[adjusted]<-Data$MSY[adjusted]/Data$g[adjusted]
  
  Data$k[adjusted]<-Data$Bmsy[adjusted]/Data$BtoKRatio[adjusted]
  
  # Return MsyData with adjusted MSY
  
  return(Data)
}