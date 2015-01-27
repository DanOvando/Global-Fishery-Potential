FisheriesUpside<-function(ProjectionData,DenominatorPolicy)
{
  ids<-unique(ProjectionData$IdOrig[ProjectionData$Year==2012])
    
  for(a in 1:length(ids))
  {
    show(a)
    
    temp<-ProjectionData[ProjectionData$IdOrig==ids[a],]
    
    NPV<-ddply(temp,c('IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T))
    
    NPV$PercChangeFromSQ<-100*((NPV$FinalNPV/NPV$FinalNPV[NPV$Policy==DenominatorPolicy])-1)
    
    FinalYr<-temp[temp$Year==max(temp$Year),c('IdOrig','Year','Policy','Catch','Biomass','BvBmsy')]
    
    FinalYr$PercChangeFromSQTotalCatch<-100*((FinalYr$Catch/FinalYr$Catch[FinalYr$Policy==DenominatorPolicy])-1)
    
    FinalYr$PercChangeFromSQTotalBiomass<-100*((FinalYr$Biomass/FinalYr$Biomass[FinalYr$Policy==DenominatorPolicy])-1)
    
    FinalYr$Recovered[FinalYr$BvBmsy>=0.95]<-TRUE
    
    FinalYr$Recovered[FinalYr$BvBmsy<0.95]<-FALSE
    
    # add in NPV values
    for (b in 1:nrow(FinalYr))
    {
      FinalYr$PercChangeFromSQNPV[b]<-NPV$PercChangeFromSQ[NPV$Policy==FinalYr$Policy[b]]
    } # close for NPV
    
    if(a==1)
    {
      FisheriesUpside<-FinalYr
    } # close if
    
    if(a>1)
    {
      FisheriesUpside<-rbind(FisheriesUpside,FinalYr)
    } # close if
    
  } # close ids loop
  
  FisheriesUpside$TripBottomLine[FisheriesUpside$PercChangeFromSQTotalCatch>0 & FisheriesUpside$PercChangeFromSQTotalBiomass>0 &
                                   FisheriesUpside$PercChangeFromSQNPV>0]<-TRUE
  
  PercentTripleBottom<-ddply(FisheriesUpside,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
                             PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
  
  write.csv(FisheriesUpside,file=paste(ResultFolder,'Fishery Upsides.csv',sep=''))
  
  return(list(FisheriesUpside=FisheriesUpside,TripBottomLine=PercentTripleBottom))
  
} # close function
