FisheriesUpsideV2<-function(ProjectionData,DenominatorPolicy)
{
  # Calculate fishery NPV's by policy
  
  ids<-unique(ProjectionData$IdOrig[ProjectionData$Year==2012])
  
  NPV<-ddply(ProjectionData[ProjectionData$IdOrig %in% ids,],c('IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T))
  
  denominator<-NPV[NPV$Policy==DenominatorPolicy,c('IdOrig','FinalNPV')]
  
  colnames(denominator)<-c('IdOrig','DenominatorNPV')
  
  NPV<-merge(NPV,denominator)
  
  NPV$PercChangeFromSQ<-100*((NPV$FinalNPV/NPV$DenominatorNPV)-1)
  
  # Find steady state values for each fishery
  
  FinalYr<-ProjectionData[ProjectionData$Year==max(ProjectionData$Year),c('IdOrig','Year','Policy','Catch','Biomass','BvBmsy')]
  
  denominator2<-FinalYr[FinalYr$Policy==DenominatorPolicy,c('IdOrig','Year','Catch','Biomass','BvBmsy')]
  
  colnames(denominator2)<-c('IdOrig','Year','CatchSQ','BiomassSQ','BvBmsySQ')
  
  FinalYr<-merge(FinalYr,denominator2)
  
  FinalYr$PercChangeFromSQTotalCatch<-100*((FinalYr$Catch/FinalYr$CatchSQ)-1)
  
  FinalYr$PercChangeFromSQTotalBiomass<-100*((FinalYr$Biomass/FinalYr$BiomassSQ)-1)
  
  FinalYr$Recovered[FinalYr$BvBmsy>=0.95]<-TRUE
  
  FinalYr$Recovered[FinalYr$BvBmsy<0.95]<-FALSE
  
  # Add in NPV values
  
  policy<-unique(FinalYr$Policy)
  
  for(d in 1:length(policy))
  {
    FinalYr$NPV[FinalYr$Policy==policy[d]]<-NPV$FinalNPV[NPV$Policy==policy[d]]
    
    FinalYr$PercChangeFromSQNPV[FinalYr$Policy==policy[d]]<-NPV$PercChangeFromSQ[NPV$Policy==policy[d]]
  }
  
  # Find fisheries satisfying triple bottom line
  
  FinalYr$TripBottomLine[FinalYr$PercChangeFromSQTotalCatch>0 & FinalYr$PercChangeFromSQTotalBiomass>0 &
                                   FinalYr$PercChangeFromSQNPV>0]<-TRUE
  
  PercentTripleBottom<-ddply(FinalYr,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
                             PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
  
  write.csv(FinalYr,file=paste(ResultFolder,'Fishery Upsides.csv',sep=''))
  
  return(list(FinalYr=FinalYr,TripBottomLine=PercentTripleBottom))
  
} # close function
