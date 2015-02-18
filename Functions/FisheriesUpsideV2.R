############################################------------------------------------------------------
##
## Calculate upside of individual fisheries 
## and compare to status quo policy
##
############################################


FisheriesUpsideV2<-function(ProjectionData)
{
  # Define data subsets to analyze with different status quo policies------------------------
  
  groups<-c('ram','cs','other')
  
  # Loop over subsets and calculate upside---------------------------------------------------
  
  tempUpside<-list()
  
  for(a in 1:length(groups)) 
  {
    if(groups[a]=='ram') # RAM gets Fmsy policy
    {
      Data<-ProjectionData[ProjectionData$Dbase=='RAM' & ProjectionData$CatchShare!=1,]
      
      DenominatorPolicy<-'Fmsy'
    }
    
    if(groups[a]=='cs') # Known catch share stocks get Optimal policy
    {
      Data<-ProjectionData[ProjectionData$CatchShare==1,]
      
      DenominatorPolicy<-'Opt'
    }
    
    if(groups[a]=='other') # all other stocks get Open Access
    {
      Data<-ProjectionData[ProjectionData$Dbase!='RAM' & ProjectionData$CatchShare!=1,]
      
      DenominatorPolicy<-'StatusQuoOpenAccess'
    }
  
    # Calculate fishery NPV's by policy
    
    ids<-unique(Data$IdOrig[Data$Year==2012])
    
    NPV<-ddply(Data[Data$IdOrig %in% ids,],c('Country','IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T))
    
    denominator<-NPV[NPV$Policy==DenominatorPolicy,c('Country','IdOrig','FinalNPV')]
    
    colnames(denominator)<-c('Country','IdOrig','DenominatorNPV')
    
    NPV<-merge(NPV,denominator)
    
    NPV$PercChangeFromSQ<-100*((NPV$FinalNPV/NPV$DenominatorNPV)-1)
    
    NPV$AbsChangeFromSQ<-NPV$FinalNPV-NPV$DenominatorNPV
    
    # Find steady state values for each fishery
    
    FinalYr<-Data[Data$Year==max(Data$Year),c('Country','IdOrig','Year','Policy','Catch','Biomass','Profits','BvBmsy')]
    
    denominator2<-FinalYr[FinalYr$Policy==DenominatorPolicy,c('Country','IdOrig','Year','Catch','Biomass','Profits','BvBmsy')]
    
    colnames(denominator2)<-c('Country','IdOrig','Year','CatchSQ','BiomassSQ','ProfitsSQ','BvBmsySQ')
    
    FinalYr<-merge(FinalYr,denominator2)
    
    FinalYr$PercChangeFromSQTotalCatch<-100*((FinalYr$Catch/FinalYr$CatchSQ)-1)
    
    FinalYr$PercChangeFromSQTotalBiomass<-100*((FinalYr$Biomass/FinalYr$BiomassSQ)-1)
    
    FinalYr$PercChangeFromSQProfits<-100*((FinalYr$Profits/FinalYr$ProfitsSQ)-1)
    
    FinalYr$AbsChangeFromSQTotalCatch<-FinalYr$Catch-FinalYr$CatchSQ
    
    FinalYr$AbsChangeFromSQTotalBiomass<-FinalYr$Biomass-FinalYr$BiomassSQ
    
    FinalYr$AbsChangeFromSQProfits<-FinalYr$Profits-FinalYr$ProfitsSQ
    
    FinalYr$Recovered[FinalYr$BvBmsy>=0.95]<-TRUE
    
    FinalYr$Recovered[FinalYr$BvBmsy<0.95]<-FALSE
    
    # Add in NPV values
    
    policy<-unique(FinalYr$Policy)
    
    for(d in 1:length(policy))
    {
      FinalYr$NPV[FinalYr$Policy==policy[d]]<-NPV$FinalNPV[NPV$Policy==policy[d]]
      
      FinalYr$PercChangeFromSQNPV[FinalYr$Policy==policy[d]]<-NPV$PercChangeFromSQ[NPV$Policy==policy[d]]
      
      FinalYr$AbsChangeFromSQNPV[FinalYr$Policy==policy[d]]<-NPV$AbsChangeFromSQ[NPV$Policy==policy[d]]
    }
    
    for(e in 1:length(ids))
    {
      FinalYr$NPVSQ[FinalYr$IdOrig==ids[e]]<-NPV$FinalNPV[NPV$IdOrig==ids[e] & NPV$Policy==DenominatorPolicy]
    }
    
    tempUpside[[a]]<-FinalYr
    
  }
  
  FinalYr<-ldply(tempUpside,data.frame)
  
  ## Find fisheries satisfying triple bottom line----------------------------------------------------------
  
  FinalYr$TripBottomLine[FinalYr$PercChangeFromSQTotalCatch>0 & FinalYr$PercChangeFromSQTotalBiomass>0 &
                                   FinalYr$PercChangeFromSQNPV>0]<-TRUE
  
  PercentTripleBottom<-ddply(FinalYr,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
                             Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                             PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
  
  write.csv(FinalYr,file=paste(ResultFolder,'Fishery Upsides.csv',sep=''))
  
  ## Aggregate totals by country and recalculate percent upsides------------------------------------------
  
  CountryUpsides<-ddply(FinalYr,c('Country','Policy','Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                        TotalNPV=sum(NPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),TotalNPVSQ=sum(NPVSQ,na.rm=T))
  
  CountryUpsides$PercChangeFromSQTotalCatch<-100*((CountryUpsides$TotalCatch/CountryUpsides$TotalCatchSQ)-1)
  
  CountryUpsides$PercChangeFromSQTotalBiomass<-100*((CountryUpsides$TotalBiomass/CountryUpsides$TotalBiomassSQ)-1)
  
  CountryUpsides$PercChangeFromSQProfits<-100*((CountryUpsides$TotalProfits/CountryUpsides$TotalProfitsSQ)-1)
  
  CountryUpsides$PercChangeFromSQNPV<-100*((CountryUpsides$TotalNPV/CountryUpsides$TotalNPVSQ)-1)
  
  CountryUpsides$AbsChangeFromSQTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalCatchSQ
  
  CountryUpsides$AbsChangeFromSQTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBiomassSQ
  
  CountryUpsides$AbsChangeFromSQProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalProfitsSQ
  
  CountryUpsides$AbsChangeFromSQNPV<-CountryUpsides$TotalNPV-CountryUpsides$TotalNPVSQ
  
  return(list(FinalYr=FinalYr,TripBottomLine=PercentTripleBottom,CountryUpsides=CountryUpsides))
  
} # close function
