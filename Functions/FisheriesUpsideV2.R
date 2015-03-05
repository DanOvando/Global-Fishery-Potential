############################################------------------------------------------------------
##
## Calculate upside of individual fisheries 
## and compare to status quo policy
## 
## Status quo policies are different for RAM, catch share, and unassessed stocks
##
##
############################################

# SubsetName<-'Overfish Only'
# LumpedName<-'Unlumped Projection Data'

FisheriesUpsideV2<-function(ProjectionData,BaselineYear,LumpedName,SubsetName)
{
    
  # Define data subsets to analyze with different status quo policies------------------------
  
  if(SubsetName=='All Stocks')
  {
    groups<-c('ram','cs','overFunderB','McToefids')
  }
  
  if(SubsetName=='Overfish Only')
  {
    groups<-c('ram','cs','overFunderB')
  }
  
  
  tempUpside<-list()
  
  tempTimeTrend<-list()
  
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
    
    if(groups[a]=='overFunderB') # all other stocks get Open Access
    {
      offids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & ProjectionData$Dbase!='RAM' & ProjectionData$CatchShare!=1 &
                                      ((ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy<1) | (ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy>1) |
                                         (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy<1))]
      
      Data<-ProjectionData[ProjectionData$IdOrig %in% offids,]
      
      DenominatorPolicy<-'StatusQuoOpenAccess'
    }
  
    if(SubsetName=='All Stocks' & groups[a]=='McToefids') # all other stocks get Open Access
    {
      Mctofids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & ProjectionData$Dbase!='RAM' & ProjectionData$CatchShare!=1 & 
                                        (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy>1)]
      
      Data<-ProjectionData[ProjectionData$IdOrig %in% Mctofids,]
      
      DenominatorPolicy<-'StatusQuoBForever'
    }
    
    # Choose to run upside on all stocks or just overfished
    
    if (SubsetName=='Overfish Only') 
    {
      OverfishIds<-Data$IdOrig[Data$Year==2012 & (Data$BvBmsy<1 | Data$FvFmsy>1)]
      
      TrevorDenomData<-Data
      
      Data<-Data[Data$IdOrig %in% OverfishIds,]
    }
    
    
    # Calculate fishery NPV's by policy
    
    ids<-unique(Data$IdOrig[Data$Year==2012])
    
    NPV<-ddply(Data[Data$IdOrig %in% ids & Data$Policy!='Historic',],c('Country','IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T),TotalFood=sum(Catch,na.rm=T))
    
    denominator<-NPV[NPV$Policy==DenominatorPolicy,c('Country','IdOrig','FinalNPV','TotalFood')]
    
    colnames(denominator)<-c('Country','IdOrig','NPVSQ','FoodSQ')
    
    NPV<-merge(NPV,denominator)
    
    NPV$PercChangeFromSQNPV<-100*((NPV$FinalNPV/NPV$NPVSQ)-1)
    
    NPV$AbsChangeFromSQNPV<-NPV$FinalNPV-NPV$NPVSQ
    
    NPV$PercChangeFromSQFood<-100*((NPV$TotalFood/NPV$FoodSQ)-1)
    
    NPV$AbsChangeFromSQFood<-NPV$TotalFood-NPV$FoodSQ
    
    
    # Calculate baseline metrics values 
    
    Baseline<-Data[Data$Year==BaselineYear & Data$Policy=='Historic',]
    
    # Analyze time trends in metrics
    
    TimeTrend<-Data[Data$Year>BaselineYear,c('IdOrig','Policy','Year','CommName','Country','BvBmsy','FvFmsy','MSY','Catch','Profits','DiscProfits','Biomass')]
    
    TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]
    
    # Add baseline values to TimeTrend. Values are all the same regardless of policy
    
    base<-Baseline[,c('IdOrig','CommName','Country','Catch','Profits','Biomass')]
    
    colnames(base)<-c('IdOrig','CommName','Country','BaselineCatch','BaselineProfits','BaselineBiomass')
    
    TimeTrend<-merge(TimeTrend,base)
    
    ### Calculate Upside Metrics----------------------------------------------------- 
      
      # Time trend values
      
    TimeTrend$PercChangeTotalProfits<-100*((TimeTrend$Profits/TimeTrend$BaselineProfits)-1) #Percent change in  profits from current
    
    TimeTrend$PercChangeTotalCatch<-100*((TimeTrend$Catch/TimeTrend$BaselineCatch)-1) #Percent change in  catch from current
    
    TimeTrend$PercChangeTotalBiomass<-100*((TimeTrend$Biomass/TimeTrend$BaselineBiomass)-1) #Percent change in  biomass from current
    
    TimeTrend$AbsChangeTotalProfits<-TimeTrend$Profits-TimeTrend$BaselineProfits #absolute change in  profits from current
    
    TimeTrend$AbsChangeTotalCatch<-TimeTrend$Catch-TimeTrend$BaselineCatch #Percent change in  catch from current
    
    TimeTrend$AbsChangeTotalBiomass<-TimeTrend$Biomass-TimeTrend$BaselineBiomass #Percent change in  biomass from current
    
    # Find steady state values for each fishery
    
    FinalYr<-TimeTrend[TimeTrend$Year==max(Data$Year,na.rm=T),]
    
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
    
    FinalYr<-merge(FinalYr,NPV)
    
    # Store subset results in list
    
    tempUpside[[a]]<-FinalYr
    
    tempTimeTrend[[a]]<-TimeTrend
    
    show(a)
  }
  
  FisheryUpside<-ldply(tempUpside,data.frame)
  
  TimeTrend<-ldply(tempTimeTrend,data.frame)
  
  ## Find fisheries satisfying triple bottom line----------------------------------------------------------
  
  FisheryUpside$TripBottomLine[FisheryUpside$PercChangeFromSQTotalCatch>0 & FisheryUpside$PercChangeFromSQTotalBiomass>0 &
                                   FisheryUpside$PercChangeFromSQNPV>0]<-TRUE
  
  PercentTripleBottom<-ddply(FisheryUpside,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
                             Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                             PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
  
  write.csv(FisheryUpside,file=paste(ResultFolder,LumpedName,SubsetName,' Fishery Upsides.csv',sep=''))
  
  ## Aggregate totals by country and recalculate percent upsides------------------------------------------
  
  CountryUpsides<-ddply(FisheryUpside,c('Country','Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                        TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
                        TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
                        TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T))
  
  CountryUpsides$PercChangeTotalProfits<-100*((CountryUpsides$TotalProfits/CountryUpsides$TotalBaselineProfits)-1) #Percent change in  profits from current
  
  CountryUpsides$PercChangeTotalCatch<-100*((CountryUpsides$TotalCatch/CountryUpsides$TotalBaselineCatch)-1) #Percent change in  catch from current
  
  CountryUpsides$PercChangeTotalBiomass<-100*((CountryUpsides$TotalBiomass/CountryUpsides$TotalBaselineBiomass)-1) #Percent change in  biomass from current
  
  CountryUpsides$AbsChangeTotalProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalBaselineProfits #absolute change in  profits from current
  
  CountryUpsides$AbsChangeTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalBaselineCatch #Percent change in  catch from current
  
  CountryUpsides$AbsChangeTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBaselineBiomass #Percent change in  biomass from current
  
  CountryUpsides$PercChangeFromSQTotalCatch<-100*((CountryUpsides$TotalCatch/CountryUpsides$TotalCatchSQ)-1)
  
  CountryUpsides$PercChangeFromSQTotalBiomass<-100*((CountryUpsides$TotalBiomass/CountryUpsides$TotalBiomassSQ)-1)
  
  CountryUpsides$PercChangeFromSQProfits<-100*((CountryUpsides$TotalProfits/CountryUpsides$TotalProfitsSQ)-1)
  
  CountryUpsides$PercChangeFromSQNPV<-100*((CountryUpsides$TotalNPV/CountryUpsides$TotalNPVSQ)-1)
  
  CountryUpsides$PercChangeFromSQFood<-100*((CountryUpsides$TotalFood/CountryUpsides$TotalFoodSQ)-1)
  
  CountryUpsides$AbsChangeFromSQTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalCatchSQ
  
  CountryUpsides$AbsChangeFromSQTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBiomassSQ
  
  CountryUpsides$AbsChangeFromSQProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalProfitsSQ
  
  CountryUpsides$AbsChangeFromSQNPV<-CountryUpsides$TotalNPV-CountryUpsides$TotalNPVSQ
  
  CountryUpsides$AbsChangeFromSQFood<-CountryUpsides$TotalFood-CountryUpsides$TotalFoodSQ
  
  write.csv(CountryUpsides,file=paste(ResultFolder,LumpedName,SubsetName,' Country Upsides.csv',sep=''))
  
  # Global--------------------------------------
  
  GlobalUpsides<-ddply(FisheryUpside,c('Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                        TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
                        TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
                        TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T))
  
  GlobalUpsides$PercChangeTotalProfits<-100*((GlobalUpsides$TotalProfits/GlobalUpsides$TotalBaselineProfits)-1) #Percent change in  profits from current
  
  GlobalUpsides$PercChangeTotalCatch<-100*((GlobalUpsides$TotalCatch/GlobalUpsides$TotalBaselineCatch)-1) #Percent change in  catch from current
  
  GlobalUpsides$PercChangeTotalBiomass<-100*((GlobalUpsides$TotalBiomass/GlobalUpsides$TotalBaselineBiomass)-1) #Percent change in  biomass from current
  
  GlobalUpsides$AbsChangeTotalProfits<-GlobalUpsides$TotalProfits-GlobalUpsides$TotalBaselineProfits # Absolute change in  profits from current
  
  GlobalUpsides$AbsChangeTotalCatch<-GlobalUpsides$TotalCatch-GlobalUpsides$TotalBaselineCatch # Absolute change in  catch from current
  
  GlobalUpsides$AbsChangeTotalBiomass<-GlobalUpsides$TotalBiomass-GlobalUpsides$TotalBaselineBiomass # Absolute change in  biomass from current
  
  GlobalUpsides$PercChangeFromSQTotalCatch<-100*((GlobalUpsides$TotalCatch/GlobalUpsides$TotalCatchSQ)-1)
  
  GlobalUpsides$PercChangeFromSQTotalBiomass<-100*((GlobalUpsides$TotalBiomass/GlobalUpsides$TotalBiomassSQ)-1)
  
  GlobalUpsides$PercChangeFromSQProfits<-100*((GlobalUpsides$TotalProfits/GlobalUpsides$TotalProfitsSQ)-1)
  
  GlobalUpsides$PercChangeFromSQNPV<-100*((GlobalUpsides$TotalNPV/GlobalUpsides$TotalNPVSQ)-1)
  
  GlobalUpsides$PercChangeFromSQFood<-100*((GlobalUpsides$TotalFood/GlobalUpsides$TotalFoodSQ)-1)
  
  GlobalUpsides$AbsChangeFromSQTotalCatch<-GlobalUpsides$TotalCatch-GlobalUpsides$TotalCatchSQ
  
  GlobalUpsides$AbsChangeFromSQTotalBiomass<-GlobalUpsides$TotalBiomass-GlobalUpsides$TotalBiomassSQ
  
  GlobalUpsides$AbsChangeFromSQProfits<-GlobalUpsides$TotalProfits-GlobalUpsides$TotalProfitsSQ
  
  GlobalUpsides$AbsChangeFromSQNPV<-GlobalUpsides$TotalNPV-GlobalUpsides$TotalNPVSQ
  
  GlobalUpsides$AbsChangeFromSQFood<-GlobalUpsides$TotalFood-GlobalUpsides$TotalFoodSQ
  
  write.csv(GlobalUpsides,file=paste(ResultFolder,LumpedName,SubsetName,' Global Upsides.csv',sep=''))
  
  
  return(list(FisheryUpside=FisheryUpside,TripBottomLine=PercentTripleBottom,CountryUpsides=CountryUpsides,GlobalUpside=GlobalUpsides))
  
} # close function
