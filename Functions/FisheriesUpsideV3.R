##################################################################---------------------------------------------------------------------
#
# Function to calculate the upside of multiple fishery recovery policies
#
# Tyler Clavelle
# Revised: 5/27/14
#
# Code description: This code calculates percent and absolute upsides of each recovery policy in the GFR analysis relative to a single
# denominator policy (Currently BAU).
#
##################################################################---------------------------------------------------------------------

# Data<-UnlumpedProjectionData
# DenominatorPolicy<-'Business As Usual Optimistic'
# RecoveryThreshold<-0.8
# SubsetName<-'All Stocks'

FisheriesUpsideV3<-function(Data,BaselineYear,DenominatorPolicy,RecoveryThreshold,LumpedName,SubsetName,IncludeNEIs)
{
  
  # Choose to run upside on all stocks or just overfished
  if (SubsetName=='Overfish Only') 
  {
    OverfishIds<-Data$IdOrig[Data$Year==BaselineYear & (Data$BvBmsy<1 | Data$FvFmsy>1)]
    
    Data<-Data[Data$IdOrig %in% OverfishIds,]
  }
  
  # Write function to calculate % change
  PercChange<- function(A,B)
  {
    PC<- ((A-B)/(B))*100*sign(B)
    
    PC[B<=0 & (A-B)>0]<- 999
    
    PC[B<=0 & (A-B)<=0]<- -999
    
    return(PC)
  }
  
  # Calculate fishery NPV's by policy
  ids<- unique(Data$IdOrig[Data$Year==BaselineYear])

#     NPV<-ddply(Data[Data$IdOrig %in% ids & Data$Policy!='Historic',],c('Country','IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T),TotalFood=sum(Catch,na.rm=T))

  NPV<- subset(Data, (IdOrig %in% ids) & Policy!='Historic') %>%
    group_by(Country,IdOrig,Policy) %>%
    summarize(FinalNPV=sum(DiscProfits,na.rm=T),TotalFood=sum(Catch,na.rm=T))
    

  denominator<-NPV[NPV$Policy==DenominatorPolicy,c('Country','IdOrig','FinalNPV','TotalFood')]
  
  colnames(denominator)<-c('Country','IdOrig','NPVSQ','FoodSQ')
  
  NPV<-merge(NPV,denominator)
  
  # Calculate % and absolute upsides in NPV
  NPV$PercChangeFromSQNPV<- PercChange(NPV$FinalNPV,NPV$NPVSQ)
  
  NPV$AbsChangeFromSQNPV<-NPV$FinalNPV-NPV$NPVSQ
  
  NPV$PercChangeFromSQFood<-PercChange(NPV$TotalFood,NPV$FoodSQ)
  
  NPV$AbsChangeFromSQFood<-NPV$TotalFood-NPV$FoodSQ
  
  # Calculate baseline metrics values 
  Baseline<-Data[Data$Year==BaselineYear & Data$Policy=='Historic',]
  
  # Analyze time trends in metrics
  TimeTrend<-Data[Data$Year>BaselineYear,c('IdOrig','Policy','Year','CommName','IdLevel','Country','BvBmsy','FvFmsy','MSY','Catch','Profits','DiscProfits','Biomass')]
  
  TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]
  
  # Add baseline values to TimeTrend. Values are all the same regardless of policy
  
  base<-Baseline[,c('IdOrig','CommName','Country','BvBmsy','FvFmsy','Catch','Profits','Biomass')]
  
  colnames(base)<-c('IdOrig','CommName','Country','BaselineBvBmsy','BaselineFvFmsy','BaselineCatch','BaselineProfits','BaselineBiomass')
  
  TimeTrend<-merge(TimeTrend,base)
  
  TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]
  
  ### Calculate Fishery Upside Metrics----------------------------------------------------- 
  
  ## Time trend values 
  
  # % upside relative to today
  TimeTrend$PercChangeTotalProfits<- PercChange(TimeTrend$Profits,TimeTrend$BaselineProfits)  #Percent change in  profits from current
  
  TimeTrend$PercChangeTotalCatch<- PercChange(TimeTrend$Catch,TimeTrend$BaselineCatch) #Percent change in  catch from current

  TimeTrend$PercChangeTotalBiomass<- PercChange(TimeTrend$Biomass,TimeTrend$BaselineBiomass) #Percent change in  biomass from current
  
  # Absolute upside relative to today
  TimeTrend$AbsChangeTotalProfits<-TimeTrend$Profits-TimeTrend$BaselineProfits #absolute change in  profits from current
  
  TimeTrend$AbsChangeTotalCatch<-TimeTrend$Catch-TimeTrend$BaselineCatch #Percent change in  catch from current
  
  TimeTrend$AbsChangeTotalBiomass<-TimeTrend$Biomass-TimeTrend$BaselineBiomass #Percent change in  biomass from current
  
  # Add in SQ values
  denominator2<-TimeTrend[TimeTrend$Policy==DenominatorPolicy,c('Country','IdOrig','Year','Catch','Biomass','Profits','BvBmsy')]
  
  colnames(denominator2)<-c('Country','IdOrig','Year','CatchSQ','BiomassSQ','ProfitsSQ','BvBmsySQ')
  
  TimeTrend<-merge(TimeTrend,denominator2)
  
  TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]
  
  # % relative to SQ  
  TimeTrend$PercChangeFromSQTotalCatch<-PercChange(TimeTrend$Catch,TimeTrend$CatchSQ)
  
  TimeTrend$PercChangeFromSQTotalBiomass<- PercChange(TimeTrend$Biomass,TimeTrend$BiomassSQ)
  
  TimeTrend$PercChangeFromSQProfits<-PercChange(TimeTrend$Profits,TimeTrend$ProfitsSQ)
  
  # Absolute relative to SQ
  TimeTrend$AbsChangeFromSQTotalCatch<-TimeTrend$Catch-TimeTrend$CatchSQ
  
  TimeTrend$AbsChangeFromSQTotalBiomass<-TimeTrend$Biomass-TimeTrend$BiomassSQ
  
  TimeTrend$AbsChangeFromSQProfits<-TimeTrend$Profits-TimeTrend$ProfitsSQ
  
  # Recovered or not
  TimeTrend$Recovered[TimeTrend$BvBmsy>=RecoveryThreshold]<-TRUE
  
  TimeTrend$Recovered[TimeTrend$BvBmsy<RecoveryThreshold]<-FALSE
  
  # Indicate the denominator policy used to make all the SQ calculations
  TimeTrend$DenominatorPolicy<-DenominatorPolicy
  
  # ***If desired later plot and calculate triple bottom line trajectories here***
  
  # Find steady state values for each fishery
  FinalYr<-TimeTrend[TimeTrend$Year==max(Data$Year,na.rm=T),]
  
  # Add in NPV values
  FinalYr<-merge(FinalYr,NPV)
  
  FisheryUpside<-FinalYr
  
  # Find fisheries satisfying triple bottom line over time
  FisheryUpside$TripBottomLine[FisheryUpside$PercChangeFromSQTotalCatch>0 & FisheryUpside$PercChangeFromSQTotalBiomass>0 &
                                 FisheryUpside$PercChangeFromSQNPV>0]<-TRUE

  # write csv
  write.csv(FisheryUpside,file=paste(ResultFolder,LumpedName,SubsetName,' Fishery Upsides.csv',sep=''))

  PercentTripleBottom<- FisheryUpside %>%
    group_by(Policy) %>%
    summarize(PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
              Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
              PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
    
#     ddply(FisheryUpside,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
#                              Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
#                              PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
#   
    
  PercentTripleBottom<- FisheryUpside %>%
    group_by(Policy) %>%
    summarize(PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
              Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
              PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
    
#     ddply(FisheryUpside,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
#                              Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
#                              PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
#   
#   PercentTripleBottom<-ddply(FisheryUpside,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
#                              Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
#                              PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
#   
  #######################################################################################################
  #
  # Aggregate totals by country and recalculate percent upsides------------------------------------------
  #
  #######################################################################################################
  
  # Aggregate by country
  
  CountryUpsides<- FisheryUpside %>%
    group_by(Country,Policy) %>%
    summarize(TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
              TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
              TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
              TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
    
#     ddply(FisheryUpside,c('Country','Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
#                         TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
#                         TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
#                         TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
#   
  
#   CountryUpsides<-ddply(FisheryUpside,c('Country','Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
#                         TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
#                         TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
#                         TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
#   
  # % change from today
  CountryUpsides$PercChangeTotalProfits<-PercChange(CountryUpsides$TotalProfits,CountryUpsides$TotalBaselineProfits)#Percent change in  profits from current
  
  CountryUpsides$PercChangeTotalCatch<- PercChange(CountryUpsides$TotalCatch,CountryUpsides$TotalBaselineCatch)#Percent change in  catch from current
  
  CountryUpsides$PercChangeTotalBiomass<- PercChange(CountryUpsides$TotalBiomass,CountryUpsides$TotalBaselineBiomass) #Percent change in  biomass from current
  
  # Absolute change from today
  CountryUpsides$AbsChangeTotalProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalBaselineProfits #absolute change in  profits from current
  
  CountryUpsides$AbsChangeTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalBaselineCatch #Percent change in  catch from current
  
  CountryUpsides$AbsChangeTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBaselineBiomass #Percent change in  biomass from current
  
  # % change from SQ
  CountryUpsides$PercChangeFromSQTotalCatch<- PercChange(CountryUpsides$TotalCatch,CountryUpsides$TotalCatchSQ)
  
  CountryUpsides$PercChangeFromSQTotalBiomass<- PercChange(CountryUpsides$TotalBiomass,CountryUpsides$TotalBiomassSQ)
  
  CountryUpsides$PercChangeFromSQProfits<- PercChange(CountryUpsides$TotalProfits,CountryUpsides$TotalProfitsSQ)
  
  CountryUpsides$PercChangeFromSQNPV<- PercChange(CountryUpsides$TotalNPV,CountryUpsides$TotalNPVSQ)
  
  CountryUpsides$PercChangeFromSQFood<- PercChange(CountryUpsides$TotalFood,CountryUpsides$TotalFoodSQ)
  
  # Absolute change from SQ
  CountryUpsides$AbsChangeFromSQTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalCatchSQ
  
  CountryUpsides$AbsChangeFromSQTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBiomassSQ
  
  CountryUpsides$AbsChangeFromSQProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalProfitsSQ
  
  CountryUpsides$AbsChangeFromSQNPV<-CountryUpsides$TotalNPV-CountryUpsides$TotalNPVSQ
  
  CountryUpsides$AbsChangeFromSQFood<-CountryUpsides$TotalFood-CountryUpsides$TotalFoodSQ
  
  # Aggregate by country and ID level to examine importance of NEIs
  if(IncludeNEIs==TRUE)
  {
#     NeiUpsides<-ddply(FisheryUpside,c('Country','Policy','IdLevel'),summarize,TotalProfits=sum(Profits,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
#                           TotalProfitsSQ=sum(ProfitsSQ,na.rm=T), AbsChangeFromSQProfits=TotalProfits-TotalProfitsSQ, AbsChangeTotalProfits=TotalProfits-TotalBaselineProfits)
#     
    NeiUpsides<- FisheryUpside %>%
      group_by(Country,Policy,IdLevel) %>%
      summarize(TotalProfits=sum(Profits,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
                TotalProfitsSQ=sum(ProfitsSQ,na.rm=T), AbsChangeFromSQProfits=TotalProfits-TotalProfitsSQ, 
                AbsChangeTotalProfits=TotalProfits-TotalBaselineProfits)
      
#       ddply(FisheryUpside,c('Country','Policy','IdLevel'),summarize,TotalProfits=sum(Profits,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
#                       TotalProfitsSQ=sum(ProfitsSQ,na.rm=T), AbsChangeFromSQProfits=TotalProfits-TotalProfitsSQ, AbsChangeTotalProfits=TotalProfits-TotalBaselineProfits)
#     
    
    # Abs upside in profits from NEIs
#     NeiUpsides<-ddply(NeiUpsides,c('Country','Policy'),mutate,TotalProfitsUpside=sum(AbsChangeTotalProfits,na.rm=T),
#                       TotalProfitsUpsideFromSQ=sum(AbsChangeFromSQProfits,na.rm=T),PercOfUpside=100*(AbsChangeTotalProfits/TotalProfitsUpside),
#                       PercOfUpsideFromSQ=100*(AbsChangeFromSQProfits/TotalProfitsUpsideFromSQ))
#     
    NeiUpsides<- NeiUpsides %>%
      group_by(Country,Policy) %>%
      mutate(TotalProfitsUpside=sum(AbsChangeTotalProfits,na.rm=T),
             TotalProfitsUpsideFromSQ=sum(AbsChangeFromSQProfits,na.rm=T),PercOfUpside=100*(AbsChangeTotalProfits/TotalProfitsUpside),
             PercOfUpsideFromSQ=100*(AbsChangeFromSQProfits/TotalProfitsUpsideFromSQ))
      
#       ddply(NeiUpsides,c('Country','Policy'),mutate,TotalProfitsUpside=sum(AbsChangeTotalProfits,na.rm=T),
#                       TotalProfitsUpsideFromSQ=sum(AbsChangeFromSQProfits,na.rm=T),PercOfUpside=100*(AbsChangeTotalProfits/TotalProfitsUpside),
#                       PercOfUpsideFromSQ=100*(AbsChangeFromSQProfits/TotalProfitsUpsideFromSQ))
#     
#     
    # Find countries where over 50% of profit increase relative to SQ for Catch Share Three comes from NEIs 
    NeiUpsides$NeiUpsideSQOver50<-FALSE
    
    NeiUpsides$NeiUpsideSQOver50[NeiUpsides$IdLevel=='Neis' & NeiUpsides$PercOfUpsideFromSQ>50]<-TRUE
    
    # Sort by profit upside relative to BAU by and save csv of top 20 resutls for Catch Share Three. 10 of these countries should correspond to Fig 2
    NeiProfitResults<-NeiUpsides[NeiUpsides$IdLevel=='Neis' & NeiUpsides$Policy=='Catch Share Three',c('Country','Policy','IdLevel','AbsChangeFromSQProfits',
                                                                                                       'PercOfUpsideFromSQ','TotalProfitsUpsideFromSQ')]
    
    NeiProfitResults<-NeiProfitResults[with(NeiProfitResults,order(-TotalProfitsUpsideFromSQ)),]
    
    # Relabele policy and scenario to match text
    NeiProfitResults$Scenario<-'Conservation Concern'
    
    NeiProfitResults$Policy<-'RBFM'
    
    colnames(NeiProfitResults)<-c('Country','Policy','Identification Level','Profit Upside Relative to BAU from NEIs','Percent of Total Profit Upside from NEIs','Total Profit Upside','Scenario')
    
    NeiProfitResults<-NeiProfitResults[,c('Country','Policy','Scenario','Identification Level','Profit Upside Relative to BAU from NEIs','Total Profit Upside','Percent of Total Profit Upside from NEIs')]
    
#     NeiProfitResults<-NeiProfitResults[1:20,]
    
    write.csv(file=paste(ResultFolder,LumpedName,SubsetName,' Profit Upsides From NEIs.csv',sep=''),NeiProfitResults)
    
    # Save table of countries with over 50% profit upside from NEIs to use for indicating in Figure 2
    NeiCntrys<-NeiUpsides[NeiUpsides$IdLevel=='Neis',c('Country','Policy','NeiUpsideSQOver50')]
    
  #   ggplot(NeiUpsides[NeiUpsides$Policy %in% c('Catch Share Three','CatchShare'),],aes(x=PercOfUpsideFromSQ,fill=IdLevel)) +
  #     geom_density(alpha=0.6) +
  #     facet_wrap(~Policy)
    
    # Label countries with high NEI percentage in CountryUpsides dataset
    # NOTE: this is only accurate for Catch Share Three because there are no negatives. Will be wrong for other policies where there could be negative profits
    # relative to SQ
    CountryUpsides<-join(CountryUpsides,NeiCntrys,by=c('Country','Policy'))
  
    CountryUpsides$NeiUpsideSQOver50[is.na(CountryUpsides$NeiUpsideSQOver50)]<-FALSE

} # close NEIs loop

  # write csv
  write.csv(CountryUpsides,file=paste(ResultFolder,LumpedName,SubsetName,' Country Upsides.csv',sep=''))
  
#######################################################################################################
#
# Aggregate global totals and recalculate upsides------------------------------------------------------
#
#######################################################################################################

  GlobalUpsides<- FisheryUpside %>%
    group_by(Policy) %>%
    summarize(TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
              TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
              TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
              TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
    
    
#    GlobalUpsides<-ddply(FisheryUpside,c('Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
#                        TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
#                        TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
#                        TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
#   
  # % upside relative to today   
  GlobalUpsides$PercChangeTotalProfits<-PercChange(GlobalUpsides$TotalProfits,GlobalUpsides$TotalBaselineProfits) #Percent change in  profits from current
  
  GlobalUpsides$PercChangeTotalCatch<- PercChange(GlobalUpsides$TotalCatch,GlobalUpsides$TotalBaselineCatch) #Percent change in  catch from current
  
  GlobalUpsides$PercChangeTotalBiomass<- PercChange(GlobalUpsides$TotalBiomass,GlobalUpsides$TotalBaselineBiomass) #Percent change in  biomass from current
  
  # Absolute upside relative to today
  GlobalUpsides$AbsChangeTotalProfits<-GlobalUpsides$TotalProfits-GlobalUpsides$TotalBaselineProfits # Absolute change in  profits from current
  
  GlobalUpsides$AbsChangeTotalCatch<-GlobalUpsides$TotalCatch-GlobalUpsides$TotalBaselineCatch # Absolute change in  catch from current
  
  GlobalUpsides$AbsChangeTotalBiomass<-GlobalUpsides$TotalBiomass-GlobalUpsides$TotalBaselineBiomass # Absolute change in  biomass from current
  
  # % upside relative to SQ
  GlobalUpsides$PercChangeFromSQTotalCatch<- PercChange(GlobalUpsides$TotalCatch,GlobalUpsides$TotalCatchSQ)
  
  GlobalUpsides$PercChangeFromSQTotalBiomass<- PercChange(GlobalUpsides$TotalBiomass,GlobalUpsides$TotalBiomassSQ)
  
  GlobalUpsides$PercChangeFromSQProfits<- PercChange(GlobalUpsides$TotalProfits,GlobalUpsides$TotalProfitsSQ)
  
  GlobalUpsides$PercChangeFromSQNPV<- PercChange(GlobalUpsides$TotalNPV,GlobalUpsides$TotalNPVSQ)
  
  GlobalUpsides$PercChangeFromSQFood<- PercChange(GlobalUpsides$TotalFood,GlobalUpsides$TotalFoodSQ)
  
  # Absolute upside relative to SQ
  GlobalUpsides$AbsChangeFromSQTotalCatch<-GlobalUpsides$TotalCatch-GlobalUpsides$TotalCatchSQ
  
  GlobalUpsides$AbsChangeFromSQTotalBiomass<-GlobalUpsides$TotalBiomass-GlobalUpsides$TotalBiomassSQ
  
  GlobalUpsides$AbsChangeFromSQProfits<-GlobalUpsides$TotalProfits-GlobalUpsides$TotalProfitsSQ
  
  GlobalUpsides$AbsChangeFromSQNPV<-GlobalUpsides$TotalNPV-GlobalUpsides$TotalNPVSQ
  
  GlobalUpsides$AbsChangeFromSQFood<-GlobalUpsides$TotalFood-GlobalUpsides$TotalFoodSQ
  
  # write csv
  write.csv(GlobalUpsides,file=paste(ResultFolder,LumpedName,SubsetName,' Global Upsides.csv',sep=''))
  
  # return list of tables
  return(list(FisheryUpside=FisheryUpside,TimeTrend=TimeTrend,TripBottomLine=PercentTripleBottom,CountryUpsides=CountryUpsides,GlobalUpside=GlobalUpsides))
  
} # close function

