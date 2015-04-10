
# Data<-ProjectionData
# DenominatorPolicy<-'Business As Usual Optimistic'
# RecoveryThreshold<-0.8
# SubsetName<-'All Stocks'


##########HERE's the code
#ProjectionData <- read.csv("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/Unlumped Projection Data.csv")
#BaselineYear <- 2012
#DenominatorPolicy <- 'Business As Usual'
#RecoveryThreshold<-0.8
#SubsetName<-'All Stocks'
#library(plyr)

#size<-dim(Data)[1]
#for (i in 1:size) {
#if (Data$Year[i]==2050){
#w<-subset(FisheryUpside, FisheryUpside$IdOrig==Data$IdOrig[i] & FisheryUpside$Policy==Data$Policy[i] & FisheryUpside$Year==Data$Year[i],select=c(FinalNPV))
#if (w>0){Data$Id[i]<-w} else {Data$Id[i]<-0}
#}
#}
#Data$Id[1:1000]
#save(Data, file = "//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/UDataNPV.RData")
#load("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/UDataNPV.RData")
#############################

FisheriesUpsideV3_REN<-function(Data,BaselineYear,DenominatorPolicy,RecoveryThreshold,LumpedName,SubsetName)
{
#   Data<-ProjectionData
  ###Choose to run upside on all stocks or just overfished
  if (SubsetName=='Overfish Only') 
  {
    OverfishIds<-Data$IdOrig[Data$Year==BaselineYear & (Data$BvBmsy<1 | Data$FvFmsy>1)]  
    Data<-Data[Data$IdOrig %in% OverfishIds,]
  }
  
  PercChange<- function(A,B)
  {
    PC<- ((A-B)/(B))*100*sign(B) 
    return(PC)
  }

  ### Calculate fishery NPV's by policy
  
  ids<-unique(Data$IdOrig[Data$Year==BaselineYear])
  
  NPV<-ddply(Data[Data$IdOrig %in% ids & Data$Policy!='Historic',],c('Country','IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T),TotalFood=sum(Catch,na.rm=T))
  
  denominator<-NPV[NPV$Policy==DenominatorPolicy,c('Country','IdOrig','FinalNPV','TotalFood')]
  
  colnames(denominator)<-c('Country','IdOrig','NPVSQ','FoodSQ')
  
  NPV<-merge(NPV,denominator)
  
  NPV$PercChangeFromSQNPV<- PercChange(NPV$FinalNPV,NPV$NPVSQ)
  NPV$AbsChangeFromSQNPV<-NPV$FinalNPV-NPV$NPVSQ  
  NPV$PercChangeFromSQFood<-PercChange(NPV$TotalFood,NPV$FoodSQ)
  NPV$AbsChangeFromSQFood<-NPV$TotalFood-NPV$FoodSQ 
  
  # Calculate baseline metrics values 
  
  Baseline<-Data[Data$Year==BaselineYear & Data$Policy=='Historic',]
  
  # Analyze time trends in metrics
  
  TimeTrend<-Data[Data$Year>BaselineYear,c('IdOrig','Policy','Year','CommName','Country','BvBmsy','FvFmsy','MSY','Catch','Profits','DiscProfits','Biomass')]
  
  TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]#or just use TimeTrend<-TimeTrend[order(IdOrig,Year),]
  
  # Add baseline values to TimeTrend. Values are all the same regardless of policy
  
  base<-Baseline[,c('IdOrig','CommName','Country','BvBmsy','FvFmsy','Catch','Profits','Biomass')]
  
  colnames(base)<-c('IdOrig','CommName','Country','BaselineBvBmsy','BaselineFvFmsy','BaselineCatch','BaselineProfits','BaselineBiomass')
  
  TimeTrend<-merge(TimeTrend,base)
  
  TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]
  
  ### Calculate Upside Metrics----------------------------------------------------- 
  
  # Time trend values
  
TimeTrend$PercChangeTotalProfits<- PercChange(TimeTrend$Profits,TimeTrend$BaselineProfits)  #Percent change in  profits from current
TimeTrend$PercChangeTotalCatch<- PercChange(TimeTrend$Catch,TimeTrend$BaselineCatch) #Percent change in  catch from current
TimeTrend$PercChangeTotalBiomass<- PercChange(TimeTrend$Biomass,TimeTrend$BaselineBiomass) #Percent change in  biomass from current

  TimeTrend$AbsChangeTotalProfits<-TimeTrend$Profits-TimeTrend$BaselineProfits #absolute change in  profits from current
  TimeTrend$AbsChangeTotalCatch<-TimeTrend$Catch-TimeTrend$BaselineCatch #Percent change in  catch from current
  TimeTrend$AbsChangeTotalBiomass<-TimeTrend$Biomass-TimeTrend$BaselineBiomass #Percent change in  biomass from current

  # Add in SQ values
  
  denominator2<-TimeTrend[TimeTrend$Policy==DenominatorPolicy,c('Country','IdOrig','Year','Catch','Biomass','Profits','BvBmsy')]
  
  colnames(denominator2)<-c('Country','IdOrig','Year','CatchSQ','BiomassSQ','ProfitsSQ','BvBmsySQ')
  
  TimeTrend<-merge(TimeTrend,denominator2)
  
  TimeTrend<-TimeTrend[with(TimeTrend,order(IdOrig,Year)),]
  
  # Calculate % relative to SQ
  
TimeTrend$PercChangeFromSQTotalCatch<-PercChange(TimeTrend$Catch,TimeTrend$CatchSQ)
TimeTrend$PercChangeFromSQTotalBiomass<- PercChange(TimeTrend$Biomass,TimeTrend$BiomassSQ)
TimeTrend$PercChangeFromSQProfits<-PercChange(TimeTrend$Profits,TimeTrend$ProfitsSQ)

  TimeTrend$AbsChangeFromSQTotalCatch<-TimeTrend$Catch-TimeTrend$CatchSQ
  TimeTrend$AbsChangeFromSQTotalBiomass<-TimeTrend$Biomass-TimeTrend$BiomassSQ
  TimeTrend$AbsChangeFromSQProfits<-TimeTrend$Profits-TimeTrend$ProfitsSQ
  
  TimeTrend$Recovered[TimeTrend$BvBmsy>=RecoveryThreshold]<-TRUE
  TimeTrend$Recovered[TimeTrend$BvBmsy<RecoveryThreshold]<-FALSE
    
  # Indicate the denominator policy used to make all the SQ calculations
  
  TimeTrend$DenominatorPolicy<-DenominatorPolicy
    
  # If desired later plot and calculate triple bottom line trajectories here
  
  # Find steady state values for each fishery
  
  FinalYr<-TimeTrend[TimeTrend$Year==max(Data$Year,na.rm=T),]
  
  # Add in NPV values
  
  FinalYr<-merge(FinalYr,NPV)
    
  FisheryUpside<-FinalYr
  
  ## Find fisheries satisfying triple bottom line over time----------------------------------------------------------
  
  FisheryUpside$TripBottomLine[FisheryUpside$PercChangeFromSQTotalCatch>0 & FisheryUpside$PercChangeFromSQTotalBiomass>0 &
                                 FisheryUpside$PercChangeFromSQNPV>0]<-TRUE
  
  # write csv
  ##write.csv(FisheryUpside,file=paste(LumpedName,SubsetName,' Fishery Upsides.csv',sep=''))
  
  PercentTripleBottom<-ddply(FisheryUpside,c('Policy'),summarize,PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),
                             Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                             PercentTripleBottom=100*(sum(TripBottomLine,na.rm=T)/length(unique(IdOrig))))
  
 ## Aggregate totals by country and recalculate percent upsides------------------------------------------
  
  CountryUpsides<-ddply(FisheryUpside,c('Country','Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                        TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
                        TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
                        TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
  
CountryUpsides$PercChangeTotalProfits<-PercChange(CountryUpsides$TotalProfits,CountryUpsides$TotalBaselineProfits)#Percent change in  profits from current
CountryUpsides$PercChangeTotalCatch<- PercChange(CountryUpsides$TotalCatch,CountryUpsides$TotalBaselineCatch)#Percent change in  catch from current
CountryUpsides$PercChangeTotalBiomass<- PercChange(CountryUpsides$TotalBiomass,CountryUpsides$TotalBaselineBiomass) #Percent change in  biomass from current

  CountryUpsides$AbsChangeTotalProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalBaselineProfits #absolute change in  profits from current
  CountryUpsides$AbsChangeTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalBaselineCatch #Percent change in  catch from current 
  CountryUpsides$AbsChangeTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBaselineBiomass #Percent change in  biomass from current
  
CountryUpsides$PercChangeFromSQTotalCatch<- PercChange(CountryUpsides$TotalCatch,CountryUpsides$TotalCatchSQ)
CountryUpsides$PercChangeFromSQTotalBiomass<- PercChange(CountryUpsides$TotalBiomass,CountryUpsides$TotalBiomassSQ)
CountryUpsides$PercChangeFromSQProfits<- PercChange(CountryUpsides$TotalProfits,CountryUpsides$TotalProfitsSQ)
CountryUpsides$PercChangeFromSQNPV<- PercChange(CountryUpsides$TotalNPV,CountryUpsides$TotalNPVSQ)
CountryUpsides$PercChangeFromSQFood<- PercChange(CountryUpsides$TotalFood,CountryUpsides$TotalFoodSQ)

  CountryUpsides$AbsChangeFromSQTotalCatch<-CountryUpsides$TotalCatch-CountryUpsides$TotalCatchSQ
  CountryUpsides$AbsChangeFromSQTotalBiomass<-CountryUpsides$TotalBiomass-CountryUpsides$TotalBiomassSQ
  CountryUpsides$AbsChangeFromSQProfits<-CountryUpsides$TotalProfits-CountryUpsides$TotalProfitsSQ
  CountryUpsides$AbsChangeFromSQNPV<-CountryUpsides$TotalNPV-CountryUpsides$TotalNPVSQ
  CountryUpsides$AbsChangeFromSQFood<-CountryUpsides$TotalFood-CountryUpsides$TotalFoodSQ
  
  ##write.csv(CountryUpsides,file=paste(LumpedName,SubsetName,' Country Upsides.csv',sep=''))
  
# Global--------------------------------------
#takes FisheryUpside then calculate all the stats
  
  GlobalUpsides<-ddply(FisheryUpside,c('Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                       TotalBaselineCatch=sum(BaselineCatch,na.rm=T),TotalBaselineBiomass=sum(BaselineBiomass,na.rm=T),TotalBaselineProfits=sum(BaselineProfits,na.rm=T),
                       TotalNPV=sum(FinalNPV,na.rm=T),TotalCatchSQ=sum(CatchSQ,na.rm=T),TotalBiomassSQ=sum(BiomassSQ,na.rm=T),TotalProfitsSQ=sum(ProfitsSQ,na.rm=T),
                       TotalNPVSQ=sum(NPVSQ,na.rm=T),TotalFood=sum(TotalFood,na.rm=T),TotalFoodSQ=sum(FoodSQ,na.rm=T),TotalMSY=sum(MSY,na.rm=T),Stocks=length(unique(IdOrig)))
  
GlobalUpsides$PercChangeTotalProfits<-PercChange(GlobalUpsides$TotalProfits,GlobalUpsides$TotalBaselineProfits) #Percent change in  profits from current
GlobalUpsides$PercChangeTotalCatch<- PercChange(GlobalUpsides$TotalCatch,GlobalUpsides$TotalBaselineCatch) #Percent change in  catch from current
GlobalUpsides$PercChangeTotalBiomass<- PercChange(GlobalUpsides$TotalBiomass,GlobalUpsides$TotalBaselineBiomass) #Percent change in  biomass from current

  GlobalUpsides$AbsChangeTotalProfits<-GlobalUpsides$TotalProfits-GlobalUpsides$TotalBaselineProfits # Absolute change in  profits from current
  GlobalUpsides$AbsChangeTotalCatch<-GlobalUpsides$TotalCatch-GlobalUpsides$TotalBaselineCatch # Absolute change in  catch from current
  GlobalUpsides$AbsChangeTotalBiomass<-GlobalUpsides$TotalBiomass-GlobalUpsides$TotalBaselineBiomass # Absolute change in  biomass from current
  
GlobalUpsides$PercChangeFromSQTotalCatch<- PercChange(GlobalUpsides$TotalCatch,GlobalUpsides$TotalCatchSQ)
GlobalUpsides$PercChangeFromSQTotalBiomass<- PercChange(GlobalUpsides$TotalBiomass,GlobalUpsides$TotalBiomassSQ)
GlobalUpsides$PercChangeFromSQProfits<- PercChange(GlobalUpsides$TotalProfits,GlobalUpsides$TotalProfitsSQ)
GlobalUpsides$PercChangeFromSQNPV<- PercChange(GlobalUpsides$TotalNPV,GlobalUpsides$TotalNPVSQ)
GlobalUpsides$PercChangeFromSQFood<- PercChange(GlobalUpsides$TotalFood,GlobalUpsides$TotalFoodSQ)
  
  GlobalUpsides$AbsChangeFromSQTotalCatch<-GlobalUpsides$TotalCatch-GlobalUpsides$TotalCatchSQ
  GlobalUpsides$AbsChangeFromSQTotalBiomass<-GlobalUpsides$TotalBiomass-GlobalUpsides$TotalBiomassSQ
  GlobalUpsides$AbsChangeFromSQProfits<-GlobalUpsides$TotalProfits-GlobalUpsides$TotalProfitsSQ
  GlobalUpsides$AbsChangeFromSQNPV<-GlobalUpsides$TotalNPV-GlobalUpsides$TotalNPVSQ
  GlobalUpsides$AbsChangeFromSQFood<-GlobalUpsides$TotalFood-GlobalUpsides$TotalFoodSQ
  
  #write.csv(GlobalUpsides,file=paste(ResultFolder,LumpedName,SubsetName,' Global Upsides.csv',sep=''))
  ##write.csv(GlobalUpsides,file=paste(LumpedName,SubsetName,' Global Upsides.csv',sep=''))
  
#1. global baseline
#a. biomass in 2012 
#colnames(base)<-c('IdOrig','CommName','Country','BaselineBvBmsy','BaselineFvFmsy','BaselineCatch','BaselineProfits','BaselineBiomass')

GlobalBaseline<-ddply(Baseline,c('Policy'),summarize,Biomass_in_2012=sum(Biomass,na.rm=T),Profit_in_2012=sum(Profits,na.rm=T),Harvest_in_2012=sum(Catch,na.rm=T),
				Nstock_QI=length(IdOrig[BvBmsy>1 & FvFmsy>1]),Nstock_QII=length(IdOrig[BvBmsy<1 & FvFmsy>1]),Nstock_QIII=length(IdOrig[BvBmsy<1 & FvFmsy<1]),Nstock_QIV=length(IdOrig[BvBmsy>1 & FvFmsy<1]),
				Ncatch_QI=sum(Catch[BvBmsy>1 & FvFmsy>1],na.rm=T),Ncatch_QII=sum(Catch[BvBmsy<1 & FvFmsy>1],na.rm=T),Ncatch_QIII=sum(Catch[BvBmsy<1 & FvFmsy<1],na.rm=T),Ncatch_QIV=sum(Catch[BvBmsy>1 & FvFmsy<1],na.rm=T),
				Nbiomass_QI=sum(Biomass[BvBmsy>1 & FvFmsy>1],na.rm=T),Nbiomass_QII=sum(Biomass[BvBmsy<1 & FvFmsy>1],na.rm=T),Nbiomass_QIII=sum(Biomass[BvBmsy<1 & FvFmsy<1],na.rm=T),Nbiomass_QIV=sum(Biomass[BvBmsy>1 & FvFmsy<1],na.rm=T),
				Nprofits_QI=sum(Profits[BvBmsy>1 & FvFmsy>1],na.rm=T),Nprofits_QII=sum(Profits[BvBmsy<1 & FvFmsy>1],na.rm=T),Nprofits_QIII=sum(Profits[BvBmsy<1 & FvFmsy<1],na.rm=T),Nprofits_QIV=sum(Profits[BvBmsy>1 & FvFmsy<1],na.rm=T),
				N_Fisheries=length(unique(IdOrig)),N_RAM=length(Dbase[Dbase=="RAM"]),N_SOFIA=length(Dbase[Dbase=="SOFIA"]),N_FAO=length(Dbase[Dbase=="FAO"]),
				MedianBvBmsy2012=median(BvBmsy,na.rm=T),MedianFvFmsy2012=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),FisheriesAtRisk=length(IdOrig[BvBmsy<1 | FvFmsy>1])*100/length(unique(IdOrig)))
CountryBaseline<-ddply(Baseline,c('Country','Policy'),summarize,Biomass_in_2012=sum(Biomass,na.rm=T),Profit_in_2012=sum(Profits,na.rm=T),Harvest_in_2012=sum(Catch,na.rm=T),
				Nstock_QI=length(IdOrig[BvBmsy>1 & FvFmsy>1]),Nstock_QII=length(IdOrig[BvBmsy<1 & FvFmsy>1]),Nstock_QIII=length(IdOrig[BvBmsy<1 & FvFmsy<1]),Nstock_QIV=length(IdOrig[BvBmsy>1 & FvFmsy<1]),
				Ncatch_QI=sum(Catch[BvBmsy>1 & FvFmsy>1],na.rm=T),Ncatch_QII=sum(Catch[BvBmsy<1 & FvFmsy>1],na.rm=T),Ncatch_QIII=sum(Catch[BvBmsy<1 & FvFmsy<1],na.rm=T),Ncatch_QIV=sum(Catch[BvBmsy>1 & FvFmsy<1],na.rm=T),
				Nbiomass_QI=sum(Biomass[BvBmsy>1 & FvFmsy>1],na.rm=T),Nbiomass_QII=sum(Biomass[BvBmsy<1 & FvFmsy>1],na.rm=T),Nbiomass_QIII=sum(Biomass[BvBmsy<1 & FvFmsy<1],na.rm=T),Nbiomass_QIV=sum(Biomass[BvBmsy>1 & FvFmsy<1],na.rm=T),
				Nprofits_QI=sum(Profits[BvBmsy>1 & FvFmsy>1],na.rm=T),Nprofits_QII=sum(Profits[BvBmsy<1 & FvFmsy>1],na.rm=T),Nprofits_QIII=sum(Profits[BvBmsy<1 & FvFmsy<1],na.rm=T),Nprofits_QIV=sum(Profits[BvBmsy>1 & FvFmsy<1],na.rm=T),
				N_Fisheries=length(unique(IdOrig)),N_RAM=length(Dbase[Dbase=="RAM"]),N_SOFIA=length(Dbase[Dbase=="SOFIA"]),N_FAO=length(Dbase[Dbase=="FAO"]),
				MedianBvBmsy2012=median(BvBmsy,na.rm=T),MedianFvFmsy2012=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),FisheriesAtRisk=length(IdOrig[BvBmsy<1 | FvFmsy>1])*100/length(unique(IdOrig)))

GlobalResults<-ddply(FisheryUpside,c('Policy'),summarize,AnnuityOutTo2050=sum(FinalNPV,na.rm=T)*(0.05/(1-((1+0.05)^-38))),ProfitIn2050=sum(Profits,na.rm=T),BiomassIn2050=sum(Biomass,na.rm=T),HarvestIn2050=sum(Catch,na.rm=T))
CountryResults<-ddply(FisheryUpside,c('Country','Policy'),summarize,AnnuityOutTo2050=sum(FinalNPV,na.rm=T)*(0.05/(1-((1+0.05)^-38))),ProfitIn2050=sum(Profits,na.rm=T),BiomassIn2050=sum(Biomass,na.rm=T),HarvestIn2050=sum(Catch,na.rm=T))


#percent increase here
#5
#AnnuCons <- (0.05/(1-((1+0.05)^-38)))



#GlobalPercentage<-ddply(Data,c('CanProject'),summarize,
#	PercentChangeBiomass_CatchShareVsToday_Rebuild=(sum(Biomass[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Biomass[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Biomass[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T),
#	PercentChangeHarvest_CatchShareVsToday_Rebuild=(sum(Catch[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Catch[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Catch[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T),
#	PercentChangeAnnuity_CatchShareVsToday_Rebuild=((sum(DiscProfits[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T)*(0.05/(1-((1+0.05)^-38)))) - sum(Profits[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Profits[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T),

#	PercentChangeBiomass_CatchShareVsBAU_Rebuild=(sum(Biomass[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Biomass[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Biomass[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T),
#	PercentChangeHarvest_CatchShareVsBAU_Rebuild=(sum(Catch[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Catch[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Catch[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T),
#	PercentChangeAnnuity_CatchShareVsBAU_Rebuild=(sum(DiscProfits[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(DiscProfits[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(DiscProfits[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T),

#	PercentChangeBiomass_CatchShareVsToday_All=(sum(Biomass[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Biomass[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Biomass[Year==BaselineYear],na.rm=T),
#	PercentChangeHarvest_CatchShareVsToday_All=(sum(Catch[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Catch[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Catch[Year==BaselineYear],na.rm=T),
#	PercentChangeAnnuity_CatchShareVsToday_All=((sum(DiscProfits[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T)*(0.05/(1-((1+0.05)^-38)))) - sum(Profits[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Profits[Year==BaselineYear],na.rm=T),

#	PercentChangeBiomass_CatchShareVsBAU_All=(sum(Biomass[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Biomass[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T),
#	PercentChangeHarvest_CatchShareVsBAU_All=(sum(Catch[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Catch[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T),
#	PercentChangeAnnuity_CatchShareVsBAU_All=(sum(DiscProfits[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(DiscProfits[Year==2050 & Policy=='Business As Usual' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(DiscProfits[Year==2050 & Policy=='Business As Usual'],na.rm=T)
#)

Denom_GlobalPercentage<-ddply(Data,c('CanProject'),summarize,
	Biomass_CatchShareVsToday_Rebuild=1,
	Harvest_CatchShareVsToday_Rebuild=1,
	Annuity_CatchShareVsToday_Rebuild=1,

	Biomass_CatchShareVsBAU_Rebuild=1,
	Harvest_CatchShareVsBAU_Rebuild=1,
	Annuity_CatchShareVsBAU_Rebuild=1,

	Biomass_CatchShareVsToday_All=sum(Biomass[Year==BaselineYear],na.rm=T),
	Harvest_CatchShareVsToday_All=sum(Catch[Year==BaselineYear],na.rm=T),
	Annuity_CatchShareVsToday_All=abs(sum(Profits[Year==BaselineYear],na.rm=T)),

	Biomass_CatchShareVsBAU_All=sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Harvest_CatchShareVsBAU_All=sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Annuity_CatchShareVsBAU_All=abs(sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T))#####i changed DiscProfits to Id
)
Denom_CountryPercentage<-ddply(Data,c('Country'),summarize,
	Biomass_CatchShareVsToday_Rebuild=1+sum(Biomass[Year==BaselineYear],na.rm=T)*0,
	Harvest_CatchShareVsToday_Rebuild=1+sum(Biomass[Year==BaselineYear],na.rm=T)*0,
	Annuity_CatchShareVsToday_Rebuild=1+sum(Biomass[Year==BaselineYear],na.rm=T)*0,

	Biomass_CatchShareVsBAU_Rebuild=1+sum(Biomass[Year==BaselineYear],na.rm=T)*0,
	Harvest_CatchShareVsBAU_Rebuild=1+sum(Biomass[Year==BaselineYear],na.rm=T)*0,
	Annuity_CatchShareVsBAU_Rebuild=1+sum(Biomass[Year==BaselineYear],na.rm=T)*0,

	Biomass_CatchShareVsToday_All=sum(Biomass[Year==BaselineYear],na.rm=T),
	Harvest_CatchShareVsToday_All=sum(Catch[Year==BaselineYear],na.rm=T),
	Annuity_CatchShareVsToday_All=abs(sum(Profits[Year==BaselineYear],na.rm=T)),

	Biomass_CatchShareVsBAU_All=sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Harvest_CatchShareVsBAU_All=sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Annuity_CatchShareVsBAU_All=abs(sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T))
)

OverfishIds<-Data$IdOrig[Data$Year==BaselineYear & (Data$BvBmsy<1 | Data$FvFmsy>1)]  
Data2<-Data[Data$IdOrig %in% OverfishIds,]

GlobalPercentage<-ddply(Data2,c('CanProject'),summarize,
	Biomass_CatchShareVsToday_Rebuild=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==BaselineYear],na.rm=T))*100/sum(Biomass[Year==BaselineYear],na.rm=T),
	Harvest_CatchShareVsToday_Rebuild=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==BaselineYear],na.rm=T))*100/sum(Catch[Year==BaselineYear],na.rm=T),
	Annuity_CatchShareVsToday_Rebuild=((sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T)*(0.05/(1-((1+0.05)^-38)))) - sum(Profits[Year==BaselineYear],na.rm=T))*100/abs(sum(Profits[Year==BaselineYear],na.rm=T)),

	Biomass_CatchShareVsBAU_Rebuild=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100/sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Harvest_CatchShareVsBAU_Rebuild=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100/sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Annuity_CatchShareVsBAU_Rebuild= ( sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T) )*100/abs(sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T)),

#print(sum(DiscProfits[Year==2050 & Policy=='CatchShare'],na.rm=T)),print(sum(DiscProfits[Year==2050 & Policy=='Business As Usual'],na.rm=T)),print(sum(DiscProfits[Year==2050 & Policy=='Business As Usual'],na.rm=T))

	Biomass_CatchShareVsToday_All=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==BaselineYear],na.rm=T))*100,
	Harvest_CatchShareVsToday_All=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==BaselineYear],na.rm=T))*100,
	Annuity_CatchShareVsToday_All=((sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T)*(0.05/(1-((1+0.05)^-38)))) - sum(Profits[Year==BaselineYear],na.rm=T))*100,

	Biomass_CatchShareVsBAU_All=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100,
	Harvest_CatchShareVsBAU_All=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100,
	Annuity_CatchShareVsBAU_All=(sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100
)

CountryPercentage<-ddply(Data2,c('Country'),summarize,
	Biomass_CatchShareVsToday_Rebuild=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==BaselineYear],na.rm=T))*100/sum(Biomass[Year==BaselineYear],na.rm=T),
	Harvest_CatchShareVsToday_Rebuild=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==BaselineYear],na.rm=T))*100/sum(Catch[Year==BaselineYear],na.rm=T),
	Annuity_CatchShareVsToday_Rebuild=((sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T)*(0.05/(1-((1+0.05)^-38)))) - sum(Profits[Year==BaselineYear],na.rm=T))*100/abs(sum(Profits[Year==BaselineYear],na.rm=T)),

	Biomass_CatchShareVsBAU_Rebuild=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100/sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Harvest_CatchShareVsBAU_Rebuild=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100/sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T),
	Annuity_CatchShareVsBAU_Rebuild= ( sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T) )*100/abs(sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T)),

#print(sum(DiscProfits[Year==2050 & Policy=='CatchShare'],na.rm=T)),print(sum(DiscProfits[Year==2050 & Policy=='Business As Usual'],na.rm=T)),print(sum(DiscProfits[Year==2050 & Policy=='Business As Usual'],na.rm=T))

	Biomass_CatchShareVsToday_All=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==BaselineYear],na.rm=T))*100,
	Harvest_CatchShareVsToday_All=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==BaselineYear],na.rm=T))*100,
	Annuity_CatchShareVsToday_All=((sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T)*(0.05/(1-((1+0.05)^-38)))) - sum(Profits[Year==BaselineYear],na.rm=T))*100,

	Biomass_CatchShareVsBAU_All=(sum(Biomass[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Biomass[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100,
	Harvest_CatchShareVsBAU_All=(sum(Catch[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(Catch[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100,
	Annuity_CatchShareVsBAU_All=(sum(NPV[Year==2050 & Policy=='Catch Share Three'],na.rm=T) - sum(NPV[Year==2050 & Policy=='Business As Usual'],na.rm=T))*100
)


#CountryPercentage<-ddply(Data,c('Country'),summarize,
#	(sum(Biomass[Year==2050 & Policy=='CatchShare' & (BvBmsy<1 | FvFmsy>1) ],na.rm=T) - sum(Biomass[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T))*100/sum(Biomass[Year==BaselineYear & (BvBmsy<1 | FvFmsy>1) ],na.rm=T)
#)

REF<-Baseline


  return(list(FisheryUpside=FisheryUpside,TimeTrend=TimeTrend,TripBottomLine=PercentTripleBottom,CountryUpsides=CountryUpsides,GlobalUpside=GlobalUpsides,GlobalBaseline=GlobalBaseline,CountryBaseline=CountryBaseline,
		GlobalResults=GlobalResults,CountryResults=CountryResults,GlobalPercentage=GlobalPercentage,CountryPercentage=CountryPercentage,Denom_GlobalPercentage=Denom_GlobalPercentage,Denom_CountryPercentage=Denom_CountryPercentage,REF=REF))
  
} # close function

