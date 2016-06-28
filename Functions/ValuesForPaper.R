##################################################################---------------------------------------------------------------------
#
# Number Summary for Upside Paper
#
# Tyler
# 12/3/2015
#
# Code description: Code calculates basic numbers from the upside analysis for use in the main text
#
##################################################################---------------------------------------------------------------------

# DataU<-UnlumpedProjectionData
# DataL<-ProjectionData

ResultsForPaper<-function(DataU,DataL,RawData,BaselineYear=2012,ResultFolder)
{
  ## Build result table
  results<- as.data.frame(matrix(NA,ncol = 2))
  colnames(results)<-c('Metric','Result')
  
  # vector of policies of interest
  pols<-c('Business As Usual','Business As Usual Pessimistic','CatchShare','Catch Share Three','Fmsy','Fmsy Three')
  
  ## Calculate number of fisheries
  
  # Unlumped
  fishU<-DataU %>%
    filter(Year==2050 & Policy=='Catch Share Three') %>%
    group_by(Policy) %>%
    summarize(Fisheries=length(unique(IdOrig)))
  
  results[1,'Metric']<-"Unlumped Fisheries"
  results[1,'Result']<-fishU$Fisheries
  
  # Lumped
  fishL<-DataL %>%
    filter(Year==2050 & Policy=='Catch Share Three') %>%
    group_by(Policy) %>%
    summarize(Fisheries=length(unique(IdOrig)))
  
  results[2,'Metric']<-"Lumped Fisheries"
  results[2,'Result']<-fishL$Fisheries
  
  ## % Fisheries with BvBmsy <0.8 in 2050
  recovered<-DataL %>%
    dplyr::select(IdOrig,Policy,Country,SciName,Year,Catch,BvBmsy,FvFmsy) %>%
    mutate(Recovered=BvBmsy>=0.8) %>%
    group_by(Policy,Year) %>%
    summarize(Fisheries=length(IdOrig),TotalCatch=sum(Catch,na.rm=T),RecoveredFisheries=length(IdOrig[Recovered==TRUE]),
              CatchFromRecovered=sum(Catch[Recovered==TRUE],na.rm=TRUE),PercRecovered=100*sum(Recovered==TRUE)/length(IdOrig),
              PercNeedRecovery=100-PercRecovered) %>%
    ungroup() %>%
    filter(Year %in% c(2012,2050) & Policy %in% pols)

  results[3:8,'Metric']<-c("% Recovered,BAU CC","% Recovered, BAU All","% Recovered, RBFM CC",
                           "% Recovered, RBFM All","% Recovered, Fmsy All","% Recovered,Fmsy All")
  
  results[3:8,'Result']<-round(recovered$PercRecovered,2)
  
  results[9:14,'Metric']<-c("% Need Recovery,BAU CC","% Need Recovery, BAU All","% Need Recovery, RBFM CC",
                           "% Need Recovery, RBFM All","% Need Recovery, Fmsy All","% Need Recovery,Fmsy All")
  
  results[9:14,'Result']<-round(recovered$PercNeedRecovery,2)
  
  ## Time to recovery
  
  # RBFM
  rbfmtime<-median(TimeToRecover(DataL,Policy = 'CatchShare',BaselineYear = 2012)$TimeToRecover,na.rm=T)
  
  results[15,'Metric']<-"Median recovery time, RBFM"
  results[15,'Result']<-rbfmtime

  # FMSY
  fmsytime<-median(TimeToRecover(DataL,Policy = 'Fmsy',BaselineYear = 2012)$TimeToRecover,na.rm=T)
  
  results[16,'Metric']<-"Median recovery time, Fmsy"
  results[16,'Result']<-fmsytime
  
  ## conservation concern stocks
  concern<-DataL %>%
    filter(Year %in% c(2012) & (BvBmsy<1 | FvFmsy>1)) %>%
    dplyr::select(IdOrig,Policy,Year,BvBmsy,FvFmsy) 
  
  concern<-100*(length(unique(concern$IdOrig))/length(unique(DataL$IdOrig[DataL$Year==2012])))

  results[17,'Metric']<-"% stocks of con. concern"
  results[17,'Result']<-concern
  
  ## Median status
  medians<-DataL %>%
    filter(Year==BaselineYear) %>%
    group_by(Policy) %>%
    summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T))

  results[18:19,'Metric']<-c("Median B",'Median F')
  results[18:19,'Result']<-c(medians$MedianB,medians$MedianF)  
  
  # Percent coverage
  coverage<-DataL %>%
    filter(Year==2012) %>%
    group_by(Year) %>%
    summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
    mutate(PercCovered=100*(TotalCatch/79700000))
  
  results[20,'Metric']<-'% Global catch covered'
  results[20,'Result']<-coverage$PercCovered
  
  # Percent triple bottom line (lumped)
  fishupsides<-read.csv(file=paste(ResultFolder,'Lumped Projection DataAll Stocks Fishery Upsides.csv',sep = ''),stringsAsFactors = F)
  
  tbl<-fishupsides %>%
    filter(Policy %in% c('CatchShare','Catch Share Three')) %>%
    dplyr::select(IdOrig,Country,Year,Policy,AbsChangeFromSQProfits,AbsChangeFromSQTotalCatch,AbsChangeFromSQTotalBiomass) %>%
    mutate(TBL=AbsChangeFromSQProfits>0 & AbsChangeFromSQTotalCatch>0 & AbsChangeFromSQTotalBiomass>0) %>%
    group_by(Policy) %>%
    summarize(Stocks=length(unique(IdOrig)),TBLstock=sum(TBL,na.rm=T),
              PercentTripleBottom=100*(sum(TBL,na.rm=T)/Stocks))
  
  results[21,'Metric']<-'% Stocks w/ triple bottom line'
  results[21,'Result']<-tbl$PercentTripleBottom[tbl$Policy=='Catch Share Three']
  

  # Percent triple bottom line (lumped)
  fishupsides<-read.csv(file=paste(ResultFolder,'Lumped Projection DataAll Stocks Fishery Upsides.csv',sep = ''),stringsAsFactors = F)
  
  tbl<-fishupsides %>%
    filter(Policy %in% c('CatchShare','Catch Share Three')) %>%
    dplyr::select(IdOrig,Country,Year,Policy,AbsChangeFromSQProfits,AbsChangeFromSQTotalCatch,AbsChangeFromSQTotalBiomass) %>%
    mutate(TBL=AbsChangeFromSQProfits>0 & AbsChangeFromSQTotalCatch>0 & AbsChangeFromSQTotalBiomass>0) %>%
    group_by(Policy) %>%
    summarize(Stocks=length(unique(IdOrig)),TBLstock=sum(TBL,na.rm=T),
              PercentTripleBottom=100*(sum(TBL,na.rm=T)/Stocks))
  
  results[21,'Metric']<-'% Stocks w/ triple bottom line'
  results[21,'Result']<-tbl$PercentTripleBottom[tbl$Policy=='Catch Share Three']
  
  ## Percent triple bottom line for top 30 countries (lumped)
  topcntrys<-RawData %>%
    filter(Dbase=='FAO' & Year==2012) %>%
    group_by(Country) %>%
    summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
    arrange(desc(TotalCatch)) %>%
    slice(1:30)
  
  cntryupsides<-read.csv(file=paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep = ''),stringsAsFactors = F)

  cntrytbl<-cntryupsides %>%
    filter(Policy %in% c('CatchShare','Catch Share Three') & Country %in% topcntrys$Country) %>%
    dplyr::select(Country,Policy,AbsChangeFromSQProfits,AbsChangeFromSQTotalCatch,AbsChangeFromSQTotalBiomass) %>%
    mutate(TBL=AbsChangeFromSQProfits>0 & AbsChangeFromSQTotalCatch>0 & AbsChangeFromSQTotalBiomass>0) %>%
    group_by(Policy) %>%
    summarize(Country=length(unique(Country)),TBLcntry=sum(TBL,na.rm=T),
              PercentTripleBottom=100*(sum(TBLcntry,na.rm=T)/Country))
  
  results[22,'Metric']<-'# Top 30 countries w/ triple bottom line'
  results[22,'Result']<-cntrytbl$TBLcntry[tbl$Policy=='Catch Share Three']

  ## Fisheries by database
  dbase<-DataL %>%
    filter(Year==2050) %>%
    group_by(Dbase) %>%
    summarize(Stocks=length(unique(IdOrig)))
  
  results[23:24,'Metric']<-c('RAM stocks','Unassessed stocks')
  results[23:24,'Result']<-c(dbase$Stocks[dbase$Dbase=='RAM'],sum(dbase$Stocks[dbase$Dbase!='RAM']))
  
  ## Cost revenue ratios
  cratios<-read.csv(file=paste(ResultFolder,'Cost_Revenue_Ratio_AllData.csv',sep=''),stringsAsFactors = F)
  
  results[25,'Metric']<-'Mean cost/revenue'
  results[25,'Result']<-cratios$Mean
  
  ## Pecuniary and optimization effects
  fig3<-DataL %>%
    filter(Year==2050) %>%
    group_by(Policy) %>%
    summarize(TotalProfits=sum(Profits,na.rm=T))
  
  # Pecuniary effect
  opt<-100*round((fig3$TotalProfits[fig3$Policy=='Opt']-fig3$TotalProfits[fig3$Policy=='Fmsy'])/
               (fig3$TotalProfits[fig3$Policy=='CatchShare']-fig3$TotalProfits[fig3$Policy=='Fmsy']),2)
  # Opt effect
  pec<-100*round((fig3$TotalProfits[fig3$Policy=='CatchShare']-fig3$TotalProfits[fig3$Policy=='Opt'])/
          (fig3$TotalProfits[fig3$Policy=='CatchShare']-fig3$TotalProfits[fig3$Policy=='Fmsy']),2)
  
  results[26:27,'Metric']<-c('Optimization effect','Pecuniary effect')
  results[26:27,'Result']<-c(opt,pec)
  
  ## Abstract upsides
  abstract<-read.csv(file=paste(ResultFolder,'Global_Scenario_Results_Final.csv',sep=''),stringsAsFactors = F) %>%
    filter(Year==2050)
  
  ups1<-subset(abstract,Policy=='RBFM' & Scenario=='Conservation Concern',c('AbsChangeCatch','AbsChangeBiomass','AbsChangeProfits'))

  results[28:30,'Metric']<-c('RBFM catch upside relative to today','RBFM biomass upside relative to today','RBFM biomass upside relative to today')
  results[28:30,'Result']<-c(ups1$AbsChangeCatch,ups1$AbsChangeBiomass,ups1$AbsChangeProfits)
  
  ups2<-subset(abstract,Policy=='RBFM' & Scenario=='All Stocks',c('AbsChangeCatchBAU','AbsChangeBiomassBAU','AbsChangeProfitsBAU'))
  
  results[31:33,'Metric']<-c('RBFM catch upside relative to BAU','RBFM biomass upside relative to BAU','RBFM biomass upside relative to BAU')
  results[31:33,'Result']<-c(ups2$AbsChangeCatchBAU,ups2$AbsChangeBiomassBAU,ups2$AbsChangeProfitsBAU)

  write.csv(results,file=paste(ResultFolder,'Results for manuscript.csv',sep = ''))
  
  return(results)
  
}