

# RecoveryThreshold<-0.9

RecoveryTrend<-function(ProjectionData,RecoveryThreshold,OnlyOverfish)
{
  
  RecoveryData<-ProjectionData
  
  if(OnlyOverfish==TRUE)
  {
    OverfishedIds<-ProjectionData$IdOrig[ProjectionData$Year==2012 & ProjectionData$BvBmsy<1]
    
    RecoveryData<-ProjectionData[ProjectionData$IdOrig %in% OverfishedIds,]
  }

  
  # Identify recovered fisheries through time (definiing recovery as >=0.95)
  RecoveryData$Recovered[RecoveryData$BvBmsy>RecoveryThreshold]<-TRUE
  
  RecoveryTrend<-ddply(RecoveryData[RecoveryData$Year>BaselineYear,],c('Policy','Year'),summarize,
                       PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
  
  # Calculate historic path of these fisheries
  HistoricTrend<-ddply(RecoveryData[RecoveryData$Year<BaselineYear,],c('Policy','Year'),summarize,
                       PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
  
  Trend<-rbind(HistoricTrend[HistoricTrend$Year>=1970,],RecoveryTrend)
  
  ### Upside trajectory plots------------------------------------------------------------------
  
  pdf(file=paste(FigureFolder, 'Recovery Trajectories.pdf',sep=''),height=10,width=12)
  
  # plot recovery trend
  
  print(ggplot(Trend[Trend$Policy %in% c('Historic','StatusQuoOpenAccess','Opt','CloseDown','StatusQuoBForever'),],aes(x=Year,y=PercentRecovered,color=Policy)) +
          geom_line(size=2) +
          labs(y=paste('Percent Above B/Bmsy of ',RecoveryThreshold,sep='')) +
          theme(text=element_text(size=18)))
  
  # plot catch trend
  
  print(ggplot(Trend[Trend$Policy %in% c('Historic','StatusQuoOpenAccess','CatchShare','Opt'),],aes(x=Year,y=TotalCatch,color=Policy)) +
    geom_line(size=2) +
    labs(title='Trends and Future Prospects for Global Fisheries',y='Total Catch (MT)') +
    theme(text=element_text(size=18)))
  
  # plot profit trend
  
  print(ggplot(Trend[Trend$Policy %in% c('Historic','StatusQuoOpenAccess','CatchShare','Opt'),],aes(x=Year,y=TotalProfit,color=Policy)) +
    geom_line(size=2) +
    labs(title='Trends and Future Prospects for Global Fisheries',y=paste('Percent Above B/Bmsy of ',RecoveryThreshold,sep='')) +
    theme(text=element_text(size=18)))
  
  dev.off()
  
  return(Trend)
}