

# RecoveredThreshold<-0.95

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
                       PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))))
  
  # Calculate historic path of these fisheries
  HistoricTrend<-ddply(RecoveryData[RecoveryData$Year<BaselineYear,],c('Policy','Year'),summarize,
                       PercentRecovered=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))))
  
  Trend<-rbind(HistoricTrend[HistoricTrend$Year>=1970,],RecoveryTrend)
  
  # plot recovery trend
  pdf(file=paste(FigureFolder, 'Recovery Trajectories.pdf',sep='')) 
  print(ggplot(Trend,aes(x=Year,y=PercentRecovered,color=Policy)) +
          geom_line() +
          labs(title='Recovery Pathways for Currently Overfished Stocks',y=paste('Percent Below B/Bmsy of ',RecoveryThreshold,sep='')))
  dev.off()
  
  return(Trend)
}