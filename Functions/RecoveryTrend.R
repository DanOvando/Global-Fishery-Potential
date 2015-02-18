# 
# OnlyOverfish<-FALSE
# RecoveryThreshold<-0.9

RecoveryTrend<-function(ProjectionData,RecoveryThreshold,OnlyOverfish)
{
  
  RecoveryData<-ProjectionData
  
  if(OnlyOverfish==TRUE)
  {
    OverfishedIds<-ProjectionData$IdOrig[ProjectionData$Year==2012 & ProjectionData$BvBmsy<1]
    
    RecoveryData<-ProjectionData[ProjectionData$IdOrig %in% OverfishedIds,]
  }

  ### Create new 'hybrid' SQ policy for plot-------------------------------------------------

  ram<-RecoveryData[RecoveryData$Policy=='Fmsy' & RecoveryData$Dbase=='RAM' & RecoveryData$CatchShare!=1,]

  cs<-RecoveryData[RecoveryData$Policy=='Opt' & RecoveryData$CatchShare==1,]

  other<-RecoveryData[RecoveryData$Policy=='StatusQuoOpenAccess' & RecoveryData$Dbase!='RAM' & RecoveryData$CatchShare!=1,]

  hybrid<-rbind(ram,cs,other)

  hybrid$Policy<-'Business As Usual'
  
  RecoveryData<-rbind(RecoveryData,hybrid)
  
  # Identify recovered fisheries through time (definiing recovery as >=0.9)
  RecoveryData$Recovered[RecoveryData$BvBmsy>RecoveryThreshold]<-TRUE
  
  RecoveryTrend<-ddply(RecoveryData[RecoveryData$Year>BaselineYear,],c('Policy','Year'),summarize,Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                       PercentHealthy=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
  
  # Calculate historic path of these fisheries
  HistoricTrend<-ddply(RecoveryData[RecoveryData$Year<=BaselineYear,],c('Policy','Year'),summarize,Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                       PercentHealthy=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
  
  Trend<-rbind(HistoricTrend[HistoricTrend$Year>=1975,],RecoveryTrend)
  
  PlotTrend<-melt(Trend,measure.vars=c('PercentRecovered','TotalCatch','TotalProfit'))
  
  ### Upside trajectory plots------------------------------------------------------------------
  
ggplot(PlotTrend[PlotTrend$Policy %in% c('Historic','Fmsy','Opt','CatchShare','Business As Usual'),],aes(x=Year,y=value,color=Policy)) +
    geom_line(size=2) +
    facet_wrap(~variable,scales='free_y',ncol=1) +
    theme(text=element_text(size=18))
  
#   # plot recovery trend
#   
#   StatusPlot<-ggplot(Trend[Trend$Policy %in% c('Historic','StatusQuoOpenAccess','Opt','CloseDown','StatusQuoBForever'),],aes(x=Year,y=PercentRecovered,color=Policy)) +
#               geom_line(size=2) +
#               labs(y=paste('Percent Above B/Bmsy of ',RecoveryThreshold,sep='')) +
#               theme(text=element_text(size=18))
#   
#   # plot catch trend
#   
#   CatchPlot<-ggplot(Trend[Trend$Policy %in% c('Historic','StatusQuoOpenAccess','CatchShare','Opt','CloseDown'),],aes(x=Year,y=TotalCatch,color=Policy)) +
#               geom_line(size=2) +
#               labs(title='Trends and Future Prospects for Global Fisheries',y='Total Catch (MT)') +
#               theme(text=element_text(size=18))
#   
#   # plot profit trend
#   
# ProfitPlot<-ggplot(Trend[Trend$Policy %in% c('Historic','StatusQuoOpenAccess','CatchShare','Opt'),],aes(x=Year,y=TotalProfit,color=Policy)) +
#             geom_line(size=2) +
#             labs(title='Trends and Future Prospects for Global Fisheries',y=paste('Percent Above B/Bmsy of ',RecoveryThreshold,sep='')) +
#             theme(text=element_text(size=18))
#   
#   pdf(file=paste(FigureFolder, 'Recovery Trajectories.pdf',sep=''),height=10,width=12)
#   
#   grid.arrange(arrangeGrob(StatusPlot,CatchPlot,ProfitPlot,ncol=1))
#   
#   dev.off()
  
  return(Trend)
}