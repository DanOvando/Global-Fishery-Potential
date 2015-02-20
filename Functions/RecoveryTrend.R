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
  
  RecoveryTrend<-ddply(RecoveryData,c('Policy','Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),
                       Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                       PercentHealthy=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))))
  
  PlotTrend<-melt(RecoveryTrend[RecoveryTrend$Year>=1950,],measure.vars=c('PercentHealthy','TotalCatch','TotalProfit'))
  
  ### Upside trajectory plots------------------------------------------------------------------

pdf(file=paste(FigureFolder, 'Recovery Trajectories.pdf',sep=''),height=10,width=16)  
  
print(ggplot(PlotTrend[PlotTrend$Policy %in% c('Historic','Fmsy','Opt','CatchShare','Business As Usual'),],aes(x=Year,y=value,color=Policy)) +
    geom_line(size=2) +
    facet_wrap(~variable,scales='free_y',ncol=1) +
    theme(text=element_text(size=18)))
 
dev.off()

#   # plot recovery trends separately
  
  StatusPlot<-ggplot(PlotTrend[PlotTrend$variable=='PercentHealthy' & PlotTrend$Policy %in% c('Historic','Fmsy','Opt','CatchShare','Business As Usual'),],
                     aes(x=Year,y=value,color=Policy)) +
              geom_line(size=2) +
              labs(y=paste('Percent of Stocks Above B/Bmsy of ',RecoveryThreshold,sep='')) +
              theme(text=element_text(size=18))
  
#   # plot catch trend
#   
CatchPlot<-ggplot(PlotTrend[PlotTrend$variable=='TotalCatch' & PlotTrend$Policy %in% c('Historic','Fmsy','Opt','CatchShare','Business As Usual'),],
                   aes(x=Year,y=value,color=Policy)) +
  geom_line(size=2) +
  labs(y='Total Catch (MT)') +
  theme(text=element_text(size=18))
#   
#   # plot profit trend
  
ProfitPlot<-ggplot(PlotTrend[PlotTrend$variable=='TotalProfit' & PlotTrend$Policy %in% c('Historic','StatusQuoOpenAccess','CatchShare','Opt'),],aes(x=Year,y=value,color=Policy)) +
            geom_line(size=2) +
            coord_cartesian(ylim=c(-100000000000,100000000000)) +
            labs(title='Trends and Future Prospects for Global Fisheries',y=paste('Percent Above B/Bmsy of ',RecoveryThreshold,sep='')) +
            theme(text=element_text(size=18))
  
#   pdf(file=paste(FigureFolder, 'Recovery Trajectories.pdf',sep=''),height=10,width=12)
#   
#   grid.arrange(arrangeGrob(StatusPlot,CatchPlot,ProfitPlot,ncol=1))
#   
#   dev.off()
  
  return(Trend)
}