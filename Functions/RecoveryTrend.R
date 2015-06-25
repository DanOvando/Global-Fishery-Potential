# RecoveryData<-UnlumpedProjectionData
# OnlyOverfish<-FALSE
# RecoveryThreshold<-0.8
# StartYear<-1980

RecoveryTrend<-function(ProjectionData,RecoveryThreshold,OnlyOverfish,StartYear)
{
  
  RecoveryData<-ProjectionData
  
  if(OnlyOverfish==TRUE)
  {
    OverfishedIds<-ProjectionData$IdOrig[ProjectionData$Year==2012 & (ProjectionData$BvBmsy<1 | ProjectionData$FvFmsy>1)]
    
    RecoveryData<-ProjectionData[ProjectionData$IdOrig %in% OverfishedIds,]
  }
  
  RecoveryData$Recovered[RecoveryData$BvBmsy>RecoveryThreshold]<-TRUE
  
  RecoveryTrend<- RecoveryData %>%
    group_by(Policy,Year) %>%
    summarize(TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),
              Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
              PercentHealthy=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))))
    
#   RecoveryTrend<-ddply(RecoveryData,c('Policy','Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),
#                        Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
#                        PercentHealthy=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))))
#   
  
  PlotTrend<-melt(RecoveryTrend[RecoveryTrend$Year>=StartYear,],measure.vars=c('PercentHealthy','TotalCatch','TotalProfit','TotalBiomass'))
  
  write.csv(PlotTrend,file=paste(ResultFolder,'PlotTrend.csv',sep=''))
  
  ### Percent healthy trajectory plots------------------------------------------------------------------

pdf(file=paste(FigureFolder,'Percent Healthy Recovery Trend.pdf',sep=''),width=8,height=5)

print(ggplot(PlotTrend[PlotTrend$variable=='PercentHealthy' & PlotTrend$Policy %in% c('Historic','Fmsy','Opt','Business As Usual Pessimistic','Business As Usual Optimistic'),],
  aes(x=Year,y=value,color=Policy)) +
  geom_line(size=1) +
  labs(y=paste('Percent of Stocks Above B/Bmsy of ',RecoveryThreshold,sep='')) +
  theme_classic() +
  theme(
    plot.background=element_rect(color='black',fill=NULL),
    text=element_text(size=10),
    panel.border=element_rect(color='black',fill=NA),
    panel.grid.major=element_line(linetype=2,size=1),
    legend.key=element_blank(),
    legend.position='bottom',
    legend.direction='horizontal',
    legend.background=element_rect(color='black')))

dev.off()
     
#   # plot catch trend
#   
# CatchPlot<-ggplot(PlotTrend[PlotTrend$variable=='TotalCatch' & PlotTrend$Policy %in% c('Historic','Fmsy','Opt','CatchShare','Business As Usual'),],
#                    aes(x=Year,y=value,color=Policy)) +
#   geom_line(size=2) +
#   labs(y='Total Catch (MT)') +
#   theme(text=element_text(size=18))
# #   
# #   # plot profit trend
#   
# ProfitPlot<-ggplot(PlotTrend[PlotTrend$variable=='TotalProfit' & PlotTrend$Policy %in% c('Historic','StatusQuoOpenAccess','CatchShare','Opt'),],aes(x=Year,y=value,color=Policy)) +
#             geom_line(size=2) +
#             coord_cartesian(ylim=c(-100000000000,100000000000)) +
#             labs(title='Trends and Future Prospects for Global Fisheries',y=paste('Percent Above B/Bmsy of ',RecoveryThreshold,sep='')) +
#             theme(text=element_text(size=18))
  
#   pdf(file=paste(FigureFolder, 'Recovery Trajectories.pdf',sep=''),height=10,width=12)
#   
#   grid.arrange(arrangeGrob(StatusPlot,CatchPlot,ProfitPlot,ncol=1))
#   
#   dev.off()
  
  # return()
}