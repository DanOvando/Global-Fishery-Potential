# RecoveryData<-UnlumpedProjectionData
# OnlyOverfish<-FALSE
# RecoveryThreshold<-0.8
# StartYear<-1980

RecoveryTrend<-function(ProjectionData,RecoveryThreshold,OnlyOverfish,StartYear)
{
  
  RecoveryData<-ProjectionData
  
  RecoveryData<-RecoveryData
  
  if(OnlyOverfish==TRUE)
  {
    OverfishedIds<-ProjectionData$IdOrig[ProjectionData$Year==2012 & ProjectionData$BvBmsy<1]
    
    RecoveryData<-ProjectionData[ProjectionData$IdOrig %in% OverfishedIds,]
  }

  ### Create two new 'hybrid' SQ policies for plot-------------------------------------------------

  # 1) Where all non RAM and Catch share stocks go to Open Access
  
  ram<-RecoveryData[RecoveryData$Policy=='Fmsy' & RecoveryData$Dbase=='RAM' & RecoveryData$CatchShare!=1,]

  ramids<-unique(ram$IdOrig)
  
  cs<-RecoveryData[RecoveryData$Policy=='Fmsy' & RecoveryData$CatchShare==1,]

  csids<-unique(cs$IdOrig)
  
  otherids<-RecoveryData$IdOrig[RecoveryData$Year==2012 & (!(RecoveryData$IdOrig %in% c(ramids,csids)))]

  other<-RecoveryData[(RecoveryData$IdOrig %in% c(otherids)) & RecoveryData$Policy=='StatusQuoOpenAccess',]
  
  hybrid<-rbind(ram,cs,other)

  hybrid$Policy<-'Business As Usual'
  
  PlotData<-rbind(RecoveryData,hybrid)
  
  # 2)
  
  overFFids<-RecoveryData$IdOrig[RecoveryData$Year==2012 & (!(RecoveryData$IdOrig %in% c(ramids,csids)) & 
                         ((RecoveryData$FvFmsy>1 & RecoveryData$BvBmsy<1) | (RecoveryData$FvFmsy>1 & RecoveryData$BvBmsy>1) |
                            (RecoveryData$FvFmsy<1 & RecoveryData$BvBmsy<1)))]
  
  overff<-RecoveryData[(RecoveryData$IdOrig %in% overFFids) & RecoveryData$Policy=='StatusQuoOpenAccess',]
  
  mctofids<-RecoveryData$IdOrig[RecoveryData$Year==2012 & (!(RecoveryData$IdOrig %in% c(ramids,csids)) & 
                          (RecoveryData$FvFmsy<1 & RecoveryData$BvBmsy>1))]
  
  mctofid<-RecoveryData[(RecoveryData$IdOrig %in% mctofids) & RecoveryData$Policy=='StatusQuoBForever',]
  
  hybrid2<-rbind(ram,cs,overff,mctofid)
  
  hybrid2$Policy<-'Business As Usual Optimistic'
  
  PlotData<-rbind(PlotData,hybrid2)
  
  # Identify recovered fisheries through time (definiing recovery as >=0.9)
  
  PlotData$Recovered[PlotData$BvBmsy>RecoveryThreshold]<-TRUE
  
  RecoveryTrend<-ddply(PlotData,c('Policy','Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),
                       Stocks=length(unique(IdOrig)),TotalRecovered=sum(Recovered,na.rm=T),
                       PercentHealthy=100*(sum(Recovered,na.rm=T)/length(unique(IdOrig))))
  
  PlotTrend<-melt(RecoveryTrend[RecoveryTrend$Year>=StartYear,],measure.vars=c('PercentHealthy','TotalCatch','TotalProfit'))
  
  ### Upside trajectory plots------------------------------------------------------------------

# pdf(file=paste(FigureFolder, '2015 Recovery Trajectories.pdf',sep=''),height=10,width=16)  
#   
# print(ggplot(PlotTrend[PlotTrend$Policy %in% c('Historic','Fmsy','Opt','CatchShare','Business As Usual'),],aes(x=Year,y=value,color=Policy)) +
#     geom_line(size=2) +
#     facet_wrap(~variable,scales='free_y',ncol=1) +
#     theme(text=element_text(size=18)))
#  
# dev.off()

#   # plot recovery trends separately

pdf(file=paste(FigureFolder,'Percent Healthy Recovery Trend.pdf',sep=''),width=8,height=5)

print(ggplot(PlotTrend[PlotTrend$variable=='PercentHealthy' & PlotTrend$Policy %in% c('Historic','Fmsy','Opt','Business As Usual','Business As Usual Optimistic'),],
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
  
  
  
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))       
  
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