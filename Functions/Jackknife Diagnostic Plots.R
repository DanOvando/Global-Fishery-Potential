JackknifePlots<- function(PlotJack,FigureFolder)
{
  PlotJack$ProportionalError[PlotJack$ProportionalError>500]<- 500
  
  pdf(file=paste(FigureFolder,'Observed vs Predicted BvBmsy.pdf',sep=''))
  print(ggplot(data=subset(PlotJack,Model='PRM'),aes(y=ModelBvBmsy,x=RamBvBmsy))+geom_point(alpha=0.2)+facet_wrap(~Model)
   +geom_smooth(method='lm')+geom_abline(intercept=0,slope=1,color='red'))
  dev.off()
  
  pdf(file=paste(FigureFolder,'Observed vs Predicted MSY.pdf',sep=''))
  print(ggplot(data=subset(PlotJack,Model='Cmsy'),aes(y=CmsyMSY,x=RamMSY))+geom_point(alpha=0.2)+facet_wrap(~Model)
   +geom_smooth(method='lm')+geom_abline(intercept=0,slope=1,color='red'))
  dev.off()
  
  pdf(file=paste(FigureFolder,'Proportional error in BvBmsy by time and country.pdf',sep=''))
  B_Error_Time_Country<- (ggplot(data=subset(PlotJack,Year>1985 & Model=='CmsyB'),aes(factor(Year),ProportionalError))+
     geom_boxplot(outlier.shape=NA,fill='steelblue2')+
     coord_cartesian(ylim=c(-200,200))+
     #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
     facet_wrap(~Country,scales='free')+
     geom_abline(intercept=0,slope=0)+
     ylab('Proportional Error (%) in B/Bmsy')+
     xlab('Time')+
     scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))
  print(B_Error_Time_Country)
  
  dev.off()
  
  
  pdf(file=paste(FigureFolder,'Proportional error in FvFmsy by time and country.pdf',sep=''))
  F_Error_Time_Country<- (ggplot(data=subset(PlotJack,Year>1985 & Model=='CmsyB' & is.infinite(ProportionalFError)==F & is.na(ProportionalFError)==F),aes(factor(Year),ProportionalFError))+
    geom_boxplot(outlier.shape=NA,fill='steelblue2')+
    coord_cartesian(ylim=c(-200,200))+
    #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
    facet_wrap(~Country,scales='free')+
    geom_abline(intercept=0,slope=0)+
    ylab('Proportional Error (%) in F/Fmsy')+
    xlab('Time')+
    scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))
  print(F_Error_Time_Country)
  
  dev.off()
  
  pdf(file=paste(FigureFolder,'Proportional error in MSY by country.pdf',sep=''))
  MSY_Error_By_Country<- (ggplot(data=subset(PlotJack,Year==2012 & Model=='CmsyB'),aes(x=Country,y=ProportionalMSYError))+
    geom_boxplot(outlier.shape=NA,fill='steelblue2')+
    coord_cartesian(ylim=c(-200,200))+
    geom_abline(intercept=0,slope=0)+
    xlab("")+
    ylab('Proportional Error (%) in MSY')+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
  print(MSY_Error_By_Country)
  
  dev.off()
  
  pdf(file=paste(FigureFolder,'Proportional error in MSY.pdf',sep=''))
  MSY_Error<- (ggplot(data=subset(PlotJack,Year==2012 & Model=='CmsyB'),aes(x=1,y=ProportionalMSYError))+
                 geom_boxplot(outlier.shape=NA,fill='steelblue2')+
                 coord_cartesian(ylim=c(-100,100))+
                 geom_abline(intercept=0,slope=0)+
                 ylab('Proportional Error (%) in MSY')+xlab('MSY'))
  print(MSY_Error)
  
  dev.off()
  
  pdf(file=paste(FigureFolder,'Proportional error in BvBmsy by time.pdf',sep=''))
  
 B_Error_By_Time<- ( ggplot(data=subset(PlotJack,Year>1985 & Model=='CmsyB'),aes(factor(Year),ProportionalError))+
    geom_boxplot(outlier.shape=NA,fill='steelblue2')+
    coord_cartesian(ylim=c(-200,200))+
    #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
    geom_abline(intercept=0,slope=0)+
    ylab('Proportional Error (%) in B/Bmsy')+
    xlab('Time')+
    scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))
  
  print(B_Error_By_Time)
  
  dev.off()
  
 pdf(file=paste(FigureFolder,'Proportional error in FvFmsy by time.pdf',sep=''))
 
 F_Error_By_Time<- ( ggplot(data=subset(PlotJack,Year>1985 & Model=='CmsyB' & is.infinite(ProportionalFError)==F & is.na(ProportionalFError)==F),aes(factor(Year),ProportionalFError))+
                       geom_boxplot(outlier.shape=NA,fill='steelblue2')+
                       coord_cartesian(ylim=c(-200,200))+
                       #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
                       geom_abline(intercept=0,slope=0)+
                       ylab('Proportional Error (%) in F/Fmsy')+
                       xlab('Time')+
                       scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))
 
 print(F_Error_By_Time)
 
 dev.off() 
 
 
 
  pdf(file=paste(FigureFolder,'Proportional error in FvFmsy by time.pdf',sep=''))
  
 F_Error_By_Time<- (ggplot(data=subset(PlotJack,Year>1985 & Model=='CmsyB'),aes(factor(Year),ProportionalFError))+
                      geom_boxplot(outlier.shape=NA,fill='steelblue2')+
                      coord_cartesian(ylim=c(-200,200))+
                      #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
                      geom_abline(intercept=0,slope=0)+
                      ylab('Proportional Error (%) in F/Fmsy')+
                      xlab('Time')+
                      scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))
 
  print(F_Error_By_Time)
  
  dev.off()
  
  
  pdf(file=paste(FigureFolder,'Proportional error in MSY by Species Category.pdf',sep=''))
  
  print(ggplot(data=subset(PlotJack,Year==BaselineYear),aes((SpeciesCatName),ProportionalMSYError))+
          geom_boxplot(outlier.shape=NA,fill='steelblue2')+
          coord_cartesian(ylim=c(-200,200))+
          #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
          #         facet_wrap(~Country,scales='free')+
          geom_abline(intercept=0,slope=0)+
          ylab('Proportional Error in MSY (%)')+
          xlab('')+
          theme(axis.text.x=element_text(angle = 45, hjust = 0.9,vjust=0.9)))
  dev.off()
  
  pdf(file=paste(FigureFolder,'Proportional BvBmsy Error as a function of catch.pdf',sep=''))
 B_Error_By_Catch<- (ggplot(data=subset(PlotJack,Year>2000 & is.na(ProportionalError)==F),aes((Catch),(ProportionalError)))+
                       geom_point(aes(color=RamBvBmsy))+
                       #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
                       geom_abline(intercept=0,slope=0)+
                       geom_smooth(method='lm')+
                       ylab('Proportional Error (%) in BvBmsy')+
                       xlab('Catch')) 
 
  print(B_Error_By_Catch)
  #         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
  dev.off()
  
 
 
  pdf(file=paste(FigureFolder,'Proportional BvBmsy Error as a function of  log catch.pdf',sep=''))
  B_Error_By_LogCatch<- (ggplot(data=subset(PlotJack,Year>2000 & is.na(ProportionalError)==F & Catch>0),aes(log(Catch),(ProportionalError)))+
                           geom_point(aes(color=RamBvBmsy))+
                           #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
                           geom_abline(intercept=0,slope=0)+
                           geom_smooth(method='lm')+
                           ylab('Proportional Error (%) in BvBmsy')+
                           xlab('log(Catch)'))
  print(B_Error_By_LogCatch)
  #         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
  dev.off()
  
  pdf(file=paste(FigureFolder,'Proportional BvBmsy Error as a function of  RAM BvBmsy.pdf',sep=''))
  
 B_Error_By_RamB<- (ggplot(data=subset(PlotJack,Year>2000 & is.na(ProportionalError)==F),aes((RamBvBmsy),(ProportionalError)))+
                      geom_point(color='steelblue2',alpha=0.6)+
                      #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
                      geom_abline(intercept=0,slope=0)+
                      ylab('Proportional Error (%) in BvBmsy')+
                      xlab('RAM BvBmsy'))
 
  print(B_Error_By_RamB)
  #         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
  dev.off()
  
  LifetimeJack<- ddply(PlotJack,c('Id'),summarize,CatchMSY_MSY=unique(CmsyMSY),RamMSY=unique(RamMSY),TotalCatch=sum(Catch,na.rm=T))
  
  LifetimeJack$ProportionalMSYError<- 100*((LifetimeJack$CatchMSY_MSY-LifetimeJack$RamMSY)/LifetimeJack$RamMSY)
  
  pdf(file=paste(FigureFolder,'Proportional MSY Error as a function of log Lifetime Catch.pdf',sep=''))
  MSY_Error_By_LogCatch<- (ggplot(data=LifetimeJack,aes(log(TotalCatch),(ProportionalMSYError)))+
                             geom_point(color='steelblue2',alpha=0.8)+
                             #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
                             geom_abline(intercept=0,slope=0)+
                             geom_smooth(method='lm')+
                             ylab('Proportional Error (%) in MSY')+
                             xlab('log Lifetime Catch'))
  print(MSY_Error_By_LogCatch)
  #         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
  dev.off()
  
  save(B_Error_Time_Country,F_Error_Time_Country,MSY_Error,MSY_Error_By_LogCatch,F_Error_By_Time,
       B_Error_By_RamB,B_Error_By_Time,B_Error_By_LogCatch,F_Error_By_Time,MSY_Error_By_Country,file=paste(FigureFolder,'Jacknife Plots.rdata',sep=''))
   
}