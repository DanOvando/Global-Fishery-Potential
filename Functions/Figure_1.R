##########################################
### Global Upside Model Paper
### Figure 1: Country level upside 
### for Economically Optimal Policy
##########################################

# CumulativesFinal used for NPV values relative to status quo
# FinalYearFinal used for all other values relative to status quo

# DotSize<-'NPV'
# XVar<-'Fish'
# YVar<-'Food'
# Limit<-200
# Policy<-'Opt'
# Metric<-'Absolute'

Figure1<-function(CumulativesFinal,FinalYearFinal,Policy,Limit)
{
  # subset upside data to countries of interest
  Cumulatives<-CumulativesFinal[CumulativesFinal$Policy==Policy & !(CumulativesFinal$Country %in% c('Global','EU','Asia','Multinational','Lumped')),]
  
  FinalYear<-FinalYearFinal[FinalYearFinal$Policy==Policy & !(FinalYearFinal$Country %in% c('Global','EU','Asia','Multinational','Lumped')),]
  
  # Build dataframe to plot based on axis inputs and country
  c<-Cumulatives$Country
  p<-Cumulatives$Policy
  
  PlotData<-data.frame(c,p)
  colnames(PlotData)<-c("Country",'Policy')
  
  PlotData$PercXVar<-NA
  PlotData$PercYVar<-NA
  PlotData$PercSize<-NA
  PlotData$AbsXVar<-NA
  PlotData$AbsYVar<-NA
  PlotData$AbsSize<-NA
  
  for (a in 1:nrow(PlotData))
  {
    WhereC<-Cumulatives$Country==PlotData$Country[a] & Cumulatives$Policy==PlotData$Policy[a]
    
    WhereF<-FinalYear$Country==PlotData$Country[a] & FinalYear$Policy==PlotData$Policy[a]
    
    # Dot Size = NPV
    PlotData$PercSize[a]<-Cumulatives$NPV[WhereC]
    PlotData$AbsSize[a]<-Cumulatives$AbsNPV[WhereC]
    
    # Y-axis = Food
    PlotData$PercYVar[a]<-FinalYear$PercChangeFromSQTotalCatch[WhereF]
    PlotData$AbsYVar[a]<-FinalYear$AbsChangeFromSQTotalCatch[WhereF] 
    
    # X-axis = Fish
    PlotData$PercXVar[a]<-FinalYear$PercChangeFromSQTotalBiomass[WhereF]
    PlotData$AbsXVar[a]<-FinalYear$AbsChangeFromSQTotalBiomass[WhereF]
  }
  
  PlotData$PercXVar[PlotData$PercXVar>Limit]<-Limit 
  PlotData$PercYVar[PlotData$PercYVar>Limit]<-Limit
  PlotData$PercSize[PlotData$PercSize>Limit]<-Limit
  
  # Figure 1  
  pdf(file=paste(FigureFolder,'Figure 1 Percent.pdf',sep=''),height=8,width=10,pointsize=6)
  # Percentages
  panelA<-ggplot(PlotData,aes(PercXVar,PercYVar,size=PercSize)) +
    geom_point(color='blue',alpha=0.6) +
    coord_cartesian(xlim=c(-10,100),ylim=c(-10,100)) +
    scale_size_continuous(range=c(2,20), 
                          breaks=c(20,40,60,80,100,120,140,160,180,200), 
                          labels=c("20%","40%",'60%','80%','100%','120%','140%','160%','180%','200%')) +
    theme(text=element_text(size=18)) +
    geom_abline(intercept=0,slope=0) +
    geom_vline(xintercept=0) +
    labs( x = 'Percent Change from Status Quo Fish',
          y ="Percent Change from Status Quo Food",size='% Change from\n Status Quo NPV')
  
  print(panelA)
  dev.off()
  
  # Absolutes
  pdf(file=paste(FigureFolder,'Figure 1 Absolutes.pdf',sep=''),height=8,width=10,pointsize=6)
  panelB<-ggplot(PlotData,aes(AbsXVar,AbsYVar,size=AbsSize)) +
    geom_point(color='green',alpha=0.6) +
    scale_size_continuous(range=c(2,20)) +
    theme(text=element_text(size=18)) +
    geom_abline(intercept=0,slope=0) +
    geom_vline(xintercept=0) +
    labs( x = 'Absolute Change from Status Quo Fish',
          y ="Absolute Change from Status Quo Food",size='Absolute Change from\n Status Quo NPV')
  
  print(panelB)
  dev.off()
  
  # Both Percent and Absolute printed together
  pdf(file=paste(FigureFolder,'Figure 1 Both.pdf',sep=''),height=8,width=16,pointsize=6)
  print(grid.arrange(arrangeGrob(panelA,panelB,ncol=2)))  
  dev.off()
  
  return(FigureOneData=PlotData)
}