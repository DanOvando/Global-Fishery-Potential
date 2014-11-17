#####################
##
## Upside Plot 2.0
##
#####################

UpsidePlot<-function(CumulativesFinal,FinalYearFinal,Policy,XVar,YVar,DotSize,Limit)
{
  # DotSize<-'Food'
  # XVar<-'PercChangeTotalBiomass'
  # YVar<-'NPV'
  # Limit<-300
  # Policy<-'CatchShare'
  
  Cumulatives<-CumulativesFinal[CumulativesFinal$Policy==Policy,]
  
  FinalYear<-FinalYearFinal[FinalYearFinal$Policy==Policy,]
  
  # Build dataframe to plot based on axis inputs and country
  c<-Cumulatives$Country
  
  if(DotSize=="Food") { s<-Cumulatives$Food}
  if(DotSize=="NPV"){s<-Cumulatives$NPV}
  if(DotSize=="Fish"){s<-Cumulatives$Fish}
  
  if(YVar=="NPV") { y<-Cumulatives$NPV}
  if(YVar=="PercChangeTotalProfits"){y<-FinalYear$PercChangeTotalProfits}
  if(YVar=="PercChangeFromSQMedianProfits"){y<-FinalYear$PercChangeFromSQMedianProfits}
  if(YVar=="PercChangeMedianProfits"){y<-FinalYear$PercChangeMedianProfits}
  
  if(XVar=='PercChangeFromSQTotalBiomass'){x<-FinalYear$PercChangeFromSQTotalBiomass} 
  if(XVar=='PercChangeTotalBiomass'){x<-FinalYear$PercChangeTotalBiomass}
  if(XVar=='PercChangeFromSQMedianBiomass'){x<-FinalYear$PercChangeFromSQMedianBiomass}
  if(XVar=='PercChangeMedianBiomass'){x<-FinalYear$PercChangeMedianBiomass}
  if(XVar=='PercChangeTotalCatch'){x<-FinalYear$PercChangeTotalCatch}
  if(XVar=="Food") { x<-Cumulatives$Food}
  
  PlotData<-data.frame(c,x,y,s)
  colnames(PlotData)<-c("Country","xVar","yVar","Size")
  
  PlotData$xVar[PlotData$xVar>Limit]<-Limit # change value to limit variable?
  PlotData$yVar[PlotData$yVar>Limit]<-Limit
  PlotData$Size[PlotData$Size>100]<-100
  
  pdf(file=paste(FigureFolder,'Economic Upside Plot.pdf',sep=''),height=10,width=14,pointsize=6)
  print(ggplot(PlotData,aes(xVar,yVar,size=Size)) +
          geom_point(aes(color=Country)) +
          #   guides(color=FALSE) +
          coord_cartesian(xlim=c(-30,Limit),ylim=c(-30,Limit)) +
          scale_size_continuous(range=c(6,12), 
                                breaks=c(25,50,75,100), 
                                labels=c("25%", "50%",'75%','100+%')) +
          theme(text=element_text(size=20)) +
          geom_abline(intercept=0,slope=0) +
          geom_vline(xintercept=0) +
          labs(title=paste(Policy,"Upside Percentages",sep=" "), x = "Percent Change from Current Biomass",
               y = "Percent Change from SQ NPV",size="% Change\n SQ Food"))
  dev.off()
  
  return()
}



