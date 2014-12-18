##########################################
### Global Upside Model Paper
### Figure 2: Upside Comparisons of P1-P4
### for select major fishing nations
##########################################

# CumulativesFinal used for NPV values relative to status quo
# FinalYearFinal used for all other values relative to status quo
# Countries<-c('Global','USA','China','Indonesia','New Zealand','Mexico')

# P0 = Status Quo
# P1 = Fmsy
# P2 = CloseDown
# P3 = Opt
# P4 = Catch Shares
# DotSize<-'NPV'
# XVar<-'Fish'
# YVar<-'Food'
# Limit<-100

Figure2<-function(CumulativesFinal,FinalYearFinal,Countries,Limit)
{
  # subset upside data to countries of interest
  Cumulatives<-CumulativesFinal[CumulativesFinal$Country %in% Countries,]
  
  FinalYear<-FinalYearFinal[FinalYearFinal$Country %in% Countries,]
  
  # Build dataframe to plot based on axis inputs and country
  c<-Cumulatives$Country
  p<-Cumulatives$Policy
  
  Plot2Data<-data.frame(c,p)
  colnames(Plot2Data)<-c("Country",'Policy')
  
  Plot2Data$xVar<-NA
  Plot2Data$yVar<-NA
  Plot2Data$Size<-NA
  
  for (a in 1:nrow(Plot2Data))
  {
    WhereC<-Cumulatives$Country==Plot2Data$Country[a] & Cumulatives$Policy==Plot2Data$Policy[a]
    
    WhereF<-FinalYear$Country==Plot2Data$Country[a] & FinalYear$Policy==Plot2Data$Policy[a]
    
    # Dotsize = NPV
    Plot2Data$Size[a]<-Cumulatives$NPV[WhereC]
    
    Plot2Data$yVar[a]<-FinalYear$PercChangeFromSQTotalCatch[WhereF]
    
    Plot2Data$xVar[a]<-FinalYear$PercChangeFromSQTotalBiomass[WhereF] 
  }
  
  Plot2Data$xVar[Plot2Data$xVar>Limit]<-Limit 
  Plot2Data$yVar[Plot2Data$yVar>Limit]<-Limit
  Plot2Data$Size[Plot2Data$Size>Limit]<-Limit
  
  # Figure 2 - Plot data for P1-P4 and wrap by Country
  pdf(file=paste(FigureFolder,'Figure 2.pdf',sep=''),height=10,width=14,pointsize=6)
  print(ggplot(Plot2Data[Plot2Data$Policy!='Food',],aes(xVar,yVar,size=Size)) +
          geom_point(aes(color=Policy),alpha=0.6) +
          facet_wrap(~Country,scales='free') +
          coord_cartesian(xlim=c(-30,100),ylim=c(-30,100)) +
          scale_size_continuous(range=c(2,20), 
                                breaks=c(20,40,60,80,100,120,140,160,180,200), 
                                labels=c("20%","40%",'60%','80%','100%','120%','140%','160%','180%','200%')) +
          theme(text=element_text(size=20)) +
          guides(color=guide_legend(override.aes=list(size=5))) +
          geom_abline(intercept=0,slope=0) +
          geom_vline(xintercept=0) +
          labs(x = "Percent Change from Status Quo Fish",
               y ="Percent Change from Status Quo Food",size="% Change\n SQ NPV"))
  dev.off()
  
  return(Figure2Data=Plot2Data)
}