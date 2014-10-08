UpsidePlot<-function(Data,Policy)
{
  
#   UpsideData<-CumulativesFinal[CumulativesFinal$Policy=="CatchShare" & is.na(CumulativesFinal$NPV)==F,]
  
  UpsideData<-Data[Data$Policy==Policy & is.na(Data$NPV)==F,]
  
  
  UpsideData$NPV[UpsideData$NPV>500]<-500
  UpsideData$Fish[UpsideData$Fish>200]<-200
  
  pdf(file=paste(FigureFolder,"Upside Plot.pdf",sep=''))
  layout(t(matrix(c(1,2))), widths = c(4,1), heights = c(1,1), respect = FALSE)
  par(mai=c(1,1,1,0),oma=c(1,1,0,1))
  plot(UpsideData$NPV~UpsideData$Fish,pch=16,ylim=c(-100,500),xlim=c(0,200),cex=2+(UpsideData$Food/100),
       col=rainbow((length(unique(UpsideData$Country)))),xlab="Percent Change from SQ Total Biomass",ylab="Percent Change from SQ NPV",
        main="Percentage Upsides")
  
  abline(h=0,v=0,lty=2)
  plot.new()
  par(mai=c(0,0,0,0))
  legend('center',legend=unique(UpsideData$Country),pch=16,col=rainbow((length(unique(UpsideData$Country)))),cex=0.7,bty='n')
 dev.off()
 
 return()
}



