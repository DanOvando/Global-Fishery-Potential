CodyPlots<- function()
{
  
  data<-read.csv(paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep=''))
  data2<-read.csv(paste(ResultFolder,'UnLumped Projection DataOverfish Only Country Upsides.csv',sep=''))
  # source("colorlegend2.R")
  
  unique(data$Policy)
  Policy<-'CatchShare'
  AllStocks<-data
  OverfishStocks<-data2
  head(OverfishStocks)
  # Subset to desired policy
  AllStocks  		<-AllStocks[AllStocks$Policy==Policy,]
  OverfishStocks		<-OverfishStocks[OverfishStocks$Policy==Policy,]
  
  # subset to stocks that have overfished stocks
  tmp				<-unique(OverfishStocks$Country)
  tmp2				<-unique(AllStocks$Country)
  useInd			<-match(tmp,tmp2)
  useInd			<-useInd[!is.na(useInd)]
  
  AllStocks			<-AllStocks[useInd,]
  
  #==calculate quantities to graph
  colnames(AllStocks)
  #==find global values
  #trevGLBChangeBioCur	<-100*(sum(OverfishStocks$AbsChangeTotalBiomass)/sum(AllStocks$TotalBiomass))
  #trevGLBChangeCatchCur	<-100*(sum(OverfishStocks$AbsChangeTotalCatch)/sum(AllStocks$TotalCatch))
  trevGLBChangeBioCur	<-100*(sum(OverfishStocks$AbsChangeTotalBiomass)/sum(AllStocks$TotalBiomassBaseline))
  trevGLBChangeCatchCur	<-100*(sum(OverfishStocks$AbsChangeTotalCatch)/sum(AllStocks$TotalCatchBaseline))
  trevGLBChangeBioSQ	<-100*(sum(OverfishStocks$AbsChangeTotalBiomass)/sum(AllStocks$TotalBiomassSQ))
  trevGLBChangeCatchSQ	<-100*(sum(OverfishStocks$AbsChangeTotalCatch)/sum(AllStocks$TotalCatchSQ))
  GlobalMSY			<-sum(AllStocks$TotalMSY)
  
  #==trevor denominator
  #trevPercChangeBioCur	<-100*OverfishStocks$AbsChangeTotalBiomass/AllStocks$TotalBiomass
  #trevPercChangeCatchCur	<-100*OverfishStocks$AbsChangeTotalCatch/AllStocks$TotalCatch
  trevPercChangeBioCur	<-100*OverfishStocks$AbsChangeTotalBiomass/AllStocks$TotalBaselineBiomass
  trevPercChangeCatchCur	<-100*OverfishStocks$AbsChangeTotalCatch/AllStocks$TotalBaselineCatch
  trevPercChangeBioSQ	<-100*OverfishStocks$AbsChangeFromSQTotalBiomass/AllStocks$TotalBiomassSQ
  trevPercChangeCatchSQ	<-100*OverfishStocks$AbsChangeFromSQTotalCatch/AllStocks$TotalCatchSQ
  trevPercChangeNPV		<-(100*(OverfishStocks$TotalNPV-OverfishStocks$TotalNPVSQ)/AllStocks$TotalNPVSQ)*sign(AllStocks$TotalNPVSQ)
  
  #=========================================
  # TEST OUT JUST AN FMSY RATHER THAN THE SQ
  #=========================================
  
  pdf(file=paste(FigureFolder,'Figure 1.pdf',sep=''))
  #pull Fmsy data
  #trevPercChangeBioSQ	<-100*OverfishStocks$AbsChangeTotalBiomass/AllStocks$TotalBaselineBiomass
  #trevPercChangeCatchSQ	<-100*OverfishStocks$AbsChangeTotalCatch/AllStocks$TotalBaselineCatch
  
  #trevPercChangeBioSQ	<-100*OverfishStocks$AbsChangeFromSQTotalBiomass/AllStocks$TotalBiomassSQ
  #trevPercChangeCatchSQ	<-100*OverfishStocks$AbsChangeFromSQTotalCatch/AllStocks$TotalCatchSQ
  #trevPercChangeNPV		<-100*(OverfishStocks$TotalNPV-OverfishStocks$TotalNPVSQ)/AllStocks$TotalNPVSQ
  
  
  #cbind(as.character(AllStocks$Country),AllStocks$TotalMSY,
  #cbind(as.character(OverfishStocks$Country),OverfishStocks$PercChangeFromSQTotalCatch)
  
  #==bound x and y
  cutin<- -105
  cutoff<- 2000
  xlimIn<-c(cutin,cutoff)
  ylimIn<-c(cutin,cutoff)
  sizeCirc<-.45
  colNPVrng<-300
  trevPercChangeNPV[trevPercChangeNPV>cutoff]		<-cutoff
  trevPercChangeCatchCur[trevPercChangeCatchCur>cutoff]	<-cutoff
  trevPercChangeCatchSQ[trevPercChangeCatchSQ>cutoff]	<-cutoff
  trevPercChangeBioCur[trevPercChangeBioCur>cutoff]	<-cutoff
  trevPercChangeBioSQ[trevPercChangeBioSQ>cutoff]		<-cutoff
  
  
  #=========================================
  # plot first panel
  #=========================================
  colQuant3			<-trevPercChangeCatchCur
  colQuant4			<-trevPercChangeCatchSQ
  colQuant			<-OverfishStocks$PercChangeTotalCatch
  colQuant2			<-OverfishStocks$PercChangeFromSQTotalCatch
  radius 			<- sqrt( AllStocks$TotalMSY/ pi )
  radius2 			<- sqrt( OverfishStocks$TotalMSY/ pi )
  
  max(AllStocks$TotalMSY)
  #==scales circle size between two graphs
  sizeCirc2			<-sizeCirc*max(radius2, na.rm=T)/max(radius,na.rm=T)
  #==make colors for catch
  bound<-max(c(abs(colQuant),abs(colQuant2),abs(colQuant3),abs(colQuant4)),na.rm=T)
  bound<-150
  colrange<-seq(-bound,bound,)			# Tyler had these bounded
  col <-colorRampPalette(c("green","white","blue"))(length(colrange))
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
  
  #==Plot the plot
  dev.new(width=6,height=6)
  #pdf("C:/Users/Cody/Desktop/Figure1.pdf",height=6,width=6)
  par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,2,1)) 
  
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  
  par(new=T)
  symbols(x=OverfishStocks$PercChangeTotalBiomass,y=OverfishStocks$PercChangeFromSQNPV,circles=radius2,
          bg=useCol,fg='black',inches=sizeCirc2,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  # points(x=trevGLBChangeBioCur,y=trevGLBChangeCatchCur,
  #            col="gold",pch=16,cex=2)
  
  #legend.bubble(x=cutoff,y=cutoff, z=c(2,7,15), maxradius = sizeCirc, n = 3, round = 0, bty = "n", mab = 1.2, 
  #    bg = NULL, inset = 0, pch = 21, pt.bg = NULL, txt.cex = .65, 
  #    txt.col = NULL, font = NULL)
  #legend(x=100,y=153,bty='n',legend="Total MSY (mt)",cex=.8)
  legend(x=1.5*cutin,y=cutoff, "Today",bty='n',cex=.8)
  
  par(xpd=NA)
  text(x=.5*cutoff,y=1.11*cutoff,"% change in catch",cex=.8)
  color.legend2(0,cutoff*1.06,cutoff*.94,1.08*cutoff,rect.col=col,legend="")
  par(xpd=NA)
  text(x=-62,y=1.075*cutoff,"<-100%",cex=.65)
  text(x=cutoff*1.02,y=1.075*cutoff,">100%",cex=.65)
  par(xpd=FALSE)
  
  #=========================================
  # plot second panel
  #=========================================
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant2[i]) == min(abs(colrange-colQuant2[i])))] )
  
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  par(new=T)
  symbols(x=OverfishStocks$PercChangeFromSQTotalBiomass,y=OverfishStocks$PercChangeFromSQNPV,circles=radius2,
          bg=useCol,fg='black',inches=sizeCirc2,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  legend(x=1.7*cutin,y=cutoff, "Status quo",bty='n',cex=.8)
  
  #=========================================
  # plot third panel
  #=========================================
  
  
  #==make colors for catch
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant3[i]) == min(abs(colrange-colQuant3[i])))] )
  
  #==plot
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  
  par(new=T)
  
  
  symbols(x=trevPercChangeBioCur,y=trevPercChangeNPV,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  
  #legend.bubble(x=450,y=450, z=c(2,8,17), maxradius = 175, n = 3, round = 0, bty = "n", mab = 1.2, 
  #    bg = NULL, inset = 0, pch = 21, pt.bg = NULL, txt.cex = 1, 
  #    txt.col = NULL, font = NULL)
  legend(x=250,y=700,bty='n',legend="Total MSY (mt)",cex=.8)
  
  
  #=========================================
  # plot fourth panel
  #=========================================
  useCol<-rep(0,length(colQuant2))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant4[i]) == min(abs(colrange-colQuant4[i])))] )
  
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n')
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  par(new=T)
  
  symbols(x=trevPercChangeBioSQ,y=trevPercChangeNPV,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  
  # points(x=trevGLBChangeBioSQ,y=trevGLBChangeCatchSQ,
  #            col="gold",pch=16,cex=2)
  
  mtext(side=1,outer=T,"% change in biomass",line=2)
  mtext(side=2,outer=T,"% change in NPV",line=2.2)
  #mtext(side=4,"Overfished",adj=.8,las=3,outer=T)
  #mtext(side=4,"All stocks",adj=.2,las=3,outer=T)
  legend(x=1.7*cutin,y=cutoff, "Status quo",bty='n',cex=.8)
  
  
  dev.off()
  
  
  
}