CodyPlots<- function(ResultFolder,FigureFolder,Policy)
{
#   Policy<- 'CatchShare'
  #================================
  # compare: upside (optimal) to current for overfished stocks (trevor denominator)
  # compare: upside (optimal) to status quo for overfished stocks (trevor denominator)
  # both are in totals
  #================================
  library(plotrix)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(mapplots)
  library("ggmap")
  library(maptools)
  library(maps)
  library(rworldmap)
  library(SDMTools)
  
  ############# Figure 1 #############
  
  data<-read.csv(paste(ResultFolder,'Unlumped Projection DataAll Stocks Country Upsides.csv',sep=''))
  data2<-read.csv(paste(ResultFolder,'Unlumped Projection DataOverfish Only Country Upsides.csv',sep=''))
  
  #data<-read.csv("C:/Users/Cody/Desktop/UpsideData/PT figures/Lumped Projection DataAll Stocks Country Upsides.csv")
  #data2<-read.csv("C:/Users/Cody/Desktop/UpsideData/PT figures/Lumped Projection DataOverfish Only Country Upsides.csv")
  
  unique(data$Policy)
  AllStocks<-data
  OverfishStocks<-data2
  
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
  
  #pull Fmsy data
  #trevPercChangeBioSQ	<-100*OverfishStocks$AbsChangeTotalBiomass/AllStocks$TotalBaselineBiomass
  #trevPercChangeCatchSQ	<-100*OverfishStocks$AbsChangeTotalCatch/AllStocks$TotalBaselineCatch
  
  #trevPercChangeBioSQ	<-100*OverfishStocks$AbsChangeFromSQTotalBiomass/AllStocks$TotalBiomassSQ
  #trevPercChangeCatchSQ	<-100*OverfishStocks$AbsChangeFromSQTotalCatch/AllStocks$TotalCatchSQ
  #trevPercChangeNPV		<-100*(OverfishStocks$TotalNPV-OverfishStocks$TotalNPVSQ)/AllStocks$TotalNPVSQ
  
  
  #cbind(as.character(AllStocks$Country),AllStocks$TotalMSY,
  #cbind(as.character(OverfishStocks$Country),OverfishStocks$PercChangeFromSQTotalCatch)
  
  #==bound x and y
  pdf(paste(FigureFolder,'Figure 1.pdf',sep=''))
  
  cutin<- -75
  cutoff<- 250
  xlimIn<-c(cutin,cutoff)
  ylimIn<-c(cutin,cutoff)
  sizeCirc<-.35
  colNPVrng<-300
  trevPercChangeNPV[trevPercChangeNPV>cutoff]		<-1000
  trevPercChangeCatchCur[trevPercChangeCatchCur>cutoff]	<-cutoff
  trevPercChangeCatchSQ[trevPercChangeCatchSQ>cutoff]	<-cutoff
  trevPercChangeBioCur[trevPercChangeBioCur>cutoff]	<-cutoff
  trevPercChangeBioSQ[trevPercChangeBioSQ>cutoff]		<-cutoff
  
  OverfishStocks$PercChangeTotalBiomass[OverfishStocks$PercChangeTotalBiomass>cutoff]			<-cutoff
  OverfishStocks$PercChangeTotalCatch[OverfishStocks$PercChangeTotalCatch>cutoff]			<-cutoff
  OverfishStocks$PercChangeFromSQTotalBiomass[OverfishStocks$PercChangeFromSQTotalBiomass>cutoff]	<-cutoff
  OverfishStocks$PercChangeFromSQTotalCatch[OverfishStocks$PercChangeFromSQTotalCatch>cutoff]	<-cutoff
  
  outs<-cbind(OverfishStocks$PercChangeTotalBiomass,OverfishStocks$PercChangeTotalCatch,
              OverfishStocks$PercChangeFromSQTotalBiomass,OverfishStocks$PercChangeFromSQTotalCatch,
              trevPercChangeBioCur,trevPercChangeBioSQ,trevPercChangeCatchCur,trevPercChangeCatchSQ,
              as.character(OverfishStocks$Country ))
  
  outs[,1:8]<-round(as.numeric(outs[,1:8]),2)
  
  colnames(outs)<-c("Bio","Catch","BioSQ","CatSQ","BioTRV","CatchTRV","BioSQtrv","CatSQtrv","Country")
#   write.csv(outs,"C:/Users/Cody/Desktop/Fig1data.csv")
  #=========================================
  # plot first panel
  #=========================================
  colQuant3			<-trevPercChangeNPV
  colQuant4			<-trevPercChangeNPV
  colQuant			<-OverfishStocks$PercChangeFromSQNPV
  colQuant2			<-OverfishStocks$PercChangeFromSQNPV
  radius 			<- sqrt( AllStocks$TotalMSY/ pi )
  radius2 			<- sqrt( OverfishStocks$TotalMSY/ pi )
  
  #==scales circle size between two graphs
  sizeCirc2			<-sizeCirc*max(radius2, na.rm=T)/max(radius,na.rm=T)
  #==make colors for catch
  bound<-max(c(abs(colQuant),abs(colQuant2),abs(colQuant3),abs(colQuant4)),na.rm=T)
  #hist(c(abs(colQuant),abs(colQuant2),abs(colQuant3),abs(colQuant4)))
  
  bound<-150
  colrange<-seq(0,bound,)			# Tyler had these bounded
  #col <-colorRampPalette(c("green","white","blue"))(length(colrange))
  col <-colorRampPalette(c("white","blue"))(length(colrange))
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
  
  #==Plot the plot
  #dev.new(width=6,height=6)
  # pdf("C:/Users/Cody/Desktop/Figure1.pdf",height=6,width=6)
  par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,2,1)) 
  
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  
  par(new=T)
  symbols(x=OverfishStocks$PercChangeTotalBiomass,y=OverfishStocks$PercChangeTotalCatch,circles=radius2,
          bg=useCol,fg='black',inches=sizeCirc2,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  #text(OverfishStocks$PercChangeTotalBiomass,jitter(OverfishStocks$PercChangeTotalCatch,factor=10),
  #    OverfishStocks$Country,cex=.5)
  
  legend(x=1.25*cutin,y=cutoff, "Today",bty='n',cex=.8)
  
  par(xpd=NA)
  text(x=.5*cutoff,y=1.12*cutoff,"% change in NPV",cex=.8)
  color.legend2(0,cutoff*1.07,cutoff*.94,1.09*cutoff,rect.col=col,legend="")
  par(xpd=NA)
  text(x=-10,y=1.09*cutoff,"0",cex=.65)
  text(x=cutoff*1.05,y=1.09*cutoff,">1000%",cex=.65)
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
  symbols(x=OverfishStocks$PercChangeFromSQTotalBiomass,y=OverfishStocks$PercChangeFromSQTotalCatch,circles=radius2,
          bg=useCol,fg='black',inches=sizeCirc2,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  legend(x=1.4*cutin,y=cutoff, "BAU",bty='n',cex=.8)
  #text(OverfishStocks$PercChangeFromSQTotalBiomass,jitter(OverfishStocks$PercChangeFromSQTotalCatch,factor=10),
  #   OverfishStocks$Country,cex=.5)
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
  
  
  symbols(x=trevPercChangeBioCur,y=trevPercChangeCatchCur,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  
  #legend.bubble(x=450,y=450, z=c(2,8,17), maxradius = 175, n = 3, round = 0, bty = "n", mab = 1.2, 
  #    bg = NULL, inset = 0, pch = 21, pt.bg = NULL, txt.cex = 1, 
  #    txt.col = NULL, font = NULL)
  legend(x=250,y=700,bty='n',legend="Total MSY (mt)",cex=.8)
  legend(x=1.25*cutin,y=cutoff, "Today",bty='n',cex=.8)
  
  
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
  
  symbols(x=trevPercChangeBioSQ,y=trevPercChangeCatchSQ,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  
  mtext(side=1,outer=T,"% change in biomass",line=2)
  mtext(side=2,outer=T,"% change in catch",line=2.3)
  #mtext(side=4,"Overfished",adj=.8,las=3,outer=T)
  #mtext(side=4,"All stocks",adj=.2,las=3,outer=T)
  legend(x=1.4*cutin,y=cutoff, "BAU",bty='n',cex=.8)
  
  
  
  dev.off()
  
  ######## Figure 2 ############
  
  pdf(paste(FigureFolder,'Figure 2.pdf',sep=''))
  
  
  AllStocks<-data
  OverfishStocks<-data2
  
  # Subset to desired policy
  AllStocks  		<-AllStocks[AllStocks$Policy==Policy,]
  OverfishStocks		<-OverfishStocks[OverfishStocks$Policy==Policy,]
  
  # subset to stocks that have overfished stocks
  tmp				<-unique(OverfishStocks$Country)
  tmp2				<-unique(AllStocks$Country)
  useInd			<-match(tmp,tmp2)
  useInd			<-useInd[!is.na(useInd)]
  AllStocks			<-AllStocks[useInd,]
  
  # colnames(AllStocks)
  #==find which countries have the maximum of a quantity (NPV here)
  topCut<-11
  sizeCirc<-.65
  OverfishStocks<-OverfishStocks[order(AllStocks$AbsChangeFromSQNPV,decreasing=T),]
  AllStocks<-AllStocks[order(AllStocks$AbsChangeFromSQNPV,decreasing=T),]
  OverfishStocks<-OverfishStocks[1:topCut,]
  AllStocks<-AllStocks[1:topCut,]
  OverfishStocks<-OverfishStocks[OverfishStocks$Country!="Multinational",]
  labs<-OverfishStocks$Country
  
  #==set axes limits
  ycut<-15
  xcut<-90
  ylimIn<-c(-2,ycut)
  xlimIn<-c(-3,xcut)
  colnames(OverfishStocks)
  
  #==calculate quantities to graph
  xQuant			<-OverfishStocks$AbsChangeFromSQTotalBiomass/1000000
  yQuant			<-OverfishStocks$AbsChangeFromSQNPV/1000000000
  zQuant			<-OverfishStocks$TotalMSY
  colQuant			<-OverfishStocks$AbsChangeFromSQTotalCatch/1000000
  
  discRt			<-0.05
  TimeHor			<-15
  yQuant			<-yQuant*discRt/(1-(1+discRt)^-TimeHor)
  
  #==represent MSY by area rather than radius
  radius <- sqrt( zQuant/ pi )
  
  #==make colors for NPV
  bound<-max(abs(colQuant),na.rm=T)
  colrange<-seq(-bound,bound,(2*bound)/100)			# Tyler had these bounded
  col <-colorRampPalette(c("red","white","green"))(length(colrange))
  col <-colorRampPalette(c("green","white","blue"))(length(colrange))
  col <-colorRampPalette(c("white","lightblue","blue"))(length(colrange))
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
  
  #==Plot the plot
  #dev.new(width=6,height=6)

  par(mar=c(.1,.1,.1,.1),oma=c(4,4,4,4)) 
  #par(mar=c(.1,.1,.1,.1),oma=c(4,4,1,3)) 
  plot(-exp(50),las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  par(new=T)
  symbols(x=xQuant,y=yQuant,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  text(xQuant,jitter(yQuant,factor=10),labs,cex=.75)
  mtext(side=2,"Change in annual profit ($billion)",line=2.25)
  mtext(side=1,"Change in biomass (million MT)",line=2.25)
  
  par(xpd=NA)
  #text(x=.5*cutoff,y=1.11*cutoff,"% change in catch",cex=.8)
  mtext(side=3,"Change in catch (million MT)",line=.8,cex=.8)
  color.legend2(0,ycut*1.06,xcut*.94,1.08*ycut,rect.col=col,legend="")
  par(xpd=NA)
  text(x=-3,y=1.075*ycut,round(min(colQuant),2),cex=.85)
  text(x=xcut,y=1.075*ycut,round(max(colQuant),2),cex=.85)
  par(xpd=FALSE)
  dev.off()
  
  ######## Figure 3 ###########
  
  
  PlotTrend<-read.csv(paste(ResultFolder,'PlotTrend.csv',sep=''))
  
  pdf(paste(FigureFolder,'Figure 3.pdf',sep=''))
  
  years<-unique(PlotTrend$Year)
  xlimIn<-c(min(years),max(years))
  ylimIn<-c(-2,100)
  sizeCirc<-.1
  
  #==plot
  #==Select a policy
  #==only show catch share--no optimal
#   chosen<-c("CatchShare","Fmsy","Business As Usual Pessimistic","Business As Usual","Historic")
chosen<-c("Catch Share Three","Fmsy Three","Business As Usual Pessimistic","Business As Usual","Historic")

plotLabs<-c("Catch share",expression('F'[MSY]),"Business as usual
              (pessimistic)","Business as usual") 
  
  #==reformat
  InPlotTrend<-PlotTrend[!is.na(match(PlotTrend$Policy,chosen)),]
  newDF<-InPlotTrend[InPlotTrend$variable=="TotalCatch",]
  newDF$TotProfit<-InPlotTrend$value[InPlotTrend$variable=="TotalProfit"]
  newDF$PerHealth<-InPlotTrend$value[InPlotTrend$variable=="PercentHealthy"]
  newDF<-rbind(newDF,newDF[newDF$Year==2050&newDF$Policy=="Catch Share Three",],
               newDF[newDF$Year==2050&newDF$Policy=="Business As Usual",])
  
  newDF$PerHealth[nrow(newDF)]<-17
  newDF$PerHealth[nrow(newDF)-1]<-10
  newDF$Year[nrow(newDF)]<-1980
  newDF$Year[nrow(newDF)-1]<-1980
  
  #==make colors for catch
  NoPastProfits<-newDF$TotProfit[newDF$Policy!="Historic" &
                                   newDF$Policy!="CloseDown"&newDF$Policy!="Food"&newDF$Policy!="StatusQuoBForever" &
                                   newDF$Policy!="StatusQuoFForever" & newDF$Policy!="StatusQuoOpenAccess" ]/10000000000
  
  
  bound<-max(ceiling(abs(NoPastProfits)),na.rm=T)
  colrange<-seq(0,bound)			# Tyler had these bounded
  col <-colorRampPalette(c("red","blue"))(length(colrange))
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  
  #==make colors for catch
  useCol<-rep(0,length(newDF$TotProfit))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-newDF$TotProfit[i]/10000000000) == min(abs(colrange-newDF$TotProfit[i]/10000000000)))] )
  
  useCol[newDF$Policy=="Historic"]<-"#CCCCCC88"
  useCol[nrow(newDF)]<-'white'
  useCol[nrow(newDF)-1]<-'white'
  
  #===============
  # plot
  #============== 
#   dev.new(width=8,height=4)
  #pdf("C:/Users/Cody/Desktop/Figure3.pdf",width=8,height=4)
  par(mar=c(4,4,4,8))
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
  abline(h=0,lty=2)
  par(new=T)
  symbols(x=newDF$Year,y=as.numeric(newDF$PerHealth),circles=as.numeric(newDF$value),
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  
  plotLabs<-c("Business as usual","Business as usual
              (pessimistic)","Catch share",expression('F'[MSY])) 
  plotNames<-newDF[newDF$Year==2050,]
  plotNames$PerHealth[3]<-88
  plotNames$PerHealth[4]<-80
  par(xpd=NA)
  text(y=plotNames$PerHealth,x=2052,plotLabs,pos=4)
  par(xpd=FALSE)
  
  mtext(side=3,"Profit/year ($10b)",line=.58)
  mtext(side=3,round(min(NoPastProfits),2),adj=.06,line=-.1,)
  mtext(side=3,round(max(NoPastProfits),2),adj=.94,line=-.1)
  color.legend(1987,105,2042,110,rect.col=col,legend="")
  par(xpd=FALSE)
  mtext(side=2,expression('Fraction of stocks 
                          above 0.8 *'),line=2.2)
  mtext(side=2,expression('B'[MSY]),line=2.1,adj=.75)
  mtext(side=1,"Year",line=2)
  par(new=T)
  text(x=1987,y=3,"Total harvest (million MT)",cex=.7)
  text(x=1983,y=10,76,cex=.8)
  text(x=1983,y=17,53,cex=.8)
  dev.off()
  
####### Figure 4 ##########

AllStocks<-read.csv(paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep=''))
OverfishStocks<-read.csv(paste(ResultFolder,'UnLumped Projection DataOverfish Only Country Upsides.csv',sep=''))
PlotTrend<-  read.csv(paste(ResultFolder,'PlotTrend.csv',sep='')) 
TodayNum<-PlotTrend[PlotTrend$Year==2012 & PlotTrend$Policy=="Historic",]


library(mapplots)

# subset to countries that have overfished stocks
tmp  			<-unique(OverfishStocks$Country)
tmp2				<-unique(AllStocks$Country)
useCountry			<-intersect(tmp,tmp2)
AllStocks			<-AllStocks[!is.na(match(AllStocks$Country,useCountry)),]
OverfishStocks		<-OverfishStocks[!is.na(match(OverfishStocks$Country,useCountry)),]

unqPols		<-unique(AllStocks$Policy)
ChangeBioCur	<-rep(0,length(unqPols))
ChangeCatCur	<-rep(0,length(unqPols))
ChangeBioSQ		<-rep(0,length(unqPols))
ChangeCatSQ		<-rep(0,length(unqPols))
ChangeNPV		<-rep(0,length(unqPols))

BioCur	<-rep(0,length(unqPols))
CatCur	<-rep(0,length(unqPols))
BioSQ		<-rep(0,length(unqPols))
CatSQ		<-rep(0,length(unqPols))
NPV		<-rep(0,length(unqPols))
NPVSQ		<-rep(0,length(unqPols))
pdf(paste(FigureFolder,'Figure 4.pdf',sep=''))
labName<-c("BAU","BAU
           (Pessimistic)","Catch share",expression('F'[MSY]))
indPol<-c(1,2,3,5)
#==select a policy, sum the benefits, record
for(x in 1:length(unqPols))
{
  # Subset to desired policy
  AllSub			<-AllStocks[AllStocks$Policy==unqPols[x],]
  OverfishSub		<-OverfishStocks[OverfishStocks$Policy==unqPols[x],]
  
  #==calculate quantities to graph
  #colnames(AllStocks)
  #==find values
  ChangeBioCur[x]	<-sum(AllSub$AbsChangeTotalBiomass/1000000)
  ChangeCatCur[x]	<-sum(AllSub$AbsChangeTotalCatch/1000000)
  ChangeBioSQ[x]	<-sum(AllSub$AbsChangeFromSQTotalBiomass/1000000)
  ChangeCatSQ[x]	<-sum(AllSub$AbsChangeFromSQTotalCatch/1000000)
  ChangeNPV[x]	<-sum(AllSub$AbsChangeFromSQNPV)/10000000000
  
  BioCur[x]	<-sum(AllSub$TotalBiomass/1000000)
  CatCur[x]	<-sum(AllSub$TotalCatch/1000000)
  BioSQ[x]	<-sum(AllSub$TotalBiomassSQ/1000000)
  CatSQ[x]	<-sum(AllSub$TotalCatchSQ/1000000)
  NPV[x]	<-sum(AllSub$TotalNPV)/10000000000
  NPVSQ[x]	<-sum(AllSub$TotalNPVSQ)/10000000000
}
# BioCur<-c(BioCur,TodayNum,
discRt			<-0.05
TimeHor			<-15
NPV				<-NPV*discRt/(1-(1+discRt)^-TimeHor)
NPVSQ			<-NPVSQ*discRt/(1-(1+discRt)^-TimeHor)

xlimIn		<-c(0,1.2*max(c(BioCur)))
ylimIn		<-c(0,1.2*max(c(NPV)))
GlobalMSY 		<-sum(AllSub$TotalMSY)
#=make colors==
#==make colors for NPV

colrange<-seq(-bound,bound,bound*2/1000)			# Tyler had these bounded
colrange<-seq(min(CatCur[indPol]),max(CatCur[indPol]))
col <-colorRampPalette(c("red","blue"))(length(colrange))
for(i in 1:length(col ))
{
  if(col [i]!=0)
    col [i]<-paste(col [i],88,sep="")
}
useCol<-rep(0,length(ChangeCatSQ))
for(i in 1:length(useCol))
  try(useCol[i] <- col[which(abs(colrange-CatCur[i]) == min(abs(colrange-CatCur[i])))] )
#dev.new(width=4,height=4)
#pdf("C:/Users/Cody/Desktop/Figure4.pdf",width=4,height=4)
plot(-10000000,ylim=ylimIn,xlim=xlimIn,las=1,ylab='',xlab='')
par(new=T)
symbols(x=BioCur[indPol],y=NPV[indPol],CatCur[indPol],
        inches=.2,ylim=ylimIn,xlim=xlimIn,bg=useCol,xaxt='n', yaxt='n',ylab='',xlab='',fg=1)
#posiSpot<-c(2,1,4,4)
text(y=NPV[indPol],x=BioCur[indPol],labName,cex=.65)
mtext(side=1,"Biomass (million mt)",line=2)
mtext(side=2,"Annual profit ($10 billion)",line=2)
dev.off()
}