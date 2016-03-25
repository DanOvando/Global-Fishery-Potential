CodyPlotsProfit2050<- function(FigureFolder,ResultFolder,Policy)
{
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
  library(ggmap)
  library(maptools)
  library(maps)
  library(rworldmap)
  library(SDMTools)
  library(mapplots)
  
  ############# Figure 1 #############
  workLocal<-0
  if(workLocal==1)
  {
    ResultFolder<-"C:/Users/Cody/Desktop/UpsideData/Pt figures/"
    FigureFolder<-ResultFolder
    Policy<-"Catch Share Three"
    source("C:/Users/Cody/Desktop/UpsideData/colorlegend2.R")
  }
  
  data<-read.csv(paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep=''))
  data2<-read.csv(paste(ResultFolder,'UnLumped Projection DataOverfish Only Country Upsides.csv',sep=''))
  
  
  data<- subset(data,Country!='High Seas Tuna and Billfish')
  
  data2<- subset(data2,Country!='High Seas Tuna and Billfish')
  
  
  PlotTrend<-read.csv(paste(ResultFolder,'PlotTrend.csv',sep=''))
  pdf(file=paste(FigureFolder,'Figure 1 Profit 2050.pdf',sep=''),height=6,width=6)
  
  #==hardcoded numbers
  discRt    	<- 0.00	# discount rate in annuity calculation
  TimeHor			<- max(PlotTrend$Year)-2012		# time horizon in annuity calculation
  cutin				<- -75	# minimum value for figures
  cutoff			<- 200	# maximum value in figures
  sizeCirc			<-.4		# size of circles in figures
  NPVcut			<-1000 	# cuttoff for NPV
  colCut			<- 400		# cutoff for the largest % change in profit for coloring
  xlimIn<-c(cutin,cutoff)
  ylimIn<-c(cutin,cutoff)
  
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
  #==trevor denominator
  trevPercChangeBioCur	  <-100*OverfishStocks$AbsChangeTotalBiomass/AllStocks$TotalBaselineBiomass
  trevPercChangeCatchCur	<-100*OverfishStocks$AbsChangeTotalCatch/AllStocks$TotalBaselineCatch
  trevPercChangeBioSQ	    <-100*OverfishStocks$AbsChangeFromSQTotalBiomass/AllStocks$TotalBiomassSQ
  trevPercChangeCatchSQ	  <-100*OverfishStocks$AbsChangeFromSQTotalCatch/AllStocks$TotalCatchSQ
  trevPercChangeProfitCur		    <-(100*(OverfishStocks$AbsChangeTotalProfits)/AllStocks$TotalBaselineProfits)*sign(AllStocks$TotalBaselineProfits)
  trevPercChangeProfitSQ  	    <-(100*(OverfishStocks$AbsChangeFromSQProfits)/AllStocks$TotalProfitsSQ)*sign(AllStocks$TotalProfitsSQ)
  
  trevPercChangeProfitCur[AllStocks$TotalBaselineProfits<=0 & (OverfishStocks$AbsChangeTotalProfits)>0]<- 999
  trevPercChangeProfitCur[AllStocks$TotalBaselineProfits<=0 & (OverfishStocks$AbsChangeTotalProfits)<=0]<- -999
  
  trevPercChangeProfitSQ[AllStocks$TotalProfitsSQ<=0 & (OverfishStocks$AbsChangeFromSQProfits)<=0]<- -999
  trevPercChangeProfitSQ[AllStocks$TotalProfitsSQ<=0 & (OverfishStocks$AbsChangeFromSQProfits)>0]<- 999
  
  #==bound x and y
  
  trevPercChangeProfitCur[trevPercChangeProfitCur>NPVcut]<-NPVcut
  trevPercChangeProfitSQ[trevPercChangeProfitSQ>NPVcut]<-NPVcut
  trevPercChangeCatchCur[trevPercChangeCatchCur>cutoff]	<-cutoff
  trevPercChangeCatchSQ[trevPercChangeCatchSQ>cutoff]	<-cutoff
  trevPercChangeBioCur[trevPercChangeBioCur>cutoff]	<-cutoff
  trevPercChangeBioSQ[trevPercChangeBioSQ>cutoff]		<-cutoff
  
  OverfishStocks$PercChangeTotalBiomass[OverfishStocks$PercChangeTotalBiomass>cutoff]			<-cutoff
  OverfishStocks$PercChangeTotalCatch[OverfishStocks$PercChangeTotalCatch>cutoff]			<-cutoff
  OverfishStocks$PercChangeFromSQTotalBiomass[OverfishStocks$PercChangeFromSQTotalBiomass>cutoff]	<-cutoff
  OverfishStocks$PercChangeFromSQTotalCatch[OverfishStocks$PercChangeFromSQTotalCatch>cutoff]	<-cutoff
  
  #==find annuity
#   overfishAnn		<-OverfishStocks$TotalNPV*discRt/(1-(1+discRt)^-TimeHor)
#   
#   #==quantities that determine the color
#   colQuant<-100*(overfishAnn-OverfishStocks$TotalBaselineProfits)/OverfishStocks$TotalBaselineProfits*sign(OverfishStocks$TotalBaselineProfits)
#   
#   colQuant[OverfishStocks$TotalBaselineProfits<=0 & (overfishAnn-OverfishStocks$TotalBaselineProfits)>0]<- 999
#   
#   colQuant[OverfishStocks$TotalBaselineProfits<=0 & (overfishAnn-OverfishStocks$TotalBaselineProfits)<=0]<- -999
#   
#   colQuant3<-100*(overfishAnn-OverfishStocks$TotalBaselineProfits)/AllStocks$TotalBaselineProfits*sign(AllStocks$TotalBaselineProfits)
#   
#   colQuant3[AllStocks$TotalBaselineProfits<=0 & (overfishAnn-OverfishStocks$TotalBaselineProfits)>0]<- 999
#   
#   colQuant3[AllStocks$TotalBaselineProfits<=0 & (overfishAnn-OverfishStocks$TotalBaselineProfits)<=0]<- -999
  
  colQuant      <-OverfishStocks$PercChangeTotalProfits
  colQuant3     <-trevPercChangeProfitCur
  colQuant4			<-trevPercChangeProfitSQ
  colQuant2			<-OverfishStocks$PercChangeFromSQProfits
  
  colQuant[colQuant>colCut]	<-colCut
  colQuant2[colQuant2>colCut]	<-colCut
  colQuant3[colQuant3>colCut]	<-colCut
  colQuant4[colQuant4>colCut]	<-colCut
  
  colQuant[colQuant< -colCut]		<- -colCut
  colQuant2[colQuant2< -colCut]	<- -colCut
  colQuant3[colQuant3< -colCut]	<- -colCut
  colQuant4[colQuant4< -colCut]	<- -colCut
  
  #==quantities that determine the size of circles
  radius 			<- sqrt( AllStocks$TotalMSY/ pi )
  radius2 			<- sqrt( OverfishStocks$TotalMSY/ pi )
  
  #==select large stocks to plot the name of
  plotCountries<-as.character(OverfishStocks$Country)
  for(x in 1:length(radius2))
    if(radius2[x]<550) plotCountries[x]<-""
  
  #==select large stocks to plot the name of for all stocks
  plotCountriesAll<-as.character(OverfishStocks$Country)
  for(x in 1:length(radius))
    if(radius[x]<700) plotCountriesAll[x]<-""
  
  #==scales circle size between two graphs
  sizeCirc2			<-sizeCirc*max(radius2, na.rm=T)/max(radius,na.rm=T)
  
  #==make colors for catch
  bound	  <-max(c((colQuant),(colQuant2),(colQuant3),(colQuant4)),na.rm=T)
  bound2	<-min(c((colQuant),(colQuant2),(colQuant3),(colQuant4)),na.rm=T)
  bound2<- 0
  bigBnd	<-max(bound,abs(bound2))
  colrange	<-seq(-bigBnd,bigBnd,(bound-bound2)/150)			
  col 		<-colorRampPalette(c("green","white","blue"))(length(colrange))
  
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
  
  legendCol<-col[(which(abs(colrange-bound2) == min(abs(colrange-bound2)))):(which(abs(colrange-bound) == min(abs(colrange-bound))))]
  
  #=====================================
  # output figure data for comparisons
  #=====================================
  #=figure data
  outs	<-cbind(OverfishStocks$PercChangeTotalBiomass,OverfishStocks$PercChangeTotalCatch,OverfishStocks$TotalMSY,colQuant,as.character(OverfishStocks$Country ))
  outs2	<-cbind(OverfishStocks$PercChangeFromSQTotalBiomass,OverfishStocks$PercChangeFromSQTotalCatch,OverfishStocks$TotalMSY,colQuant2,as.character(OverfishStocks$Country ))
  outs3	<-cbind(trevPercChangeBioCur,trevPercChangeCatchCur,AllStocks$TotalMSY,colQuant3,as.character(OverfishStocks$Country ))
  outs4	<-cbind(trevPercChangeBioSQ,trevPercChangeCatchSQ,AllStocks$TotalMSY,colQuant4,as.character(OverfishStocks$Country ))
  
  colnames(outs)<-c("Bio","Catch","MSY","Profit","Country")
  colnames(outs2)<-c("Bio","Catch","MSY","Profit","Country")
  colnames(outs3)<-c("Bio","Catch","MSY","Profit","Country")
  colnames(outs4)<-c("Bio","Catch","MSY","Profit","Country")
  
  #==CHANGE THIS TO OUTPUT WHEREEVER YOU WANAT
  write.csv(outs,paste(ResultFolder,'Fig1Adata_Profits2050.csv',sep=''))
  write.csv(outs2,paste(ResultFolder,'Fig1Bdata_Profits2050.csv',sep=''))
  write.csv(outs3,paste(ResultFolder,'Fig1Cdata_Profits2050.csv',sep=''))
  write.csv(outs4,paste(ResultFolder,'Fig1Ddata_Profits2050.csv',sep=''))
  
  #=========================================
  # plot first panel
  #=========================================
  #dev.new(width=6,height=6)
  #pdf("C:/Users/Cody/Desktop/Figure1.pdf",height=6,width=6)
  #windows()
  par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,2,1)) 
  
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  
  par(new=T)
  symbols(x=OverfishStocks$PercChangeTotalBiomass,y=OverfishStocks$PercChangeTotalCatch,circles=radius2,
          bg=useCol,fg='black',inches=sizeCirc2,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  #   text(OverfishStocks$PercChangeTotalBiomass,OverfishStocks$PercChangeTotalCatch,plotCountries,cex=.5)
  
  legend(x=1.25*cutin,y=cutoff, "(A)",bty='n',cex=.8)
  
  par(xpd=NA)
  text(x=.5*cutoff,y=1.125*cutoff,"% change in profit",cex=.7)
  color.legend2(0,cutoff*1.07,cutoff*.94,1.10*cutoff,rect.col=legendCol,legend="")
  par(xpd=NA)
  text(x=-20,y=1.09*cutoff,paste("<=",round(bound2)),cex=.65)
  text(x=cutoff*1.025,y=1.09*cutoff,paste(">",bound),cex=.65)
  par(xpd=FALSE)
  
  #=========================================
  # plot second panel
  #=========================================
  #==make a legend
  plotx<-c(OverfishStocks$PercChangeFromSQTotalBiomass,.9*cutoff,.9*cutoff)
  ploty<-c(OverfishStocks$PercChangeFromSQTotalCatch,.9*cutoff,.94*cutoff)
  plotz<-c(radius2,max(radius2)*.9,max(radius2)*.5)
  
  #==make colors for catch
  useCol<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant2[i]) == min(abs(colrange-colQuant2[i])))] )
  useCol<-c(useCol,"white","white")
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  par(new=T)
  symbols(x=plotx,y=ploty,circles=plotz,
          bg=useCol,fg='black',inches=sizeCirc2,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  legend(x=1.4*cutin,y=cutoff, "(B)",bty='n',cex=.8)
  text(x=.9*cutoff,y=.94*cutoff,round((pi*(max(radius2)*.5)^2)/1000000,1),cex=.65)
  text(x=.9*cutoff,y=.82*cutoff,round((pi*(max(radius2)*.9)^2)/1000000,1),cex=.65)
  text(x=.89*cutoff,y=.7*cutoff,"MSY (MMT)",cex=.65)
  #   text(OverfishStocks$PercChangeFromSQTotalBiomass,OverfishStocks$PercChangeFromSQTotalCatch,plotCountries,cex=.5)
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
  #   text(trevPercChangeBioCur,trevPercChangeCatchCur,plotCountriesAll,cex=.5) 
  legend(x=230,y=700,bty='n',legend="Total MSY (mt)",cex=.8)
  legend(x=1.25*cutin,y=cutoff, "(C)",bty='n',cex=.8)
  
  
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
  
  mtext(side=1,outer=T,"% Change in Biomass",line=2)
  mtext(side=2,outer=T,"% Change in Catch",line=2.3)
  legend(x=1.4*cutin,y=cutoff, '(D)',bty='n',cex=.8)
  #   text(trevPercChangeBioSQ,trevPercChangeCatchSQ,plotCountriesAll,cex=.5) 
  
  dev.off()
  #===============================================
  # break out panels by themselves
  #===============================================
  
  #pdf("C:/Users/Cody/Desktop/Figure1.pdf",height=6,width=6)
  #windows()
  Fig1Panel<-function(xin,yin,zin,colQuantIn,title,cutin,cutoff)
  {
    pdf(paste(FigureFolder,title,".pdf",sep=""),height=5,width=5)
    xlimIn<-c(cutin,cutoff)
    ylimIn<-c(cutin,cutoff)
    #==make a legend
    plotx<-c(xin,.7*cutin,.7*cutin)
    ploty<-c(yin,.86*cutoff,.9*cutoff)
    plotz<-c(zin,max(zin)*.7,max(zin)*.3)
    
    #==make colors for catch
    useCol<-rep(0,length(colQuantIn))
    for(i in 1:length(useCol))
      try(useCol[i] <- col[which(abs(colrange-colQuantIn[i]) == min(abs(colrange-colQuantIn[i])))] )
    useCol<-c(useCol,"white","white")
    
    par(mar=c(.1,.1,.1,.1),oma=c(4,4,2,1)) 
    plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
    abline(h=0,lty=2)
    abline(v=0,lty=2)
    
    par(new=T)
    symbols(x=plotx,y=ploty,circles=plotz,
            bg=useCol,fg='black',inches=sizeCirc2,las=1,
            ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n',yaxt='n')
    
    par(xpd=NA)
    text(x=.5*cutoff,y=1.125*cutoff,"% change in profit",cex=.7)
    color.legend2(0,cutoff*1.07,cutoff*.94,1.10*cutoff,rect.col=legendCol,legend="")
    par(xpd=NA)
    text(x=-20,y=1.09*cutoff,paste("<=",round(bound2)),cex=.65)
    text(x=cutoff*1.025,y=1.09*cutoff,paste(">",bound),cex=.65)
    par(xpd=FALSE)
    mtext(side=1,outer=T,"% Change in Biomass",line=2)
    mtext(side=2,outer=T,"% Change in Catch",line=2.3)
    text(x=.7*cutin,y=.9*cutoff,round((pi*(max(zin)*.5)^2)/1000000,1),cex=.65)
    text(x=.7*cutin,y=.75*cutoff,round((pi*(max(zin)*.9)^2)/1000000,1),cex=.65)
    text(x=.7*cutin,y=.63*cutoff,"MSY (MMT)",cex=.65)
    dev.off()
  }
  
  
  sizeCirc2<-.6
  Fig1Panel(OverfishStocks$PercChangeTotalBiomass,OverfishStocks$PercChangeTotalCatch,radius2,colQuant,"Figure1a",-75,200)
  Fig1Panel(OverfishStocks$PercChangeFromSQTotalBiomass,OverfishStocks$PercChangeFromSQTotalCatch,radius2,colQuant2,"Figure1b",-75,200)
  Fig1Panel(trevPercChangeBioCur,trevPercChangeCatchCur,radius,colQuant3,"Figure1c",-75,200)
  Fig1Panel(trevPercChangeBioSQ,trevPercChangeCatchSQ,radius,colQuant4,"Figure1d",-75,200)
  
  
  ######## Figure 2 ############
  
  pdf(paste(FigureFolder,'Figure 2 Profit 2050.pdf',sep=''),height=5,width=8)
  
  data<-read.csv(paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep=''))
  data2<-read.csv(paste(ResultFolder,'UnLumped Projection DataOverfish Only Country Upsides.csv',sep=''))
  
  
  data<- subset(data,Country!='High Seas Tuna and Billfish')
  
  data2<- subset(data2,Country!='High Seas Tuna and Billfish')
  
  
  AllStocks<-data
  OverfishStocks<-data2
  
  AllStocks<- subset(AllStocks,Country!='High Seas Tuna and Billfish')
  
  OverfishStocks<- subset(OverfishStocks,Country!='High Seas Tuna and Billfish')
  
  
  par(mfrow=c(1,2),mar=c(3,.1,4,.1),oma=c(1.5,3,0,4))
  #==fixed quantities
  discRt  	<-0.00
  TimeHor		<-max(PlotTrend$Year)-2012
  topCut		<-11		# number of countries at the top to take
  sizeCirc	<-.65		# size of circle
  ycut			<-14
  xcut			<-120
  legendX		<-20
  legendY		<-8.5
  ylimIn		<-c(-.25,ycut)
  xlimIn		<-c(-3,xcut)
  
  # Subset to desired policy
  AllStocks			    <-AllStocks[AllStocks$Policy==Policy,]
  OverfishStocks		<-OverfishStocks[OverfishStocks$Policy==Policy,]
  # subset to countries that have overfished stocks
  tmp				  <-unique(OverfishStocks$Country)
  tmp2				<-unique(AllStocks$Country)
  useInd			<-match(tmp,tmp2)
  useInd			<-useInd[!is.na(useInd)]
  AllStocks		<-AllStocks[useInd,]
  
  #==find which countries have the maximum of a quantity (NPV here)
  OverfishStocks  <-OverfishStocks[order(AllStocks$AbsChangeFromSQProfits,decreasing=T),]
  AllStocks       <-AllStocks[order(AllStocks$AbsChangeFromSQProfits,decreasing=T),]
  OverfishStocks  <-OverfishStocks[1:topCut,]
  AllStocks       <-AllStocks[1:topCut,]
  OverfishStocks  <-OverfishStocks[OverfishStocks$Country!="Multinational",]
  
  #==calculate quantities to graph
  xQuant			<-OverfishStocks$AbsChangeFromSQTotalBiomass/1000000
  yQuant			<-OverfishStocks$AbsChangeFromSQProfits/1000000000
  zQuant			<-OverfishStocks$TotalMSY
  colQuant		<-OverfishStocks$AbsChangeFromSQTotalCatch/1000000
#   yQuant			<-yQuant*discRt/(1-(1+discRt)^-TimeHor)
  
  labs<-c(as.character(OverfishStocks$Country),round(max(zQuant)/1000000,1)*.8,round(min(zQuant)/1000000,1))
  labs[2:(topCut-1)]<-""
  colLabs<-rep(1,length(labs))
  
  if(IncludeNEIs==TRUE)
  {
    colLabs[which(OverfishStocks$NeiProfitsOver50)]<-2
  }
  
  #==add circles for legend
  xQuant<-c(xQuant,legendX,legendX)
  yQuant<-c(yQuant,10.2,11.3)
  zQuant<-c(zQuant,max(zQuant)*.8,min(zQuant))
  colQuant<-c(colQuant,max(colQuant),min(colQuant))
  
  #==represent MSY by area rather than radius
  radius 	<- sqrt( zQuant/ pi )
  
  #==make colors for profit
  bound    <-max(colQuant,na.rm=T)
  bound2	<-min(colQuant,na.rm=T)
  bigBnd	<-max(bound,abs(bound2))
  colrange	<-seq(-bigBnd,bigBnd,(bound-bound2)/150)			
  col 		<-colorRampPalette(c("green","white","blue"))(length(colrange))
  
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol	<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
  
  #==for legend
  useCol[length(useCol)]<-"white"
  useCol[length(useCol)-1]<-"white"
  
  legendCol<-col[(which(abs(colrange-bound2) == min(abs(colrange-bound2)))):(which(abs(colrange-bound) == min(abs(colrange-bound))))]
  
  #=====================================
  # output figure data for comparisons
  #=====================================
  #=figure data
  outs	<-cbind(xQuant[1:topCut],yQuant[1:topCut],zQuant[1:topCut],colQuant[1:topCut],labs[1:topCut])
  colnames(outs)<-c("Bio","Profit","MSY","Catch","Country")
  
  #==CHANGE THIS TO OUTPUT WHEREEVER YOU WANAT
  write.csv(outs,paste(ResultFolder,'Fig2data_Profits2050.csv',sep=''))
  
  #==Plot the plot
  #dev.new(width=6,height=6)
  #pdf("C:/Users/Cody/Desktop/Figure2a.pdf",height=6,width=6)
  #   windows()
  #par(mar=c(.1,.1,.1,.1),oma=c(4,4,4,4)) 
  
  plot(-exp(50),las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
  #abline(h=0,lty=2)
  #abline(v=0,lty=2)
  par(new=T)
  symbols(x=xQuant,y=yQuant,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n')
  text(xQuant,jitter(yQuant,factor=0),labs,cex=.75,col=colLabs)
  mtext(side=2,"Change in Annual Profit ($ Billion)",line=2.25)
  mtext(side=1,"Change in Biomass (MMT)",outer=T)
  text(y=legendY*.84,x=legendX,"MSY ",cex=.7)
  # text(y=legendY*.75,x=legendX+5,"(MMT, of conservation concern)",cex=.7)
  par(xpd=NA)
  #text(x=.5*cutoff,y=1.11*cutoff,"% change in catch",cex=.8)
  mtext(side=3,"Change in Catch (MMT)",line=.8,cex=.8)
  color.legend2(0,ycut*1.06,xcut*.94,1.08*ycut,rect.col=legendCol,legend="")
  par(xpd=NA)
  text(x=-3,y=1.075*ycut,round(min(colQuant),0),cex=.85)
  text(x=xcut,y=1.075*ycut,round(max(colQuant),0),cex=.85)
  par(xpd=FALSE)
  text(x=110,y=0,"A")
  #====put in a box around the un-marked circles
  rect(-5,-.5,45,5.8,lty=2,border=2)
  arrows(46,2.9,110,2.9,lty=2,col=2)
  
  
  #============================================
  # plot the second panel
  #===========================================
  
  discRt    <-0.00
  TimeHor		<-max(PlotTrend$Year)-2012
  topCut		<-11		# number of countries at the top to take
  sizeCirc	<-.65		# size of circle
  ycut			<-5
  xcut			<-35
  legendX		<-16
  legendY		<-4.95
  ylimIn		<-c(-.01,ycut)
  xlimIn		<-c(-1.5,xcut)
  
  AllStocks<-data
  OverfishStocks<-data2
  # Subset to desired policy
  AllStocks			    <-AllStocks[AllStocks$Policy==Policy,]
  OverfishStocks		<-OverfishStocks[OverfishStocks$Policy==Policy,]
  
  # subset to countries that have overfished stocks
  tmp				  <-unique(OverfishStocks$Country)
  tmp2				<-unique(AllStocks$Country)
  useInd			<-match(tmp,tmp2)
  useInd			<-useInd[!is.na(useInd)]
  AllStocks		<-AllStocks[useInd,]

  #==find which countries have the maximum of a quantity (NPV here)
  OverfishStocks  <-OverfishStocks[order(AllStocks$AbsChangeFromSQProfits,decreasing=T),]
  AllStocks       <-AllStocks[order(AllStocks$AbsChangeFromSQProfits,decreasing=T),]
  OverfishStocks  <-OverfishStocks[1:topCut,]
  AllStocks       <-AllStocks[1:topCut,]
  OverfishStocks  <-OverfishStocks[OverfishStocks$Country!="Multinational",]
  
  
  #==calculate quantities to graph
  xQuant			<-OverfishStocks$AbsChangeFromSQTotalBiomass/1000000
  yQuant			<-OverfishStocks$AbsChangeFromSQProfits/1000000000
  zQuant			<-OverfishStocks$TotalMSY
  colQuant		<-OverfishStocks$AbsChangeFromSQTotalCatch/1000000
#   yQuant			<-yQuant*discRt/(1-(1+discRt)^-TimeHor)
  
  labs<-c(as.character(OverfishStocks$Country))
  
  colLabs<-rep(1,length(labs))
  
  # Indicate which countries to label red
  if(IncludeNEIs==TRUE)
  {
    colLabs[which(OverfishStocks$NeiProfitsOver50)]<-2
  }
  
  # Shorten long names
  abbrev<-data.frame(c('Republic of Korea','Taiwan Province of China'),c('S. Korea','Taiwan'),stringsAsFactors = F)
  colnames(abbrev)<-c('original','plotname')
  
  if(any(labs %in% abbrev$original))
  {
    swap<-labs[labs %in% abbrev$original]
    
    for(a in 1:length(swap)) 
    { 
      swapnew<-abbrev$plotname[abbrev$original==swap[a]]
      b<-match(swap[a],labs)
      labs[b]<-swapnew
    }
  }
  
  #==represent MSY by area rather than radius
  radius 	<- sqrt( zQuant/ pi )
  
  #==make colors for profit
  bound    <-max(colQuant,na.rm=T)
  bound2	<-min(colQuant,na.rm=T)
  bigBnd	<-max(bound,abs(bound2))
  colrange	<-seq(-bigBnd,bigBnd,(bound-bound2)/150)			
  col 		<-colorRampPalette(c("green","white","blue"))(length(colrange))
  
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol	<-rep(0,length(colQuant))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
  
  legendCol<-col[(which(abs(colrange-bound2) == min(abs(colrange-bound2)))):(which(abs(colrange-bound) == min(abs(colrange-bound))))]
  plot(-exp(50),las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n')
  #abline(h=0,lty=2)
  #abline(v=0,lty=2)
  par(new=T)
  symbols(x=xQuant,y=yQuant,circles=radius,
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,xaxt='n',yaxt='n')
  axis(side=4,las=1)
  text(xQuant,jitter(yQuant,factor=0),labs,cex=.65,col=colLabs)
  #text(xQuant,yQuant,labs,cex=.65)
  text(x=45,y=0,"B")
  mtext(side=4,"Change in Annual Profit ($ Billion)",line=2.25)
  
  
  dev.off()
  
  
  ######## Figure 4 ###########
  pdf(paste(FigureFolder,'Figure 4 Profit 2050.pdf',sep=''),width=8,height=4)
  
  years<-unique(PlotTrend$Year)
  xlimIn<-c(min(years),max(years))
  ylimIn<-c(-2,100)
  sizeCirc<-.1
  
  #==plot
  #==Select a policy
  #==only show catch share--no optimal
  chosen<-c("Catch Share Three",'Fmsy Three',"Business As Usual Pessimistic","Business As Usual","Historic")
  plotLabs<-c("RBFM",expression('F'[MSY]),"Business as usual
              (pessimistic)","Business as usual") 
  
  #==reformat
  InPlotTrend<-PlotTrend[!is.na(match(PlotTrend$Policy,chosen)),]
  newDF<-InPlotTrend[InPlotTrend$variable=="TotalCatch",]
  newDF$TotProfit<-InPlotTrend$value[InPlotTrend$variable=="TotalProfit"]
  newDF$PerHealth<-InPlotTrend$value[InPlotTrend$variable=="PercentHealthy"]
  newDF<-rbind(newDF,newDF[newDF$Year==2050&newDF$Policy=="CatchShare",],
               newDF[newDF$Year==2050&newDF$Policy=="Business As Usual",])
  
  #==this makes the legend
  newDF$PerHealth[nrow(newDF)]<-22
  newDF$PerHealth[nrow(newDF)-1]<-15
  newDF$Year[nrow(newDF)]<-1980
  newDF$Year[nrow(newDF)-1]<-1980
  newDF$value[nrow(newDF)]<-.5*max(newDF$value)
  newDF$value[nrow(newDF)-1]<-.9*max(newDF$value)
  
  # newDF$PerHealth[nrow(newDF)]<-NA
  #  newDF$PerHealth[nrow(newDF)-1]<-NA
  # newDF$Year[nrow(newDF)]<-NA
  # newDF$Year[nrow(newDF)-1]<-NA
  
  #==make colors for catch
  newDF$TotProfit<- newDF$TotProfit*10
  
  NoPastProfits<- newDF$TotProfit[newDF$Policy!="Historic" &
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
  par(mar=c(4,4,4,4))
  plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
  abline(h=0,lty=2)
  par(new=T)
  symbols(x=newDF$Year,y=as.numeric(newDF$PerHealth),circles=as.numeric(newDF$value),
          bg=useCol,fg='black',inches=sizeCirc,las=1,
          ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
  
  plotLabs<-c("BAU (all stocks)","BAU (conservation concern)","RBFM",expression('F'[MSY])) 
  #plotNames<-newDF[newDF$Year==2050,]
  #plotNames$PerHealth[3]<-88
  #plotNames$PerHealth[4]<-80
  #par(xpd=NA)
  #text(y=plotNames$PerHealth,x=2052,plotLabs,pos=4)
  #par(xpd=FALSE)
  text(y=c(18,37,85,55),x=2042,plotLabs,cex=.8)
  
  mtext(side=3,"Profit/year ($ Billion)",line=.58)
  mtext(side=3,round(min(NoPastProfits),-1),adj=.06,line=-.05,)
  mtext(side=3,round(max(NoPastProfits),-1),adj=.94,line=-.05)
  color.legend(1987,105,2042,110,rect.col=col,legend="")
  par(xpd=FALSE)
  mtext(side=2,expression('% Stocks above 0.95 B/B'[MSY]),line=2.2)
  #   mtext(side=2,expression('B'[MSY]),line=2.1,adj=.75)
  mtext(side=1,"Year",line=2)
  par(new=T)
  text(x=1985,y=28,"Total harvest (MMT)",cex=.7)
  text(x=1983,y=15,round(as.numeric(newDF$value)[nrow(newDF)-1]/1000000),cex=.8)
  text(x=1983,y=22,round(as.numeric(newDF$value)[nrow(newDF)]/1000000),cex=.8)
  dev.off()
  
  ####### Figure 3 ##########
  
  data<-read.csv(paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep=''))
  data2<-read.csv(paste(ResultFolder,'UnLumped Projection DataOverfish Only Country Upsides.csv',sep=''))
  
  
  AllStocks<-data
  OverfishStocks<-data2
  
  TodayNum<-PlotTrend[PlotTrend$Year==2012 & PlotTrend$Policy=="Historic",]
  pdf(paste(FigureFolder,'Figure 3 Profit 2050.pdf',sep=''),width=6,height=6)
  
  discRt  		<- 0.00	# discount rate in annuity calculation
  TimeHor			<- max(PlotTrend$Year)-2012		# time horizon in annuity calculation
  sizeCirc			<-.5
  catchAdj			<-2.9
  # subset to countries that have overfished stocks
  tmp				<-unique(OverfishStocks$Country)
  tmp2				<-unique(AllStocks$Country)
  useCountry			<-intersect(tmp,tmp2)
#   AllStocks			<-AllStocks[!is.na(match(AllStocks$Country,useCountry)),]
#   OverfishStocks		<-OverfishStocks[!is.na(match(OverfishStocks$Country,useCountry)),]
  
  unqPols		<-unique(AllStocks$Policy)
  ChangeBioCur	<-rep(0,length(unqPols))
  ChangeCatCur	<-rep(0,length(unqPols))
  ChangeBioSQ		<-rep(0,length(unqPols))
  ChangeCatSQ		<-rep(0,length(unqPols))
  ChangeProfit		<-rep(0,length(unqPols))
  
  BioCur	<-rep(0,length(unqPols))
  CatCur	<-rep(0,length(unqPols))
  BioSQ		<-rep(0,length(unqPols))
  CatSQ		<-rep(0,length(unqPols))
  Profit		<-rep(0,length(unqPols))
  ProfitSQ		<-rep(0,length(unqPols))
  PolName<- NULL  
  labName 	<-c("BAU","BAU","RBFM","RBFM",expression('F'[MSY]),
               expression('F'[MSY]),"Today")
  labNameSimp	<-c("BAU","BAU (Pessimistic)","Catch share","Fmsy","Today")
  WantPols<- c('Catch Share Three','Fmsy Three','Business As Usual','Business As Usual Pessimistic',
               'CatchShare','Fmsy')
  indPol<- which(unqPols %in% WantPols)
  col2<-"#4d4d9466"
  col3<-"#66c26666"
  col4<-'#e0e0d166'
  inCols<-c(col2,col3,col2,col3,col3, col2,col4)
  #==select a policy, sum the benefits, record
  for(x in 1:length(unqPols))
  {
    # Subset to desired policy
    AllSub			<-AllStocks[AllStocks$Policy==unqPols[x],]
    OverfishSub		<-OverfishStocks[OverfishStocks$Policy==unqPols[x],]
    
    #==calculate quantities to graph
    BioCur[x]	<-sum(AllSub$TotalBiomass/1000000)
    CatCur[x]	<-sum(AllSub$TotalCatch/1000000)
    BioSQ[x]	<-sum(AllSub$TotalBiomassSQ/1000000)
    CatSQ[x]	<-sum(AllSub$TotalCatchSQ/1000000)
    Profit[x]	<-sum(AllSub$TotalProfits)/1000000000
    ProfitSQ[x]	<-sum(AllSub$TotalProfitsSQ)/1000000000
    PolName[x]<- as.character(unqPols[x])
  }
  
  #==point for 'today'
  baseBio<-sum(AllSub$TotalBaselineBiomass[AllSub$Policy=="StatusQuoOpenAccess"])/1000000
  baseCat<-sum(AllSub$TotalBaselineCatch[AllSub$Policy=="StatusQuoOpenAccess"])/1000000
  basePft<-sum(AllSub$TotalBaselineProfits[AllSub$Policy=="StatusQuoOpenAccess"])/1000000000
  
  #==annuity NPV
#   NPV				<-NPV*discRt/(1-(1+discRt)^-TimeHor)
  xlimIn		<-c(350,1.04*max(c(BioCur)))
  ylimIn		<-c(0,1.11*max(c(Profit)))
  GlobalMSY 		<-sum(AllSub$TotalMSY)
  
  #=make colors==
  catIn<-c(CatCur[indPol],baseCat)
  colrange<-seq(min(CatCur[indPol]),max(CatCur[indPol]))
  col <-colorRampPalette(c("red","blue"))(length(colrange))
  for(i in 1:length(col ))
  {
    if(col [i]!=0)
      col [i]<-paste(col [i],88,sep="")
  }
  useCol<-rep(0,length(catIn))
  for(i in 1:length(useCol))
    try(useCol[i] <- col[which(abs(colrange-catIn[i]) == min(abs(colrange-catIn[i])))] )
  
  #=====================================
  # output figure data for comparisons
  #=====================================
  #=figure data
  outs	<-cbind(c(BioCur[indPol],baseBio),c(Profit[indPol],basePft),c(CatCur[indPol],baseCat),c(PolName[indPol],'Today'))
  colnames(outs)<-c("Bio","Profit","Catch","Policy")
  #==CHANGE THIS TO OUTPUT WHEREEVER YOU WANAT
  write.csv(outs,paste(ResultFolder,'Fig3data_Profit2050.csv',sep=''))
  
  plotx<-c(BioCur[indPol],baseBio)
  ploty<-c(Profit[indPol],basePft)
  plotz<-c(CatCur[indPol],baseCat)
  
  plot(-10000000,ylim=ylimIn,xlim=xlimIn,las=1,ylab='',xlab='')
  par(new=T)
  symbols(x=plotx,y=ploty,plotz,
          inches=sizeCirc,ylim=ylimIn,xlim=xlimIn,bg=inCols,xaxt='n', yaxt='n',ylab='',xlab='',fg=1)
  
  
  Yind<-c(Profit[indPol],basePft)
  Yind2<-c(Profit[indPol]-catchAdj,basePft-catchAdj)
  Xind<-c(BioCur[indPol],baseBio)
  Xind2<-c(BioCur[indPol],baseBio)
  #,round(c(CatCur[indPol],baseCat),1)
  
  # Adjust the position of the bubble labels. order is (BAU, BAUP, RBFM CC, RBFM All, Fmsy All, Fmsy, Today)
  Xind<-  Xind+c(-120,0,0,0,0,0,120) 
  Xind2<-Xind2+c(-120,0,0,0,0,0,120)
  Yind<-  Yind+c(0,0,0,0,9,0,0)
  Yind2<-Yind2+c(0,0,0,0,9,0,0)
  
  text(y=Yind,x=Xind,labName,cex=.85)
  text(y=Yind2,x=Xind2,round(plotz,digits=1),cex=.65)
  
  
  mtext(side=1,"Biomass (MMT)",line=2)
  mtext(side=2,"Annual Profit ($ Billions)",line=2)
  legend("bottomleft",bty='n',col=c(col2,col3),pch=16,c("Policy applied to stocks of conservation concern",
                                                        "Policy applied to all stocks"))
  dev.off()
}
