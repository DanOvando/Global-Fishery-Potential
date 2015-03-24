#================================
# compare: upside (optimal) to current for overfished stocks (trevor denominator)
# compare: upside (optimal) to status quo for overfished stocks (trevor denominator)
# both are in totals
#================================
library(plotrix)
source("C:/Users/Cody/Desktop/UpsideData/colorlegend2.R")
data<-read.csv("C:/Users/Cody/Desktop/UpsideData/PT figures/UnLumped Projection DataAll Stocks Country Upsides.csv")
data2<-read.csv("C:/Users/Cody/Desktop/UpsideData/PT figures/UnLumped Projection DataOverfish Only Country Upsides.csv")

Policy<-'CatchShare'
AllStocks<-data
OverfishStocks<-data2

# Subset to desired policy
 AllStocks			<-AllStocks[AllStocks$Policy==Policy,]
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
 ycut<-50
 xcut<-100
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

pdf(file=paste(FigureFolder,'Figure 2.pdf',sep=''))

#==Plot the plot
 dev.new(width=6,height=6)
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