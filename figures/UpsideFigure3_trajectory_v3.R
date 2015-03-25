PlotTrend<-  read.csv("C:/Users/Cody/Desktop/UpsideData/PT figures/PlotTrend.csv") 
library(plotrix)
years<-unique(PlotTrend$Year)
xlimIn<-c(min(years),max(years))
ylimIn<-c(-2,100)
sizeCirc<-.1

#==plot
#==Select a policy
#==only show catch share--no optimal
chosen<-c("CatchShare","Fmsy","Business As Usual Pessimistic","Business As Usual","Historic")
plotLabs<-c("Catch share",expression('F'[MSY]),"Business as usual
(pessimistic)","Business as usual") 

#==reformat
InPlotTrend<-PlotTrend[!is.na(match(PlotTrend$Policy,chosen)),]
newDF<-InPlotTrend[InPlotTrend$variable=="TotalCatch",]
newDF$TotProfit<-InPlotTrend$value[InPlotTrend$variable=="TotalProfit"]
newDF$PerHealth<-InPlotTrend$value[InPlotTrend$variable=="PercentHealthy"]
newDF<-rbind(newDF,newDF[newDF$Year==2050&newDF$Policy=="CatchShare",],
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
 dev.new(width=8,height=4)
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
#dev.off()