PlotTrend<-  read.csv("C:/Users/Cody/Desktop/UpsideData/PT figures/PlotTrend.csv") 
library(plotrix)
years<-unique(PlotTrend$Year)

#==plot
#==Select a policy
#==only show catch share--no optimal
chosen<-c("CatchShare","Fmsy","Business As Usual Pessimistic","Business As Usual Optimistic","Historic")
plotLabs<-c("Catch share",expression('F'[MSY]),"Business as usual
(pessimistic)","Business as usual") 
unique(PlotTrend$Policy)
#==make colors for catch
NoPastProfits<-PlotTrend$value[PlotTrend$variable=="TotalProfit"&PlotTrend$Policy!="Historic" &
 PlotTrend$Policy!="CloseDown"&PlotTrend$Policy!="Food"&PlotTrend$Policy!="StatusQuoBForever" &
PlotTrend$Policy!="StatusQuoFForever" & PlotTrend$Policy!="StatusQuoOpenAccess" ]/10000000000

 bound<-max(abs(NoPastProfits),na.rm=T)
 colrange<-seq(0,bound)			# Tyler had these bounded
 col <-colorRampPalette(c("red","blue"))(length(colrange))
 for(i in 1:length(col ))
  {
   if(col [i]!=0)
    col [i]<-paste(col [i],88,sep="")
  }



 dev.new(width=8,height=4)
 par(mar=c(4,4,4,8))
for(x in 1:length(chosen))
{
tmp<-PlotTrend[PlotTrend$Policy==chosen[x],]
tmpYr<-unique(tmp$Year)
TotCat<-tmp$value[tmp$variable=="TotalCatch"]
TotPro<-tmp$value[tmp$variable=="TotalProfit"]
Healthy<-tmp$value[tmp$variable=="PercentHealthy"]

#=========================================
# plot first panel
#=========================================
  zQuant			<-TotCat/1000000
  colQuant			<-TotPro/10000000000
  #radius <- sqrt( zQuant/ pi )
  radius<-zQuant
  sizeCirc<-.1
#==make colors for catch
 useCol<-rep(0,length(colQuant))
 for(i in 1:length(useCol))
  try(useCol[i] <- col[which(abs(colrange-colQuant[i]) == min(abs(colrange-colQuant[i])))] )
if(chosen[x]=="Historic")
 useCol<-"#CCCCCC88"
xlimIn<-c(min(years),max(years))
ylimIn<-c(-2,100)
#==Plot the plot

 if(x==1)
 {
 plot(-100000,las=1,ylab="",xlab="",ylim=ylimIn,xlim=xlimIn)
  abline(h=0,lty=2)
 }

  par(new=T)
 symbols(x=tmpYr,y=Healthy,circles=radius,
            bg=useCol,fg='black',inches=sizeCirc,las=1,
		ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
 par(xpd=TRUE)
 if(chosen[x]!="Historic" & chosen[x]!="Fmsy" & chosen[x]!="Opt")
    text(y=Healthy[length(tmpYr)],x=2050,plotLabs[x],pos=4)
  if(chosen[x]=="Fmsy")
   text(y=Healthy[length(tmpYr)]+5,x=2050,plotLabs[x],pos=4) 
  if(chosen[x]=="Opt")
   text(y=Healthy[length(tmpYr)]-2,x=2050,plotLabs[x],pos=4) 
 
 par(xpd=FALSE)
}

par(xpd=TRUE)
text(x=0,y=192,"<100%",cex=.65)
text(x=168,y=192,">100%",cex=.65)
mtext(side=3,"Profit/year ($10b)",line=.58)
mtext(side=3,0,adj=.1,line=-.1,)
mtext(side=3,round(max(NoPastProfits),2),adj=.95,line=-.1)
color.legend(1987,105,2042,110,rect.col=col)
par(xpd=FALSE)
mtext(side=2,expression('Fraction of stocks 
beneath 0.8 *'),line=2.2)
mtext(side=2,expression('B'[MSY]),line=2.1,adj=.75)
mtext(side=1,"Year",line=2)
par(new=T)
symbols(x=1980,y=10,circles=1,inches=.09,
 ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
par(new=T)
symbols(x=1980,y=18,circles=1,inches=.07,
 ylab="",xlab="",ylim=ylimIn,xlim=xlimIn,yaxt='n',xaxt='n')
text(x=1987,y=3,"Total harvest (million MT)",cex=.7)
text(x=1983,y=10,80,cex=.8)
text(x=1983,y=18,60,cex=.8)
