AllStocks<-read.csv("C:/Users/Cody/Desktop/UpsideData/UnLumped Projection DataAll Stocks Country Upsides.csv")
OverfishStocks<-read.csv("C:/Users/Cody/Desktop/UpsideData/UnLumped Projection DataOverfish Only Country Upsides.csv")
library(mapplots)
# subset to countries that have overfished stocks
 tmp				<-unique(OverfishStocks$Country)
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

labName<-c("Catch share",expression('F'[MSY]),"Optimal","Status quo")

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
 colrange<-seq(min(CatCur[c(1,3,5,8)]),max(CatCur[c(1,3,5,8)]))
 col <-colorRampPalette(c("red","blue"))(length(colrange))
 for(i in 1:length(col ))
  {
   if(col [i]!=0)
    col [i]<-paste(col [i],88,sep="")
  }
 useCol<-rep(0,length(ChangeCatSQ))
 for(i in 1:length(useCol))
  try(useCol[i] <- col[which(abs(colrange-CatCur[i]) == min(abs(colrange-CatCur[i])))] )
dev.new(width=4,height=4)
plot(-10000000,ylim=ylimIn,xlim=xlimIn,las=1,ylab='',xlab='')
par(new=T)
symbols(x=BioCur[c(1,3,5,8)],y=NPV[c(1,3,5,8)],CatCur[c(1,3,5,8)],
 inches=.2,ylim=ylimIn,xlim=xlimIn,bg=useCol,xaxt='n', yaxt='n',ylab='',xlab='',fg=1)
#posiSpot<-c(2,1,4,4)
text(y=NPV[c(1,3,5,8)],x=BioCur[c(1,3,5,8)],labName,cex=.65)
mtext(side=1,"Biomass (million mt)",line=2)
mtext(side=2,"Annual profit ($10 billion)",line=2)

legend.bubble(x=600,y=5, z=c(30,80), maxradius = 2.05, n = 2, round = 0, bty = "n", mab = 1.2, 
    bg = NULL, inset = 0, pch = 21, txt.cex = .65, 
    txt.col = NULL, font = NULL)
legend(x=400,y=4,bty='n',legend="Global harvest 
  (million mt)",cex=.8)