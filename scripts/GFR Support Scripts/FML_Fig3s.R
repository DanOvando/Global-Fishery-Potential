pdf(file = '../Misc Analyses/Figure 3 Profit 2050 Just Today.pdf',width=6,height=6)

plot(-10000000,ylim=ylimIn,xlim=xlimIn,las=1,ylab='',xlab='')
par(new=T)

dat<-read.csv(file = 'Results/PNAS Submission - 6.01 global demand common phi/Data/Fig3data_Profit2050.csv',stringsAsFactors = F)

dat<-dat %>%
  filter(Policy %in% c('Business As Usual','Catch Share Three','Fmsy Three','Today'))

dat$Policy<-c('BAU','RBFM','Fmsy','Today')

sizeCirc			<-.5
catchAdj			<-2.9

inCols<-c(col2,col2,col2,col4)

# index for subsetting policies
dat<-dat[4,]

symbols(x=dat$Bio,y=dat$Profit,dat$Catch,
        inches=sizeCirc,ylim=ylimIn,xlim=xlimIn,bg=inCols[4],xaxt='n', yaxt='n',ylab='',xlab='',fg=1)

Yind<-dat$Profit
Yind2<-c(dat$Profit-catchAdj)
Xind<-dat$Bio
Xind2<-dat$Bio


# Adjust the position of the bubble labels. order is (BAU, RBFM CC, Fmsy CC, Today)
# Xind<-  Xind+c(-120,0,0,120)
# Xind2<-Xind2+c(-120,0,0,120)
# Yind<-  Yind+c(0,0,0,0)
# Yind2<-Yind2+c(0,0,0,0)

Xind<-  Xind+c(120)
Xind2<-Xind2+c(120)
Yind<-  Yind+c(0)
Yind2<-Yind2+c(0)

labName 	<-c("BAU","RBFM",expression('F'[MSY]),"Today")

text(y=Yind,x=Xind,dat$Policy,cex=.85)
text(y=Yind2,x=Xind2,round(dat$Catch,digits=1),cex=.65)


mtext(side=1,"Biomass (MMT)",line=2)
mtext(side=2,"Annual Profit ($ Billions)",line=2)
legend("bottomleft",bty='n',col=c(col2,col3),pch=16,c("Policy applied to stocks of conservation concern",
                                                      "Policy applied to all stocks"))

dev.off()
