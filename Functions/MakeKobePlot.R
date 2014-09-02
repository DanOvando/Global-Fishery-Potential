MakeKobePlot<- function(Data,PlotYear,FigureName)
{
  
#     Data<- BiomassStatus$D
  
# FaoData<- MsyData[MsyData$Dbase=='FAO',]
# 
# (FaoData$Catch/FaoData$MSY)/FaoData$BvBmsy
# 
# 
#  PlotYear<- 2006
# # 
#  FigureName<- 'Trial Kobe Plot'


if (any(Data$BvBmsy<0,na.rm=T)){Data$BvBmsy<- exp(Data$BvBmsy)}
  
BaselineData<- subset(Data,subset=Year==PlotYear & is.na(FvFmsy)==F & is.na(BvBmsy)==F)

BaselineData$FvFmsy[BaselineData$FvFmsy>2]<- 2

BaselineData$BvBmsy[BaselineData$BvBmsy>2]<- 2

DensityData<- (kde2d(BaselineData$BvBmsy,BaselineData$FvFmsy,n=500))

FTrend<- ddply(Data,c('Year','Dbase'),summarize,FTrend=mean(FvFmsy,na.rm=T))

KobeColors<- colorRampPalette(c("lightskyblue", "white",'gold1'))

DbaseColors<- colorRampPalette(c('Red','Black','Green'))

# BaselinData$BvBmsy[BaselineData$Dbase=='RAM']<- 2

pdf(file=paste(FigureFolder,FigureName,'.pdf',sep=''))
# par(mfrow=c(1,2))
layout(t(matrix(c(1,2))), widths = c(4,1), heights = c(1,1), respect = FALSE)
par(mai=c(1,1,1,0))
image(DensityData$x,DensityData$y,DensityData$z,col=KobeColors(50),xlab='B/Bmsy',ylab='F/Fmsy')
points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=16,col=(as.factor(BaselineData$Dbase)),cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
abline(h=1,v=1,lty=2)
plot.new()
par(mai=c(0,0,0,0))
legend('center',legend=unique(BaselineData$Dbase),pch=16,col=(as.factor(unique(BaselineData$Dbase))),cex=0.7,bty='n')
dev.off()


}