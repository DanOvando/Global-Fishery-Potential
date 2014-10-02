
# TestCatchMSY ------------------------------------------------------------

#This code runs comparisons of Catch MSY and PRM predictions against RAM data

#Load in predictions of B/Bmsy made my the PRM

load('Results/Scratch/Data/Global Fishery Recovery Results.rdata')

PrmPred<- BiomassData[BiomassData$Dbase=='RAM',]

PrmStatus<- AnalyzeFisheries(PrmPred,'Scratch CMSY Diagnostics','Year',min(PrmPred$Year):max(PrmPred$Year),RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)$Data

RamMsy<- ddply(PrmStatus,c('IdOrig'),summarize,RamMsy=mean(MSY,na.rm=T))

PrmStatus$MSY<- NA

CmsyPred<- RunCatchMSY(PrmStatus,ExcludeForageFish,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,n,300,FALSE)

CmsyPred<- CmsyPred$Data

CatchMsyMsy<- ddply(CmsyPred,c('IdOrig'),summarize,CatchMsyMsy=mean(MSY,na.rm=T))

JointMsy<- join(RamMsy,CatchMsyMsy,by='IdOrig')

pdf(file='Diagnostics/Catch MSY MSY Diagnostics.pdf')
  xyplot(log(CatchMsyMsy) ~ log(RamMsy),data=JointMsy,auto.key=T,xlab='Log RAM MSY',ylab='Log CatchMSY MSY', panel=function(x,y,...)
  {
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()

BioPreds<- ddply(CmsyPred,c('IdOrig','Year'),summarize,RamBvBmsy=exp(LogBvBmsy),CmsyBvBmsy=CatchMSYBvBmsy,M1BvBmsy=exp(M1Prediction),
                 M2BvBmsy=exp(M2Prediction),M3BvBmsy=exp(M3Prediction),M4BvBmsy=exp(M4Prediction),M6BvBmsy=exp(M6Prediction))



PredictionNames<- colnames(BioPreds[4:9])

RamData<- BioPreds[,1:3]

FlatBio<- cbind(RamData,BioPreds[,PredictionNames[1]])

FlatBio$Source<- PredictionNames[1]

BioNames<- c('IdOrig','Year','RamBvBmsy','Prediction','Source')

colnames(FlatBio)<- BioNames

for (p in 2:length(PredictionNames))
{
  TempBio<- cbind(RamData,BioPreds[,PredictionNames[p]])
  
  TempBio$Source<- PredictionNames[p]
  
  colnames(TempBio)<- BioNames
  
  if (p>1)
  {
    FlatBio<- rbind(FlatBio,TempBio)
  }
}


pdf(file='Diagnostics/PRM and Cmsy BvBmsy Diagnostics.pdf')

xyplot(log(Prediction) ~ log(RamBvBmsy) |Source,data=FlatBio,subset= Source=='CmsyBvBmsy' | Source=='M1BvBmsy',xlab=' Log RAM B/Bmsy',ylab='Log Predicted B/Bmsy', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()

# Apply CatchMSY to those predictions

#Plot
