# 1. Loop over RAM stocks you can use
# 
# 2. run regression on all except the selected ram stock
# 
# 3. apply PRM to omitted RAM stock
# 
# 4. Run catchMSY without priors
# 
# 5. Run catchMSY with priors
# 
# 6. Store Real B/Bmsy, F/Fmsy, MSY, PRM B/Bmsy, MSY BvBmsy, FvFmsy, MSY with and without priors, and year
rm(list=ls())
load('Results/Dec 28 Full Run/Data/Global Fishery Recovery Results.rdata')
library(car)
library(plyr)
library(lattice)
library(rfishbase)
library(stringr)
library(RCurl)
library(XML)
library(MASS)
library(prettyR)
library(zoo)
library(proftools)
library(snowfall)
library(parallel)
# library(shiny)
library(ggplot2)
library(gridExtra)
library(reshape2)

RamData<- RamData[RamData$Year<=BaselineYear,]

Regions<- unique(RamData$Country)[1]

RamIds<- unique(RamData$IdOrig)

JackStore<- as.data.frame(matrix(NA,nrow=0,ncol=14))

colnames(JackStore)<- c('Assessid','Year','Country','Catch','RamB','RamF','RamMSY','PrmB','CmsyB','CmsyF','CmsyMSY','CmsyBnoP','CmsyFnoP','CmsyMSYnoP')

NumCatchMSYIterations<- 2000

ErrorSize<- 0.95

TransbiasIterations<- 1000

sigR<- 0.05

# NewRam<- MaidService(RawData[RawData$Dbase=='RAM',],OverlapMode,BaselineYear) #Filter out unusable stocks, prepare data for regression and use



for (n in 1:length(Regions))
{
  
  RamIds<- unique(RamData$IdOrig[RamData$Country==Regions[n]])
  
  for (r in 1) #:length(RamIds))
  {
    
    Omit<- RamData[RamData$IdOrig%in%RamIds,]
    
    if (2>1) #(sum(is.na(Omit$Catch))==0)
    {
      
      #       FirstCatch<- which(is.na(Omit$Catch)==F)[1]
      #       
      #       Omit<- Omit[(FirstCatch+4):dim(Omit)[1],]
      
      Omit$CatchToRollingMax[is.na(Omit$CatchToRollingMax)]<- 0
      
#       Omit$Catch<- na.approx(Omit$Catch)
      
      Jacked<- RamData[!(RamData$IdOrig%in%RamIds),]
      
      TempJack<- as.data.frame(matrix(NA,nrow=dim(Omit)[1],ncol=14))
      
      colnames(TempJack)<- c('Assessid','Year','Country','Catch','RamB','RamF','RamMSY','PrmB','CmsyB','CmsyF','CmsyMSY','CmsyBnoP','CmsyFnoP','CmsyMSYnoP')
      
      TempJack[,c('Assessid','Year','Country','Catch','RamB','RamF','RamMSY')]<- Omit[,c('IdOrig','Year','Country','Catch','BvBmsy','FvFmsy','MSY')]
      
      JackModel<- RunRegressions(Jacked,Regressions,'Real Stocks')
      
      RealModelFactorLevels<- NULL
      
      Models<- names(Regressions)
      
      TempOmitted<- NULL
      
      for (m in 1:length(names(Regressions)))
      {
        Model<- names(Regressions)[m]
        eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))
      }
      
      Jacked<- InsertFisheryPredictions(Jacked,JackModel) #Add fishery predictions back into main dataframe
      
      RealModelSdevs<- CreateSdevBins(JackModel$Models,Jacked,TransbiasBin)
      
      
      AllPossible<- unique(data.frame(I(Jacked$SpeciesCatName),I(Jacked$SpeciesCat)))
      
      colnames(AllPossible)<- c('SpeciesCatNames','SpeciesCat')
      
      RamPossibleCats<- unique(RamData$SpeciesCatName)
      
      Models<- Models[Models!='M7']
      
      for (m in 1:length(Models)) #Apply models to species level fisheries
      {
        
        TempModelName<- Models[m]
        
        eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))
        
        eval(parse(text=paste('TempModel<- RealModels$',TempModelName,sep='')))
        
        ProxyCats<- AssignNearestSpeciesCategory(Omit,TempLevel,AllPossible)
        
        Predictions<- predict(TempModel,ProxyCats$Data)
        
        eval(parse(text=paste('Omit$',TempModelName,'Prediction<- Predictions',sep='')))  
      }
      
      BiomassData<- Omit #Only store fisheries that have some form of biomass estimates
      
      BiomassData$BvBmsy<- NA
      
      BiomassData$BestModel<- NA
      
      BiomassData$LogBvBmsy<- NA
      
      BiomassData$FvFmsy<- NA
      
      BiomassData$MSY<- NA
      
      BiomassColumns<- (grepl('BvBmsy',colnames(Omit)) | grepl('Prediction',colnames(Omit))) & grepl('LogBvBmsy',colnames(Omit))==F
      
      
      AvailableBio<- (BiomassData[,BiomassColumns])
      
      HasSomething<-  rowSums(is.na(AvailableBio))<dim(AvailableBio)[2]
      
      BiomassData<- BiomassData[HasSomething,]
      
      AvailableBio<- AvailableBio[HasSomething,]

      TempJack<- TempJack[HasSomething,]
      
      AvailableBioMarker<- matrix(rep((1:dim(AvailableBio)[2]),dim(AvailableBio)[1]), dim(AvailableBio)[1],dim(AvailableBio)[2],byrow=TRUE)
      
      AvailableBioMarker<- AvailableBioMarker*(is.na(AvailableBio)==F)
      
      AvailableBioMarker[AvailableBioMarker==0]<- NA
      
      BestModel<- apply(AvailableBioMarker,1,min,na.rm=T)
      
      BestBio<- NULL
      for (b in 1:dim(AvailableBio)[1])
      {
        BestBio[b]<- AvailableBio[b,BestModel[b]]
      }
      
      BestBio[BestModel==1]<- log(BestBio[BestModel==1])
      
      BestModelnames<- c('RAM',ModelNames)
      
      BestModelNames<- BestModelnames[sort(unique(BestModel))]
      
      BestModel<- as.factor((BestModel))
      
      levels(BestModel)<- BestModelNames
      
      BiomassData$BestModel<- BestModel
      
      BiomassData$BvBmsy<- BestBio
      
      BiomassData<- AssignEconomicData(BiomassData,BvBmsyOpenAccess) #Assign price and cost data to each stock
      
      BiomassData$RanCatchMSY<- F
      
      BiomassData$IdLevel<- 'Species'
      
      #       BiomassData$BestModel<- unique(BiomassData$BestModel[is.na(BiomassData$BestModel)==F])
      
      BiomassData$Dbase<- 'FAO'
      
      BiomassData$CatchMSYBvBmsy_LogSd<- NA
      
      OmitStatus<- AnalyzeFisheries(BiomassData,'JackStat','Year',min(BiomassData$Year):max(BiomassData$Year),RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
      
      TempJack[,c('PrmB')]<- OmitStatus$Data$BvBmsy
      
      CatchMSYresults<- (RunCatchMSY(OmitStatus$Data,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps))
      
      TempJack[,c('CmsyB','CmsyF','CmsyMSY')]<- CatchMSYresults[,c('CatchMSYBvBmsy','FvFmsy','MSY')]
      
      OmitStatus$Data$BvBmsySD<- NA
      
      CatchMSYresults<- (RunCatchMSY(OmitStatus$Data,1,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps))
      
      TempJack[,c('CmsyBnoP','CmsyFnoP','CmsyMSYnoP')]<- CatchMSYresults[,c('CatchMSYBvBmsy','FvFmsy','MSY')]
      
      show(paste(100*(r/length(RamIds)),' % Done with JackKnife',sep=''))
      
      JackStore<- rbind(JackStore,TempJack)
    } #Close if all catch loop
  } #Close stock loop
} #Close regions loop


Prm<- cbind(JackStore[,c('Assessid','Year','Country','Catch','RamB','PrmB','RamMSY','CmsyMSY','RamF','CmsyF')],'PRM')

CmsyB<- cbind(JackStore[,c('Assessid','Year','Country','Catch','RamB','CmsyB','RamMSY','CmsyMSY','RamF','CmsyF')],'CmsyB')

CmsyBnoP<- cbind(JackStore[,c('Assessid','Year','Country','Catch','RamB','CmsyBnoP','RamMSY','CmsyMSY','RamF','CmsyF')],'CmsyBnoP')

colnames(Prm)<- c('Id','Year','Country','Catch','RamBvBmsy','ModelBvBmsy','RamMSY','CmsyMSY','RamF','CmsyF','Model')

colnames(CmsyB)<- c('Id','Year','Country','Catch','RamBvBmsy','ModelBvBmsy','RamMSY','CmsyMSY','RamF','CmsyF','Model')

colnames(CmsyBnoP)<- c('Id','Year','Country','Catch','RamBvBmsy','ModelBvBmsy','RamMSY','CmsyMSY','RamF','CmsyF','Model')

PlotJack<- rbind(Prm,CmsyB,CmsyBnoP)

SpeciesInfo<- RamData[,c('IdOrig','SpeciesCatName','MaxLength','AgeMat','VonBertK')]

colnames(SpeciesInfo)<- c('Id','SpeciesCatName','MaxLength','AgeMat','VonBertK')

PlotJack<- join(PlotJack, SpeciesInfo,by='Id',match='first')

save(PlotJack,JackStore,file='Diagnostics/JackKnife.rdata')

load(file='Diagnostics/JackKnife.rdata')

pdf(file='Diagnostics/Observed vs Predicted BvBmsy Diagnostic Plots.pdf')
xyplot((ModelBvBmsy) ~ (RamBvBmsy) | Model,subset=Year>2005,data=PlotJack,xlab=' Log RAM B/Bmsy',ylab='Log Predicted B/Bmsy', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()

pdf(file='Diagnostics/Year by Year Observed vs Predicted BvBmsy Diagnostic Plots.pdf')
xyplot((ModelBvBmsy) ~ (RamBvBmsy) | as.factor(Year),subset=Model=='PRM',data=PlotJack,xlab=' Log RAM B/Bmsy',ylab='Log Predicted B/Bmsy', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()




pdf(file='Diagnostics/Observed vs Predicted FvFmsy Diagnostic Plots.pdf')
xyplot((CmsyF) ~ (RamF),subset=Year>2005,data=JackStore,xlab=' RAM F/Fmsy',ylab=' Predicted F/Fmsy', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()

pdf(file='Diagnostics/Observed vs Predicted MSY Diagnostic Plots.pdf')
xyplot(log(CmsyMSY) ~ log(RamMSY),subset=Year>2005,data=JackStore,xlab='Log RAM MSY',ylab=' Log  Predicted MSY', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()



pdf(file='Diagnostics/CMSY MSY Proportional Error Over Time.pdf')
boxplot(((CmsyMSY-RamMSY)/RamMSY)~ Year,data=JackStore,outline=FALSE,horizontal=FALSE,xlab='Year',ylab='CatchMSY MSY Proportional Error')
abline(h=0)
dev.off()

pdf(file='Diagnostics/CMSY FvFmsy Proportional Error Boxplots.pdf')
boxplot(((CmsyF-RamF)/RamF)~ Year,data=subset(JackStore,Year<2012),outline=FALSE,horizontal=FALSE,ylab='CatchMSY FvFmsy Proportional Error')
abline(h=0)
dev.off()

pdf(file='Diagnostics/CMSY BvBmsy Proportional Error Boxplots.pdf')
# par(mfrow=c(2,1))
boxplot(((CmsyB-RamB)/RamB)~ Year,data=JackStore,outline=FALSE,horizontal=FALSE,ylab='CatchMSY BvBmsy With Prior Proportional Error')
abline(h=0)
dev.off()


PlotJack$ProportionalError<- 100*((PlotJack$ModelBvBmsy-PlotJack$RamBvBmsy)/PlotJack$RamBvBmsy)

PlotJack$ProportionalMSYError<- 100*((PlotJack$CmsyMSY-PlotJack$RamMSY)/PlotJack$RamMSY)

PlotJack$ProportionalFError<- 100*((PlotJack$CmsyF-PlotJack$RamF)/PlotJack$RamF)


pdf(file='Diagnostics/Proportional Error by Model Boxplots.pdf')
boxplot( ProportionalError~Model ,data=PlotJack,outline=F,ylab='% Proportional Errori in BvBmsy')
dev.off()

pdf(file='Diagnostics/Proportional Error Boxplots by Country.pdf')
boxplot( ProportionalError ~Country ,data=PlotJack,outline=F,ylab='% Proportional Error in BvBmsy')
dev.off()



pdf(file='Diagnostics/Proportional error in BvBmsy by time and country.pdf')

print(ggplot(data=subset(PlotJack,Year>1985),aes(factor(Year),ProportionalError))+
  geom_boxplot(outlier.shape=NA,fill='steelblue2')+
coord_cartesian(ylim=c(-200,200))+
#   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
  facet_wrap(~Country,scales='free')+
  geom_abline(intercept=0,slope=0)+
ylab('Proportional Error (%) in B/Bmsy')+
  xlab('Time')+
  scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))

dev.off()

# pdf(file='Diagnostics/Proportional error in FvFmsy by time and country.pdf')
# 
# PlotJack$ProportionalFError[is.infinite(PlotJack$ProportionalFError)]<- NA
# 
# PlotJack$ProportionalFError<- pmin(PlotJack$ProportionalFError,200)
# print(ggplot(data=subset(PlotJack,Year>1985),aes(factor(Year),ProportionalFError))+
#         geom_boxplot(outlier.shape=NA,fill='steelblue2')+
# #          coord_cartesian(ylim=c(-200,200))+
#         facet_wrap(~Country)+
#         geom_abline(intercept=0,slope=0)+
#         ylab('Proportional Error (%)')+
#         xlab('Time'))
# #         scale_x_discrete(breaks=as.character(seq(from=1986,to=2012,by=8))))
# 
# dev.off()


pdf(file='Diagnostics/Proportional error in MSY by country.pdf')


print(ggplot(data=subset(PlotJack,Year==BaselineYear),aes((Country),ProportionalMSYError))+
        geom_boxplot(outlier.shape=NA,fill='steelblue2')+
        coord_cartesian(ylim=c(-200,200))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
#         facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        ylab('Proportional Error in MSY (%)'))
dev.off()


pdf(file='Diagnostics/Proportional error in MSY by Species Category.pdf')


print(ggplot(data=subset(PlotJack,Year==BaselineYear),aes((SpeciesCatName),ProportionalMSYError))+
        geom_boxplot(outlier.shape=NA,fill='steelblue2')+
        coord_cartesian(ylim=c(-200,200))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        #         facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        ylab('Proportional Error in MSY (%)')+
        xlab('')+
        theme(axis.text.x=element_text(angle = -100, hjust = 0)))
dev.off()

pdf(file='Diagnostics/Proportional error in BvBmsy by Species Category.pdf')


print(ggplot(data=subset(PlotJack,Year==BaselineYear),aes((SpeciesCatName),ProportionalError))+
        geom_boxplot(outlier.shape=NA,fill='steelblue2')+
        coord_cartesian(ylim=c(-200,200))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        #         facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        ylab('Proportional Error in BvBmsy (%)')+
        xlab('')+
        theme(axis.text.x=element_text(angle = -100, hjust = 0)))
dev.off()






# pdf(file='Diagnostics/PRM and CMSY Proportional Error Boxplots by Country over Time Short.pdf')
# 
# print(ggplot(data=subset(PlotJack,Year>2000 & is.na(ProportionalError)==F),aes(factor(Year),ProportionalError))+
#         geom_boxplot(fill='steelblue2')+
#         coord_cartesian(ylim=c(-100,200))+
#         #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
#         facet_wrap(~Country,scales='free')+
#         geom_abline(intercept=0,slope=0)+
#         ylab('Proportional Error (%)')+
#         xlab('Time')+
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
# dev.off()


pdf(file='Diagnostics/Proportional BvBmsy Error as a function of catch and country.pdf')

PlotJack$RamBvBmsy[PlotJack$RamBvBmsy>2]<- 2
print(ggplot(data=subset(PlotJack,Year>2000 & is.na(ProportionalError)==F),aes((Catch),(ProportionalError)))+
        geom_point(aes(color=RamBvBmsy))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Proportional Error (%) in BvBmsy')+
        xlab('Catch'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

pdf(file='Diagnostics/Proportional BvBmsy Error as a function of log catch and country.pdf')

PlotJack$RamBvBmsy[PlotJack$RamBvBmsy>2]<- 2
print(ggplot(data=subset(PlotJack,Year>2000 & is.na(ProportionalError)==F),aes(log(Catch),(ProportionalError)))+
        geom_point(aes(color=RamBvBmsy))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Proportional Error (%) in BvBmsy')+
        xlab('Catch'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

pdf(file='Diagnostics/Proportional MSY Error as a function of catch and country.pdf')

PlotJack$RamBvBmsy[PlotJack$RamBvBmsy>2]<- 2
print(ggplot(data=subset(PlotJack,Year==BaselineYear & is.na(ProportionalMSYError)==F),aes((Catch),(ProportionalMSYError)))+
        geom_point(aes(color=RamBvBmsy))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Proportional Error (%) in MSY')+
        xlab('Catch'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

PlotJack$Catch[PlotJack$Catch==0]<- NA

pdf(file='Diagnostics/Proportional MSY Error as a function of log catch and country.pdf')

print(ggplot(data=subset(PlotJack,Year==BaselineYear & is.na(ProportionalMSYError)==F),aes(log(Catch),(ProportionalMSYError)))+
        geom_point(aes(color=RamBvBmsy))+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Proportional Error (%) in MSY')+
        xlab('Log Catch'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

TotalPerformance<- ddply(PlotJack,c('Id'),summarize,MeanBioError=mean(ProportionalError,na.rm=T),MeanMSYError=mean(ProportionalMSYError,na.rm=T),Country=unique(Country),TotalCatch=sum(Catch,na.rm=T))


pdf(file='Diagnostics/Proportional error in BvBmsy as a function of lifetime catch.pdf')

print(ggplot(data=TotalPerformance,aes((TotalCatch),(MeanBioError)))+
        geom_point(color='steelblue1')+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Mean Proportional Error (%) in BvBmsy')+
        xlab('Catch'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

pdf(file='Diagnostics/Proportional error in MSY as a function of lifetime catch.pdf')


print(ggplot(data=subset(TotalPerformance,is.na(MeanMSYError)==F),aes((TotalCatch),(MeanMSYError)))+
        geom_point(color='steelblue1')+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Proportional Error (%) in MSY')+
        xlab('Catch'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

pdf(file='Diagnostics/Proportional error in BvBmsy as a function of lifetime log catch.pdf')

print(ggplot(data=TotalPerformance,aes(log(TotalCatch),(MeanBioError)))+
        geom_point(color='steelblue1')+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Mean Proportional Error (%) in BvBmsy')+
        xlab('log(Catch)'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()

pdf(file='Diagnostics/Proportional error in MSY as a function of lifetime log catch.pdf')


print(ggplot(data=subset(TotalPerformance,is.na(MeanMSYError)==F),aes(log(TotalCatch),(MeanMSYError)))+
        geom_point(color='steelblue1')+
        #   coord_cartesian(ylim = range(boxplot(PlotJack$ProportionalError, plot=FALSE)$stats)*c(.8, 1.5))+
        facet_wrap(~Country,scales='free')+
        geom_abline(intercept=0,slope=0)+
        geom_smooth(method='lm')+
        ylab('Proportional Error (%) in MSY')+
        xlab('log(Catch)'))
#         scale_x_discrete(breaks=as.character(seq(from=2000,to=2012,by=4))))
dev.off()


