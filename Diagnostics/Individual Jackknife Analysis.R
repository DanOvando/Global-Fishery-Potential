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
load('Results/3.0/Data/Global Fishery Recovery Results.rdata')
NumCPUs<- 1
FigureFolder<- paste(BatchFolder,'Diagnostics/Individual Jackknife/',sep='')
dir.create(FigureFolder,recursive=T)

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
sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)




RamData<- RamData[RamData$Year<=BaselineYear,]

Regions<- unique(RamData$Country)

RamIds<- unique(RamData$IdOrig)

JackStore<- as.data.frame(matrix(NA,nrow=0,ncol=14))

colnames(JackStore)<- c('Assessid','Year','Country','Catch','RamB','RamF','RamMSY','PrmB','CmsyB','CmsyF','CmsyMSY','CmsyBnoP','CmsyFnoP','CmsyMSYnoP')

NumCatchMSYIterations<- 25000

ErrorSize<- 0.95

TransbiasIterations<- 1000

NumCPUs<- 1

sigR<- 0

# NewRam<- MaidService(RawData[RawData$Dbase=='RAM',],OverlapMode,BaselineYear) #Filter out unusable stocks, prepare data for regression and use


# for (n in 1:length(Regions))
# {

RamIds<- unique(RamData$IdOrig)

for (r in 1:length(RamIds))
{
  
  Omit<- RamData[RamData$IdOrig%in%RamIds[r],]
  
  if (sum(is.na(Omit$Catch))==0)
  {
    
    #       FirstCatch<- which(is.na(Omit$Catch)==F)[1]
    #       
    #       Omit<- Omit[(FirstCatch+4):dim(Omit)[1],]
    
    Omit$CatchToRollingMax[is.na(Omit$CatchToRollingMax)]<- 0
    
    #       Omit$Catch<- na.approx(Omit$Catch)
    
    Jacked<- RamData[!(RamData$IdOrig%in%RamIds[r]),]
    
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
    
    BiomassColumns<- (grepl('BvBmsy$',colnames(Omit)) | grepl('Prediction',colnames(Omit))) & grepl('LogBvBmsy',colnames(Omit))==F
    
    
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

    CatchMSYresults<- (RunCatchMSY(OmitStatus$Data,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps)$MsyData)
    
    TempJack[,c('CmsyB','CmsyF','CmsyMSY')]<- CatchMSYresults[,c('CatchMSYBvBmsy','FvFmsy','MSY')]
    
#     OmitStatus$Data$BvBmsySD<- NA
#     
#     CatchMSYresults<- (RunCatchMSY(OmitStatus$Data,1,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps)$MsyData)
#     
#     TempJack[,c('CmsyBnoP','CmsyFnoP','CmsyMSYnoP')]<- CatchMSYresults[,c('CatchMSYBvBmsy','FvFmsy','MSY')]
    
    show(paste(100*(r/length(RamIds)),' % Done with JackKnife',sep=''))
    
    JackStore<- rbind(JackStore,TempJack)
  } #Close if all catch loop
} #Close stock loop


Prm<- cbind(JackStore[,c('Assessid','Year','Country','Catch','RamB','PrmB','RamMSY','CmsyMSY','RamF','CmsyF')],'PRM')

CmsyB<- cbind(JackStore[,c('Assessid','Year','Country','Catch','RamB','CmsyB','RamMSY','CmsyMSY','RamF','CmsyF')],'Cmsy')

# CmsyBnoP<- cbind(JackStore[,c('Assessid','Year','Country','Catch','RamB','CmsyBnoP','RamMSY','CmsyMSY','RamF','CmsyF')],'CmsyBnoP')

colnames(Prm)<- c('Id','Year','Country','Catch','RamBvBmsy','ModelBvBmsy','RamMSY','CmsyMSY','RamF','CmsyF','Model')

colnames(CmsyB)<- c('Id','Year','Country','Catch','RamBvBmsy','ModelBvBmsy','RamMSY','CmsyMSY','RamF','CmsyF','Model')

# colnames(CmsyBnoP)<- c('Id','Year','Country','Catch','RamBvBmsy','ModelBvBmsy','RamMSY','CmsyMSY','RamF','CmsyF','Model')

# PlotJack<- rbind(Prm,CmsyB,CmsyBnoP)
PlotJack<- rbind(Prm,CmsyB)

SpeciesInfo<- RamData[,c('IdOrig','SpeciesCatName','MaxLength','AgeMat','VonBertK')]

colnames(SpeciesInfo)<- c('Id','SpeciesCatName','MaxLength','AgeMat','VonBertK')

PlotJack<- join(PlotJack, SpeciesInfo,by='Id',match='first')
# 
# save(PlotJack,JackStore,file=paste(ResultFolder,'Individual JackKnife.rdata',sep=''))
# 
# load(file=paste(ResultFolder,'Individual JackKnife.rdata',sep=''))

FigureFolder<- paste(BatchFolder,'Diagnostics/Individual Jackknife/',sep='')

# PlotJack$ModelBvBmsy[PlotJack$ModelBvBmsy>2.5]<- 2.5

PlotJack$ProportionalBError<- 100*((PlotJack$ModelBvBmsy-PlotJack$RamBvBmsy)/PlotJack$RamBvBmsy)

# PlotJack$ProportionalPRMError<- 100*((PlotJack$ModelBvBmsy-PlotJack$RamBvBmsy)/PlotJack$RamBvBmsy)

PlotJack$ProportionalMSYError<- 100*((PlotJack$CmsyMSY-PlotJack$RamMSY)/PlotJack$RamMSY)

PlotJack$ProportionalFError<- 100*((PlotJack$CmsyF-PlotJack$RamF)/PlotJack$RamF)

PlotJack<- subset(PlotJack,is.na(CmsyMSY)==F)

JackknifePlots(PlotJack,FigureFolder)


