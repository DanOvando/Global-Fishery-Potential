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

RamIds<- unique(RamData$IdOrig)

JackStore<- as.data.frame(matrix(NA,nrow=0,ncol=12))

colnames(JackStore)<- c('Assessid','Year','RamB','RamF','RamMSY','PrmB','CmsyB','CmsyF','CmsyMSY','CmsyBnoP','CmsyFnoP','CmsyMSYnoP')

NumCatchMSYIterations<- 10000

ErrorSize=.95

TransbiasIterations<- 1000

sigR<- 0.05

for (r in 1:length(RamIds))
{
  
  Omit<- RamData[RamData$IdOrig==RamIds[r],]
  
  if (sum(is.na(Omit$Catch))==0)
  {
    
    FirstCatch<- which(is.na(Omit$Catch)==F)[1]
    
    Omit<- Omit[(FirstCatch+4):dim(Omit)[1],]
    
    Omit$CatchToRollingMax[is.na(Omit$CatchToRollingMax)]<- 0
    
    Omit$Catch<- na.approx(Omit$Catch)
    
    Jacked<- RamData[RamData$IdOrig!=RamIds[r],]
    
    TempJack<- as.data.frame(matrix(NA,nrow=dim(Omit)[1],ncol=12))
    
    colnames(TempJack)<- c('Assessid','Year','RamB','RamF','RamMSY','PrmB','CmsyB','CmsyF','CmsyMSY','CmsyBnoP','CmsyFnoP','CmsyMSYnoP')
    
    TempJack[,c('Assessid','Year','RamB','RamF','RamMSY')]<- Omit[,c('IdOrig','Year','BvBmsy','FvFmsy','MSY')]
    
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
    
    RealModelSdevs<- CreateSdevBins(JackModel,Jacked,TransbiasBin)
    
    
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
    
    BiomassData<- AssignEconomicData(BiomassData) #Assign price and cost data to each stock
    
    BiomassData$RanCatchMSY<- F
    
    BiomassData$IdLevel<- 'Species'
    
    BiomassData$BestModel<- unique(BiomassData$BestModel[is.na(BiomassData$BestModel)==F])
    
    
    BiomassData$Dbase<- 'FAO'
    
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
}



Prm<- cbind(JackStore[,c('Assessid','Year','RamB','PrmB')],'PRM')

CmsyB<- cbind(JackStore[,c('Assessid','Year','RamB','CmsyB')],'CmsyB')

CmsyBnoP<- cbind(JackStore[,c('Assessid','Year','RamB','CmsyBnoP')],'CmsyBnoP')

colnames(Prm)<- c('Id','Year','RamBvBmsy','ModelBvBmsy','Model')

colnames(CmsyB)<- c('Id','Year','RamBvBmsy','ModelBvBmsy','Model')

colnames(CmsyBnoP)<- c('Id','Year','RamBvBmsy','ModelBvBmsy','Model')

PlotJack<- rbind(Prm,CmsyB,CmsyBnoP)

save(PlotJack,JackStore,file='Diagnostics/JackKnife.rdata')

pdf(file='Observed vs Predicted Diagnostic Plots.pdf')
xyplot((ModelBvBmsy) ~ (RamBvBmsy) | Model,subset=Year>2005,data=PlotJack,xlab=' Log RAM B/Bmsy',ylab='Log Predicted B/Bmsy', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()


pdf(file='Observed vs Predicted FvFmsy Diagnostic Plots.pdf')
xyplot((CmsyF) ~ (RamF),subset=Year>2005,data=JackStore,xlab=' RAM F/Fmsy',ylab=' Predicted F/Fmsy', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()

pdf(file='Observed vs Predicted MSY Diagnostic Plots.pdf')
xyplot(log(CmsyMSY) ~ log(RamMSY),subset=Year>2005,data=JackStore,xlab='Log RAM MSY',ylab=' Log  Predicted MSY', panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
  panel.lmline(x,y,col='Salmon',...)
}
)
dev.off()



pdf(file='PRM and CMSY MSY Proportional Error Boxplots.pdf')
boxplot(((CmsyMSY-RamMSY)/RamMSY)~ Year,data=JackStore,outline=FALSE,horizontal=FALSE,xlab='Year',ylab='CatchMSY MSY Proportional Error')
abline(h=0)
dev.off()

pdf(file='PRM and CMSY FvFmsy Proportional Error Boxplots.pdf')
boxplot(((CmsyF-RamF)/RamF)~ Year,data=JackStore,outline=FALSE,horizontal=FALSE,ylab='CatchMSY FvFmsy Proportional Error')
abline(h=0)
dev.off()

pdf(file='PRM and CMSY BvBmsy Proportional Error Boxplots.pdf')
# par(mfrow=c(2,1))
boxplot(((CmsyB-RamB)/RamB)~ Year,data=JackStore,outline=FALSE,horizontal=FALSE,ylab='CatchMSY BvBmsy With Prior Proportional Error')
abline(h=0)
# 
# boxplot(((CmsyBnoP-RamB)/RamB)~ Year,data=JackStore,outline=FALSE,horizontal=FALSE,ylab='CatchMSY BvBmsy no Prior Proportional Error')
# abline(h=0)
dev.off()


pdf(file='PRM and CMSY Proportional Error Boxplots.pdf')
boxplot( 100*((ModelBvBmsy-RamBvBmsy)/RamBvBmsy)~Model ,data=PlotJack,outline=F,ylab='% Proportional Error')
dev.off()


