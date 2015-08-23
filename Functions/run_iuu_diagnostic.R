run_iuu_diagnostic<- function(Data,Regressions,IUULevel,NumCatchMSYIterations,BatchFolder,SubSample,
                              RealModelSdevs,NeiModelSdevs, NumCPUs = 1)
{
  FigureFolder<- paste(BatchFolder,'Diagnostics/IUU/',sep='')
  
  dir.create(FigureFolder,recursive=T)
  
  IdVar <<- 'IdOrig'
  
  FigureFolder <<- FigureFolder
  
  NumSamples<- length(unique(Data$IdOrig))
  
  Samp<- sample(unique(Data$IdOrig),(1-SubSample)*NumSamples,replace=F)
  
  Data<- subset(Data,IdOrig %in% Samp )
  
  Models<- names(Regressions)
  
  Data<- subset(Data,IdLevel=='Species' & Dbase=='FAO')
  
  CheckData<- subset(Data,IdLevel=='Species' & Dbase=='FAO')
  
  #   IUULevel<- 1.2
  
  #   NumCatchMSYIterations<- 5000
  
  ErrorSize=.95
  
  TransbiasIterations<- 1000
  
  sigR<- 0
  
  ## Apply Catch Error
  
  Data$Catch<- Data$Catch*IUULevel
  
  Data$MaxCatch<- Data$MaxCatch*IUULevel
  
  
  ## Store Current Values
  
  Data$OriginalBvBmsy<- Data$BvBmsy
  
  Data$OriginalMSY<- Data$MSY
  
  Data$OriginalFvFmsy<- Data$FvFmsy
  
  Data$Originalg<- Data$g
  
  Data$BvBmsy<- NA
  
  Data$MSY<- NA
  
  Data$FvFmsy<- NA
  
  Data$g<- NA
  
  ## Apply Regression
  
  FaoSpeciesPossibleCats<- unique(Data$SpeciesCatName)
  
  AllPossible<- unique(data.frame(I(Data$SpeciesCatName),I(Data$SpeciesCat)))
  
  colnames(AllPossible)<- c('SpeciesCatNames','SpeciesCat')
  # Apply regressions -------------------------------------------------------
  
  Models<- Models[Models!='M7']
  
  RealModelFactorLevels <- NULL
  
  for (m in 1:length(names(Regressions)))
  {
    Model<- names(Regressions)[m]
    eval(parse(text=paste('RealModelFactorLevels$',Model,'<- Regressions$',Model,'$xlevels$SpeciesCatName',sep='')))
  }
  
  for (m in 1:length(Models)) #Apply models to species level fisheries
  {
    
    TempModelName<- Models[m]
    
    eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))
    
    eval(parse(text=paste('TempModel<- Regressions$',TempModelName,sep='')))
    ProxyCats<- AssignNearestSpeciesCategory(Data,TempLevel,AllPossible)
    
    Predictions<- predict(TempModel,ProxyCats$Data)
    
    eval(parse(text=paste('Data$',TempModelName,'Prediction<- Predictions',sep='')))    
  }
  
  #   BiomassColumns<- (grepl('BvBmsy$',colnames(Data)) | grepl('Prediction',colnames(Data))) & grepl('LogBvBmsy',colnames(Data))==F
  
  BiomassColumns<- grepl('Prediction',colnames(Data))
  
  BioNames<- colnames(Data)[BiomassColumns]
  
  HasBiomass<- rowSums(is.na(Data[,BiomassColumns]))<length(BioNames)
  
  BiomassData<- Data[HasBiomass,] #Only store fisheries that have some form of biomass estimates
  
  MissingData<- Data[HasBiomass==F & Data$Dbase=='FAO',]
  
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
  
  #   BestBio[BestModel==1]<- log(BestBio[BestModel==1])
  
  ModelNames <- names(Regressions)
  
  BestModelnames<- ModelNames
  
  BestModelNames<- BestModelnames[sort(unique(BestModel))]
  
  BestModel<- as.factor((BestModel))
  
  levels(BestModel)<- BestModelNames
  
  BiomassData$BestModel<- BestModel
  
  BiomassData$BvBmsy<- BestBio
  
  BiomassData$PRMBvBmsy<- BestBio
  
  #   BiomassData$CommName<- as.character((BiomassData$CommName))
  
  BiomassData$SciName<- as.character((BiomassData$SciName))
  
  BiomassData$SpeciesCatName<- as.character(BiomassData$SpeciesCatName)
  
  BiomassData$RanCatchMSY<- F
  
  # Run First Analisis of Current Status --------------------------------------------------
  
  BiomassData$CatchMSYBvBmsy_LogSd<- NA

  
  GlobalStatus<- AnalyzeFisheries(Data = BiomassData,BatchName = 'Baseline Global Status',GroupingVars = 'Year',
                                  Years = min(BiomassData$Year):max(BiomassData$Year),
                                  RealModelSdevs = RealModelSdevs,NeiModelSdevs = NeiModelSdevs,TransbiasBin = 0.9,J = 1000)
  
  #   RAMStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='RAM',],'RAM Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  # Calculate MSY -----------------------------------------------------------
  
  sigR<- 0
  
  GlobalStatus$BvBmsySD[GlobalStatus$Data$Dbase=='SOFIA']<- 0.1
  
  #   arg<- sample(GlobalStatus$Data$IdOrig,100,replace=F)
  
  CatchMSYresults<- (RunCatchMSY(Data = GlobalStatus$Data, n = NumCatchMSYIterations,NumCPUs = NumCPUs))
  
  PostData<- CatchMSYresults$MsyData  
  
  PostData$bPE<- 100*((PostData$BvBmsy-PostData$OriginalBvBmsy)/PostData$OriginalBvBmsy)
  
  PostData$fPE<- 100*((PostData$FvFmsy-PostData$OriginalFvFmsy)/PostData$OriginalFvFmsy)
  
  PostData$MSYPE<- 100*((PostData$MSY-PostData$OriginalMSY)/PostData$OriginalMSY)
  
  PostData$gPE<- 100*((PostData$g-PostData$Originalg)/PostData$Originalg)
  
  Diagnostics<- melt(PostData[,c('bPE','fPE','MSYPE')])
  
  Diagnostics$IUULevel <- IUULevel
  
  pdf(paste(FigureFolder,'IUU Effect.pdf',sep=''))
  iuu_plot<- (ggplot(data=Diagnostics,aes(x=value))
  +geom_density(fill='steelblue2',alpha=0.6)+geom_vline(aes(xintercept = 0))+geom_vline(aes(xintercept=100*(unique(IUULevel)-1)),color='red')
  +facet_wrap(~variable,scales = 'free')+xlab('Proportional Error %')+xlim(c(-50,100)))
  print(iuu_plot)
  dev.off()
  
  save(iuu_plot,file = paste(FigureFolder,'iuu_plot.Rdata',sep = ''))
  
  return(list(PostData=PostData,IUUPlot=iuu_plot))
}