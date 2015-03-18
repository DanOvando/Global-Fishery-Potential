
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################

# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

# Read in and process data ------------------------------------------------------------

if (RunAnalyses==TRUE)
{
  
  if (file.exists(paste(ResultFolder,'Cleaned Compiled Database.csv',sep=''))==F)
  {
    
    #     source('Database_Build.R') #Build Tyler's database
    
    fulldata<- DatabaseBuild()
    
    Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes 
    
    Spec_Region_RAM=read.csv('Data/Ram_Regions_031115.csv',stringsAsFactors=F) # list of RAM Assessed IDs previously matched to species code and FAO Region
    
    Spec_Region_RAM$RegionFAO<- gsub("/",",",Spec_Region_RAM$RegionFAO,fixed=T) # change / to , for use in string parsing during filtering function
    
    fulldata$phi<- DefaultPhi
    
    write.csv(file=paste(ResultFolder,"fulldata.csv",sep=""),fulldata)
    
    show('Done building raw database')
    
    RawData<- fulldata
    
    RawData$FvFmsy<- RawData$UvUmsytouse
    
    FullData<- fulldata
    
    ### TEMPORARY ###
    
    FullData<-FullData[!(FullData$IdOrig %in% c('SEFSC-RSNAPGM-1872-2011-HIVELY')),] # GoMex Red Snapper
    
    if (SubSample>0)
    {
      
      FaoIds<- unique(FullData$IdOrig[FullData$Dbase=='FAO'])
      
      SampleIds<- sample(FaoIds,SubSample*length(FaoIds),replace=FALSE)
      # # # 
      FullData<-  FullData[! FullData[,IdVar] %in% SampleIds,]
    }
    FullData$FvFmsy<- FullData$UvUmsytouse
    
    rm(fulldata)
    
    FullData<-RamSciNameAdjuster(FullData,VersionToUse='SciNameToUse') # adjust SciNames of RAM stocks that have synonyms and other variations not found in AFSIS list
    
    CleanedData<- MaidService(FullData,OverlapMode,BaselineYear) #Filter out unusable stocks, prepare data for regression and use
    
    DroppedStocks<- CleanedData$DroppedStocks
    
    StitchIds<-CleanedData$StitchIds
    
    FullData<- CleanedData$CleanedData
    
    AllOverlap<-CleanedData$AllOverlap
    
    MultinationalOverlap<-CleanedData$MultinationalOverlapIds
    
    FullData<-RamSciNameAdjuster(FullData,VersionToUse='scientificname') # adjust SciNames of RAM stocks back to original names for FindFishbase
    
    FullData<- FindFishbase(FullData)
    
    FullData<-FindResilience(FullData)
    
    #     Overlap<- ddply(FullData,c('Year'),summarize,FullDataCatch=sum(Catch,na.rm=T))
    # 
    #     Raw<- ddply(RawData[RawData$Dbase=='FAO',],c('Year'),summarize,FullDataCatch=sum(Catch,na.rm=T))
    #     
    #     m<- join(Overlap,Raw,by='Year')
    #     
    #     quartz()
    #     matplot(m[,2:3])
    
    #     rm(CleanedData)
    
    write.csv(file=paste(ResultFolder,'Cleaned Compiled Database.csv',sep=''),FullData)
    
    write.csv(file=paste(ResultFolder,'Omitted Stocks.csv',sep=''),DroppedStocks)
    
    write.csv(file=paste(ResultFolder,'Raw Database.csv',sep=''),RawData)
    
  }
  else 
  {
    FullData<- read.csv(paste(ResultFolder,'Cleaned Compiled Database.csv',sep=''),stringsAsFactors=F)
    
    RawData<- read.csv(paste(ResultFolder,'Raw Database.csv',sep=''),stringsAsFactors=F)
    
  }
  
  FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA
  
  ModelNames<- names(Regressions) #Create columns to store the results of each PRM
  
  for (m in 1:length(ModelNames))
  {
    
    eval(parse(text=paste('FullData$',ModelNames[m],'Marker<- FALSE',sep='')))
    
    eval(parse(text=paste('FullData$',ModelNames[m],'Prediction<- NA',sep='')))
  }
  
  SofiaData<-  FullData[FullData$Dbase=='SOFIA',]
  
  RamData<- FullData[FullData$Dbase=='RAM',]
  
  FaoData<- FullData[FullData$Dbase=='FAO',]
  
  show('Raw Data Processed')
  
  #   # Create synthetic stocks -------------------------------------------------
  #   
  #   if (GroupMethod=='All')
  #   {
  #     Groups<- unique(FullData$SpeciesCatName,na.rm=T)
  #     
  #     Groups<- Groups[is.na(Groups)==F]
  #   }
  #   if (GroupMethod=='Nei')
  #   {
  #     Groups<- unique(FaoData$SpeciesCatName[ (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F])
  #   }
  #   
  #   SyntheticData<- StitchFish(RawData[RawData$Dbase=='RAM' ,],IdVar,Groups,GroupSamples,Iterations) 
  #   
  #   SyntheticData$BvBmsy[SyntheticData$BvBmsy>OutlierBvBmsy]<- NA
  #   
  #   show('Synthetic Stocks Created')
  #   
  #   for (m in 1:length(ModelNames))
  #   {
  #     
  #     eval(parse(text=paste('SyntheticData$',ModelNames[m],'Marker<- FALSE',sep='')))
  #     
  #     eval(parse(text=paste('SyntheticData$',ModelNames[m],'Prediction<- NA',sep='')))
  #     
  #   }
  
  # Prepare data for regression ---------------------------------------------
  
  #   library(proftools)
  #   
  #   #   if (CapRefs==T)
  #   #   {
  #   #     RamData$BvBmsy[RamData$BvBmsy>1.9]<- 1.9
  #   # 
  #   #     RamData$FvFmsy[RamData$FvFmsy>1.9]<- 1.9
  #   #   }
  #   #   
  #   Fisheries<- (unique(SyntheticData$IdOrig[SyntheticData$Catch>0]))
  # 
  #   SyntheticFormatRegressionResults<- mclapply(1:(length(Fisheries)), FormatForRegression,mc.cores=NumCPUs,Data=SyntheticData,Fisheries=Fisheries,DependentVariable=DependentVariable,CatchVariables=CatchVariables,CatchLags=CatchLags,LifeHistoryVars=LifeHistoryVars,IsLog=IsLog,IdVar=IdVar) 
  #   
  #   SyntheticData <- ldply (SyntheticFormatRegressionResults, data.frame)
  #   
  #   sfInit( parallel=TRUE, cpus=NumCPUs,slaveOutfile="SyntheticRegressionFormatProgress.txt" )
  #   
  #   
  #   
  #   sfExport('Data','Fisheries','DependentVariable','CatchVariables','CatchLags','LifeHistoryVars','IsLog','IdVar')
  #   
  #   SyntheticFormatRegressionResults <- (sfClusterApplyLB(1:(length(Fisheries)), FormatForRegression))      
  #   
  #   sfStop()
  #   
  #   rm(Data)
  
  show('Data prepared for regression')
  
  # Rprof(NULL)
  #  RProfData<- readProfileData('Rprof.out')
  #  flatProfile(RProfData,byTotal=TRUE)
  
  # Run regressions ---------------------------------------------------------
  
  RegressionResultsAllRam<- RunRegressions(RamData,Regressions,'Real Stocks') # run regressions using all RAM stock values
  
  RamData<- RegressionResultsAllRam$RamData # retain all RamData, not just the RamData with assesment only value
  
  RealModels<- RegressionResultsAllRam$Models # use regression models estimated from assessment BvBmsy values only
  
  ModelFitIds<-RamData$IdOrig[grepl('-est-',RamData$BvBmsyUnit)] # find stock ids with model fit BvBmsy values
  
  RegressionResults<- RunRegressions(RamData[!(RamData$IdOrig %in% ModelFitIds),],Regressions,'Real Stocks') # run regressions using assesment BvBmsy values only
  
  if(RegressAllRam==FALSE)
  {
    RealModels<- RegressionResults$Models # use regression models estimated from assessment BvBmsy values only
  }
  
  RealModelFactorLevels<- NULL
  
  Models<- names(Regressions)
  
  show('Regressions Run')
  
  # Process Regressions -----------------------------------------------------
  
  ## Determine species category levels that were used in each model run
  
  TempOmitted<- NULL
  for (m in 1:length(names(Regressions)))
  {
    Model<- names(Regressions)[m]
    eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))
  }
  
  #   RamData<- InsertFisheryPredictions(RamData,RealModels) #Add fishery predictions back into main dataframe
  
  if(RegressAllRam==TRUE) # generate SDevs depending on whether regression was run all all ram data or just assessment values
  {
    RealModelSdevs<- CreateSdevBins(RealModels,RamData,TransbiasBin)
  }
  
  if(RegressAllRam==FALSE)
  {
    RealModelSdevs<- CreateSdevBins(RealModels,RegressionResults$RamData,TransbiasBin)
  }
  
  save(RealModels,RealModelSdevs,file=paste(ResultFolder,'PrmRegressions.Rdata',sep=''))
  
  #   NeiRegressions<- list()
  #   
  #   NeiRegressions$M6<- Regressions$M6
  #   
  #   NeiRegressions$M7<- Regressions$M7
  #   
  #   SyntheticData$ExtendedTime<- FALSE
  #   
  #   NeiModels<- RunRegressions(SyntheticData,NeiRegressions,'Synthetic Stocks')
  #   
  #   SyntheticData<- NeiModels$RamData
  #   
  #   NeiModels<- NeiModels$Model
  #   
  #   NeiModelFactorLevels<- NULL
  #   
  #   for (m in 1:length(names(Regressions)))
  #   {
  #     Model<- names(Regressions)[m]
  #     
  #     eval(parse(text=paste('NeiModelFactorLevels$',Model,'<- NeiModels$',Model,'$xlevels$SpeciesCatName',sep='')))
  #     
  #   }
  #   
  #   #     SyntheticData<- InsertFisheryPredictions(SyntheticData,NeiModels) #Add fishery predictions back into main dataframe
  #   
  #   NeiModelSdevs<- CreateSdevBins(NeiModels,SyntheticData,TransbiasBin)
  #   
  # Prepare data for regression application ---------------------------------
  
  WhereFaoNeis<- (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F #Find unassessed NEIs
  
  WhereFaoMarineFish<- grepl('not identified',FaoData$SpeciesCatName)
  
  FaoSpeciesLevel<- FaoData[WhereFaoNeis==F & WhereFaoMarineFish==F ,] #Fao stocks named to the species level
  
  FaoNeiLevel<- FaoData[WhereFaoNeis,] #fao species named to the nei or spp level
  
  FaoMarineFishLevel<- FaoData[WhereFaoMarineFish,] #completely unidentified marine goo
  
  TempLevel<- NULL
  
  TempModel<- NULL
  
  show('Regressions Processed')
  
  # Prep for dummy species categories  ----------------------------------------
  
  AllPossible<- unique(data.frame(I(FullData$SpeciesCatName),I(FullData$SpeciesCat)))
  
  colnames(AllPossible)<- c('SpeciesCatNames','SpeciesCat')
  
  RamPossibleCats<- unique(RamData$SpeciesCatName)
  
  FaoSpeciesPossibleCats<- unique(FaoSpeciesLevel$SpeciesCatName)
  
  FaoNeiPossibleCats<- unique(FaoNeiLevel$SpeciesCatName)
  
  FaoMarineFishPossibleCats<- unique(FaoMarineFishLevel$SpeciesCatName)
  
  
  # Apply regressions -------------------------------------------------------
  
  Models<- Models[Models!='M7']
  
  for (m in 1:length(Models)) #Apply models to species level fisheries
  {
    
    TempModelName<- Models[m]
    
    eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))
    
    eval(parse(text=paste('TempModel<- RealModels$',TempModelName,sep='')))
    
    ProxyCats<- AssignNearestSpeciesCategory(FaoSpeciesLevel,TempLevel,AllPossible)
    
    Predictions<- predict(TempModel,ProxyCats$Data)
    
    eval(parse(text=paste('FaoSpeciesLevel$',TempModelName,'Prediction<- Predictions',sep='')))    
  }
  
  #   TempLevel<- NeiModelFactorLevels$M6 
  #   
  #   ProxyCats<- AssignNearestSpeciesCategory(FaoNeiLevel,TempLevel,AllPossible)$Data
  #   
  #   Predictions<- predict(NeiModels$M6,ProxyCats) #Apply nei model
  #   
  FaoNeiLevel$M6Prediction<- 999
  
  FaoMarineFishLevel$M7Prediction<- 999
  
  show('Regressions Applied')
  
  # Assign and identify best predicted biomass to stocks  ---------------------------------------
  
  
  HasAllRefs<- ddply(RamData,c('IdOrig'),summarize,HasAllBFM=any(is.na(BvBmsy)==F & is.na(FvFmsy)==F & is.na(MSY)==F))
  
  RamData<- RamData[RamData$IdOrig %in% HasAllRefs$IdOrig[HasAllRefs$HasAllBFM==T],]
  
  # Arg<- ddply(RamData,c('IdOrig'),summarize,HasFinalF=any(is.na(FvFmsy)==F & is.na(BvBmsy)==F & is.na(MSY)==F & Year==2011))
  
  MissingF<- is.na(RamData$FvFmsy) & RamData$Year==BaselineYear 
  
  RamData$FvFmsy[MissingF]<- (RamData$Catch[MissingF]/RamData$MSY[MissingF])/RamData$BvBmsy[MissingF]
  
  #For now, solve for F/Fmsy in BaselineYear, ask Hiveley about this though. Check whether you always have B/Bmsy and MSY and Catch in BaselineYear
  
  if (IncludeNEIs==TRUE)
  {
    PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel,FaoNeiLevel,FaoMarineFishLevel) #Bind all data back together
  }
  if (IncludeNEIs==FALSE)
  {
    PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel) #Bind all data back together
  }
  BiomassColumns<- (grepl('BvBmsy$',colnames(PredictedData)) | grepl('Prediction',colnames(PredictedData))) & grepl('LogBvBmsy',colnames(PredictedData))==F
  
  BioNames<- colnames(PredictedData)[BiomassColumns]
  
  HasBiomass<- rowSums(is.na(PredictedData[,BiomassColumns]))<length(BioNames)
  
  BiomassData<- PredictedData[HasBiomass,] #Only store fisheries that have some form of biomass estimates
  
  MissingData<- PredictedData[HasBiomass==F & PredictedData$Dbase=='FAO',]
  
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
  
  BiomassData$PRMBvBmsy<- BestBio
  
  #   BiomassData$CommName<- as.character((BiomassData$CommName))
  
  BiomassData$SciName<- as.character((BiomassData$SciName))
  
  BiomassData$SpeciesCatName<- as.character(BiomassData$SpeciesCatName)
  
  WhereNeis<- (grepl('nei',BiomassData$CommName) | grepl('spp',BiomassData$SciName)) & grepl('not identified',BiomassData$SpeciesCatName)==F & (BiomassData$Dbase=='FAO') #Find unassessed NEIs
  
  WhereUnidentified<- grepl('not identified',BiomassData$SpeciesCatName)
  
  WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level
  
  BiomassData$IdLevel[WhereNeis]<- 'Neis'
  
  BiomassData$IdLevel[WhereUnidentified]<- 'Unidentified'
  
  BiomassData$IdLevel[WhereSpeciesLevel]<- 'Species'
  
  #   BiomassData<- AssignEconomicData(BiomassData) #Assign price and cost data to each stock
  
  BiomassData$Price<-NA # Price and BvBmsyOpenAccess variable must be created before Analyze fisheries. Will be filled later by Assign EconData
  
  BiomassData$BvBmsyOpenAccess<-NA
  
  BiomassData$RanCatchMSY<- F
  
  show('Results Processed')
  
  # Run First Analisis of Current Status --------------------------------------------------
  
  BiomassData$CatchMSYBvBmsy_LogSd<- NA
  
  GlobalStatus<- AnalyzeFisheries(BiomassData,'Baseline Global Status','Year',min(BiomassData$Year):max(BiomassData$Year),RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  #   RAMStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='RAM',],'RAM Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  # Calculate MSY -----------------------------------------------------------
  
  sigR<- 0
  
  GlobalStatus$BvBmsySD[GlobalStatus$Data$Dbase=='SOFIA']<- 0.1
  
  #   arg<- sample(GlobalStatus$Data$IdOrig,100,replace=F)
  
  CatchMSYresults<- (RunCatchMSY(GlobalStatus$Data,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps))
  
  #   CatchMSYresults<- (RunCatchMSY(GlobalStatus$Data[GlobalStatus$Data$IdOrig %in% arg,],ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps))
  
  #   CatchMSYresults<- (RunCatchMSY(GlobalStatus$Data[GlobalStatus$Data$IdOrig=='10041-FAO-41-44',],ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps))
  
  show("Completed CatchMSY")
  
  CatchMSYPossibleParams<- CatchMSYresults$PossibleParams
  
  MsyData<- CatchMSYresults$MsyData
  
  MsyData$MSY[MsyData$SpeciesCatName==ForageFish]<-MsyData$MSY[MsyData$SpeciesCatName==ForageFish]*0.75 # reduce forage fish MSY by 25%
  
  BiomassData$MSY<- MsyData$MSY #Assign MSY back to BiomassData estimates
  
  BiomassData$FvFmsy[MsyData$RanCatchMSY==T]<- MsyData$FvFmsy[MsyData$RanCatchMSY==T]
  
  BiomassData$BvBmsy[MsyData$RanCatchMSY==T]<- log(MsyData$BvBmsy[MsyData$RanCatchMSY==T])
  
  BiomassData$CatchMSYBvBmsy_LogSd[MsyData$RanCatchMSY==T]<- (MsyData$CatchMSYBvBmsy_LogSd[MsyData$RanCatchMSY==T])
  
  BiomassData$RanCatchMSY[MsyData$RanCatchMSY==T]<- TRUE
  
  BvBmsyOpenAccess<-FindOpenAccess(MsyData,BaselineYear,BOAtol) # find open access equilibrium for each species cat using results in MsyData
  
  BiomassData<- AssignEconomicData(BiomassData,BvBmsyOpenAccess) #Assign price and cost data back to each stock in biomass data
  
  #Run quick diagnostic of CatchMSY results
  pdf(file=paste(FigureFolder,'Catch MSY vs PRM BvBmsy predictions.pdf',sep=''))
  print(xyplot(  CatchMSYBvBmsy ~ BvBmsy | Dbase,data=MsyData,xlab='PRM BvBmsy',ylab='CMSY BvBmsy',panel=function(x,y,...){
    panel.xyplot(x,y,...)
    panel.abline(a=0,b=1,lty=2)
  }))
  dev.off()
  
  
  MsyData$PercentGain<- 100*(MsyData$MSY/MsyData$Catch-1)
  
  #   MsyData$Country[MsyData$Country=='United States of America']<- 'USA'
  
  # Run projection analysis -------------------------------------------------
  
  MsyData$Price<-BiomassData$Price
  
  MsyData$BvBmsyOpenAccess<-BiomassData$BvBmsyOpenAccess
  
  MsyData$Price[is.na(MsyData$Price)]<- mean(MsyData$Price,na.rm=T) #Apply mean price to fisheries with no price
  
  MsyData$k[MsyData$Dbase=='RAM']<- (MsyData$Bmsy/MsyData$BtoKRatio)[MsyData$Dbase=='RAM']
  
  #   MsyData$g[MsyData$Dbase=='RAM']<-4*MsyData$MSY[MsyData$Dbase=='RAM']/MsyData$k[MsyData$Dbase=='RAM']
  
  MsyData$g[MsyData$Dbase=='RAM']<- ((MsyData$MSY*(1/MsyData$BtoKRatio))/MsyData$k)[MsyData$Dbase=='RAM']
  
  MsyData$g[is.na(MsyData$g)]<- mean(MsyData$g,na.rm=T) #FIX THIS XXX Apply mean r to fisheries with no r THIS WAS ASSIGNING ALL RAM STOCKS THE MEAN r VALUE
  
  #   if(IncludeNEIs==TRUE)
  #   {
  #     MsyData<-NeiVersion2(MsyData,Level='All',MinStocks=2)
  #   }
  
  MsyData$CanProject<- is.na(MsyData$MSY)==F & is.na(MsyData$g)==F #Identify disheries that have both MSY and r
  
  save(file=paste(ResultFolder,"MsyData.rdata",sep=""),MsyData)
  
  MsyData$BestModel<- as.character(MsyData$BestModel)
  
  #   if(SubSample>0)
  #   {
  save.image(file=paste(ResultFolder,'Test Results Prior to Projections.rdata',sep=''))
  #   }
  
  ProjectionData<- RunProjection(MsyData[MsyData$CanProject==T,],BaselineYear,NumCPUs,StatusQuoPolicy) #Run projections on MSY data that can be projected
  
  PolicyStorage<- ProjectionData$PolicyStorage
  
  write.csv(file=paste(ResultFolder,'PolicyStorage.csv',sep=''),PolicyStorage)
  
  ProjectionData<- ProjectionData$DataPlus
  
  show("Completed Projections")
  
  if (IncludeNEIs==TRUE)
  {
    NeiData<- NearestNeighborNeis(BiomassData,MsyData,ProjectionData,BaselineYear) #Run Nearest Neighbor NEI analysis
    
    #Put NEI stocks back in the appropriate dataframes, remove stocks still missing data
    
    ProjectionData<- rbind(ProjectionData,NeiData$ProjNeis)
    
    BiomassData<- rbind(BiomassData,NeiData$BiomassNeis)
  }
  
  BiomassData<- BiomassData[BiomassData$BvBmsy!=999 & is.infinite(BiomassData$BvBmsy)==FALSE & is.na(BiomassData$BvBmsy)==F,]
  
  MsyData<- MsyData[is.na(MsyData$MSY)==F,]
  
  ProjectionData<- ProjectionData[ProjectionData$CanProject==T,]
  
#   ProjectionData$Biomass<- (ProjectionData$BvBmsy* (2* ProjectionData$MSY/ProjectionData$r))
  
#   ProjectionData$Biomass<- (ProjectionData$BvBmsy* (ProjectionData$k *ProjectionData$BtoKRatio))
  
  ProjectionData$DiscProfits<- ProjectionData$Profits * (1+Discount)^-(ProjectionData$Year-BaselineYear)
  
  OriginalProjectionData<- ProjectionData
  
  OriginalFullData<- FullData
  
  OriginalMsyData<- MsyData
  
  OriginalBiomassData<- BiomassData
  
  save.image(file=paste(ResultFolder,'Global Fishery Recovery Results.rdata',sep=''))
  
} #Close RunAnalyses If


if (RunAnalyses==F) #Load baseline versions of key dataframes for analysis after complete runs
{
  
  FullData<- OriginalFullData #Complete database, post filtering/cleaning etc
  
  BiomassData<- OriginalBiomassData #Fisheries that have B/Bmsy
  
  MsyData<- OriginalMsyData #Fisheries that have B/Bmsy and MSY
  
  ProjectionData<- OriginalProjectionData #Fisheries that have B/Bmsy, MSY, and we've run the projections
  
}


### Process results and prepare summary tables --------------------------------------------------

# Remove NEIs and forage fish if desired

if (IncludeNEIs==FALSE)  
{
  ProjectionData<- ProjectionData[ProjectionData$IdLevel=='Species',]
}

if (IncludeForageFish==FALSE)  
{
  ProjectionData<- ProjectionData[ProjectionData$SpeciesCatName%in%ForageFish==F,]
}

# Add new "Business as Usual Policies" by combining the results of the respective status quo policies for certain types of stocks, outlined in the function

ProjectionData<-BuildPolicyBAUs(ProjectionData,BaselineYear)

# Calculate fishery upsides on full ProjectionData prior to unlumping stocks

UpsideAllStocks<-FisheriesUpsideV3(ProjectionData,BaselineYear,DenominatorPolicy='Business As Usual Optimistic',
                                   RecoveryThreshold=0.8,LumpedName='Lumped Projection Data',SubsetName='All Stocks')

UpsideOverfishOnly<-FisheriesUpsideV3(ProjectionData,BaselineYear,DenominatorPolicy='Business As Usual Optimistic',
                                      RecoveryThreshold=0.8,LumpedName='Lumped Projection Data',SubsetName='Overfish Only')


# Unlump lumped fisheries and create separate ProjectionData dataframe with unlumped stocks

UnlumpedData<-UnlumpFisheries(ProjectionData,RawData,BaselineYear,YearsBack=0,StitchIds)

UnlumpedProjectionData<-ProjectionData[!grepl('Lumped',ProjectionData$IdOrig),]

UnlumpedProjectionData<-rbind(UnlumpedProjectionData, UnlumpedData)

write.csv(file=paste(ResultFolder,'Unlumped Projection Data.csv',sep=''),UnlumpedProjectionData)

# Distribute benefits of multinational RAM stocks to countries 

UnlumpedProjectionData<-DivyMultinational(Data=UnlumpedProjectionData,RawData,BaselineYear,YearsBack=4)

# Calculate fishery upsides from UnlumpedProjectionData

UnlumpedUpsideAllStocks<-FisheriesUpsideV3(UnlumpedProjectionData,BaselineYear,DenominatorPolicy='Business As Usual Optimistic',
                                           RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='All Stocks')

write.csv(file=paste(ResultFolder,'Unlumped Country Upsides All Stocks.csv',sep=''),UnlumpedUpsideAllStocks$CountryUpsides)

UnlumpedUpsideOverfishOnly<-FisheriesUpsideV3(UnlumpedProjectionData,BaselineYear,DenominatorPolicy='Business As Usual Optimistic',
                                              RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='Overfish Only')

write.csv(file=paste(ResultFolder,'Unlumped Country Upsides Overfish Only.csv',sep=''),UnlumpedUpsideOverfishOnly$CountryUpsides)


# Calculate global upsides relative to Trevor denominator

GlobalUpsideTrevor<-TrevorDenominator(GlobalUpsideOverF=UnlumpedUpsideOverfishOnly$GlobalUpside,GlobalUpsideAll=UnlumpedUpsideAllStocks$GlobalUpside,Discount) 


### Plot figures for paper and diagnostics  --------------------------------------------------

## ******* INSERT CODY'S PLOT SCRIPTS *************** 

# FIGURE 3 - Recovery Trajectories

RecoveryTrends<-RecoveryTrend(ProjectionData=ProjectionData,RecoveryThreshold=0.8,OnlyOverfish=FALSE,StartYear=1980)

# Global Kobe Plot

MakeKobePlot(ProjectionData,BaselineYear,'Global Kobe Plot.pdf')

### Diagnostics and Summary Tables -----------------------------------------------------------------------------


# Projection validation data for Chris

ProjectionValidationData<-ProjectionValidation(ProjectionData,BaselineYear)

# Produce Country Summary table and Stock List (returns list with Country summaries and Stock list, writes csvs of both)

PercentCoverage<-StockAndCountrySummary(UnlumpedProjectionData,ProjectionData,StitchIds,BaselineYear)

# Summarize current status by ISSCAAP and FAO Region

StatusByRegionAndISSCAAP<-RegionFaoAndISSCAAPSummary(ProjectionData,BaselineYear)

# Evaluate cost:revenue ratios of full Projection data before CountriesToRun analysis

CostRevenues<-CostRevCheck(ProjectionData,RawData,BaselineYear)

# Plot historical status of RAM and unassessed stocks

# StatusISSCAAP<-StatusPlots(FullData,BiomassData,BaselineYear,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)

# Test C parameter sensitivity to BOA level

# SensitivityCParam<-CParamSensitivity(Data=MsyData,BaselineYear,beta)

# Plot histograms of steady state status for chosen subset of projection data

# SteadyStateStatus(Data=ProjectionData,Subset=c('Steady State Status Analyzed Projection Data.pdf')) # histograms of b by policy for complete ProjectionData

# Save final image

save.image(file=paste(ResultFolder,'Global Fishery Recovery Complete Results.rdata',sep=''))

# Publish in Science ------------------------------------------------------











### Original Run Analyses== FALSE and CountriesToRun Loop---------------------------------------------------------------------



# # BiomassData<- join(BiomassData,MsyData[,c('IdOrig','CatchMSYBvBmsy_LogSd')],by='IdOrig',match='first')
# 
# # Assign labels and prepare results for analysis --------------------------
# 
# 
# WhereNeis<- (grepl('nei',FullData$CommName) | grepl('spp',FullData$SciName)) & grepl('not identified',FullData$SpeciesCatName)==F #Find unassessed NEIs
# 
# WhereUnidentified<- grepl('not identified',FullData$SpeciesCatName)
# 
# WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level
# 
# FullData$IdLevel[WhereNeis]<- 'Neis'
# 
# FullData$IdLevel[WhereUnidentified]<- 'Unidentified'
# 
# FullData$IdLevel[WhereSpeciesLevel]<- 'Species'
# 
# Policies<- unique(ProjectionData$Policy)
# 
# ProjectionData$Biomass<- (ProjectionData$BvBmsy* (2* ProjectionData$MSY/ProjectionData$r))
# 
# SteadyStateStatus(Data=ProjectionData,Subset=c('Steady State Status Complete Projection Data.pdf')) # histograms of b by policy for complete ProjectionData
# 
# if (IncludeUnderfished==FALSE) #Remove projections for underfished stocks if desired
# {
#   
#   CurrentlyOverfished<- ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & ProjectionData$BvBmsy<1]
#   
#   ProjectionData<- ProjectionData[ProjectionData$IdOrig %in% CurrentlyOverfished ,]
#   
# }
# 
# if (IncludeNEIs==FALSE) #Remove NEIs if desired 
# {
#   ProjectionData<- ProjectionData[ProjectionData$IdLevel=='Species',]
#   
#   BiomassData<- BiomassData[BiomassData$IdLevel=='Species',]
#   
#   MsyData<- MsyData[MsyData$IdLevel=='Species',]
#   
# }
# 
# if (IncludeForageFish==FALSE) #Remove forage fish species if desired 
# {
#   ProjectionData<- ProjectionData[ProjectionData$SpeciesCatName%in%ForageFish==F,]
#   
#   BiomassData<- BiomassData[BiomassData$SpeciesCatName%in%ForageFish==F,]
# }
# 
# # evaluate cost:revenue ratios of full Projection data before CountriesToRun analysis
# CostRevenues<-CostRevCheck(ProjectionData,RawData,BaselineYear)
# 
# # Plot historical status of RAM and unassessed stocks
# StatusISSCAAP<-StatusPlots(FullData,BiomassData,BaselineYear,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
# 
# # Test C parameter sensitivity to BOA level
# SensitivityCParam<-CParamSensitivity(Data=MsyData,BaselineYear,beta)
# 
# # Plot histograms of steady state status for chosen subset of projection data
# SteadyStateStatus(Data=ProjectionData,Subset=c('Steady State Status Analyzed Projection Data.pdf')) # histograms of b by policy for complete ProjectionData
# 
# #Reframe 0 profits/catch to prevent infinites and NANs in 
# 
# ProjectionData$Profits[ProjectionData$Profits<0]<- 0.0001 #Reframe 0 profits/catch to prevent infinites and NANs in 
# 
# ProjectionData$Catch[ProjectionData$Catch==0]<- 0.0001
# 
# 
# 
# # Prepare summary tables --------------------------------------------------
# 
# 
# # Build empty version of Chris's database to be populated with FinalYear results. Make separate percent change and absolute change tables then use rbind later
# # 3 data frames to be merged at end.
# # 1) Policy results relative to baseline values. 2) Policy results relative to Status Quo Policy in Final Year 3) Cumulative Policy results relative to Status Quo
# 
# ResultMetricsBaselineNames<-c("PercChangeTotalProfits","PercChangeTotalCatch","PercChangeTotalBiomass","PercChangeMedianProfits","PercChangeMedianCatch",
#                               "PercChangeMedianBiomass", "AbsChangeTotalProfits","AbsChangeTotalCatch","AbsChangeTotalBiomass","AbsChangeMedianProfits","AbsChangeMedianCatch",
#                               "AbsChangeMedianBiomass")
# 
# ResultMetricsSQFinalNames<-c("PercChangeFromSQTotalBiomass","PercChangeFromSQTotalProfits","PercChangeFromSQTotalCatch","PercChangeFromSQMedianBiomass", "PercChangeFromSQMedianProfits",
#                              "PercChangeFromSQMedianCatch","AbsChangeFromSQTotalBiomass","AbsChangeFromSQTotalProfits","AbsChangeFromSQTotalCatch", "AbsChangeFromSQMedianBiomass",
#                              "AbsChangeFromSQMedianProfits","AbsChangeFromSQMedianCatch")
# 
# ResultMetricsSQCumFinalNames<-c("PercChangeFromSQCum_NPV","PercChangeFromSQCum_Food","PercChangeFromSQCum_Fish","PercChangeFromSQCumMedianProfits","PercChangeFromSQCumMedianCatch","PercChangeFromSQMedianBiomass",
#                                 "AbsChangeFromSQCumProfits","AbsChangeFromSQCumFood","AbsChangeFromSQCumFish","AbsChangeFromSQCumMedianProfits","AbsChangeFromSQCumMedianCatch","AbsChangeFromSQCumMedianBiomass")
# 
# ResultMetricsBaselineTable<-data.frame(matrix(NA,nrow=length(CountriesToRun),ncol=length(ResultMetricsBaselineNames)+1))
# colnames(ResultMetricsBaselineTable)<-c('Region',ResultMetricsBaselineNames)
# 
# ResultMetricsSQFinalTable<-data.frame(matrix(NA,nrow=length(CountriesToRun),ncol=length(ResultMetricsSQFinalNames)+1))
# colnames(ResultMetricsSQFinalTable)<-c('Region',ResultMetricsSQFinalNames)
# 
# ResultMetricsSQCumFinalTable<-data.frame(matrix(NA,nrow=length(CountriesToRun),ncol=length(ResultMetricsSQCumFinalNames)+1))
# colnames(ResultMetricsSQCumFinalTable)<-c('Region',ResultMetricsSQCumFinalNames)
# 
# 
# # CountriesToRun<-c("Global","USA","China","Indonesia","Philippines","Peru","Chile","Mexico","Japan","Myanmar","Viet Nam","EU","Parties to the Nauru Agreement",EUCountries)
# 
# # CountriesToRun<- CountriesToRun[!grepl('Grenadines',(CountriesToRun))]
# 
# #  BiomassData$RanCatchMSY<- TRUE
# 
# if (CapRefs==T)
# {
#   BiomassData$BvBmsy[BiomassData$BvBmsy>1.9]<- 1.9
#   BiomassData$FvFmsy[BiomassData$FvFmsy>1.9]<- 1.9
#   
#   MsyData$BvBmsy[MsyData$BvBmsy>1.9]<- 1.9
#   MsyData$FvFmsy[MsyData$FvFmsy>1.9]<- 1.9
#   
#   ProjectionData$BvBmsy[ProjectionData$BvBmsy>1.9]<- 1.9
#   ProjectionData$FvFmsy[ProjectionData$FvFmsy>1.9]<- 1.9
#   
# }
# 
# if(CountriesToRun=='All')
# {
#   AllCountries<-unique(ProjectionData$Country)
#   CountriesToRun<-c('Global','EU','Asia','Parties to the Nauru Agreement',AllCountries) 
# }
# 
# for (c in 1:length(CountriesToRun)) # Run analyses on each desired region
# {
#   
#   show(CountriesToRun[c])
#   if (CountriesToRun[c]=='Global'){
#     FullData_CountryLocater<-  FullData$Country %in% unique(FullData$Country)
#     Biomass_CountryLocater<-  BiomassData$Country %in% unique(BiomassData$Country)
#     Proj_CountryLocater<- ProjectionData$Country %in% unique(ProjectionData$Country)
#     Nei_CountryLocater<-FaoNeiLevel$Country %in% unique(FaoNeiLevel$Country)
#     
#   } else if (CountriesToRun[c]=='Parties to the Nauru Agreement')
#   {
#     Biomass_CountryLocater<- BiomassData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
#     Proj_CountryLocater<- ProjectionData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
#     FullData_CountryLocater<- FullData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
#     Nei_CountryLocater<-FaoNeiLevel$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
#     
#   } else if (CountriesToRun[c]=='EU')
#   {
#     
#     EuStocks<- Spec_Region_RAM$assessid[Spec_Region_RAM$region=='European Union']
#     
#     
#     Biomass_CountryLocater<- BiomassData$Country %in% EUCountries | BiomassData$IdOrig %in% EuStocks
#     Proj_CountryLocater<- ProjectionData$Country %in% EUCountries | ProjectionData$IdOrig %in% EuStocks
#     FullData_CountryLocater<- FullData$Country %in% EUCountries | FullData$IdOrig %in% EuStocks
#     Nei_CountryLocater<-FaoNeiLevel$Country %in% EUCountries | FaoNeiLevel$IdOrig %in% EuStocks
#     
#   } else if (CountriesToRun[c]=='China')
#   {
#     Biomass_CountryLocater<- BiomassData$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
#     Proj_CountryLocater<- ProjectionData$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
#     FullData_CountryLocater<- FullData$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
#     Nei_CountryLocater<-FaoNeiLevel$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
#     
#   } else if (CountriesToRun[c]=='Asia')
#   {
#     Biomass_CountryLocater<- BiomassData$Country %in% AsianCountries
#     Proj_CountryLocater<- ProjectionData$Country %in% AsianCountries
#     FullData_CountryLocater<- FullData$Country %in% AsianCountries
#     Nei_CountryLocater<-FaoNeiLevel$Country %in% AsianCountries
#     
#   } else
#   {
#     Biomass_CountryLocater<- BiomassData$Country==CountriesToRun[c] 
#     Proj_CountryLocater<- ProjectionData$Country==CountriesToRun[c] 
#     FullData_CountryLocater<- FullData$Country==CountriesToRun[c] 
#     Nei_CountryLocater<-FaoNeiLevel$Country==CountriesToRun[c]
#     
#   }
#   
#   NumStocks<- length(unique(BiomassData$IdOrig[Biomass_CountryLocater]))
#   if(sum(Biomass_CountryLocater,na.rm=T)>0 & sum(Proj_CountryLocater,na.rm=T)>0 & NumStocks>5)
#   {
#     
#     # Analyze Current Status/Kobe Plot Trends  ----------------------------------------------------------
#     
#     BiomassStatus<- AnalyzeFisheries(BiomassData[Biomass_CountryLocater,],paste(CountriesToRun[c],' Status',sep=''),'Year',1990:BaselineYear,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
#     
#     if (CapRefs==T)
#     {
#       BiomassStatus$Data$BvBmsy[BiomassStatus$Data$BvBmsy>1.9]<- 1.9
#     }
#     
#     if(CountriesToRun[c]=="Global" & SaveRDS==TRUE) # For updating Shiny Kobe Plot Data
#     {
#       saveRDS(BiomassStatus$Data,"BetaApp/data/KobeAppData.rds")
#     } 
#     
#     if (BiomassStatus$CatchStats$Catch$NumberOfStocks>5)
#     {
#       MakeKobePlot(BiomassStatus$Data,BaselineYear,paste(paste(CountriesToRun[c],' Kobe Plot',sep='')))
#     }
#     # Analyze Projections -----------------------------------------------------
#     
#     TempProjectionData<- ProjectionData[Proj_CountryLocater,]
#     
#     TempProjectionData$DiscProfits<- TempProjectionData$Profits * (1+Discount)^-(TempProjectionData$Year-BaselineYear)
#     
#     if(CountriesToRun[c]=='Global')
#     {
#       FisheriesUpside<-FisheriesUpside(TempProjectionData=TempProjectionData)
#     }
#     
#     #Calculate baseline metrics values 
#     
#     Baseline<- ddply(subset(TempProjectionData,Year==BaselineYear & Policy=='Historic'),c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
#                      TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
#                      MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),NumberOfStocks=length(unique(IdOrig))
#                      ,DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
#     
#     BaselineMarker<- as.data.frame(matrix(NA,nrow=length(unique(TempProjectionData$Policy))-1,ncol=dim(Baseline)[2]))
#     
#     colnames(BaselineMarker)<- colnames(Baseline)
#     
#     BaselineMarker[,c(1,3:dim(Baseline)[2])]<- Baseline[,c(1,3:dim(Baseline)[2])]
#     
#     BaselineMarker$Policy<- Policies[Policies!='Historic']
#     
#     #Analyze time trends in metrics
#     TimeTrend<- ddply(TempProjectionData,c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
#                       TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),
#                       NumberOfStocks=length(unique(IdOrig)),DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
#     
#     TimeTrend<- rbind(TimeTrend,BaselineMarker)
#     
#     YearOrder<- order(TimeTrend$Year)
#     
#     TimeTrend<- TimeTrend[YearOrder,]
#     
#     # Calculate Upside Metrics -----------------------------------------------------
#     
#     
#     TimeTrend$PercChangeTotalProfits<-100*(TimeTrend$TotalProfits/Baseline$TotalProfits-1) #Percent change in total profits from current
#     
#     TimeTrend$PercChangeTotalCatch<-100*(TimeTrend$TotalCatch/Baseline$TotalCatch-1) #Percent change in total catch from current
#     
#     TimeTrend$PercChangeTotalBiomass<-100*(TimeTrend$TotalBiomass/Baseline$TotalBiomass-1) #Percent change in total biomass from current
#     
#     TimeTrend$PercChangeMedianProfits<-100*(TimeTrend$MedianProfits/Baseline$MedianProfits-1) #Percent change in median profits from current
#     
#     TimeTrend$PercChangeMedianCatch<-100*(TimeTrend$MedianCatch/Baseline$MedianCatch-1) #Percent change in median catch from current
#     
#     TimeTrend$PercChangeMedianBiomass<-100*(TimeTrend$MedianBvBmsy/Baseline$MedianBvBmsy-1) #Percent change in median B/Bmsy from current
#     
#     TimeTrend$AbsChangeTotalProfits<-TimeTrend$TotalProfits-Baseline$TotalProfits #absolute change in total profits from current
#     
#     TimeTrend$AbsChangeTotalCatch<-TimeTrend$TotalCatch-Baseline$TotalCatch #Percent change in total catch from current
#     
#     TimeTrend$AbsChangeTotalBiomass<-TimeTrend$TotalBiomass-Baseline$TotalBiomass #Percent change in total biomass from current
#     
#     TimeTrend$AbsChangeMedianProfits<-TimeTrend$MedianProfits-Baseline$MedianProfits #Absolute change in median profits from current
#     
#     TimeTrend$AbsChangeMedianCatch<-TimeTrend$MedianCatch-Baseline$MedianCatch #Percent change in median catch from current
#     
#     TimeTrend$AbsChangeMedianBiomass<-TimeTrend$MedianBvBmsy-Baseline$MedianBvBmsy #Percent change in median B/Bmsy from current
#     
#     
#     # Calculate cumulative changes in metrics ------ 
#     
#     Cumulatives<- ddply(TimeTrend[TimeTrend$Year>BaselineYear,],c('Policy'),summarize,NPV=sum(DiscProfits,na.rm=T),Food=sum(TotalCatch,na.rm=T),Fish=sum(TotalBiomass,na.rm=T),
#                         MedianProfits=sum(MedianProfits,na.rm=T),MedianCatch=sum(MedianCatch,na.rm=T),MedianBiomass=sum(MedianBvBmsy,na.rm=T))
#     
#     CumeNumbers<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
#     
#     CumeNumbersAbs<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
#     
#     CumeNumbers<- 100*(t(t(CumeNumbers)/CumeNumbers[which(Cumulatives$Policy=='SQ'),])-1) # Calculate % Change cumulative metrics
#     
#     CumeNumbersAbs<- t(t(CumeNumbersAbs)-CumeNumbersAbs[which(Cumulatives$Policy=='SQ'),]) # Calculate absolute change in cumulative metrics  
#     
#     Cumulatives[,2:dim(Cumulatives)[2]]<- CumeNumbers   
#     
#     Cumulatives[,8:13]<-CumeNumbersAbs
#     
#     colnames(Cumulatives)[8:13]<-paste("Abs",colnames(CumeNumbersAbs),sep="")
#     
#     Cumulatives[,colnames(Cumulatives)=="AbsMedianBiomass"]<-Cumulatives$AbsMedianBiomass/(ProjectionTime)
#     
#     Cumulatives<- Cumulatives[Cumulatives$Policy!='SQ' & Cumulatives$Policy!='Historic',]
#     
#     Cumulatives$Country<-CountriesToRun[c]
#     
#     if(c==1){CumulativesFinal<-Cumulatives} 
#     if(c>1){CumulativesFinal<-rbind(CumulativesFinal,Cumulatives)}
#     
#     # Calculate  metrics in final year -----------------------------------------------------
#     
#     FinalYear<- TimeTrend[TimeTrend$Year==max(TimeTrend$Year),]  
#     
#     FinalYear$AbsChangeFromSQTotalProfits<- FinalYear$TotalProfits-FinalYear$TotalProfits[FinalYear$Policy=='SQ'] # Absolute change in total profits relative to BAU
#     
#     FinalYear$AbsChangeFromSQTotalCatch<- FinalYear$TotalCatch-FinalYear$TotalCatch[FinalYear$Policy=='SQ'] # Absolute change in total catch relative to BAU
#     
#     FinalYear$AbsChangeFromSQTotalBiomass<- FinalYear$TotalBiomass-FinalYear$TotalBiomass[FinalYear$Policy=='SQ'] # Absolute change in total biomass relative to BAU
#     
#     FinalYear$PercChangeFromSQTotalProfits<- 100*(FinalYear$TotalProfits/FinalYear$TotalProfits[FinalYear$Policy=='SQ']-1) # Percent change in total profits relative to BAU
#     
#     FinalYear$PercChangeFromSQTotalCatch<- 100*(FinalYear$TotalCatch/FinalYear$TotalCatch[FinalYear$Policy=='SQ']-1) # Percent change in total catch relative to BAU
#     
#     FinalYear$PercChangeFromSQTotalBiomass<- 100*(FinalYear$TotalBiomass/FinalYear$TotalBiomass[FinalYear$Policy=='SQ']-1) # Percent change in total biomass relative to BAU
#     
#     FinalYear$PercChangeFromSQMedianProfits<- 100*(FinalYear$MedianProfits/FinalYear$MedianProfits[FinalYear$Policy=='SQ']-1) # Percent change in median profits relative to BAU
#     
#     FinalYear$PercChangeFromSQMedianCatch<- 100*(FinalYear$MedianCatch/FinalYear$MedianCatch[FinalYear$Policy=='SQ']-1) # Percent change in median catch relative to BAU
#     
#     FinalYear$PercChangeFromSQMedianBiomass<- 100*(FinalYear$MedianBvBmsy/FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ']-1) # Percent change in median B/Bmsy relative to BAU
#     
#     FinalYear$AbsChangeFromSQMedianProfits<- FinalYear$MedianProfits-FinalYear$MedianProfits[FinalYear$Policy=='SQ']  # Absolute change in median profits relative to BAU
#     
#     FinalYear$AbsChangeFromSQMedianCatch<- FinalYear$MedianCatch-FinalYear$MedianCatch[FinalYear$Policy=='SQ'] # Absolute change in median catch relative to BAU
#     
#     FinalYear$AbsChangeFromSQMedianBiomass<- FinalYear$MedianBvBmsy-FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ'] # Absolute change in median B/Bmsy relative to BAU
#     
#     FinalYear$Country<-CountriesToRun[c]
#     
#     if(c==1){FinalYearFinal<-FinalYear} 
#     if(c>1){FinalYearFinal<-rbind(FinalYearFinal,FinalYear)}
#     
#     # Populate summary table of results  -----------------------------------------------------
#     
#     PercChangeFromSQTotalVars<-c("PercChangeFromSQTotalBiomass","PercChangeFromSQTotalProfits",'PercChangeFromSQTotalCatch')
#     
#     PercChangeFromSQMedianVars<-c("PercChangeFromSQMedianBiomass","PercChangeFromSQMedianProfits","PercChangeFromSQMedianCatch")
#     
#     AbsChangeFromSQTotalVars<-c("AbsChangeFromSQTotalBiomass","AbsChangeFromSQTotalProfits","AbsChangeFromSQTotalCatch")
#     
#     AbsChangeFromSQMedianVars<-c("AbsChangeFromSQMedianBiomass","AbsChangeFromSQMedianProfits","AbsChangeFromSQMedianCatch")
#     
#     ResultMetricsBaselineTable[c,c('Region')]<-CountriesToRun[c]
#     
#     ResultMetricsSQFinalTable[c,c('Region')]<-CountriesToRun[c]
#     
#     ResultMetricsSQCumFinalTable[c,c('Region')]<-CountriesToRun[c]
#     
#     ResultMetricsBaselineTable[c,ResultMetricsBaselineNames]<-FinalYear[FinalYear$Policy=="CatchShare",ResultMetricsBaselineNames]
#     
#     ResultMetricsSQFinalTable[c,PercChangeFromSQTotalVars]<-FinalYear[FinalYear$Policy=="CatchShare",PercChangeFromSQTotalVars]
#     
#     ResultMetricsSQFinalTable[c,PercChangeFromSQMedianVars]<-FinalYear[FinalYear$Policy=="CatchShare",PercChangeFromSQMedianVars]
#     
#     ResultMetricsSQFinalTable[c,AbsChangeFromSQTotalVars]<-FinalYear[FinalYear$Policy=="CatchShare",AbsChangeFromSQTotalVars]
#     
#     ResultMetricsSQFinalTable[c,AbsChangeFromSQMedianVars]<-FinalYear[FinalYear$Policy=="CatchShare",AbsChangeFromSQMedianVars]
#     
#     ResultMetricsSQCumFinalTable[c,ResultMetricsSQCumFinalNames]<-Cumulatives[Cumulatives$Policy=="CatchShare",c("NPV","Food",
#                                                                                                                  "Fish","MedianProfits","MedianCatch","MedianBiomass","AbsNPV","AbsFood","AbsFish","AbsMedianProfits","AbsMedianCatch","AbsMedianBiomass")]
#     
#     PercChangeFromSQMedianVars<-c("PercChangeFromSQMedianBiomass","PercChangeFromSQMedianProfits","PercChangeFromSQMedianCatch")
#     
#     AbsChangeFromSQTotalVars<-c("AbsChangeFromSQTotalBiomass","AbsChangeFromSQTotalProfits","AbsChangeFromSQTotalCatch")
#     
#     AbsChangeFromSQMedianVars<-c("AbsChangeFromSQMedianBiomass","AbsChangeFromSQMedianProfits","AbsChangeFromSQMedianCatch")
#     
#     ResultMetricsBaselineTable[c,c('Region')]<-CountriesToRun[c]
#     
#     ResultMetricsSQFinalTable[c,c('Region')]<-CountriesToRun[c]
#     
#     ResultMetricsSQCumFinalTable[c,c('Region')]<-CountriesToRun[c]
#     
#     ResultMetricsBaselineTable[c,ResultMetricsBaselineNames]<-FinalYear[FinalYear$Policy=="CatchShare",ResultMetricsBaselineNames]
#     
#     ResultMetricsSQFinalTable[c,PercChangeFromSQTotalVars]<-FinalYear[FinalYear$Policy=="CatchShare",PercChangeFromSQTotalVars]
#     
#     ResultMetricsSQFinalTable[c,PercChangeFromSQMedianVars]<-FinalYear[FinalYear$Policy=="CatchShare",PercChangeFromSQMedianVars]
#     
#     ResultMetricsSQFinalTable[c,AbsChangeFromSQTotalVars]<-FinalYear[FinalYear$Policy=="CatchShare",AbsChangeFromSQTotalVars]
#     
#     ResultMetricsSQFinalTable[c,AbsChangeFromSQMedianVars]<-FinalYear[FinalYear$Policy=="CatchShare",AbsChangeFromSQMedianVars]
#     
#     ResultMetricsSQCumFinalTable[c,ResultMetricsSQCumFinalNames]<-Cumulatives[Cumulatives$Policy=="CatchShare",
#                                                                               c("NPV","Food","Fish","MedianProfits","MedianCatch","MedianBiomass","AbsNPV","AbsFood","AbsFish","AbsMedianProfits","AbsMedianCatch","AbsMedianBiomass")]
#     
#     ResultMetricsTable<-merge(ResultMetricsBaselineTable,ResultMetricsSQFinalTable,by='Region')
#     
#     ResultMetricsTable<-merge(ResultMetricsTable,ResultMetricsSQCumFinalTable,by = "Region")
#     
#     # Calculate data coverage statistics --------------------------------------------
#     
#     # Tallies of catch and fisheries in  FullData
#     FullTallies<- ddply(FullData[FullData_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='FullData')
#     
#     # Tallies of catch and fisheries in  BiomassData
#     
#     BiomassTallies<- ddply(BiomassData[Biomass_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='BiomassData')
#     
#     # Tallies of catch and fisheries in ProjectionData
#     
#     ProjectionTallies<- ddply(ProjectionData[Proj_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='ProjectionData')
#     
#     
#     Tallies<- rbind(FullTallies,BiomassTallies,ProjectionTallies)
#     
#     # Plot data coverage statistics
#     
#     pdf(file=paste(FigureFolder,CountriesToRun[c],' Database Analysis.pdf',sep='')) 
#     
#     print(dotplot(Fisheries ~ Dbase | Source,data=Tallies,subset=Year==BaselineYear,cex=2)  )  
#     
#     print(dotplot(Catch ~ Dbase | Source,data=Tallies,subset=Year==BaselineYear,cex=2)   ) 
#     
#     print(xyplot(Catch ~ Year | Source,group=Dbase,data=Tallies,subset=Year>=1950 & Year<= BaselineYear,cex=1,type='l',lwd=3,auto.key=T))  
#     
#     print(xyplot((Fisheries) ~ Year | Source,group=Dbase,data=Tallies,subset=Year>=1950 & Year<= BaselineYear,cex=1,type='l',lwd=3,auto.key=T))   
#     
#     dev.off()
#     
#     
#     # Save .csvs of results --------------------------------------------------
#     
#     write.csv(file=paste(ResultFolder,CountriesToRun[c],' Policy Projections.csv',sep=''),TimeTrend)
#     
#     write.csv(file=paste(ResultFolder,CountriesToRun[c],' Baseline.csv',sep=''),Baseline)
#     
#     write.csv(file=paste(ResultFolder,CountriesToRun[c],' Biomass Status.csv',sep=''),BiomassStatus$Data)
#     
#     write.csv(file=paste(ResultFolder,CountriesToRun[c],' Raw Projection Data.csv',sep=''),TempProjectionData)
#     
#     write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Current Year Data.csv',sep=''),FinalYear)
#     
#     write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Business As Usual Data.csv',sep=''),Cumulatives)    
#     
#     
#     # Plot results --------------------------------------------------
#     
#     pdf(file=paste(FigureFolder,CountriesToRun[c],' Trajectories.pdf',sep='')) 
#     
#     print(barchart(NPV ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in NPV from Business as Usual'))
#     
#     print(barchart(Food ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in Total Catch from Business as Usual'))
#     
#     print(barchart(Fish ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in Total Fish from Business as Usual'))
#     
#     print( barchart(PercChangeTotalProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Profits'))
#     
#     print(barchart(PercChangeTotalCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Catch'))
#     
#     print(a<- barchart(PercChangeTotalBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Fish'))
#     
#     print(barchart(PercChangeMedianProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Profits'))
#     
#     print(barchart(PercChangeMedianCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Catch'))
#     
#     print(barchart(PercChangeMedianBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Fish'))
#     
#     print(barchart(PercChangeFromSQTotalProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Total Profits'))
#     
#     print(barchart(PercChangeFromSQTotalCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Total Catch'))
#     
#     print(barchart(PercChangeFromSQTotalBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Total Fish'))
#     
#     print(barchart(PercChangeFromSQMedianProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Profits'))
#     
#     print(barchart(PercChangeFromSQMedianCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Catch'))
#     
#     print(barchart(PercChangeFromSQMedianBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Fish'))
#     
#     print(xyplot( PercChangeTotalProfits ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,ylab='% Change from Current Total Profits',type='l',lwd=4,auto.key=T,aspect='fill'))
#     
#     print(xyplot( PercChangeTotalCatch ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Total Catch'))
#     
#     print(xyplot( PercChangeTotalBiomass ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Total Fish'))
#     
#     print(xyplot( PercChangeMedianProfits ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,ylab='% Change from Current Median Profits',type='l',lwd=4,auto.key=T,aspect='fill'))
#     
#     print(xyplot( PercChangeMedianCatch ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Median Catch'))
#     
#     print(xyplot( MedianBvBmsy ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='Median B/Bmsy'))
#     
#     print(xyplot( MedianFvFmsy ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='Median F/Fmsy'))
#     
#     dev.off()
#     
#   } #Close if
# } #Close Country Trajectory Analysis 
# 
# if(SaveRDS==TRUE)
# {
#   saveRDS(CumulativesFinal,"UpsideApp/data/UpsideAppData.rds") # For saving cumulatives data for UpsideApp
#   saveRDS(FinalYearFinal,"UpsideApp/data/UpsideAppFinalYrData.rds") # For saving final year data for UpsideApp
# }
# 
# # upside plot of CountriesToRun
# # UpsidePlot(CumulativesFinal,FinalYearFinal,Policy='CatchShare',XVar='PercChangeTotalBiomass',YVar='NPV',DotSize='Food',Limit=300)
# 
# write.csv(file=paste(ResultFolder,"Percent Upside From Business As Usual Data.csv",sep=''),CumulativesFinal)
# write.csv(file=paste(ResultFolder,"Percent Upside From Business As Usual Data Final Year.csv",sep=''),FinalYearFinal)
# 
# # write.csv(file=paste(ResultFolder,'Chris Summary Table Data.csv',sep=''),ResultMetricsTable)
# 
# # Scale and Analyze Results -----------------------------------------------
# 
# # Figures for Paper
# 
# if(PlotFinalFigures==TRUE)
# {
#   # Figure 1: Upside plot for all countries
#   FigureOne<-Figure1(CumulativesFinal,FinalYearFinal,Policy='Opt',Limit=200)
#   
#   # Figure 2: Upside plots showing policy trade-offs for select countries
#   FigureTwo<-Figure2(CumulativesFinal, FinalYearFinal, Countries=c('Global','EU','USA','China','Indonesia','Japan'),Limit=100)
#   
#   # Figure 3: Upside from Opt and Catch Shares relative to SQ for top fishing nations
#   Fig3Countries<-c('Japan','Morocco','Republic of Korea','USA','Russian Federation','Global','Spain','Peru','Iceland','Thailand','Mexico','India',
#                    'China','Philippines','Taiwan Province of China','Indonesia','Norway','Malaysia','Multinational')
#   
#   FigureThree<-Figure3(CumulativesFinal,Countries=Fig3Countries)
# }
# 
# PriceBarPlot(SpeciesCategoriesToOmit,FigureFolder)
# 
# # Publish in Science ------------------------------------------------------
# 
# 
# 
# 
