
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
  
  NoBmsy<- is.na(ProjectionData$Bmsy)
    
#   ProjectionData$k[NoBmsy]<- ((ProjectionData$MSY/ProjectionData$g)*(1/ProjectionData$BtoKRatio))[NoBmsy]
#   
#   ProjectionData$Bmsy[NoBmsy]<- (ProjectionData$MSY/ProjectionData$g)[NoBmsy]
#   
#   ProjectionData$Biomass[NoBmsy]<- (ProjectionData$BvBmsy*ProjectionData$Bmsy)[NoBmsy]
  
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

UpsideAllStocks<-FisheriesUpsideV3(ProjectionData,BaselineYear,DenominatorPolicy='Current Management',
                                   RecoveryThreshold=0.8,LumpedName='Lumped Projection Data',SubsetName='All Stocks')

UpsideOverfishOnly<-FisheriesUpsideV3(ProjectionData,BaselineYear,DenominatorPolicy='Current Management',
                                      RecoveryThreshold=0.8,LumpedName='Lumped Projection Data',SubsetName='Overfish Only')

# Unlump lumped fisheries and create separate ProjectionData dataframe with unlumped stocks

UnlumpedData<-DivyUpSofia(ProjectionData,RawData) # Divide up SOFIA multinational

UnlumpedData<-DivyMultinational(Data=UnlumpedData,RawData,BaselineYear,YearsBack=4) # Divide up RAM multinational

UnlumpedData<-UnlumpFisheries(UnlumpedData,RawData,BaselineYear,YearsBack=4,StitchIds) # Divide up lumped FAO fisheries

UnlumpedProjectionData<-UnlumpedData

rm(UnlumpedData)

write.csv(file=paste(ResultFolder,'Unlumped Projection Data.csv',sep=''),UnlumpedProjectionData)

# Calculate fishery upsides from UnlumpedProjectionData

UnlumpedUpsideAllStocks<-FisheriesUpsideV3(UnlumpedProjectionData,BaselineYear,DenominatorPolicy='Current Management',
                                           RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='All Stocks')

write.csv(file=paste(ResultFolder,'Unlumped Country Upsides All Stocks.csv',sep=''),UnlumpedUpsideAllStocks$CountryUpsides)

UnlumpedUpsideOverfishOnly<-FisheriesUpsideV3(UnlumpedProjectionData,BaselineYear,DenominatorPolicy='Current Management',
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



