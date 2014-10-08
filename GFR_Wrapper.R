
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
    
    source('Database_Build.R') #Build Tyler's database
    
    show('Done building raw database')
    
    RawData<- fulldata
    
    RawData$FvFmsy<- RawData$UvUmsytouse
    
    FullData<- fulldata
    
    if (SubSample>0)
    {
     
      FaoIds<- unique(FullData$IdOrig[FullData$Dbase=='FAO'])
      
      SampleIds<- sample(FaoIds,SubSample*length(FaoIds),replace=FALSE)
        # # # 
        FullData<-  FullData[! FullData[,IdVar] %in% SampleIds,]
    }
    FullData$FvFmsy<- FullData$UvUmsytouse
    
    rm(fulldata)
    
    CleanedData<- MaidService(FullData,OverlapMode,BaselineYear) #Filter out unusable stocks, prepare data for regression and use
    
    DroppedStocks<- CleanedData$DroppedStocks
    
    FullData<- CleanedData$CleanedData
    
    FullData<- FindFishbase(FullData)
    
    FullData<-FindResilience(FullData)
    
    rm(CleanedData)
    
    write.csv(file=paste(ResultFolder,'Cleaned Compiled Database.csv',sep=''),FullData)
    
    write.csv(file=paste(ResultFolder,'Omitted Stocks.csv',sep=''),DroppedStocks)
    
    write.csv(file=paste(ResultFolder,'Raw Database.csv',sep=''),RawData)
    
  }
  else 
  {
    FullData<- read.csv(paste(ResultFolder,'Cleaned Compiled Database.csv',sep=''))
    
    RawData<- read.csv(paste(ResultFolder,'Raw Database.csv',sep=''))
    
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
  
  # Create synthetic stocks -------------------------------------------------
  
  if (GroupMethod=='All')
  {
    Groups<- unique(FullData$SpeciesCatName,na.rm=T)
    
    Groups<- Groups[is.na(Groups)==F]
  }
  if (GroupMethod=='Nei')
  {
    Groups<- unique(FaoData$SpeciesCatName[ (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F])
  }
  
  SyntheticData<- StitchFish(RawData[RawData$Dbase=='RAM' ,],IdVar,Groups,GroupSamples,Iterations) 
  
  SyntheticData$BvBmsy[SyntheticData$BvBmsy>OutlierBvBmsy]<- NA
  
  show('Synthetic Stocks Created')
  
  for (m in 1:length(ModelNames))
  {
    
    eval(parse(text=paste('SyntheticData$',ModelNames[m],'Marker<- FALSE',sep='')))
    
    eval(parse(text=paste('SyntheticData$',ModelNames[m],'Prediction<- NA',sep='')))
    
  }
  
  # Prepare data for regression ---------------------------------------------
  
  library(proftools)
  
  sfInit( parallel=TRUE, cpus=NumCPUs,slaveOutfile="SyntheticRegressionFormatProgress.txt" )
  
  Fisheries<- (unique(SyntheticData$IdOrig))
  
  Data<- SyntheticData
  
  sfExport('Data','Fisheries','DependentVariable','CatchVariables','CatchLags','LifeHistoryVars','IsLog','IdVar')
  
  SyntheticFormatRegressionResults <- (sfClusterApplyLB(1:(length(Fisheries)), FormatForRegression))      
  
  sfStop()
  
  rm(Data)
  
  SyntheticData <- ldply (SyntheticFormatRegressionResults, data.frame)
  
  show('Data prepared for regression')
  
  # Rprof(NULL)
  #  RProfData<- readProfileData('Rprof.out')
  #  flatProfile(RProfData,byTotal=TRUE)
  
  # Run regressions ---------------------------------------------------------
  
  RealModels<- RunRegressions(RamData,Regressions,'Real Stocks')
  
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
  
  RamData<- InsertFisheryPredictions(RamData,RealModels) #Add fishery predictions back into main dataframe
  
  RealModelSdevs<- CreateSdevBins(RealModels,RamData,TransbiasBin)
  
  NeiRegressions<- list()
  
  NeiRegressions$M6<- Regressions$M6
  
  NeiRegressions$M7<- Regressions$M7
  
  NeiModels<- RunRegressions(SyntheticData,NeiRegressions,'Synthetic Stocks')
  
  NeiModelFactorLevels<- NULL
  
  for (m in 1:length(names(Regressions)))
  {
    Model<- names(Regressions)[m]
    
    eval(parse(text=paste('NeiModelFactorLevels$',Model,'<- NeiModels$',Model,'$xlevels$SpeciesCatName',sep='')))
    
  }
  
  SyntheticData<- InsertFisheryPredictions(SyntheticData,NeiModels) #Add fishery predictions back into main dataframe
  
  NeiModelSdevs<- CreateSdevBins(NeiModels,SyntheticData,TransbiasBin)
  
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
  
  TempLevel<- NeiModelFactorLevels$M6 
  
  ProxyCats<- AssignNearestSpeciesCategory(FaoNeiLevel,TempLevel,AllPossible)$Data
  
  Predictions<- predict(NeiModels$M6,ProxyCats) #Apply nei model
  
  #   FaoNeiLevel$M6Prediction<- Predictions
  
  FaoNeiLevel$M6Prediction<- 999
  
  
  #   NotIdentifiedPredictions<- predict(NeiModels$M7,FaoMarineFishLevel) #Apply unidentified fish model
  
  #   FaoMarineFishLevel$M7Prediction<- NotIdentifiedPredictions
  
  FaoMarineFishLevel$M7Prediction<- 999
  
  show('Regressions Applied')
  
  # Assign and identify best predicted biomass to stocks  ---------------------------------------
  
  if (IncludeNEIs==TRUE)
  {
    PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel,FaoNeiLevel,FaoMarineFishLevel) #Bind all data back together
  }
  if (IncludeNEIs==FALSE)
  {
    PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel) #Bind all data back together
  }
  BiomassColumns<- (grepl('BvBmsy',colnames(PredictedData)) | grepl('Prediction',colnames(PredictedData))) & grepl('LogBvBmsy',colnames(PredictedData))==F
  
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
  
  BiomassData$CommName<- as.character((BiomassData$CommName))
  
  BiomassData$SciName<- as.character((BiomassData$SciName))
  
  BiomassData$SpeciesCatName<- as.character(BiomassData$SpeciesCatName)
  
  WhereNeis<- (grepl('nei',BiomassData$CommName) | grepl('spp',BiomassData$SciName)) & grepl('not identified',BiomassData$SpeciesCatName)==F & (BiomassData$Dbase=='FAO') #Find unassessed NEIs
  
  WhereUnidentified<- grepl('not identified',BiomassData$SpeciesCatName)
  
  WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level
  
  BiomassData$IdLevel[WhereNeis]<- 'Neis'
  
  BiomassData$IdLevel[WhereUnidentified]<- 'Unidentified'
  
  BiomassData$IdLevel[WhereSpeciesLevel]<- 'Species'
  
  BiomassData<- AssignEconomicData(BiomassData) #Assign price and cost data to each stock
  
  show('Results Processed')
  
  # Run First Analisis of Current Status --------------------------------------------------
  
  GlobalStatus<- AnalyzeFisheries(BiomassData,'Baseline Global Status','Year',min(BiomassData$Year):max(BiomassData$Year),RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  #   RAMStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='RAM',],'RAM Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  # Calculate MSY -----------------------------------------------------------
  
  sigR<- 0

  CatchMSYresults<- (RunCatchMSY(GlobalStatus$Data,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,NumCatchMSYIterations,NumCPUs,CatchMSYTrumps))

  show("Completed CatchMSY")
  MsyData<- CatchMSYresults
  
  BiomassData$MSY<- MsyData$MSY #Assign MSY back to BiomassData estimates
  
  BiomassData$FvFmsy[MsyData$RanCatchMSY==T]<- MsyData$FvFmsy[MsyData$RanCatchMSY==T]

  BiomassData$BvBmsy[MsyData$RanCatchMSY==T]<- log(MsyData$BvBmsy[MsyData$RanCatchMSY==T])
  
  
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
  
  
  MsyData$Price[is.na(MsyData$Price)]<- mean(MsyData$Price,na.rm=T) #Apply mean price to fisheries with no price
  
  MsyData$r[is.na(MsyData$r)]<- mean(MsyData$r,na.rm=T) #FIX THIS XXX Apply mean r to fisheries with no r
  
  MsyData$CanProject<- is.na(MsyData$MSY)==F & is.na(MsyData$r)==F #Identify disheries that have both MSY and r
  
  ProjectionData<- RunProjection(MsyData[MsyData$CanProject==T,],BaselineYear,NumCPUs) #Run projections on MSY data that can be projected
    
  show("Completed Projections")
  
  if (IncludeNEIs==TRUE)
  {
  NeiData<- NearestNeighborNeis(BiomassData,MsyData,ProjectionData,BaselineYear) #Run Nearest Neighbor NEI analysis
  
  #Put NEI stocks back in the appropriate dataframes, remove stocks still missing data
  
  ProjectionData<- rbind(ProjectionData,NeiData$ProjNeis)
  
  BiomassData<- rbind(BiomassData,NeiData$BiomassNeis)
  
  }
  BiomassData<- BiomassData[BiomassData$BvBmsy!=999 | is.infinite(BiomassData$BvBmsy)!=TRUE,]
  
  MsyData<- MsyData[is.na(MsyData$MSY)==F,]
  
  ProjectionData<- ProjectionData[ProjectionData$CanProject==T,]
  
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


# Assign labels and prepare results for analysis --------------------------


WhereNeis<- (grepl('nei',FullData$CommName) | grepl('spp',FullData$SciName)) & grepl('not identified',FullData$SpeciesCatName)==F #Find unassessed NEIs

WhereUnidentified<- grepl('not identified',FullData$SpeciesCatName)

WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level

FullData$IdLevel[WhereNeis]<- 'Neis'

FullData$IdLevel[WhereUnidentified]<- 'Unidentified'

FullData$IdLevel[WhereSpeciesLevel]<- 'Species'

Policies<- unique(ProjectionData$Policy)

ProjectionData$Biomass<- (ProjectionData$BvBmsy* (2* ProjectionData$MSY/ProjectionData$r))

if (IncludeUnderfished==FALSE) #Remove projections for underfished stocks if desired
{
  
  CurrentlyOverfished<- ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & ProjectionData$BvBmsy<1]
  
  ProjectionData<- ProjectionData[ProjectionData$IdOrig %in% CurrentlyOverfished ,]
  
}

if (IncludeNEIs==FALSE) #Remove NEIs if desired 
{
  ProjectionData<- ProjectionData[ProjectionData$IdLevel=='Species',]

  BiomassData<- BiomassData[BiomassData$IdLevel=='Species',]

  MsyData<- MsyData[MsyData$IdLevel=='Species',]
  
  
  
}

if (IncludeForageFish==FALSE) #Remove forage fish species if desired 
{
  ProjectionData<- ProjectionData[ProjectionData$SpeciesCatName%in%ForageFish==F,]
  
  BiomassData<- BiomassData[BiomassData$SpeciesCatName%in%ForageFish==F,]
}

#Reframe 0 profits/catch to prevent infinites and NANs in 

ProjectionData$Profits[ProjectionData$Profits<0]<- 0.0001 #Reframe 0 profits/catch to prevent infinites and NANs in 

ProjectionData$Catch[ProjectionData$Catch==0]<- 0.0001



# Prepare summary tables --------------------------------------------------


# Build empty version of Chris's database to be populated with FinalYear results. Make separate percent change and absolute change tables then use rbind later
# 3 data frames to be merged at end.
# 1) Policy results relative to baseline values. 2) Policy results relative to Status Quo Policy in Final Year 3) Cumulative Policy results relative to Status Quo

ResultMetricsBaselineNames<-c("PercChangeTotalProfits","PercChangeTotalCatch","PercChangeTotalBiomass","PercChangeMedianProfits","PercChangeMedianCatch",
                              "PercChangeMedianBiomass", "AbsChangeTotalProfits","AbsChangeTotalCatch","AbsChangeTotalBiomass","AbsChangeMedianProfits","AbsChangeMedianCatch",
                              "AbsChangeMedianBiomass")

ResultMetricsSQFinalNames<-c("PercChangeFromSQTotalBiomass","PercChangeFromSQTotalProfits","PercChangeFromSQTotalCatch","PercChangeFromSQMedianBiomass", "PercChangeFromSQMedianProfits",
                             "PercChangeFromSQMedianCatch","AbsChangeFromSQTotalBiomass","AbsChangeFromSQTotalProfits","AbsChangeFromSQTotalCatch", "AbsChangeFromSQMedianBiomass",
                             "AbsChangeFromSQMedianProfits","AbsChangeFromSQMedianCatch")

ResultMetricsSQCumFinalNames<-c("PercChangeFromSQCum_NPV","PercChangeFromSQCum_Food","PercChangeFromSQCum_Fish","PercChangeFromSQCumMedianProfits","PercChangeFromSQCumMedianCatch","PercChangeFromSQMedianBiomass",
                                "AbsChangeFromSQCumProfits","AbsChangeFromSQCumFood","AbsChangeFromSQCumFish","AbsChangeFromSQCumMedianProfits","AbsChangeFromSQCumMedianCatch","AbsChangeFromSQCumMedianBiomass")

ResultMetricsBaselineTable<-data.frame(matrix(NA,nrow=length(CountriesToRun),ncol=13))
colnames(ResultMetricsBaselineTable)<-c("Region",ResultMetricsBaselineNames)

ResultMetricsSQFinalTable<-data.frame(matrix(NA,nrow=length(CountriesToRun),ncol=13))
colnames(ResultMetricsSQFinalTable)<-c("Region",ResultMetricsSQFinalNames)

ResultMetricsSQCumFinalTable<-data.frame(matrix(NA,nrow=length(CountriesToRun),ncol=13))
colnames(ResultMetricsSQCumFinalTable)<-c("Region",ResultMetricsSQCumFinalNames)


# CountriesToRun<-c("Global","USA","China","Indonesia","Philippines","Peru","Chile","Mexico","Japan","Myanmar","Viet Nam","EU","Parties to the Nauru Agreement",EUCountries)

for (c in 1:length(CountriesToRun)) # Run analyses on each desired region
{
  
  show(CountriesToRun[c])
  if (CountriesToRun[c]=='Global'){
    FullData_CountryLocater<-  FullData$Country %in% unique(FullData$Country)
    Biomass_CountryLocater<-  BiomassData$Country %in% unique(BiomassData$Country)
    Proj_CountryLocater<- ProjectionData$Country %in% unique(ProjectionData$Country)
    Nei_CountryLocater<-FaoNeiLevel$Country %in% unique(FaoNeiLevel$Country)
    
  } else if (CountriesToRun[c]=='Parties to the Nauru Agreement')
  {
    Biomass_CountryLocater<- BiomassData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
    Proj_CountryLocater<- ProjectionData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
    FullData_CountryLocater<- FullData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
    Nei_CountryLocater<-FaoNeiLevel$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Micronesia','Tuvalu','Palau','Nauru') 
    
  } else if (CountriesToRun[c]=='EU')
  {
   
    EuStocks<- Spec_Region_RAM$assessid[Spec_Region_RAM$region=='European Union']
    
    
    Biomass_CountryLocater<- BiomassData$Country %in% EUCountries | BiomassData$IdOrig %in% EuStocks
    Proj_CountryLocater<- ProjectionData$Country %in% EUCountries | ProjectionData$IdOrig %in% EuStocks
    FullData_CountryLocater<- FullData$Country %in% EUCountries | FullData$IdOrig %in% EuStocks
    Nei_CountryLocater<-FaoNeiLevel$Country %in% EUCountries | FaoNeiLevel$IdOrig %in% EuStocks
    
  } else if (CountriesToRun[c]=='China')
  {
    Biomass_CountryLocater<- BiomassData$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
    Proj_CountryLocater<- ProjectionData$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
    FullData_CountryLocater<- FullData$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
    Nei_CountryLocater<-FaoNeiLevel$Country %in% c("China","China Hong Kong SAR","China Macao SAR")
    
  } else
  {
    Biomass_CountryLocater<- BiomassData$Country==CountriesToRun[c] 
    Proj_CountryLocater<- ProjectionData$Country==CountriesToRun[c] 
    FullData_CountryLocater<- FullData$Country==CountriesToRun[c] 
    Nei_CountryLocater<-FaoNeiLevel$Country==CountriesToRun[c]
    
  }
  
  if(sum(Biomass_CountryLocater,na.rm=T)>0 & sum(Proj_CountryLocater,na.rm=T)>0)
  {
    
    # Analyze Current Status/Kobe Plot Trends  ----------------------------------------------------------
    
    BiomassStatus<- AnalyzeFisheries(BiomassData[Biomass_CountryLocater,],paste(CountriesToRun[c],' Status',sep=''),'Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
    
    MakeKobePlot(BiomassStatus$Data,BaselineYear,paste(paste(CountriesToRun[c],' Kobe Plot',sep='')))
    
    # Analyze Projections -----------------------------------------------------
    
    TempProjectionData<- ProjectionData[Proj_CountryLocater,]
    
    TempProjectionData$DiscProfits<- TempProjectionData$Profits * (1+Discount)^-(TempProjectionData$Year-BaselineYear)
    
    #Calculate baseline metrics values 
    
    Baseline<- ddply(subset(TempProjectionData,Year==BaselineYear & Policy=='Historic'),c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                     TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                     MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),NumberOfStocks=length(unique(IdOrig))
                     ,DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    BaselineMarker<- as.data.frame(matrix(NA,nrow=length(unique(TempProjectionData$Policy))-1,ncol=dim(Baseline)[2]))
    
    colnames(BaselineMarker)<- colnames(Baseline)
    
    BaselineMarker[,c(1,3:dim(Baseline)[2])]<- Baseline[,c(1,3:dim(Baseline)[2])]
    
    BaselineMarker$Policy<- Policies[Policies!='Historic']
    
    #Analyze time trends in metrics
    TimeTrend<- ddply(TempProjectionData,c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                      TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),
                      NumberOfStocks=length(unique(IdOrig)),DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    TimeTrend<- rbind(TimeTrend,BaselineMarker)
    
    YearOrder<- order(TimeTrend$Year)
    
    TimeTrend<- TimeTrend[YearOrder,]
    
    # Calculate Upside Metrics -----------------------------------------------------
    
    
    TimeTrend$PercChangeTotalProfits<-100*(TimeTrend$TotalProfits/Baseline$TotalProfits-1) #Percent change in total profits from current
    
    TimeTrend$PercChangeTotalCatch<-100*(TimeTrend$TotalCatch/Baseline$TotalCatch-1) #Percent change in total catch from current
    
    TimeTrend$PercChangeTotalBiomass<-100*(TimeTrend$TotalBiomass/Baseline$TotalBiomass-1) #Percent change in total biomass from current
    
    TimeTrend$PercChangeMedianProfits<-100*(TimeTrend$MedianProfits/Baseline$MedianProfits-1) #Percent change in median profits from current
    
    TimeTrend$PercChangeMedianCatch<-100*(TimeTrend$MedianCatch/Baseline$MedianCatch-1) #Percent change in median catch from current
    
    TimeTrend$PercChangeMedianBiomass<-100*(TimeTrend$MedianBvBmsy/Baseline$MedianBvBmsy-1) #Percent change in median B/Bmsy from current
    
    TimeTrend$AbsChangeTotalProfits<-TimeTrend$TotalProfits-Baseline$TotalProfits #absolute change in total profits from current
    
    TimeTrend$AbsChangeTotalCatch<-TimeTrend$TotalCatch-Baseline$TotalCatch #Percent change in total catch from current
    
    TimeTrend$AbsChangeTotalBiomass<-TimeTrend$TotalBiomass-Baseline$TotalBiomass #Percent change in total biomass from current
    
    TimeTrend$AbsChangeMedianProfits<-TimeTrend$MedianProfits-Baseline$MedianProfits #Absolute change in median profits from current
    
    TimeTrend$AbsChangeMedianCatch<-TimeTrend$MedianCatch-Baseline$MedianCatch #Percent change in median catch from current
    
    TimeTrend$AbsChangeMedianBiomass<-TimeTrend$MedianBvBmsy-Baseline$MedianBvBmsy #Percent change in median B/Bmsy from current
    
    
    # Calculate cumulative changes in metrics ------ 
    
    Cumulatives<- ddply(TimeTrend[TimeTrend$Year>BaselineYear,],c('Policy'),summarize,NPV=sum(DiscProfits,na.rm=T),Food=sum(TotalCatch,na.rm=T),Fish=sum(TotalBiomass,na.rm=T),
                        MedianProfits=sum(MedianProfits,na.rm=T),MedianCatch=sum(MedianCatch,na.rm=T),MedianBiomass=sum(MedianBvBmsy,na.rm=T))
    
    CumeNumbers<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbersAbs<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbers<- 100*(t(t(CumeNumbers)/CumeNumbers[which(Cumulatives$Policy=='SQ'),])-1) # Calculate % Change cumulative metrics
    
    CumeNumbersAbs<- t(t(CumeNumbersAbs)-CumeNumbersAbs[which(Cumulatives$Policy=='SQ'),]) # Calculate absolute change in cumulative metrics  
    
    Cumulatives[,2:dim(Cumulatives)[2]]<- CumeNumbers   
    
    Cumulatives[,8:13]<-CumeNumbersAbs
    
    colnames(Cumulatives)[8:13]<-paste("Abs",colnames(CumeNumbersAbs),sep="")
    
    Cumulatives[,colnames(Cumulatives)=="AbsMedianBiomass"]<-Cumulatives$AbsMedianBiomass/(ProjectionTime)
    
    Cumulatives<- Cumulatives[Cumulatives$Policy!='SQ' & Cumulatives$Policy!='Historic',]
    
    Cumulatives$Country<-CountriesToRun[c]

    if(c==1){CumulativesFinal<-Cumulatives} 
    if(c>1){CumulativesFinal<-rbind(CumulativesFinal,Cumulatives)}

    # Calculate  metrics in final year -----------------------------------------------------
    
    FinalYear<- TimeTrend[TimeTrend$Year==max(TimeTrend$Year),]  
    
    FinalYear$AbsChangeFromSQTotalProfits<- FinalYear$TotalProfits-FinalYear$TotalProfits[FinalYear$Policy=='SQ'] # Absolute change in total profits relative to BAU
    
    FinalYear$AbsChangeFromSQTotalCatch<- FinalYear$TotalCatch-FinalYear$TotalCatch[FinalYear$Policy=='SQ'] # Absolute change in total catch relative to BAU
    
    FinalYear$AbsChangeFromSQTotalBiomass<- FinalYear$TotalBiomass-FinalYear$TotalBiomass[FinalYear$Policy=='SQ'] # Absolute change in total biomass relative to BAU
    
    FinalYear$PercChangeFromSQTotalProfits<- 100*(FinalYear$TotalProfits/FinalYear$TotalProfits[FinalYear$Policy=='SQ']-1) # Percent change in total profits relative to BAU
    
    FinalYear$PercChangeFromSQTotalCatch<- 100*(FinalYear$TotalCatch/FinalYear$TotalCatch[FinalYear$Policy=='SQ']-1) # Percent change in total catch relative to BAU
    
    FinalYear$PercChangeFromSQTotalBiomass<- 100*(FinalYear$TotalBiomass/FinalYear$TotalBiomass[FinalYear$Policy=='SQ']-1) # Percent change in total biomass relative to BAU
    
    FinalYear$PercChangeFromSQMedianProfits<- 100*(FinalYear$MedianProfits/FinalYear$MedianProfits[FinalYear$Policy=='SQ']-1) # Percent change in median profits relative to BAU
    
    FinalYear$PercChangeFromSQMedianCatch<- 100*(FinalYear$MedianCatch/FinalYear$MedianCatch[FinalYear$Policy=='SQ']-1) # Percent change in median catch relative to BAU
    
    FinalYear$PercChangeFromSQMedianBiomass<- 100*(FinalYear$MedianBvBmsy/FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ']-1) # Percent change in median B/Bmsy relative to BAU
    
    FinalYear$AbsChangeFromSQMedianProfits<- FinalYear$MedianProfits-FinalYear$MedianProfits[FinalYear$Policy=='SQ']  # Absolute change in median profits relative to BAU
    
    FinalYear$AbsChangeFromSQMedianCatch<- FinalYear$MedianCatch-FinalYear$MedianCatch[FinalYear$Policy=='SQ'] # Absolute change in median catch relative to BAU
    
    FinalYear$AbsChangeFromSQMedianBiomass<- FinalYear$MedianBvBmsy-FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ'] # Absolute change in median B/Bmsy relative to BAU
    
    # Populate summary table of results  -----------------------------------------------------
    
    ResultMetricsBaselineTable[c,1]<-CountriesToRun[c]
    
    ResultMetricsSQFinalTable[c,1]<-CountriesToRun[c]
    
    ResultMetricsSQCumFinalTable[c,1]<-CountriesToRun[c]
    
    ResultMetricsBaselineTable[c,2:13]<-FinalYear[FinalYear$Policy=="CatchShare",13:24]
    
    ResultMetricsSQFinalTable[c,2:4]<-FinalYear[FinalYear$Policy=="CatchShare",28:30]
   
    ResultMetricsSQFinalTable[c,5:7]<-FinalYear[FinalYear$Policy=="CatchShare",25:27]
    
    ResultMetricsSQFinalTable[c,8:10]<-FinalYear[FinalYear$Policy=="CatchShare",34:36]
    
    ResultMetricsSQFinalTable[c,11:13]<-FinalYear[FinalYear$Policy=="CatchShare",31:33]
    
    ResultMetricsSQCumFinalTable[c,2:13]<-Cumulatives[Cumulatives$Policy=="CatchShare",2:dim(Cumulatives)[2]]
    
    ResultMetricsTable<-merge(ResultMetricsBaselineTable,ResultMetricsSQFinalTable,by = "Region")
    
    ResultMetricsTable<-merge(ResultMetricsTable,ResultMetricsSQCumFinalTable,by = "Region")
    
    # Calculate data coverage statistics --------------------------------------------
    
    # Tallies of catch and fisheries in  FullData
    FullTallies<- ddply(FullData[FullData_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='FullData')
    
    # Tallies of catch and fisheries in  BiomassData
    
    BiomassTallies<- ddply(BiomassData[Biomass_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='BiomassData')
    
    # Tallies of catch and fisheries in ProjectionData
    
    ProjectionTallies<- ddply(ProjectionData[Proj_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='ProjectionData')
    
    
    Tallies<- rbind(FullTallies,BiomassTallies,ProjectionTallies)
    
    # Plot data coverage statistics
    
    pdf(file=paste(FigureFolder,CountriesToRun[c],' Database Analysis.pdf',sep='')) 
    
    print(dotplot(Fisheries ~ Dbase | Source,data=Tallies,subset=Year==BaselineYear,cex=2)  )  
    
    print(dotplot(Catch ~ Dbase | Source,data=Tallies,subset=Year==BaselineYear,cex=2)   ) 
    
    print(xyplot(Catch ~ Year | Source,group=Dbase,data=Tallies,subset=Year>=1950 & Year<= BaselineYear,cex=1,type='l',lwd=3,auto.key=T))  
    
    print(xyplot((Fisheries) ~ Year | Source,group=Dbase,data=Tallies,subset=Year>=1950 & Year<= BaselineYear,cex=1,type='l',lwd=3,auto.key=T))   
    
    dev.off()
    
    
    # Save .csvs of results --------------------------------------------------
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Policy Projections.csv',sep=''),TimeTrend)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Baseline.csv',sep=''),Baseline)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Biomass Status.csv',sep=''),BiomassStatus$Data)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Raw Projection Data.csv',sep=''),TempProjectionData)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Current Year Data.csv',sep=''),FinalYear)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Business As Usual Data.csv',sep=''),Cumulatives)    
    

    # Plot results --------------------------------------------------
    
    pdf(file=paste(FigureFolder,CountriesToRun[c],' Trajectories.pdf',sep='')) 
    
    print(barchart(NPV ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in NPV from Business as Usual'))
    
    print(barchart(Food ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in Total Catch from Business as Usual'))
    
    print(barchart(Fish ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in Total Fish from Business as Usual'))
    
    print( barchart(PercChangeTotalProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Profits'))
    
    print(barchart(PercChangeTotalCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Catch'))
    
    print(a<- barchart(PercChangeTotalBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Fish'))
    
    print(barchart(PercChangeMedianProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Profits'))
    
    print(barchart(PercChangeMedianCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Catch'))
    
    print(barchart(PercChangeMedianBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Fish'))
    
    print(barchart(PercChangeFromSQTotalProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Total Profits'))
    
    print(barchart(PercChangeFromSQTotalCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Total Catch'))
    
    print(barchart(PercChangeFromSQTotalBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Total Fish'))
    
    print(barchart(PercChangeFromSQMedianProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Profits'))
    
    print(barchart(PercChangeFromSQMedianCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Catch'))
    
    print(barchart(PercChangeFromSQMedianBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Fish'))
    
    print(xyplot( PercChangeTotalProfits ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,ylab='% Change from Current Total Profits',type='l',lwd=4,auto.key=T,aspect='fill'))
    
    print(xyplot( PercChangeTotalCatch ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Total Catch'))
    
    print(xyplot( PercChangeTotalBiomass ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Total Fish'))
    
    print(xyplot( PercChangeMedianProfits ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,ylab='% Change from Current Median Profits',type='l',lwd=4,auto.key=T,aspect='fill'))
    
    print(xyplot( PercChangeMedianCatch ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Median Catch'))
    
    print(xyplot( MedianBvBmsy ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='Median B/Bmsy'))
    
    print(xyplot( MedianFvFmsy ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='Median F/Fmsy'))
    
    dev.off()
    
  } #Close if
} #Close Country Trajectory Analysis 

write.csv(file=paste(ResultFolder,'Chris Summary Table Data.csv',sep=''),ResultMetricsTable)

# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------




