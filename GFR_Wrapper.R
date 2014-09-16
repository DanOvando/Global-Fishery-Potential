
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################

# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

# Read in and process data ------------------------------------------------------------

if (RunAnalyses==TRUE)
{
  
  if (file.exists(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))==F)
  {
    
    source('Database_Build.r') #Build Tyler's database
    
    # fulldata<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))
    
    RawData<- fulldata
    
    FullData<- fulldata
    
    FullData$FvFmsy<- FullData$UvUmsytouse
    
    rm(fulldata)
    
    CleanedData<- MaidService(FullData,OverlapMode)
    
    DroppedStocks<- CleanedData$DroppedStocks
    
    FullData<- CleanedData$CleanedData
    
    FullData<- FindFishbase(FullData)
    
    
    rm(CleanedData)
    
    write.csv(file=paste(ResultFolder,'Raw Compiled Database.csv',sep=''),FullData)
    
    write.csv(file=paste(ResultFolder,'Omitted Stocks.csv',sep=''),DroppedStocks)
    
  }
  if (file.exists(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))){FullData<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))}
  
  # FullData$SpeciesCatName<- as.factor( FullData$SpeciesCatName)
  
  FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA
  
  ModelNames<- names(Regressions)
  
  for (m in 1:length(ModelNames))
  {
    
    eval(parse(text=paste('FullData$',ModelNames[m],'Marker<- FALSE',sep='')))
    
    eval(parse(text=paste('FullData$',ModelNames[m],'Prediction<- NA',sep='')))
  }
  
  # Where<- FullData[,'AgeMat']==0 | is.na(FullData[,'AgeMat'])
  # 
  # FullData[Where,'AgeMat']<- NA
  
  SofiaData<-  FullData[FullData$Dbase=='SOFIA',]
  
  RamData<- FullData[FullData$Dbase=='RAM',]
  
  RamData$BvBmsy[RamData$BvBmsy>OutlierBvBmsy]<- NA
  
  FaoData<- FullData[FullData$Dbase=='FAO',]
  
  FaoData<- LumpFisheries(FaoData,SpeciesCategoriesToLump)
  
  # FaoIdSample<- sample(unique(FaoData[,IdVar]),500,replace=FALSE)
  # # # # 
  # FaoData<- FaoData[FaoData[,IdVar] %in% FaoIdSample,]
  
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
  
  SyntheticData<- StitchFish(RamData,IdVar,Groups,GroupSamples,Iterations) 
  
  SyntheticData$BvBmsy[SyntheticData$BvBmsy>OutlierBvBmsy]<- NA
  
  show('Synthetic Stocks Created')
  
  for (m in 1:length(ModelNames))
  {
    
    eval(parse(text=paste('SyntheticData$',ModelNames[m],'Marker<- FALSE',sep='')))
    
    eval(parse(text=paste('SyntheticData$',ModelNames[m],'Prediction<- NA',sep='')))
    
  }
  
  # Prepare data for regression ---------------------------------------------
  
  library(proftools)
  
  #  Rprof()
  
  RamData<- FormatForRegression(RamData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)#Add resgression data to database
  
  SyntheticData<- FormatForRegression(SyntheticData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)
  
  SofiaData<- SofiaData[is.na(SofiaData$Catch)==F,]
  
  SofiaData<- FormatForRegression(SofiaData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)#Add resgression data to database
  
  
  if (file.exists(paste(ResultFolder,'FaoData.Rdata',sep=''))==F)
  {
    
    FaoData<- FormatForRegression(FaoData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)
    
    save(file=paste(ResultFolder,'FaoData.Rdata',sep=''),FaoData)
  }
  if (file.exists(paste(ResultFolder,'FaoData.Rdata',sep='')))
  {
    load(paste(ResultFolder,'FaoData.Rdata',sep=''))
  }
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
  
  AllPossible<- unique(data.frame(FullData$SpeciesCatName,FullData$SpeciesCat))
  
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
  
  FaoNeiLevel$M6Prediction<- Predictions
  
  NotIdentifiedPredictions<- predict(NeiModels$M7,FaoMarineFishLevel) #Apply unidentified fish model
  
  FaoMarineFishLevel$M7Prediction<- NotIdentifiedPredictions
  
  show('Regressions Applied')
  
  # Clean and process predictions and data ---------------------------------------
  
  if (IncludeNEIs==1)
  {
    PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel,FaoNeiLevel,FaoMarineFishLevel) #Bind all data back together
  }
  if (IncludeNEIs==0)
  {
    PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel) #Bind all data back together
  }
  BiomassColumns<- grepl('BvBmsy',colnames(PredictedData)) | grepl('Prediction',colnames(PredictedData))
  
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
  
  # BiomassData$BestBio<- BestBio
  
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
  
  show('Results Processed')
  
  BiomassData<- AssignEconomicData(BiomassData)
  
  # Analyze Current Status --------------------------------------------------
  
  GlobalStatus<- AnalyzeFisheries(BiomassData,'Baseline Global Status','Year',min(BiomassData$Year):max(BiomassData$Year),RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  # USA<- BiomassData[BiomassData$Country=='USA' |BiomassData$Country=='United States of America' ,]
  # 
  # USAStatus<- AnalyzeFisheries(USA,'USA Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  RAMStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='RAM',],'RAM Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  # FAOStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='FAO',],'FAO Status','Year',2000:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  # 
  # IndonesiaStatus<- AnalyzeFisheries(BiomassData[BiomassData$Country=='Indonesia',],'Indonesia Status','Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  # 
  # CanadaStatus<- AnalyzeFisheries(BiomassData[BiomassData$Country=='Canada',],'Canada Status','Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
  
  # Calculate MSY -----------------------------------------------------------
  sigR<- 0
  
  CatchMSYresults<- RunCatchMSY(GlobalStatus$Data,ExcludeForageFish,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,n,SampleLength)
  
  MsyData<- CatchMSYresults$Data
  
  BiomassData$MSY<- MsyData$MSY
  
  BiomassData$FvFmsy[MsyData$HasRamFvFmsy==F]<- MsyData$FvFmsy[MsyData$HasRamFvFmsy==F]
  
  pdf(file=paste(FigureFolder,'Catch MSY vs PRM BvBmsy predictions.pdf',sep=''))
  print(xyplot(  CatchMSYBvBmsy ~ BvBmsy | Dbase,data=MsyData,xlab='PRM BvBmsy',ylab='CMSY BvBmsy',panel=function(x,y,...){
    panel.xyplot(x,y,...)
    panel.abline(a=0,b=1,lty=2)
  }))
  dev.off()
  
  MedianWorked<- median(MsyData$BvBmsy[is.na(MsyData$MSY)==F])
  
  MedianFailed<- median(MsyData$BvBmsy[is.na(MsyData$MSY)])
  
  # CurrentCatch<- sum(MsyData$Catch[MsyData$Year==2010 & is.na(MsyData$MSY)==F],na.rm=T)
  # 
  # CurrentCatch2<- sum(MsyData$Catch[MsyData$Year==2010],na.rm=T)
  
  MsyData<- MsyData[is.na(MsyData$MSY)==F,]
  
  MsyData$r[is.na(MsyData$r)]<- mean(MsyData$r,na.rm=T)
  
  MsyData$PercentGain<- 100*(MsyData$MSY/MsyData$Catch-1)
  
  # FutureMSY<- sum(MsyData$MSY[MsyData$Year==2010],na.rm=T)
  # 
  # GlobalPercentChange<- 100*(FutureMSY/CurrentCatch-1)
  # 
  # IndoCatch<- (PredictedData[PredictedData$Country=='Indonesia',])
  
  MsyData$Country[MsyData$Country=='United States of America']<- 'USA'
  
  CountryMsy<- ddply(MsyData[MsyData$Year==2010,],c('Country'),summarize,CurrentCatch= sum(Catch,na.rm=T),MSY=sum(MSY,na.rm=T),TotalGain=sum(MSY,na.rm=T)-sum(Catch,na.rm=T),
                     PercGain=(100*(sum(MSY,na.rm=T)/sum(Catch,na.rm=T)-1)),MedianBvBmsy=median(BvBmsy,na.rm=T),PercMissing=100*(sum(is.na(MSY))/length(MSY)))
  
  PercGainOrder<- order(CountryMsy$PercGain,decreasing=T)
  
  CountryMsy<- CountryMsy[PercGainOrder,]
  
  write.csv(file=paste(ResultFolder,'Country Rankings.csv',sep=''),CountryMsy)
  
  # Run projection analysis -------------------------------------------------
  # 
  # MsyData$Price<- 1000
  # 
  # MsyData$BvBmsyOpenAccess<- 0.25
  
  BaselineYear<- 2009
  
  MsyData$Price[is.na(MsyData$Price)]<- mean(MsyData$Price,na.rm=T)
  
  ProjectionData<- RunProjection(MsyData,BaselineYear)
  
  OriginalProjectionData<- ProjectionData
  
  OriginalFullData<- FullData
  
  OriginalMsyData<- MsyData
  
  OriginalBiomassData<- BiomassData
  
  
  
  
} #Close RunAnalyses If

save.image(file=paste(ResultFolder,'Global Fishery Recovery Results.rdata',sep=''))

if (RunAnalyses==F)
{

  FullData<- OriginalFullData #Complete database, post filtering/cleaning etc
  
  BiomassData<- OriginalBiomassData #Fisheries that have B/Bmsy
  
  MsyData<- OriginalMsyData #Fisheries that have B/Bmsy and MSY
  
  ProjectionData<- OriginalProjectionData #Fisheries that have B/Bmsy, MSY, and we've run the projections
  

}

ProjectionData$Country[ProjectionData$Country=='United States of America']<- 'USA'

BiomassData$Country[BiomassData$Country=='United States of America']<- 'USA'

FullData$Country[FullData$Country=='United States of America']<- 'USA'

WhereNeis<- (grepl('nei',FullData$CommName) | grepl('spp',FullData$SciName)) & grepl('not identified',FullData$SpeciesCatName)==F #Find unassessed NEIs

WhereUnidentified<- grepl('not identified',FullData$SpeciesCatName)

WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level

FullData$IdLevel[WhereNeis]<- 'Neis'

FullData$IdLevel[WhereUnidentified]<- 'Unidentified'

FullData$IdLevel[WhereSpeciesLevel]<- 'Species'

Policies<- unique(ProjectionData$Policy)

## This is where you need to calculate actual biomass for each fishery

ProjectionData$Biomass<- (ProjectionData$BvBmsy* (2* ProjectionData$MSY/ProjectionData$r))


if (OverFishedOnly==1)
{
  
  CurrentlyOverfished<- ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & ProjectionData$BvBmsy<1]
  ProjectionData<- ProjectionData[ProjectionData$IdOrig %in% CurrentlyOverfished ,]
  
}

if (IncludeNEIs==0)
{
  ProjectionData<- ProjectionData[ProjectionData$IdLevel=='Species',]
}

if (ExcludeForageFish==1)
{
  ProjectionData<- ProjectionData[ProjectionData$SpeciesCatName%in%ForageFish==F,]
  
  BiomassData<- BiomassData[BiomassData$SpeciesCatName%in%ForageFish==F,]
}

ProjectionData$Profits[ProjectionData$Profits<0]<- 0

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

# Summarize GlobalStatus Year, FAO Region, and Species Category for assigning values to NEI fisheries (New Method)

StatusByRegSpCat<-ddply(GlobalStatus$Data[GlobalStatus$Data$Dbase=="FAO",],c("Year","RegionFAO","SpeciesCatName"),summarize, MedianStatus=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),Fisheries=length(unique(IdOrig)))

for (c in 1:length(CountriesToRun)) #Workhorse analysis loop
{
  
  BaselineYear<- 2009
  
  show(CountriesToRun[c])
  if (CountriesToRun[c]=='Global'){
    FullData_CountryLocater<-  FullData$Country %in% unique(FullData$Country)
    Biomass_CountryLocater<-  BiomassData$Country %in% unique(BiomassData$Country)
    Proj_CountryLocater<- ProjectionData$Country %in% unique(ProjectionData$Country)
    Nei_CountryLocater<-FaoNeiLevel %in% unique(FaoNeiLevel$Country)
  
  } else if (CountriesToRun[c]=='Parties to the Nauru Agreement')
  {
    Biomass_CountryLocater<- BiomassData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
    Proj_CountryLocater<- ProjectionData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
    FullData_CountryLocater<- FullData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
    Nei_CountryLocater<-FaoNeiLevel$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
  
  } else if (CountriesToRun[c]=='EU')
  {
    Biomass_CountryLocater<- BiomassData$Country %in% EUCountries
    Proj_CountryLocater<- ProjectionData$Country %in% EUCountries
    FullData_CountryLocater<- FullData$Country %in% EUCountries
    Nei_CountryLocater<-FaoNeiLevel$Country %in% EUCountries
  
  } else
  {
    Biomass_CountryLocater<- BiomassData$Country==CountriesToRun[c] 
    Proj_CountryLocater<- ProjectionData$Country==CountriesToRun[c] 
    FullData_CountryLocater<- FullData$Country==CountriesToRun[c] 
    Nei_CountryLocater<-FaoNeiLevel$Country==CountriesToRun[c]
  
  }
  
  if(sum(Biomass_CountryLocater,na.rm=T)>0 & sum(Proj_CountryLocater,na.rm=T)>0)
  {
    
    # Analyze Time Trends  ----------------------------------------------------------
    
    BiomassStatus<- AnalyzeFisheries(BiomassData[Biomass_CountryLocater,],paste(CountriesToRun[c],' Status',sep=''),'Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
    
    # subset NEI's and add median values calculated in StatusByRegSpCat at beginning of loop. Add NEI entries to Biomass Status before Kobe Plot 
    
    CountryNeis<-FaoNeiLevel[Nei_CountryLocater,]
    CountryNeis<-subset(CountryNeis,(Year %in% c(2005:2011))) # just for 2005-2011
    
    CountryNeis$BestModel<-"Nei"
    CountryNeis$IdLevel<-"Nei"
    CountryNeis$Price<-NA
    CountryNeis$BvBmsyOpenAccess<-NA
    CountryNeis$BvBmsySD<-NA
    
    # apply median status to nei fisheries in matching year/region/speciescategory 
    for (i in 1:nrow(CountryNeis)){
      
      if(length(subset(StatusByRegSpCat,RegionFAO==CountryNeis$RegionFAO[i] & Year==CountryNeis$Year[i] & SpeciesCatName==CountryNeis$SpeciesCatName[i])$MedianStatus)>0){
      
      CountryNeis$BvBmsy[i]<-subset(StatusByRegSpCat,RegionFAO==CountryNeis$RegionFAO[i] & Year==CountryNeis$Year[i] & SpeciesCatName==CountryNeis$SpeciesCatName[i])$MedianStatus
      CountryNeis$FvFmsy[i]<-subset(StatusByRegSpCat,RegionFAO==CountryNeis$RegionFAO[i] & Year==CountryNeis$Year[i] & SpeciesCatName==CountryNeis$SpeciesCatName[i])$MedianFvFmsy
      }
    }
    
    BiomassStatus$Data<-rbind(BiomassStatus$Data,CountryNeis)
    
    MakeKobePlot(BiomassStatus$Data,BaselineYear,paste(paste(CountriesToRun[c],' Kobe Plot',sep='')))
    
    TempProjectionData<- ProjectionData[Proj_CountryLocater,]
    
    TempProjectionData$DiscProfits<- TempProjectionData$Profits * (1+Discount)^-(TempProjectionData$Year-BaselineYear)
    
    Baseline<- ddply(subset(TempProjectionData,Year==BaselineYear & Policy=='Historic'),c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                     TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                     MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),NumberOfStocks=length(unique(IdOrig))
                     ,DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    BaselineMarker<- as.data.frame(matrix(NA,nrow=length(unique(TempProjectionData$Policy))-1,ncol=dim(Baseline)[2]))
    
    colnames(BaselineMarker)<- colnames(Baseline)
    
    BaselineMarker[,c(1,3:dim(Baseline)[2])]<- Baseline[,c(1,3:dim(Baseline)[2])]
    
    BaselineMarker$Policy<- Policies[Policies!='Historic']
    
    TimeTrend<- ddply(TempProjectionData,c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                      TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),
                      NumberOfStocks=length(unique(IdOrig)),DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    TimeTrend<- rbind(TimeTrend,BaselineMarker)
    
    YearOrder<- order(TimeTrend$Year)
    
    TimeTrend<- TimeTrend[YearOrder,]
    
    TimeTrend$PercChangeTotalProfits<-100*(TimeTrend$TotalProfits/Baseline$TotalProfits-1)
    
    TimeTrend$PercChangeTotalCatch<-100*(TimeTrend$TotalCatch/Baseline$TotalCatch-1)
    
    TimeTrend$PercChangeTotalBiomass<-100*(TimeTrend$TotalBiomass/Baseline$TotalBiomass-1)
    
    TimeTrend$PercChangeMedianProfits<-100*(TimeTrend$MedianProfits/Baseline$MedianProfits-1)
    
    TimeTrend$PercChangeMedianCatch<-100*(TimeTrend$MedianCatch/Baseline$MedianCatch-1)
    
    TimeTrend$PercChangeMedianBiomass<-100*(TimeTrend$MedianBvBmsy/Baseline$MedianBvBmsy-1) # should this be % Change in Median Status?
    
  # add absolute changes
  
    TimeTrend$AbsChangeTotalProfits<-TimeTrend$TotalProfits-Baseline$TotalProfits
  
    TimeTrend$AbsChangeTotalCatch<-TimeTrend$TotalCatch-Baseline$TotalCatch
  
    TimeTrend$AbsChangeTotalBiomass<-TimeTrend$TotalBiomass-Baseline$TotalBiomass
  
    TimeTrend$AbsChangeMedianProfits<-TimeTrend$MedianProfits-Baseline$MedianProfits
  
    TimeTrend$AbsChangeMedianCatch<-TimeTrend$MedianCatch-Baseline$MedianCatch
  
    TimeTrend$AbsChangeMedianBiomass<-TimeTrend$MedianBvBmsy-Baseline$MedianBvBmsy
  
    
    #     TimeTrend$PercChangeFromSQMedianBiomass<-100*(TimeTrend$MedianBvBmsy/TimeTrend$MedianBvBmsy[TimeTrend$Policy=='SQ']-1)
    
    Cumulatives<- ddply(TimeTrend[TimeTrend$Year>BaselineYear,],c('Policy'),summarize,NPV=sum(DiscProfits,na.rm=T),Food=sum(TotalCatch,na.rm=T),Fish=sum(TotalBiomass,na.rm=T),
                        MedianProfits=sum(MedianProfits,na.rm=T),MedianCatch=sum(MedianCatch,na.rm=T),MedianBiomass=sum(MedianBvBmsy,na.rm=T))
    
    CumeNumbers<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbersAbs<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbers<- 100*(t(t(CumeNumbers)/CumeNumbers[which(Cumulatives$Policy=='SQ'),])-1) # percents
    
    CumeNumbersAbs<- t(t(CumeNumbersAbs)-CumeNumbersAbs[which(Cumulatives$Policy=='SQ'),]) # absolutes  
  
    Cumulatives[,2:dim(Cumulatives)[2]]<- CumeNumbers   
  
    Cumulatives[,8:13]<-CumeNumbersAbs
    colnames(Cumulatives)[8:13]<-paste("Abs",colnames(CumeNumbersAbs),sep="")
    
    Cumulatives[,colnames(Cumulatives)=="AbsMedianBiomass"]<-Cumulatives$AbsMedianBiomass/(ProjectionTime)
    
    Cumulatives<- Cumulatives[Cumulatives$Policy!='SQ' & Cumulatives$Policy!='Historic',]
    
    FinalYear<- TimeTrend[TimeTrend$Year==max(TimeTrend$Year),]  
    
    FinalYear$PercChangeFromSQMedianBiomass<- 100*(FinalYear$MedianBvBmsy/FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ']-1)
    
    FinalYear$PercChangeFromSQMedianProfits<- 100*(FinalYear$MedianProfits/FinalYear$MedianProfits[FinalYear$Policy=='SQ']-1)
    
    FinalYear$PercChangeFromSQMedianCatch<- 100*(FinalYear$MedianCatch/FinalYear$MedianCatch[FinalYear$Policy=='SQ']-1)
    
    FinalYear$PercChangeFromSQTotalBiomass<- 100*(FinalYear$TotalBiomass/FinalYear$TotalBiomass[FinalYear$Policy=='SQ']-1)
    
    FinalYear$PercChangeFromSQTotalProfits<- 100*(FinalYear$TotalProfits/FinalYear$TotalProfits[FinalYear$Policy=='SQ']-1)
    
    FinalYear$PercChangeFromSQTotalCatch<- 100*(FinalYear$TotalCatch/FinalYear$TotalCatch[FinalYear$Policy=='SQ']-1)
    
    # Add columns to FinalYear for absolute changes
    
    FinalYear$AbsChangeFromSQMedianBiomass<- FinalYear$MedianBvBmsy-FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ']
    
    FinalYear$AbsChangeFromSQMedianProfits<- FinalYear$MedianProfits-FinalYear$MedianProfits[FinalYear$Policy=='SQ']
    
    FinalYear$AbsChangeFromSQMedianCatch<- FinalYear$MedianCatch-FinalYear$MedianCatch[FinalYear$Policy=='SQ']
    
    FinalYear$AbsChangeFromSQTotalBiomass<- FinalYear$TotalBiomass-FinalYear$TotalBiomass[FinalYear$Policy=='SQ']
    
    FinalYear$AbsChangeFromSQTotalProfits<- FinalYear$TotalProfits-FinalYear$TotalProfits[FinalYear$Policy=='SQ']
    
    FinalYear$AbsChangeFromSQTotalCatch<- FinalYear$TotalCatch-FinalYear$TotalCatch[FinalYear$Policy=='SQ']
    
    # Populate Chris's Data Table
    
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
    
  # Analyze Database Composition --------------------------------------------
    
    FullTallies<- ddply(FullData[FullData_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='FullData')
    
    BiomassTallies<- ddply(BiomassData[Biomass_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='BiomassData')
    
    ProjectionTallies<- ddply(ProjectionData[Proj_CountryLocater,],c('Year','Dbase'),summarize,Fisheries=length(unique(IdOrig)),Catch=sum(Catch,na.rm=T),Source='ProjectionData')
    
    Tallies<- rbind(FullTallies,BiomassTallies,ProjectionTallies)
    
    
    pdf(file=paste(FigureFolder,CountriesToRun[c],' Database Analysis.pdf',sep='')) 
    
    print(dotplot(Fisheries ~ Dbase | Source,data=Tallies,subset=Year==BaselineYear,cex=2)  )  
    
    print(dotplot(Catch ~ Dbase | Source,data=Tallies,subset=Year==BaselineYear,cex=2)   ) 
    
    print(xyplot(Catch ~ Year | Source,group=Dbase,data=Tallies,subset=Year>=1950 & Year<= BaselineYear,cex=1,type='l',lwd=3,auto.key=T))  
    
    print(xyplot((Fisheries) ~ Year | Source,group=Dbase,data=Tallies,subset=Year>=1950 & Year<= BaselineYear,cex=1,type='l',lwd=3,auto.key=T))   
    
    dev.off()
    
    
    # Save  Results --------------------------------------------------
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Policy Projections.csv',sep=''),TimeTrend)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Baseline.csv',sep=''),Baseline)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Biomass Status.csv',sep=''),BiomassStatus$Data)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Raw Projection Data.csv',sep=''),TempProjectionData)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Current Year Data.csv',sep=''),FinalYear)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Business As Usual Data.csv',sep=''),Cumulatives)    
    
    BaselineYear<- 2005
    
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




