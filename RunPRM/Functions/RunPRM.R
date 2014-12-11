RunPRM<- function(catchhistory)
{
  
  TransIterations<- 1000 #Leave this at 1000
  
  testvariance<- 0 #for reasons to long to explain, you have to include a variable called testvariance=0 in this section
  
  #   catchhistory<- catchhistory
  
  colnames(catchhistory)<- c('IdOrig','SciName','Year','Catch')
  
  
  FullNames<- c('IdOrig', 'CommName',	'Year',	'Fmort',	'BvBmsy',	'Biomass',	'Catch',	'Id',
                'Dbase',	'BiomassMetric',	'BiomassUnit',	'FmortMetric',	'FmortUnit',	'ExploitStatus',	'VonBertK',	
                'VonBertKSource',	'VonBertKUnit',	'Temp',	'TempSource',	'TempUnit',	'MaxLength',	'MaxLengthSource',
                'MaxLengthUnit',	'AgeMat',	'AgeMatSource',	'AgeMatUnit',
                'RegionFAO',	'Bmsy',	'SSBmsy',	'Fmsy',	'Umsy',	'SciName',	'CatchUnit',	'Country',
                'MSY',	'UvUmsytouse',	'ReferenceBiomass',	'ReferenceBiomassUnits')
  
  Newcatchhistory<- as.data.frame(matrix(NA,nrow=dim(catchhistory)[1],ncol=length(FullNames)))
  
  colnames(Newcatchhistory)<-FullNames
  
  Newcatchhistory$IdOrig<- catchhistory$IdOrig
  
  Newcatchhistory$SciName<- catchhistory$SciName
  
  Newcatchhistory$Year<- catchhistory$Year
  
  Newcatchhistory$Catch<- catchhistory$Catch
  
  Fisheries<- unique(Newcatchhistory$IdOrig)
  
  DependentVariable<- 'BvBmsy' #Dependent variable in regression
  
  IsLog<- TRUE #Should dependent variable be logged?
  
  DependentName<- DependentVariable
  
  DependentName<- if (IsLog==T){paste('Log',DependentVariable,sep='')}
  
  CatchLags<- 4 #Number of years of lagged catch to create for regression
  
  LifeHistoryVars<- c('MaxLength','AgeMat','VonBertK','Temp') #Life history variables to include for potential regression
  
  IdVar<- 'IdOrig' #Id variable to use in regressions
  
  CatchVariables<- c('YearsBack','ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'MaxCatch','TimeToMaxCatch','InitialScaledCatchSlope'
                     ,'MeanScaledCatch','CatchToRollingMax')
  
  # Regressions<- list(M1=c(DependentName,'Year',CatchVariables,LifeHistoryVars,'SpeciesCatName'),M2=c(DependentName,'Year',CatchVariables,'MaxLength','AgeMat','VonBertK','SpeciesCatName'),
  #                    M3=c(DependentName,'Year',CatchVariables,'MaxLength','VonBertK','SpeciesCatName'),M4=c(DependentName,'Year',CatchVariables,'VonBertK','SpeciesCatName'),M6=c(DependentName,'Year',CatchVariables,'SpeciesCatName'),M7=c(DependentName,CatchVariables))  
  
  
  Regressions<- list(M1=c(DependentName,CatchVariables,LifeHistoryVars,'SpeciesCatName'),M2=c(DependentName,CatchVariables,'MaxLength','AgeMat','VonBertK','SpeciesCatName'),
                     M3=c(DependentName,CatchVariables,'MaxLength','VonBertK','SpeciesCatName'),M4=c(DependentName,CatchVariables,'VonBertK','SpeciesCatName'),M6=c(DependentName,CatchVariables,'SpeciesCatName'),M7=c(DependentName,CatchVariables))  
  
  
  TransbiasBin<- 0.9
  
  TransbiasIterations<- 500
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes 
  
  colnames(Spec_ISSCAAP)<- c('SpeciesCat','SciName','CommName','Family','Order')
  
  GroupNames_ISSCAAP<-read.csv("Data/ISSCAAP Codes.csv",stringsAsFactors=F)
  
  colnames(GroupNames_ISSCAAP)<- c('SpeciesCat','SpeciesCatName')
  
  SpeciesCats<- join(GroupNames_ISSCAAP,Spec_ISSCAAP,by='SpeciesCat')
  
  Data<- join(Newcatchhistory,SpeciesCats,by='SciName')
  
  for (l in 1:length(LifeHistoryVars))
  {
    Data[,colnames(Data) == LifeHistoryVars[l]]<- as.numeric(Data[,colnames(Data) == LifeHistoryVars[l]])
    
  }
  
  FormatRegressionResults<- lapply(1:(length(Fisheries)), FormatForRegression,Data=Data,Fisheries=Fisheries,DependentVariable=DependentVariable,CatchVariables=CatchVariables,CatchLags=CatchLags,LifeHistoryVars=LifeHistoryVars,IsLog=IsLog,IdVar=IdVar) 
  
  Data <- ldply (FormatRegressionResults, data.frame)
  
  load('Data/PrmRegressions.Rdata')
  
  RealModelFactorLevels<- NULL
  
  Models<- names(Regressions)
  
  # Process Regressions -----------------------------------------------------
  
  ## Determine species category levels that were used in each model run
  ModelNames<- names(Regressions)
  
  TempOmitted<- NULL
  for (m in 1:length(names(Regressions)))
  {
    Model<- names(Regressions)[m]
    eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))
  }
  
  Data<- subset(Data,is.na(SpeciesCatName)==F)
  
  for (m in 1:length(Models)) #Apply models to species level fisheries
  {
    
    TempModelName<- Models[m]
    
    TempData<- Data
    
    eval(parse(text=paste('TempModel<- RealModels$',TempModelName,sep='')))
    
    if (TempModelName!='M7')
    {
      
      eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))
      
      ProxyCats<- AssignNearestSpeciesCategory(TempData,TempLevel,AllPossible)
      
      TempData<- ProxyCats$Data
    }
    
    Predictions<- predict(TempModel,TempData)
    
    eval(parse(text=paste('Data$',TempModelName,'Prediction<- Predictions',sep='')))    
  }
  
  BiomassColumns<- (grepl('BvBmsy',colnames(Data)) | grepl('Prediction',colnames(Data))) & grepl('LogBvBmsy',colnames(Data))==F
  
  BioNames<- colnames(Data)[BiomassColumns]
  
  HasBiomass<- rowSums(is.na(Data[,BiomassColumns]))<length(BioNames)
  
  BiomassData<- Data[HasBiomass,] #Only store fisheries that have some form of biomass estimates
  
  MissingData<- Data[HasBiomass==F,]
  
  AvailableBio<- (BiomassData[,BiomassColumns])
  
  AvailableBioMarker<- matrix(rep((1:dim(AvailableBio)[2]),dim(AvailableBio)[1]), dim(AvailableBio)[1],dim(AvailableBio)[2],byrow=TRUE)
  
  AvailableBioMarker<- AvailableBioMarker*(is.na(AvailableBio)==F)
  
  AvailableBioMarker[AvailableBioMarker==0]<- NA
  
  BestModel<- apply(AvailableBioMarker,1,min,na.rm=T)
  
  BestBio<- NULL
  for (b in 1:dim(AvailableBio)[1])
  {
    BestBio[b]<- as.numeric(AvailableBio[b,BestModel[b]])
  }
  
  BestBio[BestModel==1]<- log(BestBio[BestModel==1])
  
  BestModelnames<- c('RAM',ModelNames)
  
  BestModelNames<- BestModelnames[sort(unique(BestModel))]
  
  BestModel<- as.factor((BestModel))
  
  levels(BestModel)<- BestModelNames
  
  BiomassData$BestModel<- BestModel
  
  BiomassData$BvBmsy<- BestBio
  
  
  BiomassData$SciName<- as.character((BiomassData$SciName))
  
  BiomassData$SpeciesCatName<- as.character(BiomassData$SpeciesCatName)
  
  WhereNeis<- (grepl('nei',BiomassData$CommName) | grepl('spp',BiomassData$SciName)) & grepl('not identified',BiomassData$SpeciesCatName)==F & (BiomassData$Dbase=='FAO') #Find unassessed NEIs
  
  WhereUnidentified<- grepl('not identified',BiomassData$SpeciesCatName)
  
  WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level
  
  BiomassData$IdLevel<- 'Species'
  
  BiomassData$IdLevel[WhereNeis]<- 'Neis'
  
  BiomassData$IdLevel[WhereUnidentified]<- 'Unidentified'
  
  BiomassData$IdLevel[WhereSpeciesLevel]<- 'Species'
  
  #   BiomassData<- AssignEconomicData(BiomassData) #Assign price and cost data to each stock
  
  BiomassData$Price<-NA # Price and BvBmsyOpenAccess variable must be created before Analyze fisheries. Will be filled later by Assign EconData
  
  BiomassData$BvBmsyOpenAccess<-NA
  
  BiomassData$RanCatchMSY<- F
  
  # Run First Analisis of Current Status --------------------------------------------------
  
  BiomassData$CatchMSYBvBmsy_LogSd<- NA
  
  BiomassData$Dbase<- 'FAO'
  
  Status<- AnalyzeFisheries(BiomassData,'Summaries','Year',min(BiomassData$Year):max(BiomassData$Year),RealModelSdevs,RealModelSdevs,TransbiasBin,TransbiasIterations)
  
  # arg<- Status$Individuals[,3:dim(Status$Individuals)[2]]  
  #   
  # a<-   apply(arg,1,mean,na.rm=T)
  # 
  # b<-   t(apply(arg,1,quantile,probs=c(0.025,0.25,0.75,0.975)))
  # 
  # h<- cbind(Status$Individuals[,1:2],a,b)
  
  Summary<- Status$Data[,c('IdOrig','SciName','Year','Catch','BvBmsy','BvBmsySD','BestModel')]
  
  colnames(Summary)<- c('id','sname','year','catch','BvBmsy','BvBmsy_LogSD','BestModel')
  
  
  return(list(PredictedBvBmsy=Summary,AllResults=Data))
}