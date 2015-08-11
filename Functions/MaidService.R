######################################
#Maid Service--------------------------------------------------
# This code performs a number of cleaning and processing steps on the database 
######################################

MaidService<- function(Data,OverlapMode,BaselineYear)
{
  #     Data<- FullData
  
  Data$SpeciesCatName[Data$SpeciesCatName=='']<- NA
  
  Data$Keep<- 1
  
  Data$BvBmsy[Data$BvBmsy>OutlierBvBmsy & Data$Dbase=='RAM']<- NA
  
  # Assess catch and life history information in each stock -----------------
  StockStats<- Data %>%
    group_by(IdOrig) %>%
    summarise(MeanCatch=mean(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
              TooFewCatchYears=sum(is.na(Catch)==F)<MinimumCatchYears,
              PercentMissingTooHigh=(sum(is.na(Catch))/length(Catch))>=MissingCatchTolerance,NoCatch=sum(Catch,na.rm=T)==0
              ,SpeciesCatName=unique(SpeciesCatName),NoRamOrSofiaBiomass=((any(Dbase=='RAM') | any(Dbase=='SOFIA')) & any(is.na(BvBmsy)==F)==F))
  #   StockStats<- ddply(Data,~IdOrig,summarise,MeanCatch=mean(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
  #                      TooFewCatchYears=sum(is.na(Catch)==F)<MinimumCatchYears,
  #                      PercentMissingTooHigh=(sum(is.na(Catch))/length(Catch))>=MissingCatchTolerance,NoCatch=sum(Catch,na.rm=T)==0
  #                      ,SpeciesCatName=unique(SpeciesCatName),NoRamOrSofiaBiomass=(as.numeric(any(Dbase=='RAM') | any(Dbase=='SOFIA'))*as.numeric(sum(is.na(BvBmsy)==F)==0))==1)
  #   
  
  StockStats$NoSpeciesCategory<- is.na(StockStats$SpeciesCatName)
  
  StockStats$WrongSpeciesCategory<- (StockStats$SpeciesCatName %in% SpeciesCategoriesToOmit)
  
  StockStats$DropFishery<- 0
  
  ## Mark fisheries that need to be dropped 
  StockStats$DropFishery[StockStats$NoSpeciesCategory  | StockStats$WrongSpeciesCategory 
                         | StockStats$TooFewCatchYears |
                           StockStats$PercentMissingTooHigh | StockStats$NoCatch | StockStats$NoRamOrSofiaBiomass==T]<- 1
  
  DroppedStocks<- StockStats[StockStats$DropFishery==1,]
  
  Data$Drop<- Data$IdOrig %in% DroppedStocks$IdOrig
  
  Data<- Data[Data$Drop==F,] #Remove unusable fisheries
  
  #   Data<- LumpFisheries(Data,SpeciesCategoriesToLump)
  
  Stocks<- (unique(Data$IdOrig))
  
  
  if (CommonFinalYear==T)
  {
    
    if(Sys.info()[1]!='Windows')
    {
      ExtendResults <- (mclapply(1:(length(Stocks)), ExtendTimeSeries,mc.cores=NumCPUs,Data,BaselineYear,ExtendFAO=F))      
      
    }
    if(Sys.info()[1]=='Windows')
    {
      
      sfInit( parallel=Parel, cpus=NumCPUs,slaveOutfile="ExtendTimeSeriesProgress.txt" )
      
      sfExportAll()
      
      ExtendResults <- (sfClusterApplyLB(1:(length(Stocks)), ExtendTimeSeries,Data = Data, BaselineYear = BaselineYear, ExtendFAO = F )) 
      
      sfStop()
    }
    
    Data <- ldply (ExtendResults, data.frame)
    
    show('Timeseries extended')
    
  }
  Overlap<- RemoveOverlap(Data,OverlapMode)
  
  Data<-Overlap$FilteredData
  
  #   Data$Country[Data$Dbase=="SOFIA" & grepl(", ",Data$Country)==T] <- "Multinational" # rename Country for multinational Sofia stocks to "Multinational"
  StitchedData<- LumpFisheries(Data,SpeciesCategoriesToLump)
  
  Data<-StitchedData$StitchedData
  
  StitchIds<-StitchedData$StitchIds
  
  # FIlter out bad data once again ------------------------------------------
  
  StockStats<- Data %>%
    group_by(IdOrig) %>%
    summarise(MeanCatch=mean(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
              TooFewCatchYears=sum(is.na(Catch)==F)<MinimumCatchYears,
              PercentMissingTooHigh=(sum(is.na(Catch))/length(Catch))>=MissingCatchTolerance,NoCatch=sum(Catch,na.rm=T)==0
              ,SpeciesCatName=unique(SpeciesCatName),NoRamOrSofiaBiomass=((any(Dbase=='RAM') | any(Dbase=='SOFIA')) & any(is.na(BvBmsy)==F)==F))
  
  
  
  #   
  #   StockStats<- ddply(Data,~IdOrig,summarise,MeanCatch=mean(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
  #                      TooFewCatchYears=sum(is.na(Catch)==F)<MinimumCatchYears,
  #                      PercentMissingTooHigh=(sum(is.na(Catch))/length(Catch))>=MissingCatchTolerance,NoCatch=sum(Catch,na.rm=T)==0
  #                      ,SpeciesCatName=unique(SpeciesCatName),NoRamOrSofiaBiomass=(as.numeric(any(Dbase=='RAM') | any(Dbase=='SOFIA'))*as.numeric(sum(is.na(BvBmsy)==F)==0))==1)
  #   
  StockStats$NoSpeciesCategory<- is.na(StockStats$SpeciesCatName)
  
  StockStats$WrongSpeciesCategory<- (StockStats$SpeciesCatName %in% SpeciesCategoriesToOmit)
  
  StockStats$DropFishery<- 0
  
  ## Mark fisheries that need to be dropped 
  StockStats$DropFishery[StockStats$NoSpeciesCategory  | StockStats$WrongSpeciesCategory 
                         | StockStats$TooFewCatchYears |
                           StockStats$PercentMissingTooHigh | StockStats$NoCatch | StockStats$NoRamOrSofiaBiomass==T]<- 1
  
  DroppedStocks2<- StockStats[StockStats$DropFishery==1,]
  Data$Drop<- NULL
  Data$Drop<- Data$IdOrig %in% DroppedStocks2$IdOrig
  
  Data<- Data[Data$Drop==F,] #Remove unusable fisheries
  
  Fisheries<- (unique(Data$IdOrig))
  
  FormatRegressionResults<- lapply(1:(length(Fisheries)), FormatForRegression,Data=Data,Fisheries=Fisheries,DependentVariable=DependentVariable,CatchVariables=CatchVariables,CatchLags=CatchLags,LifeHistoryVars=LifeHistoryVars,IsLog=IsLog,IdVar=IdVar) 
  #   FormatRegressionResults<- mclapply(97, FormatForRegression,mc.cores=NumCPUs,Data=Data,Fisheries=Fisheries,DependentVariable=DependentVariable,CatchVariables=CatchVariables,CatchLags=CatchLags,LifeHistoryVars=LifeHistoryVars,IsLog=IsLog,IdVar=IdVar) 
  
  
  Data <- ldply (FormatRegressionResults, data.frame)
  
  
  AllOverlap<-Overlap$AllOverlap
  
  MultinationalOverlap<-Overlap$MultinationalOverlap
  
  OverlapToRemove<- c('Ram','Sofia','SofiaRam')
  
  return(list(CleanedData=Data,DroppedStocks= DroppedStocks,AllOverlap=AllOverlap,StitchIds=StitchIds,MultinationalOverlapIds=MultinationalOverlap,DroppedStocks2= DroppedStocks2))
  
}


