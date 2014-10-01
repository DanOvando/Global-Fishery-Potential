######################################
#Maid Service--------------------------------------------------
# This code performs a number of cleaning and processing steps on the database 
######################################

MaidService<- function(Data,OverlapMode)
{
  #For Commiting
#          Data<- FullData
  
  Data$SpeciesCatName[Data$SpeciesCatName=='']<- NA
  
  Data$Keep<- 1
  
  Data$BvBmsy[Data$BvBmsy>OutlierBvBmsy & Data$Dbase=='RAM']<- NA
  
  # Assess catch and life history information in each stock -----------------
  
  StockStats<- ddply(Data,~IdOrig,summarise,MeanCatch=mean(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
                     TooFewCatchYears=sum(is.na(Catch)==F)<MinimumCatchYears,
                     PercentMissingTooHigh=(sum(is.na(Catch))/length(Catch))>=MissingCatchTolerance,NoCatch=sum(Catch,na.rm=T)==0
                     ,SpeciesCatName=unique(SpeciesCatName),NoRamOrSofiaBiomass=(as.numeric(any(Dbase=='RAM') | any(Dbase=='SOFIA'))*as.numeric(sum(is.na(BvBmsy)==F)==0))==1)
  
  StockStats$NoSpeciesCategory<- is.na(StockStats$SpeciesCatName)
  
  StockStats$WrongSpeciesCategory<- (StockStats$SpeciesCatName %in% SpeciesCategoriesToOmit)
  
  StockStats$DropFishery<- 0
  
  ## Mark fisheries that need to be dropped 
  StockStats$DropFishery[StockStats$NoSpeciesCategory  | StockStats$WrongSpeciesCategory 
                         | StockStats$TooFewCatchYears |
                           StockStats$PercentMissingTooHigh | StockStats$NoCatch | StockStats$NoRamOrSofiaBiomass==T]<- 1
  
  DroppedStocks<- StockStats[StockStats$DropFishery==1,]
  
  Data$Drop<- Data[,IdVar] %in% DroppedStocks[,IdVar]
  
  Data<- Data[Data$Drop==F,] #Remove unusable fisheries
  
  Data<- LumpFisheries(Data,SpeciesCategoriesToLump)
  
  
  if (CommonFinalYear==T)
  {
    Data<- ExtendTimeSeries(Data,BaselineYear)
  }
  
  Data<- FormatForRegression(Data,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)#Add resgression data to database
  
  Overlap<- RemoveOverlap(Data,OverlapMode)
  
  Data<-Overlap$FilteredData
  
  AllOverlap<-Overlap$AllOverlapFinal
  
  OverlapToRemove<- c('Ram','Sofia','SofiaRam')
  
  return(list(CleanedData=Data,DroppedStocks= DroppedStocks,AllOverlap=AllOverlap))
  
}


