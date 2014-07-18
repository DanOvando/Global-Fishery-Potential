######################################
#Maid Service--------------------------------------------------
# This code performs a number of cleaning and processing steps on the database 
######################################

MaidService<- function(Data)
{
  
  # Data<- FullData
  
  Data$SpeciesCatName[Data$SpeciesCatName=='']<- NA
  
  Data$Keep<- 1
  
  # Assess catch and life history information in each stock -----------------
  
  StockStats<- ddply(Data,~IdOrig,summarise,MeanCatch=mean(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
                     TooFewCatchYears=sum(is.na(Catch)==F)<MinimumCatchYears,
                     PercentMissingTooHigh=(sum(is.na(Catch))/length(Catch))>=MissingCatchTolerance
                     ,SpeciesCatName=unique(SpeciesCatName))
  
  StockStats$NoSpeciesCategory<- is.na(StockStats$SpeciesCatName)
  
  StockStats$WrongSpeciesCategory<- (StockStats$SpeciesCatName %in% SpeciesCategoriesToOmit)
  
  StockStats$NotAllowedIn<- (StockStats[,IdVar] %in% FisheriesToOmit)
  
  StockStats$DropFishery<- 0
  
  ## Mark fisheries that need to be dropped 
  StockStats$DropFishery[StockStats$NoSpeciesCategory  | StockStats$WrongSpeciesCategory 
                         | StockStats$NotAllowedIn | StockStats$TooFewCatchYears |
                           StockStats$PercentMissingTooHigh]<- 1
  
  DroppedStocks<- StockStats[StockStats$DropFishery==1,]
  
  Data$Drop<- Data[,IdVar] %in% DroppedStocks[,IdVar]
  
  Data<- Data[Data$Drop==F,] #Remove unusable fisheries
  
  return(list(CleanedData=Data,DroppedStocks= DroppedStocks))
  
}


