RunIUUDiagnostic<- function(Data,Regressions,IUULevel,CMSYIterations)
{
  Data<- MsyData
  
  Regressions<- RealReg
  
#   save(RealModels,RealModelSdevs,file=paste(ResultFolder,'PrmRegressions.Rdata',sep=''))
    
    IUULevel<- 1.2
  
  NumCatchMSYIterations<- 5000
  
  ErrorSize=.95
  
  TransbiasIterations<- 1000
  
  sigR<- 0
  
  ## Store Current Values
  
IUUData<- Data

  ## Apply Regression
  
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
    
    ProxyCats<- AssignNearestSpeciesCategory(IUUData,TempLevel,AllPossible)
    
    Predictions<- predict(TempModel,ProxyCats$Data)
    
    eval(parse(text=paste('IUUData$',TempModelName,'Prediction<- Predictions',sep='')))    
  }
  
  
  ## Run CatchMSY
  
  ## Store results
  
  
  
}