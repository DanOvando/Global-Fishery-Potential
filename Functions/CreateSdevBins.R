######################################
#CreateSdevBins--------------------------------------------------
#This code calcualtes the standard deviation bins for required for the transbias code
######################################
CreateSdevBins<- function(Models,Data,BinBreak)
{
  
#   Models<- NeiModels
#   
#   Data<- SyntheticData
#   
  ModelNames<- names(Models)
  
  SdevBins<- vector('list',length(Models))
  
  names(SdevBins)<- ModelNames
  
  for (m in 1:length(ModelNames))
  {
    
    TempModelName<- ModelNames[m]
    
    eval(parse(text=paste('TempPrediction<- Models$',TempModelName,'$fitted.values',sep='')))

    eval(parse(text=paste('TempResiduals<- Models$',TempModelName,'$residuals',sep='')))

    eval(parse(text=paste('TempFisheriesMarker<- Data$',TempModelName,'Marker',sep='')))
    
    TempFisheries<- Data[TempFisheriesMarker,]
    
    TempData<- as.data.frame(cbind(TempPrediction,TempResiduals,TempFisheries$Year))
    
    colnames(TempData)<- c('Prediction','Residual','Year')
    
    ModelStdevSummary<- TempData %>% 
      group_by(Year) %>% 
      summarise(Stdev=sd(Residual))

    HighBModelStdev<- TempData %>% 
      filter(Prediction>log(BinBreak)) %>% 
      group_by(Year) %>% 
      summarise(Stdev=sd(Residual))

    LowBModelStdev<- TempData %>% 
      filter(Prediction<=log(BinBreak)) %>% 
      group_by(Year) %>% 
      summarise(Stdev=sd(Residual))
    
    TempSdevBins<- merge(HighBModelStdev,LowBModelStdev,by='Year')
    
    colnames(TempSdevBins)<- c('Year','HighBvBmsy','LowBvBmsy')
        
    SdevBins[[m]]<- TempSdevBins
  
  }  
  
  return(SdevBins)
} #Close function loop


