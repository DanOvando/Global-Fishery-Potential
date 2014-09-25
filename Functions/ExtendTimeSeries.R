ExtendTimeSeries<- function(Data,BaselineYear)
{
#   
#    Data<- NEIs
#   
#    BaselineYear<- 2026
  
  VarNames<- colnames(Data)
  
  NewData<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]))
  
  colnames(NewData)<- VarNames
  
  Stocks<- (unique(Data$IdOrig))
  
  for (s in 1:length(Stocks))
  {
    
    Where<- Data$IdOrig==Stocks[s]
    
    TempStock<- Data[Where,]
    
    MaxYear<- max(TempStock$Year[is.na(TempStock$Catch)==F],na.rm=T)
    
    TempStock<- TempStock[1:which(TempStock$Year==MaxYear),]
    
    TempStock$ExtendedTime<- FALSE
    
    MissingYears<- max(0,(BaselineYear-MaxYear))
    
    
    if (MissingYears>0)
    {
     
      ReppedData<- RepMat(TempStock[TempStock$Year==MaxYear,],MissingYears,'Rows')
      
      ReppedData$Year<- MaxYear+(1:(MissingYears))
      
      ReppedData$ExtendedTime<- TRUE
      
      NewData<- rbind(NewData,rbind(TempStock,ReppedData))
      
    }   
    else
    {
      NewData<- rbind(NewData,TempStock)
    }
    
    show(paste(round(100*(s/length(Stocks))), '% Done With Time Series Extension',sep=''))
  }
  
#   colnames(NewData)<- c(VarNames,'ExtendedTime')
  return(NewData)
}