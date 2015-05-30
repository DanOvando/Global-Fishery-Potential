ExtendTimeSeries<- function(s,Data,BaselineYear,ExtendFAO)
{
  
  
  source('Functions/RepMat.R')
  
  VarNames<- colnames(Data)
  
#   NewData<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]))
#   
#   colnames(NewData)<- VarNames
  
  Stocks<- (unique(Data$IdOrig))
  
  #   for (s in 1:length(Stocks))
  #   {
  #     
  Where<- Data$IdOrig==Stocks[s]
  
  TempStock<- Data[Where,]
  
  MaxYear<- max(TempStock$Year[is.na(TempStock$Catch)==F],na.rm=T)
  
  if (any(Data$Dbase[Where]=='RAM') | any(Data$Dbase[Where]=='SOFIA'))
  {
    MaxYear<- max(TempStock$Year[is.na(TempStock$Catch)==F & is.na(TempStock$BvBmsy)==F],na.rm=T)
    
  }
  
  TempStock<- TempStock[1:which(TempStock$Year==MaxYear),]
  
  TempStock$ExtendedTime<- FALSE
  
  MissingYears<- max(0,(BaselineYear-MaxYear))
  
  if (any(TempStock$Dbase=='FAO') & ExtendFAO==F)
  {
    MissingYears<- 0
  }
  
  for (i in 1)
    if (MissingYears>0)
    {
      ReppedData<- RepMat(TempStock[TempStock$Year==MaxYear,],MissingYears)
      
      ReppedData$Year<- MaxYear+(1:(MissingYears))
      
      ReppedData$ExtendedTime<- TRUE
      
#       NewData<- rbind(NewData,rbind(TempStock,ReppedData))
      NewData<- rbind(TempStock,ReppedData)
      
    }   
  else
  {
    #     NewData<- rbind(NewData,TempStock)
    NewData<- TempStock
  }
  
  
  
  # show(paste(round(100*(s/length(Stocks))), '% Done With Time Series Extension',sep=''))
  write.table(paste(round(100*(s/length(Stocks))), '% Done With Time Series Extension',sep=''), file = 'ExtendTimeSeriesProgress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
  
  
  #   colnames(NewData)<- c(VarNames,'ExtendedTime')
  return(NewData)
}