TimeToRecover<-function(Data,Policy,BaselineYear)
{   
  stocks<-unique(Data$IdOrig[Data$Year==BaselineYear & Data$BvBmsy<=0.8])
  
  Data<-Data[Data$IdOrig %in% stocks,]
  
  Data<-Data[Data$Policy==Policy,]
  
  TimeToRecover<-data.frame(matrix(NA,nrow=length(stocks),ncol=4))
  colnames(TimeToRecover)<-c('IdOrig','Year','BvBmsy','TimeToRecover')
  
  for (a in 1:length(stocks))
  {
    temp<-Data[Data$IdOrig==stocks[a] & Data$BvBmsy>=0.8,c('IdOrig','Year','BvBmsy')]
    
    if(nrow(temp)>0)
    {
      TimeToRecover$TimeToRecover[a]<-min(temp$Year)-BaselineYear
      TimeToRecover$IdOrig[a]<-stocks[a]
      TimeToRecover$Year[a]<-min(temp$Year)
      TimeToRecover$BvBmsy[a]<-temp$BvBmsy[temp$Year==min(temp$Year)]
    }
    
    if(nrow(temp)==0)
    {
      TimeToRecover$TimeToRecover[a]<-48
      TimeToRecover$IdOrig[a]<-stocks[a]
      TimeToRecover$Year[a]<-'Not Recovered'
      TimeToRecover$BvBmsy[a]<-max(Data$BvBmsy[Data$IdOrig==stocks[a]])
    }      
    
    # show(a)
  }
  return(TimeToRecover)
}