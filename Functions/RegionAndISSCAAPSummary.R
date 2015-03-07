####################################################
##
## Summarize status by ISSCAAP category and Fao Region
##
#####################################################

RegionFaoAndISSCAAPSummary<-function(ProjectionData,BaselineYear)
{
  data<-ProjectionData[ProjectionData$CanProject==T,]
  
  ### Current status by ISSCAAP category---------------------------------------------------
  
  current<-ddply(data[data$Year==BaselineYear,],c('SpeciesCatName'),summarize,Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
                 MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  
  current<-current[with(current,order(-TotalMSY)),]
  
  write.csv(current,file=paste(ResultFolder,'Current Status by ISSCAAP Category.csv',sep=''))
  
  # Status through time
  
  trend<-ddply(data[data$Year %in% c(1950:BaselineYear),],c('Year','SpeciesCatName'),summarize,Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
               MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  
  pdf(file=paste(FigureFolder,'Median Status by ISSCAAP.pdf',sep=''),width=16,height=10) 
  
  print(ggplot(trend,aes(Year,MedianB)) +
          geom_line() +
          facet_wrap(~SpeciesCatName) + 
          geom_abline(intercept=1,slope=0) +
          labs(x='Year',y='Median Status',title='Median Status by ISSCAAP Category'))
  
  dev.off()
  
  ### Current status by FAO region-----------------------------------------------------------
  
  regs<-unique(data$RegionFAO[data$Dbase=='FAO'])
  
  RegionStatus<-data.frame(matrix(NA,nrow=length(regs),ncol=3))
  
  colnames(RegionStatus)<-c('RegionFAO','MedianBvBmsy','MedianFvFmsy')
  
  for(c in 1:length(regs))
  {
    temp<-data[data$Year==BaselineYear & grepl(regs[c],data$RegionFAO),]
    
    MakeKobePlot(temp,BaselineYear,FigureName=paste('FAO Region ',regs[c],' Kobe Plot.pdf',sep=''))
    
    temp<-ddply(temp,c('Year'),summarize,MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T))
        
    RegionStatus$RegionFAO[c]<-regs[c]
    
    RegionStatus$MedianBvBmsy[c]<-temp$MedianB
    
    RegionStatus$MedianFvFmsy[c]<-temp$MedianF 
  }
  
  write.csv(RegionStatus,file=paste(ResultFolder,'Status by FAO Region.csv',sep=''))
  
  return(list(StatusByISSCAAP=current,StatusByRegion=RegionStatus))
}