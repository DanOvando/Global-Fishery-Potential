######################################
#Summarize Results--------------------------------------------------
# This code produces standard summary tables and figures using the main dataframe
######################################

AnalyzeFisheries<- function(Data,BatchName,GroupingVars,Years) 
{
  
#   Data<- BiomassData
#   
#   BatchName<- 'Testing'
#   
#   GroupingVars<- c('Country','Year')
  Data<- Data[Data$Year %in% Years,]
  
  CatchStats<- list()
  
  # Analyze Catch Statistics ------------------------------------------------

  
  CatchStats$Catch<- ddply(Data,.(),summarize,NumberOfStocks=length(unique(IdOrig)),MeanCatch=mean(Catch,na.rm=T),
                MedianCatch=median(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),SDofCatch=sd(Catch,na.rm=T))
  
  CatchStats$YearCatch<- ddply(Data,.(Year),summarize,NumberOfStocks=length(unique(IdOrig)),MeanCatch=mean(Catch,na.rm=T),
                    MedianCatch=median(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),SDofCatch=sd(Catch,na.rm=T))
  
  CatchStats$CountryYearCatch<- ddply(Data,.(Country,Year),summarize,NumberOfStocks=length(unique(IdOrig)),MeanCatch=mean(Catch,na.rm=T),
                           MedianCatch=median(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),SDofCatch=sd(Catch,na.rm=T))
  
  pdf(file=paste(FigureFolder,BatchName,'.pdf',sep=''))
  plot(CatchStats$YearCatch$Year,CatchStats$YearCatch$TotalCatch,type='b',xlab='Year',ylab='Total Catch (tons)')
  plot(CatchStats$YearCatch$Year,CatchStats$YearCatch$MeanCatch,type='b',xlab='Year',ylab='Mean Catch (tons)')
  dev.off()
  
  return(list(CatchStats=CatchStats))
  
} #Close function 

