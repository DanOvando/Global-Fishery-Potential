######################################
#Summarize Results--------------------------------------------------
# This code produces standard summary tables and figures using the main dataframe
######################################

AnalyzeFisheries<- function(Data,BatchName,GroupingVars,Years,RealModelSdevs,NeiModelSdevs,TransbiasBin,J) 
{
  
#    Data<- BiomassData[BiomassData$Dbase=='RAM',]
# #   
#    BatchName<- 'Testing'
# #   
#    GroupingVars<- c('Country','Year')
# 
#   Years<- 1980:2010
# 
# RealModelSdevs<- RealModelSdevs
# 
# NeiModelSdevs<- NeiModelSdevs
# 
# TransbiasBin<- TransbiasBin
# 
# J<- TransbiasIterations

  Data<- Data[Data$Year %in% Years,]
  
  CatchStats<- list()
  
  pdf(file=paste(FigureFolder,BatchName,'.pdf',sep=''))
  
  # General Summary Stats ---------------------------------------------------
  
  SummaryStats<- list()
    
  SummaryStats$FirstYear<- min(Data$Year)

  SummaryStats$LastYear<- max(Data$Year)
  
  SummaryStats$DataBases<- ddply(Data,.(Year,Dbase),summarize,Count=length(unique(IdOrig)))
  
  SummaryStats$IdLevel<- ddply(Data,.(Year,IdLevel),summarize,Count=length(unique(IdOrig)))

SummaryStats$SpeciesCats<- ddply(Data,.(Year,SpeciesCatName),summarize,Count=length(unique(IdOrig)))


  # Analyze Catch Statistics ------------------------------------------------



  CatchStats$FisherySizes<- ddply(Data,.(IdOrig),summarize,LifetimeCatch=sum(Catch,na.rm=T)) 
  
  plot(cumsum(sort(CatchStats$FisherySizes$LifetimeCatch,decreasing=T)),xlab='Fishery',ylab='Cumulative Catch (MT)')
  
  CatchStats$Catch<- ddply(Data,.(),summarize,NumberOfStocks=length(unique(IdOrig)),MeanCatch=mean(Catch,na.rm=T),
                MedianCatch=median(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),SDofCatch=sd(Catch,na.rm=T))
  
  CatchStats$YearCatch<- ddply(Data,.(Year),summarize,NumberOfStocks=length(unique(IdOrig)),MeanCatch=mean(Catch,na.rm=T),
                    MedianCatch=median(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),SDofCatch=sd(Catch,na.rm=T))
  
  CatchStats$CountryYearCatch<- ddply(Data,.(Country,Year),summarize,NumberOfStocks=length(unique(IdOrig)),MeanCatch=mean(Catch,na.rm=T),
                           MedianCatch=median(Catch,na.rm=T),TotalCatch=sum(Catch,na.rm=T),SDofCatch=sd(Catch,na.rm=T))
  
  plot(CatchStats$YearCatch$Year,CatchStats$YearCatch$TotalCatch,type='b',xlab='Year',ylab='Total Catch (MT)')
  plot(CatchStats$YearCatch$Year,CatchStats$YearCatch$MeanCatch,type='b',xlab='Year',ylab='Mean Catch (MT)')
  
  # Analyze B/Bmsy Statistics ----------------------------------------------
  
BioStats<- list()

TempBio<- exp(Data$BestBio)

for (x in 1)
{
if (any(Data$Dbase=='FAO' & is.na(Data$BestBio)==F))
{  
  
  IdLevels<- unique(Data$IdLevel)
  
  FaoIndividualStocks<- as.data.frame(matrix(NA,nrow=0,ncol=J+2))
  
  for (i in 1:length(IdLevels))
  {
  
    Where<- Data$IdLevel==IdLevels[i] & Data$Dbase=='FAO'
    
    if (IdLevels[i]=='Species')
    {
      Sdevs<- RealModelSdevs
    }
    else
    {
      Sdevs<- NeiModelSdevs
    }
    
  TempTransbiasResults<- TransBias(Data[Where,],Sdevs,TransbiasBin,J)
  
  TempBio[Where]<- TempTransbiasResults$Individuals$raw
  
  FaoIndividualStocks<- rbind(FaoIndividualStocks,TempTransbiasResults$DynamicDistribution)
  
  }
  
  NotFao<- Data$Dbase!= 'FAO'
  
  colnames(FaoIndividualStocks)<- c('Year','Id',paste('It',1:J,sep=''))
  
  RamAndSofia<- data.frame(Data$Year[NotFao],Data$IdOrig[NotFao],as.data.frame(matrix(rep(Data$BestBio[NotFao],J),nrow=sum(NotFao),ncol=J,byrow=F)))
  
  colnames(RamAndSofia)<- c('Year','Id',paste('It',1:J,sep=''))
  
  Individuals<- rbind(FaoIndividualStocks,RamAndSofia)

#   Individuals<- (FaoIndividualStocks)
     
  MedianSeries<- as.data.frame(matrix(NA,nrow=length(Years),ncol=J))
  for (y in 1:length(Years) )
  {
    
    Where<- Individuals$Year==Years[y]    
#     Argh<- (Individuals[1:3,1:(dim(Individuals)[2])])
#     quartz()
#     hist(exp(Argh[,4]))
    
    MedianSeries[y,]<- apply(exp(Individuals[Where,3:(dim(Individuals)[2])]),2,median,na.rm=T)    
  }

MedianSeries<- t(apply(MedianSeries,1,sort))

MeanMedian<- apply(MedianSeries,1,mean)

Quantiles<- t(apply(MedianSeries,1,quantile,probs=c(0.025,.25,.75,0.975)))

BioStats$Median<- data.frame(Years,MeanMedian,Quantiles)  

colnames(BioStats$Median)<- c('Year','MeanMedian',paste('Q',100*c(0.025,.25,.75,0.975),sep=''))

plot(BioStats$Median$Year,BioStats$Median$MeanMedian,type='b',lwd=2,xlab='Year',ylab='Median B/Bmsy',pty='m')
polygon(x=c(BioStats$Median$Year,rev(BioStats$Median$Year)),y=c(BioStats$Median$Q97.5,rev(BioStats$Median$Q2.5)),
        col="lightsteelblue2",border=F)
lines(BioStats$Median$Year,BioStats$Median$MeanMedian,type='b',lwd=2)
legend('topright',legend='95% Confidence Range of Median',fill='lightsteelblue2')

}
else
{
  
  BioStats$Median<- ddply(Data,.(Year),summarize,Median=median(exp(BestBio)),Q2.5=quantile(exp(BestBio),c(0.025)),Q25=quantile(exp(BestBio),c(0.25)),
                     Q75=quantile(exp(BestBio),c(0.75)),Q97.5=quantile(exp(BestBio),c(0.975)))
  
  
  plot(BioStats$Median$Year,BioStats$Median$Median,type='b',lwd=2,xlab='Year',ylab='B/Bmsy',pty='m',ylim=c(0,3))
  polygon(x=c(BioStats$Median$Year,rev(BioStats$Median$Year)),y=c(BioStats$Median$Q75,rev(BioStats$Median$Q25)),
          col="lightsteelblue2",border=F)
  lines(BioStats$Median$Year,BioStats$Median$Median,type='b',lwd=2)
  legend('topright',legend='Interquantile Range',fill='lightsteelblue2')
  
  
}
}

SummaryStats$IdLevel$Year<- as.factor(SummaryStats$IdLevel$Year)

SummaryStats$DataBases$Year<- as.factor(SummaryStats$DataBases$Year)

SummaryStats$SpeciesCats$Year<- as.factor(SummaryStats$SpeciesCats$Year)

# pdf(file=paste(FigureFolder,BatchName,' IdLevels.pdf',sep=''))
print(dotplot(IdLevel~Count | Year,data=SummaryStats$IdLevel,xlab='Number of Fisheries',ylab='Identification Level'))

# dev.off()

# pdf(file=paste(FigureFolder,BatchName,'Databases.pdf',sep=''))
print(dotplot(Dbase~Count | Year,data=SummaryStats$DataBases,xlab='Number of Fisheries',ylab='Database'))
# dev.off()

# pdf(file=paste(FigureFolder,BatchName,'SpeciesCats.pdf',sep=''))
print(dotplot(SpeciesCatName~Count | Year,data=SummaryStats$SpeciesCats,xlab='Number of Fisheries',ylab='Species Category'))
# dev.off()
dev.off()

  return(list(CatchStats=CatchStats,Data=Data,BioStats=BioStats,SummaryStats=SummaryStats))
  
} #Close function 



