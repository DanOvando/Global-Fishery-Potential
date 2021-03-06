####################################################
##
## Summarize status by ISSCAAP category and Fao Region
##
#####################################################
# library(plyr)
# library(MASS)
# ProjectionData<-read.csv(file=paste(ResultFolder,"Unlumped ProjectionData.csv",sep=''))
RegionFaoAndISSCAAPSummary<-function(ProjectionData,BaselineYear)
{
  data<-ProjectionData[ProjectionData$CanProject==T,]
  
  ### Current status by ISSCAAP category---------------------------------------------------
  
  current<- data[data$Year==BaselineYear,] %>%
    group_by(SpeciesCatName) %>%
    summarize(Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
              MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  #   
  #   current<-ddply(data[data$Year==BaselineYear,],c('SpeciesCatName'),summarize,Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
  #                  MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  
  current<-current[with(current,order(-TotalMSY)),]
  
  write.csv(current,file=paste(ResultFolder,'Current Status by ISSCAAP Category.csv',sep=''))
  
  # Status through time
  
  trend<- data[data$Year %in% c(1950:BaselineYear),] %>%
    group_by(Year,SpeciesCatName) %>%
    summarize(Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
              MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  
  #   trend<-ddply(data[data$Year %in% c(1950:BaselineYear),],c('Year','SpeciesCatName'),summarize,Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
  #                MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  #   
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
    
    #MakeKobePlot(temp,BaselineYear,FigureName=paste('FAO Region ',regs[c],' Kobe Plot.pdf',sep=''))
    
    temp<- temp %>%
      group_by(Year) %>%
      summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T))
    
    #     temp<-ddply(temp,c('Year'),summarize,MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T))
    
    RegionStatus$RegionFAO[c]<-regs[c]
    
    RegionStatus$MedianBvBmsy[c]<-temp$MedianB
    
    RegionStatus$MedianFvFmsy[c]<-temp$MedianF 
  }
  
  write.csv(RegionStatus,file=paste(ResultFolder,'Status by FAO Region.csv',sep=''))
  
  ### Kobe plots by Ocean-----------------------------------------------------------
  
  # Vector of region ids and names
  
  zone<-c('Arctic Sea','Northwest Atlantic','Northeast Atlantic','West Central Atlantic','Eastern Central Atlantic','Mediterranean and Black Sea',
          'Southwest Atlantic','Southeast Atlantic','Western Indian Ocean','Eastern Indian Ocean','Northwest Pacific','Northeast Pacific','Western Central Pacific','Eastern Central Pacific',
          'Southwest Pacific','Southeast Pacific','Atlantic Antarctic','Indian Ocean Antarctic','Pacific Antarctic')
  
  codes<-c('18','21','27','31','34','37','41','47','51','57','61','67','71','77','81','87','48','58','88') # codes
  
  names<-data.frame(zone,codes,stringsAsFactors=F) # df of names and corresponding codes for every FAO major region
  
  
  #==the 'right' indices 
  justY	<-seq(1,length(codes),4)
  noAx	<-seq(2,length(codes),4)
  bothAx	<-seq(3,length(codes),4)
  justX	<-seq(4,length(codes),4)
  #==the ones that work...wtf
  justX	<-seq(1,length(codes),4)
  justY	<-seq(2,length(codes),4)
  noAx	<-seq(3,length(codes),4)
  bothAx	<-seq(4,length(codes),4)
  
  pdf(file=paste(FigureFolder,'Kobe Plots by FAO Region.pdf',sep=''))
  
  par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,1,1))
  
  for(a in 1:length(codes))
  {
    temp<-data[data$Year==BaselineYear & grepl(codes[a],data$RegionFAO),]
    
    tempName<-names$zone[names$code==codes[a]]
    
    if(nrow(temp)>0)
    {
      
      if (any(temp$BvBmsy<0,na.rm=T)){temp$BvBmsy<- exp(temp$BvBmsy)}
      
      BaselineData<- subset(temp,subset=Year==BaselineYear & is.na(FvFmsy)==F & is.na(BvBmsy)==F)
      
      BaselineData$FvFmsy[BaselineData$FvFmsy>4]<- 4
      
      BaselineData$BvBmsy[BaselineData$BvBmsy>2.5]<- 2.5
      
      DensityData<- (kde2d(BaselineData$BvBmsy,BaselineData$FvFmsy,n=100,lims=c(0,2.5,0,4)))
      
      FTrend<- temp %>%
        group_by(Year,Dbase) %>%
        summarize(FTrend=mean(FvFmsy,na.rm=T))

#       FTrend<- ddply(temp,c('Year','Dbase'),summarize,FTrend=mean(FvFmsy,na.rm=T))
      
      KobeColors<- colorRampPalette(c("lightskyblue", "white",'gold1'))
      
      DbaseColors<- colorRampPalette(c('Red','Black','Green'))
      
      image(DensityData$x,DensityData$y,DensityData$z,xlim=c(0,2.5),ylim=c(0,4),col=KobeColors(50),xaxt='n',yaxt='n',bty='o',las=1)
      if(!is.na(match(a,justY)))
        axis(side=2,at=seq(1,3),las=1)
      if(!is.na(match(a,justX)))
        axis(side=1,at=seq(0,2,.5),las=1)
      if(!is.na(match(a,bothAx)))
      {
        axis(side=2,at=seq(1,3),las=1)      
        axis(side=1,at=seq(0,2,.5),las=1)
        mtext(outer=T,side=1,expression(B/B[MSY]),line=2)
        mtext(outer=T,side=2,expression(F/F[MSY]),line=2)
      }
      
      points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=16,col=(as.factor(BaselineData$Dbase)),cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
      box()
      abline(h=1,v=1,lty=2)
      legend("topright",tempName,bty='n')
      # legend('center',legend=unique(BaselineData$Dbase),pch=16,col=(as.factor(unique(BaselineData$Dbase))),cex=0.7,bty='n') 
    } 
    #     show(a)
  }
  
  dev.off()
  
  return(list(StatusByISSCAAP=current,StatusByRegion=RegionStatus))
}