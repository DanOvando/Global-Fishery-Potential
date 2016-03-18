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
  
  RegionStatus<-data.frame(matrix(NA,nrow=length(regs),ncol=7))
  
  colnames(RegionStatus)<-c('RegionFAO','MedianBvBmsy','MedianFvFmsy','CatchWtMeanB','CatchWtMeanF', 'PercBelowBof1','PercBelowFof1')
  
  for(c in 1:length(regs))
  {
    temp<-data[data$Year==BaselineYear & grepl(regs[c],data$RegionFAO),]
    
    #MakeKobePlot(temp,BaselineYear,FigureName=paste('FAO Region ',regs[c],' Kobe Plot.pdf',sep=''))
    
    # temp<-ddply(temp,c('Year'),summarize,MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T))
    
    temp<- temp %>%
      mutate(bxcatch=BvBmsy*Catch,fxcatch=FvFmsy*Catch) %>%
      group_by(Year) %>%
      summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),
                WtMeanB=sum(bxcatch,na.rm=T)/sum(Catch,na.rm=T),WtMeanF=sum(fxcatch,na.rm=T)/sum(Catch,na.rm=T),
                PercBelowBof1=100*(length(BvBmsy[BvBmsy<1])/length(BvBmsy)),PercBelowFof1=100*(length(FvFmsy[FvFmsy<1])/length(FvFmsy))) %>%
      ungroup()
    
    RegionStatus$RegionFAO[c]<-regs[c]
    
    RegionStatus$MedianBvBmsy[c]<-temp$MedianB
    
    RegionStatus$MedianFvFmsy[c]<-temp$MedianF
    
    RegionStatus$CatchWtMeanB[c]<-temp$WtMeanB
    
    RegionStatus$CatchWtMeanF[c]<-temp$WtMeanF
    
    RegionStatus$PercBelowBof1[c]<-temp$PercBelowBof1
    
    RegionStatus$PercBelowFof1[c]<-temp$PercBelowFof1
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
  justY  <-seq(1,length(codes),4)
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
      
      KobeMedians<- BaselineData %>%
        mutate(bxcatch=BvBmsy*Catch,fxcatch=FvFmsy*Catch) %>%
        group_by(Year) %>%
        summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),
                  WtMeanB=sum(bxcatch,na.rm=T)/sum(Catch,na.rm=T),WtMeanF=sum(fxcatch,na.rm=T)/sum(Catch,na.rm=T))
      
      BaselineData$FvFmsy[BaselineData$FvFmsy>4]<- 4
      
      BaselineData$BvBmsy[BaselineData$BvBmsy>2.5]<- 2.5
      
      DensityData<- (kde2d(BaselineData$BvBmsy,BaselineData$FvFmsy,n=100,lims=c(0,2.5,0,4)))
      
      FTrend<- ddply(temp,c('Year','Dbase'),summarize,FTrend=mean(FvFmsy,na.rm=T))
      
      KobeColors<- colorRampPalette(c("powderblue", "white",'#FFFF99'))
      
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
      
      inCols<-as.numeric(as.factor(BaselineData$Dbase))		# inCols should be the current colors
      OutCols<-inCols
      
      # RGBcols	<-c("#000000","#FF0000","#00CC00") #black, red, green
      RGBcols<-c('#A0A0A0','#FC6969','#0B610B')
      AddTrans	<-99		# adjusts the transparency--lower is more transparent
      
      for(x in 1:3)
        OutCols[inCols==x]<-paste(RGBcols[x],AddTrans,sep="")
      
      points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=16,col=OutCols,cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
      points(KobeMedians$MedianB,KobeMedians$MedianF,pch=17,col=RGBcols[3], cex=1.5)
      points(KobeMedians$WtMeanB,KobeMedians$WtMeanF,pch=15,col=RGBcols[3], cex=1.5)
      box()
      abline(h=1,v=1,lty=2)
      legend("topright",tempName,bty='n')
      # legend('center',legend=unique(BaselineData$Dbase),pch=16,col=(as.factor(unique(BaselineData$Dbase))),cex=0.7,bty='n') 
    } 
    #     show(a)
  }
  
  dev.off()

  # ####################################################
  # ##
  # ## Make Figure One Kobe Plot
  # ##
  # #####################################################
  
  zone<-c('Global','Northeast Pacific','Northeast Atlantic','Western Central Pacific')
  
  codes<-c('all','67','27','71') # codes
  
  names<-data.frame(zone,codes,stringsAsFactors=F) # df of names and corresponding codes for every FAO major region
  
  #==the ones that work...wtf
  justX	<-4
  justY	<-1
  noAx	<-2
  bothAx	<-3
  
  pdf(file=paste(FigureFolder,'Figure One Kobe Plots.pdf',sep=''))
  
  par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,1,1))
  
  for(a in 1:length(codes))
  {
    if(codes[a]=='all') 
      {
      temp<- data
      
      tempName<-names$zone[names$code==codes[a]]
      }
    
    if(codes[a]!='all')
    {
      temp<-data[data$Year==BaselineYear & grepl(codes[a],data$RegionFAO),]
      
      tempName<-names$zone[names$code==codes[a]]
    }
    
    if(nrow(temp)>0)
    {
      
      if (any(temp$BvBmsy<0,na.rm=T)){temp$BvBmsy<- exp(temp$BvBmsy)}
      
      BaselineData<- subset(temp,subset=Year==BaselineYear & is.na(FvFmsy)==F & is.na(BvBmsy)==F)
      
      KobeMedians<- BaselineData %>%
        mutate(bxcatch=BvBmsy*Catch,fxcatch=FvFmsy*Catch) %>%
        group_by(Year) %>%
        summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),
                  WtMeanB=sum(bxcatch,na.rm=T)/sum(Catch,na.rm=T),WtMeanF=sum(fxcatch,na.rm=T)/sum(Catch,na.rm=T))
      
      BaselineData$FvFmsy[BaselineData$FvFmsy>4]<- 4
      
      BaselineData$BvBmsy[BaselineData$BvBmsy>2.5]<- 2.5
      
      DensityData<- (kde2d(BaselineData$BvBmsy,BaselineData$FvFmsy,n=100,lims=c(0,2.5,0,4)))
      
      FTrend<- ddply(temp,c('Year','Dbase'),summarize,FTrend=mean(FvFmsy,na.rm=T))
      
      KobeColors<- colorRampPalette(c("powderblue", "white",'#FFFF99'))
      
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
      
      inCols<-as.numeric(as.factor(BaselineData$Dbase))		# inCols should be the current colors
      OutCols<-inCols
      
      # RGBcols	<-c("#000000","#FF0000","#00CC00") #black, red, green
      RGBcols<-c('#A0A0A0','#FC6969','#0B610B')
      AddTrans	<-99		# adjusts the transparency--lower is more transparent
      
      for(x in 1:3)
        OutCols[inCols==x]<-paste(RGBcols[x],AddTrans,sep="")
      
      points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=16,col=OutCols,cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
      points(KobeMedians$MedianB,KobeMedians$MedianF,pch=17,col=RGBcols[3], cex=1.5)
      points(KobeMedians$WtMeanB,KobeMedians$WtMeanF,pch=15,col=RGBcols[3], cex=1.5)
      box()
      abline(h=1,v=1,lty=2)
      legend("topright",tempName,bty='n')
      # legend('center',legend=unique(BaselineData$Dbase),pch=16,col=(as.factor(unique(BaselineData$Dbase))),cex=0.7,bty='n') 
    } 
    #     show(a)
  }
  
  dev.off()


  # ####################################################
  # ##
  # ## Make SI Global Kobe Plot
  # ##
  # #####################################################
  
  zone<-c('Global','Northeast Pacific','Northeast Atlantic','Western Central Pacific')
  
  codes<-c('all','67','27','71') # codes
  
  names<-data.frame(zone,codes,stringsAsFactors=F) # df of names and corresponding codes for every FAO major region
  
  pdf(file=paste(FigureFolder,'SI Global Kobe.pdf',sep=''))
  
  par(mfrow=c(1,1),mar=c(.1,.1,.1,.1),oma=c(4,4,1,1))
  
  for(a in 1:1) # only loop over global
  {
    if(codes[a]=='all') 
    {
      temp<- data
      
      tempName<-names$zone[names$code==codes[a]]
    }
    
    if(codes[a]!='all')
    {
      temp<-data[data$Year==BaselineYear & grepl(codes[a],data$RegionFAO),]
      
      tempName<-names$zone[names$code==codes[a]]
    }
    
    if(nrow(temp)>0)
    {
      
      if (any(temp$BvBmsy<0,na.rm=T)){temp$BvBmsy<- exp(temp$BvBmsy)}
      
      BaselineData<- subset(temp,subset=Year==BaselineYear & is.na(FvFmsy)==F & is.na(BvBmsy)==F)
      
      KobeMedians<- BaselineData %>%
        mutate(bxcatch=BvBmsy*Catch,fxcatch=FvFmsy*Catch) %>%
        group_by(Year) %>%
        summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),
                  WtMeanB=sum(bxcatch,na.rm=T)/sum(Catch,na.rm=T),WtMeanF=sum(fxcatch,na.rm=T)/sum(Catch,na.rm=T))
      
      BaselineData$FvFmsy[BaselineData$FvFmsy>4]<- 4
      
      BaselineData$BvBmsy[BaselineData$BvBmsy>2.5]<- 2.5
      
      DensityData<- (kde2d(BaselineData$BvBmsy,BaselineData$FvFmsy,n=100,lims=c(0,2.5,0,4)))
      
      FTrend<- ddply(temp,c('Year','Dbase'),summarize,FTrend=mean(FvFmsy,na.rm=T))
      
      KobeColors<- colorRampPalette(c("powderblue", "white",'#FFFF99'))
      
      DbaseColors<- colorRampPalette(c('Red','Black','Green'))
      
      image(DensityData$x,DensityData$y,DensityData$z,xlim=c(0,2.5),ylim=c(0,4),col=KobeColors(50),xaxt='n',yaxt='n',bty='o',las=1)

        axis(side=2,at=seq(1,3),las=1)      
        axis(side=1,at=seq(0,2,.5),las=1)
        mtext(outer=T,side=1,expression(B/B[MSY]),line=2)
        mtext(outer=T,side=2,expression(F/F[MSY]),line=2)
      
      
      inCols<-as.numeric(as.factor(BaselineData$Dbase))		# inCols should be the current colors
      OutCols<-inCols
      
      # RGBcols	<-c("#000000","#FF0000","#00CC00") #black, red, green
      # RGBcols<-c('#A0A0A0','#FC6969','#0B610B')
      RGBcols<-c('#000000','#FF0000','#0B610B')
      
      AddTrans	<-99		# adjusts the transparency--lower is more transparent
      
      for(x in 1:3)
        OutCols[inCols==x]<-paste(RGBcols[x],AddTrans,sep="")
      
      points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=16,col=OutCols,cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
      points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=1,col=OutCols,cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
#       points(KobeMedians$MedianB,KobeMedians$MedianF,pch=17,col=RGBcols[3], cex=1.5)
#       points(KobeMedians$WtMeanB,KobeMedians$WtMeanF,pch=15,col=RGBcols[3], cex=1.5)
      box()
      abline(h=1,v=1,lty=2,lwd=3)
      # legend("topright",tempName,bty='n')
      # legend('center',legend=unique(BaselineData$Dbase),pch=16,col=(as.factor(unique(BaselineData$Dbase))),cex=0.7,bty='n') 
    } 
    #     show(a)
  }
  
  dev.off()
  
    
  return(list(StatusByISSCAAP=current,StatusByRegion=RegionStatus))
}



# ####################################################
# ##
# ## Summarize status by ISSCAAP category and Fao Region
# ##
# #####################################################
# 
# RegionFaoAndISSCAAPSummary<-function(ProjectionData,BaselineYear)
# {
#   data<-ProjectionData[ProjectionData$CanProject==T,]
#   
#   ### Current status by ISSCAAP category---------------------------------------------------
#   
#   current<-ddply(data[data$Year==BaselineYear,],c('SpeciesCatName'),summarize,Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
#                  MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
#   
#   current<-current[with(current,order(-TotalMSY)),]
#   
#   write.csv(current,file=paste(ResultFolder,'Current Status by ISSCAAP Category.csv',sep=''))
#   
#   # Status through time
#   
#   trend<-ddply(data[data$Year %in% c(1950:BaselineYear),],c('Year','SpeciesCatName'),summarize,Stocks=length(unique(IdOrig)),MedianB=median(BvBmsy,na.rm=T),
#                MedianF=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
#   
#   pdf(file=paste(FigureFolder,'Median Status by ISSCAAP.pdf',sep=''),width=16,height=10) 
#   
#   print(ggplot(trend,aes(Year,MedianB)) +
#           geom_line() +
#           facet_wrap(~SpeciesCatName) + 
#           geom_abline(intercept=1,slope=0) +
#           labs(x='Year',y='Median Status',title='Median Status by ISSCAAP Category'))
#   
#   dev.off()
#   
#   ### Current status by FAO region-----------------------------------------------------------
#   
#   regs<-unique(data$RegionFAO[data$Dbase=='FAO'])
#   
#   RegionStatus<-data.frame(matrix(NA,nrow=length(regs),ncol=3))
#   
#   colnames(RegionStatus)<-c('RegionFAO','MedianBvBmsy','MedianFvFmsy')
#   
#   for(c in 1:length(regs))
#   {
#     temp<-data[data$Year==BaselineYear & grepl(regs[c],data$RegionFAO),]
#     
#     MakeKobePlot(temp,BaselineYear,FigureName=paste('FAO Region ',regs[c],' Kobe Plot.pdf',sep=''))
#     
#     temp<-ddply(temp,c('Year'),summarize,MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T))
#         
#     RegionStatus$RegionFAO[c]<-regs[c]
#     
#     RegionStatus$MedianBvBmsy[c]<-temp$MedianB
#     
#     RegionStatus$MedianFvFmsy[c]<-temp$MedianF 
#   }
#   
#   write.csv(RegionStatus,file=paste(ResultFolder,'Status by FAO Region.csv',sep=''))
#   
#   return(list(StatusByISSCAAP=current,StatusByRegion=RegionStatus))
# }