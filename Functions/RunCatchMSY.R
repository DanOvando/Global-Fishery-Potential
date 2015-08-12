######################################
#Calculate MSY--------------------------------------------------
#This code runs CatchMSY on fisheries
###################################### 

RunCatchMSY<- function(Data,ErrorSize = 0.85,sigR = 0,Smooth = F,Display = F,BestValues = 1,ManualFinalYear = 0,n,NumCPUs,CatchMSYTrumps = T)
{
  
  Data$RanCatchMSY<- FALSE
  
  Data$HasRamMSY<-  is.na(Data$MSY)==F & Data$Dbase=='RAM'
  
  Data$HasRamFvFmsy<- is.na(Data$FvFmsy)==F & Data$Dbase=='RAM'
  
  Data$HasRamBvBmsy<- is.na(Data$BvBmsy)==F & Data$Dbase=='RAM'
  
  Data$BtoKRatio<- 1/((Data$phi+1)^(1/Data$phi))
  
  MsyData<- Data
  
  MsyData$g<- NA
  
  MsyData$k<- NA
  
  MsyData$MSYLogSd<- NA
  
  MsyData$gLogSd<- NA
  
  MsyData$KLogSd<- NA
  
  MsyData$CatchMSYBvBmsy<- NA
  
  MsyData$CatchMSYBvBmsy_LogSd<- NA
  
  CommonError<- mean(MsyData$BvBmsySD,na.rm=T)
  
  if (is.na(CommonError))
  {
    CommonError<- ErrorSize
  }
  
  # find mean range between final bio priors to pass to SnowCatchMSY_Matrix for stocks with finalbio>1
  MeanRange<-MsyData[is.na(MsyData$BvBmsySD)==F & MsyData$Year==2012,c('IdOrig','BvBmsy','BvBmsySD','BtoKRatio')]
  
  MeanRange$BoverK<-pmin(1,MeanRange$BvBmsy*MeanRange$BtoKRatio)
  
  MeanRange<-MeanRange[MeanRange$BoverK<0.95,]
  
  MeanRange$Bioerror<-MeanRange$BvBmsySD*MeanRange$BtoKRatio
  
  MeanRange$Bioerror[is.na(MeanRange$Bioerror)]<-CommonError
  
  MeanRange$FbLow<-pmax(0,qnorm(0.25,MeanRange$BoverK,MeanRange$Bioerror))
  
  MeanRange$FbHigh<-pmin(1,qnorm(0.75,MeanRange$BoverK,MeanRange$Bioerror))
  
  MeanRange$BioRange<-MeanRange$FbHigh-MeanRange$FbLow
  
  CommonRange<-mean(MeanRange$BioRange,na.rm=T) # Common range to apply to all stocks with B/K >=0.95
  
  stock_id <- unique((Data[,IdVar][Data$HasRamMSY==F & Data$BvBmsy!=999 & is.infinite(Data$BvBmsy)==F])) 
  
  if (NumCPUs>1)
  {
    
    if (Sys.info()[1]!='Windows')
    {
      
      CMSYResults <- (mclapply(1:length(stock_id), MatrixSnowCatchMSY,mc.cores=NumCPUs,Data=Data,CommonError=CommonError,CommonRange=CommonRange,sigR=sigR,Smooth=Smooth,Display=Display,BestValues=BestValues,ManualFinalYear=ManualFinalYear,n=n,NumCPUs=NumCPUs,
                             CatchMSYTrumps=CatchMSYTrumps,stock_id=stock_id,IdVar=IdVar))
    }
    if (Sys.info()[1]=='Windows')
    {
      
      sfInit( parallel=TRUE, cpus = NumCPUs)
      
      sfExportAll()
      
      sfLibrary(dplyr)
      
      CMSYResults <- (sfClusterApplyLB(1:length(stock_id), MatrixSnowCatchMSY,Data=Data,CommonError=CommonError,CommonRange=CommonRange,sigR=sigR,Smooth=Smooth,Display=Display,BestValues=BestValues,ManualFinalYear=ManualFinalYear,n=n,NumCPUs=NumCPUs,
                               CatchMSYTrumps=CatchMSYTrumps,stock_id=stock_id,IdVar=IdVar))
      
      sfStop()
    }
  }
  if (NumCPUs==1)
  {    
    pdf(file=paste(FigureFolder,'Catch-MSY Diagnostics.pdf',sep=''))
    
    CMSYResults <- (mclapply(1:length(stock_id), MatrixSnowCatchMSY,mc.cores=NumCPUs,Data=Data,CommonError=CommonError,CommonRange=CommonRange,sigR=sigR,Smooth=Smooth,Display=Display,BestValues=BestValues,ManualFinalYear=ManualFinalYear,n=n,NumCPUs=NumCPUs,
                             CatchMSYTrumps=CatchMSYTrumps,stock_id=stock_id,IdVar=IdVar))
    
    #     CMSYResults <- (mclapply(1:length(stock_id), SnowCatchMSY,mc.cores=NumCPUs,Data=Data,CommonError=CommonError,sigR=sigR,Smooth=Smooth,Display=Display,BestValues=BestValues,ManualFinalYear=ManualFinalYear,n=n,NumCPUs=NumCPUs,
    #                              CatchMSYTrumps=CatchMSYTrumps,stock_id=stock_id,IdVar=IdVar))
    
    
    dev.off()
    
    #     pdf(file=paste(FigureFolder,'Catch-MSY Diagnostics Normal.pdf',sep=''))
    #     
    #     
    #     CMSYResults <- (mclapply(1, SnowCatchMSY,mc.cores=NumCPUs,Data=Data,CommonError=CommonError,sigR=sigR,Smooth=Smooth,Display=Display,BestValues=BestValues,ManualFinalYear=ManualFinalYear,n=n,NumCPUs=NumCPUs,
    #                              CatchMSYTrumps=CatchMSYTrumps,stock_id=stock_id,IdVar=IdVar))    
    #   
    #     dev.off()
  }
  
  CmsyStore<- as.data.frame(matrix(NA,nrow=0,ncol=dim(CMSYResults[[1]]$CatchMSY)[2]))
  
  PossibleParams <- lapply(seq(along = CMSYResults), function(i)    CMSYResults[[i]]$PossibleParams)
  
  EmptyParams <- lapply(seq(along = PossibleParams), function(i)   sum(is.na(PossibleParams[[i]]))==0)
  
  HasData<- ldply(EmptyParams)
  
  PossibleParams<- PossibleParams[which(HasData==T)]
  
  CmsyStore <- lapply(seq(along = CMSYResults), function(i)    CMSYResults[[i]]$CatchMSY)
  
  PossibleParams<- ldply(PossibleParams)
  if (dim(PossibleParams)[1]>0 & sum(PossibleParams$Fail==0,na.rm=T)>=1)
  {
    PossibleParams<- PossibleParams[,c('IdOrig','g','phi','K','MSY','FinalFvFmsy','FinalBvBmsy')]
  }
  
  CmsyStore<- ldply(CmsyStore)
  
  ConCatDat<- paste(MsyData$IdOrig,MsyData$Year,sep='-')
  
  ConCatCmsy<- paste(CmsyStore$IdOrig,CmsyStore$Year,sep='-')
  
  Where<- ConCatDat %in% ConCatCmsy
  
  MsyData[Where,]<- CmsyStore
  
  return(list(MsyData=MsyData,PossibleParams=PossibleParams)) 
} #Close function





