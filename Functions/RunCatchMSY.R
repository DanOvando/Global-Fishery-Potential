######################################
#Calculate MSY--------------------------------------------------
#This code runs CatchMSY on fisheries
###################################### 

RunCatchMSY<- function(Data,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,n,NumCPUs,CatchMSYTrumps)
{
  
#         Data<- GlobalStatus$Data
  
  Data$RanCatchMSY<- FALSE
  
  Data$HasRamMSY<-  is.na(Data$MSY)==F
  
  Data$HasRamFvFmsy<- is.na(Data$FvFmsy)==F

  Data$HasRamBvBmsy<- is.na(Data$BvBmsy)==F & Data$Dbase=='RAM'
  
  MsyData<- Data
  
  MsyData$r<- NA
  
  MsyData$k<- NA
  
  MsyData$MSYLogSd<- NA
  
  MsyData$rLogSd<- NA
  
  MsyData$KLogSd<- NA
  
  MsyData$CatchMSYBvBmsy<- NA
  
  MsyData$CatchMSYBvBmsy_LogSd<- NA
  
  CommonError<- mean(MsyData$BvBmsySD,na.rm=T)
  
  stock_id <- unique((Data[,IdVar][Data$HasRamMSY==F & Data$BvBmsy!=999 & is.infinite(Data$BvBmsy)==F])) 
  
  
  
  if (NumCPUs>1)
  {
    sfInit( parallel=TRUE, cpus=NumCPUs,slaveOutfile="SnowfallMSY_Progress.txt" )
    
    sfExport('Data','ErrorSize','CommonError','sigR','Smooth','Display','BestValues','ManualFinalYear','n','NumCPUs','CatchMSYTrumps','stock_id','IdVar')
    
    CMSYResults <- (sfClusterApplyLB(1:(length(stock_id)), SnowCatchMSY))
    sfStop()
  }
  if (NumCPUs==1)
  {
    pdf(file=paste(FigureFolder,'Catch-MSY Diagnostics.pdf',sep=''))
    CMSYResults<- lapply(1:(length(stock_id)/10),SingleCatchMSY,stock_id=stock_id,Data=Data,ErrorSize=ErrorSize,sigR=sigR,Smooth=Smooth,Display=Display,BestValues=BestValues,ManualFinalYear=ManualFinalYear,n=n,NumCPUs=NumCPUs,
                         CatchMSYTrumps=CatchMSYTrumps,IdVar=IdVar)                
    dev.off()
  }
  
  
  
  
  CmsyStore<- as.data.frame(matrix(NA,nrow=0,ncol=dim(CMSYResults[[1]]$CatchMSY)[2]))
  for (l in 1:length(CMSYResults))
  {
    CmsyResults<- CMSYResults[[l]]
    
    if (CmsyResults$RanCatchMSY==T)
      
    {
      CmsyStore<- rbind(CmsyStore,CmsyResults$CatchMSY)
    }
  }
  
  ConCatDat<- paste(MsyData$IdOrig,Data$Year,sep='-')
  
  ConCatCmsy<- paste(CmsyStore$IdOrig,CmsyStore$Year,sep='-')
  
  Where<- ConCatDat %in% ConCatCmsy
  
  MsyData[Where,]<- CmsyStore
  
  return(MsyData) 
} #Close function





