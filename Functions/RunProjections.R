######################################
#Run Projections --------------------------------------------------
# This code projects fished populations forward under different policy scenarios 
######################################

RunProjection<- function(Data,BaselineYear,NumCPUs)
{
  
  # Data<-  MsyData[MsyData$CanProject==T,]
  #   Data<- MsyData[MsyData$CanProject==T & MsyData$IdOrig=='AFSC-REYEROCKBSAI-1974-2011-STACHURA',]
  
  #   #  
  #        BaselineYear<- 2011
  
  Data$MarginalCost<- NA
  
  #   Data$FvFmsy[Data$FvFmsy>=1.9]<- 1.9
  # 
  #   Data$BvBmsy[Data$BvBmsy>=1.9]<- 1.9
  
  
  
  
  # Loop over Each Stock ----------------------------------------------------
  
  Policies<- c('Opt','CatchShare','Food','SQ','Fmsy','CloseDown')
  
  Stocks<- unique(Data[Data$Year==BaselineYear,IdVar])
  
  Data<- Data[Data$Year<=BaselineYear,]
  
  #   TempStockMatrix<- as.data.frame(matrix(NA,nrow=length(Stocks)*ProjectionTime*10,ncol=dim(Data)[2]+2))
  
  TempStockMatrix<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]+2))
  
  
  colnames(TempStockMatrix)<- c(colnames(Data),'Policy','Profits')
  
  #   TempStockMatrix$IdOrig<- as.factor(TempStockMatrix$IdOrig)
  #   
  #   levels(TempStockMatrix$IdOrig)<- Stocks
  #   
  #   counter<- 1
  
  
  if (NumCPUs>1)
  {
    
    Projections <- (mclapply(1:(length(Stocks)), SnowProjections,mc.cores=NumCPUs,
                             Data=Data,BaselineYear=BaselineYear,Stocks=Stocks,IdVar=IdVar,bvec=bvec,
                             Discount=Discount,tol=tol,beta=beta,CatchSharePrice=CatchSharePrice,CatchShareCost=CatchShareCost,
                             Policies=Policies,ProjectionTime=ProjectionTime,TempStockMatrix=TempStockMatrix))
    
    #     sfInit( parallel=TRUE, cpus=NumCPUs )
    #     sfExport('Data','BaselineYear','Stocks','IdVar','bvec','Discount','tol','beta','CatchSharePrice','CatchShareCost','Policies','ProjectionTime','TempStockMatrix')
    #     #   sfExportAll()
    #     
    #     
    #     Projections <- (sfClusterApplyLB(1:(length(Stocks)), SnowProjections))
    #     sfStop()
  }
  if (NumCPUs==1)
  {    
    Projections <- (lapply(1:(length(Stocks)), SnowProjections,
                           Data=Data,BaselineYear=BaselineYear,Stocks=Stocks,IdVar=IdVar,bvec=bvec,
                           Discount=Discount,tol=tol,beta=beta,CatchSharePrice=CatchSharePrice,CatchShareCost=CatchShareCost,
                           Policies=Policies,ProjectionTime=ProjectionTime,TempStockMatrix=TempStockMatrix))
    
  }
  
  TempStockMatrix<- ldply(Projections,data.frame)
  
  Data$Policy<- NA
  
  
  Data$Profits<- NA
  
  Data$Policy[is.na(Data$Policy)]<- 'Historic'
  
  HistoricData<- Data$Policy=='Historic' 
  
  HistoricFData<- Data$Policy=='Historic' & Data$HasRamFvFmsy==F & is.na(Data$FvFmsy) & Data$Dbase!='RAM' #FIX THIS
  
  Data$FvFmsy[HistoricFData]<- (Data$Catch[HistoricFData]/Data$MSY[HistoricFData])/Data$BvBmsy[HistoricFData]
  
  c_num<- Data$Price[HistoricData] * (2-Data$BvBmsyOpenAccess[HistoricData]) * Data$BvBmsyOpenAccess[HistoricData] * Data$MSY[HistoricData]*2^beta  
  
  c_den<- ((2-Data$BvBmsyOpenAccess[HistoricData])*Data$r[HistoricData])^beta
  
  Costs<- c_num/c_den
  
  Data$MarginalCost[HistoricData]<- Costs
  
  Data$Profits[HistoricData]= Data$Price[HistoricData]*Data$MSY[HistoricData]*Data$FvFmsy[HistoricData]*Data$BvBmsy[HistoricData] - Data$MarginalCost[HistoricData]*(Data$FvFmsy[HistoricData]*Data$r[HistoricData]/2)^beta
  
  DataPlus<- rbind((Data),(TempStockMatrix))
  
  return(DataPlus)
  
}

