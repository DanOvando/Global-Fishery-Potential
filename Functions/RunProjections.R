######################################
#Run Projections --------------------------------------------------
# This code projects fished populations forward under different policy scenarios 
######################################

RunProjection<- function(Data,BaselineYear,NumCPUs)
{
  
  Data<-  MsyData[MsyData$CanProject==T,]
  #   #  
  #        BaselineYear<- 2011
  
  Data$MarginalCost<- NA
  
  
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
    sfInit( parallel=TRUE, cpus=NumCPUs )
    sfExport('Data','BaselineYear','Stocks','IdVar','bvec','Discount','tol','beta','CatchSharePrice','CatchShareCost','Policies','ProjectionTime','TempStockMatrix')
    #   sfExportAll()
    
    
    Projections <- (sfClusterApplyLB(1:(length(Stocks)), SnowProjections))
    sfStop()
  }
  if (NumCPUs==1)
  {
    
    Projections<- lapply(1:length(Stocks),SnowProjections)
  }
  
  
  
  for (l in 1:length(Projections))
  {
    
    
    TempStockMatrix<- rbind(TempStockMatrix,Projections[[l]])
    
  }
  
  
  Data$Policy<- NA
  
  
  Data$Profits<- NA
  
  Data$Policy[is.na(Data$Policy)]<- 'Historic'
  
  HistoricData<- Data$Policy=='Historic'
  
  Data$FvFmsy[HistoricData]<- (Data$Catch[HistoricData]/Data$MSY[HistoricData])/Data$BvBmsy[HistoricData]
  
  c_num<- Data$Price[HistoricData]* Data$MSY[HistoricData]*  (2-Data$BvBmsyOpenAccess[HistoricData]) * Data$BvBmsyOpenAccess[HistoricData]
  
  c_den<- ((2-Data$BvBmsyOpenAccess[HistoricData])*Data$r[HistoricData]/2)^beta
  
  Costs<- c_num/c_den
  
  Data$MarginalCost[HistoricData]<- Costs
  
  Data$Profits[HistoricData]= Data$Price[HistoricData]*Data$MSY[HistoricData]*Data$FvFmsy[HistoricData]*Data$BvBmsy[HistoricData] - Data$MarginalCost[HistoricData]*(Data$FvFmsy[HistoricData]*Data$r[HistoricData]/2)^beta
  
  DataPlus<- rbind((Data),(TempStockMatrix))
  
  return(DataPlus)
  
}
#   for (s in 1:length(Stocks))
#   {
#     
#     show(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''))
#     
#     Where<- Data[,IdVar]==Stocks[s]
#     
#     #   Where<- Data[,IdVar]== '11776-FAO-67-31'
#     
#     StockData<- Data[Where,]
#     
#     RecentStockData<-  StockData[dim(StockData)[1],]
#     
#     Price<- RecentStockData$Price
#     MSY<- RecentStockData$MSY
#     BOA<- RecentStockData$BvBmsyOpenAccess
#     r<- RecentStockData$r
#     FStatusQuo<- RecentStockData$FvFmsy
#     
#     #     FStatusQuo<- ((RecentStockData$Catch)/MSY)/RecentStockData$BvBmsy
#     
#     FStatusQuo[is.na(FStatusQuo)]<- 0
#     
#     Where<- Data[,IdVar]==Stocks[s]
#     
#     c_num <-  Price*(2-BOA)*BOA*MSY*2^beta
#     
#     c_den = ((2-BOA)*r)^beta
#     
#     cost = c_num/c_den
#     
#     Data$MarginalCost[Where]<- cost 
#     
#     MsyProfits = Price*MSY - cost*(r/2)^beta
#     
#     OptPolicy<-  RunDynamicOpt2(MSY,r,Price,cost,beta,Discount,bvec,tol)$Policy
#     
#     CatchSharePolicy<-  RunDynamicOpt2(MSY,r,CatchSharePrice*Price,CatchShareCost*cost,beta,Discount,bvec,tol)$Policy
#     
#     FoodPolicy<-  RunDynamicOpt2(MSY,r,1,0,beta,0,bvec,tol)$Policy
#     
#     SQPolicy<- FStatusQuo*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])
#     
#     FmsyPolicy<- matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])
#     
#     CloseDownPolicy<- bvec
#     
#     CloseDownPolicy[bvec<1]<- 0 
#     
#     CloseDownPolicy[bvec>=1]<- 1 
#     
#     for (p in 1:length(Policies))
#     {
#       
#       eval(parse(text=paste('Policy<-',Policies[p],'Policy',sep=''))) 
#       
#       Projection<- Sim_Forward(Policies[p],Policy,bvec,RecentStockData$BvBmsy,ProjectionTime,Price,MSY,cost,r,beta,delta)
#       
#       PolicyMatrix<- as.data.frame(matrix(NA,nrow=ProjectionTime,ncol=dim(TempStockMatrix)[2]))
#       
#       PolicyMatrix[,1:dim(RecentStockData)[2]]<- RecentStockData
#       
#       colnames(PolicyMatrix)<- c(colnames(RecentStockData),'Policy','Profits')
#       
#       PolicyMatrix$Catch<- Projection$Yields
#       
#       PolicyMatrix$BvBmsy<- Projection$BvBmsy
#       
#       PolicyMatrix$FvFmsy<- Projection$FvFmsy
#       
#       PolicyMatrix$Year<- RecentStockData$Year+(1:ProjectionTime)
#       
#       PolicyMatrix$Profits<- Projection$Profits
#       
#       PolicyMatrix$Policy<- Policies[p]
# 
#       PolicyMatrix$MarginalCost<- cost
#       
#       TempStockMatrix[counter:(counter+-1+(dim(PolicyMatrix)[1])),]<- I(PolicyMatrix)
#       
#       counter<- (counter+(dim(PolicyMatrix)[1]))
#       
#       
#     } # close policies loop
#     
#     
#   } #Close stocks loop
#   
#   TempStockMatrix<- TempStockMatrix[1:(counter-1),]
#   
#   TempStockMatrix[,grepl('Back',colnames(TempStockMatrix))]<- NA
#   
#   TempStockMatrix$ScaledCatch<- NA
#   
#   TempStockMatrix$CatchToRollingMax<- NA
#   
#   Data$Policy<- NA
#   
#   Data$Profits<- NA
#     
#   Data$Policy[is.na(Data$Policy)]<- 'Historic'
#   
#   HistoricData<- Data$Policy=='Historic'
#   
#   Data$FvFmsy[HistoricData]<- (Data$Catch[HistoricData]/Data$MSY[HistoricData])/Data$BvBmsy[HistoricData]
#   
#   c_num<- Data$Price[HistoricData]* Data$MSY[HistoricData]*  (2-Data$BvBmsyOpenAccess[HistoricData]) * Data$BvBmsyOpenAccess[HistoricData]
#   
#   c_den<- ((2-Data$BvBmsyOpenAccess[HistoricData])*Data$r[HistoricData]/2)^beta
#   
#   Costs<- c_num/c_den
#   
#   Data$MarginalCost[HistoricData]<- Costs
#   
#   Data$Profits[HistoricData]= Data$Price[HistoricData]*Data$MSY[HistoricData]*Data$FvFmsy[HistoricData]*Data$BvBmsy[HistoricData] - Data$MarginalCost[HistoricData]*(Data$FvFmsy[HistoricData]*Data$r[HistoricData]/2)^beta
#   
#   DataPlus<- rbind((Data),(TempStockMatrix))
#   
#   is.data.frame(DataPlus)
#   
#   return(DataPlus)
#   # Stack results -----------------------------------------------------------
#   
