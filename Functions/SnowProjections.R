SnowProjections<- function(s)
{
  
  sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
  
  counter<- 1

  show(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''))
  
  TempMat<- TempStockMatrix
  
  Where<- Data[,IdVar]==Stocks[s]
  
  #   Where<- Data[,IdVar]== '11776-FAO-67-31'
  
  StockData<- Data[Where,]
  
  RecentStockData<-  StockData[dim(StockData)[1],]
  
  Price<- RecentStockData$Price
  MSY<- RecentStockData$MSY
  BOA<- RecentStockData$BvBmsyOpenAccess
  r<- RecentStockData$r
  FStatusQuo<- RecentStockData$FvFmsy
  
  #     FStatusQuo<- ((RecentStockData$Catch)/MSY)/RecentStockData$BvBmsy
  
  FStatusQuo[is.na(FStatusQuo)]<- 0
  
  Where<- Data[,IdVar]==Stocks[s]
  
  c_num <-  Price*(2-BOA)*BOA*MSY*2^beta
  
  c_den = ((2-BOA)*r)^beta
  
  cost = c_num/c_den
  
  Data$MarginalCost[Where]<- cost 
  
  MsyProfits = Price*MSY - cost*(r/2)^beta
  
  OptPolicy<-  RunDynamicOpt2(MSY,r,Price,cost,beta,Discount,bvec,tol)$Policy
  
  CatchSharePolicy<-  RunDynamicOpt2(MSY,r,CatchSharePrice*Price,CatchShareCost*cost,beta,Discount,bvec,tol)$Policy
  
  FoodPolicy<-  RunDynamicOpt2(MSY,r,1,0,beta,0,bvec,tol)$Policy
  
  SQPolicy<- FStatusQuo*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])
  
  FmsyPolicy<- matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])
  
  CloseDownPolicy<- bvec
  
  CloseDownPolicy[bvec<1]<- 0 
  
  CloseDownPolicy[bvec>=1]<- 1 
  
  for (p in 1:length(Policies))
  {
    
    eval(parse(text=paste('Policy<-',Policies[p],'Policy',sep=''))) 
    
    Projection<- Sim_Forward(Policies[p],Policy,bvec,RecentStockData$BvBmsy,ProjectionTime,Price,MSY,cost,r,beta,delta)
    
    PolicyMatrix<- as.data.frame(matrix(NA,nrow=ProjectionTime,ncol=dim(TempMat)[2]))
    
    PolicyMatrix[,1:dim(RecentStockData)[2]]<- RecentStockData
    
    colnames(PolicyMatrix)<- c(colnames(RecentStockData),'Policy','Profits')
    
    PolicyMatrix$Catch<- Projection$Yields
    
    PolicyMatrix$BvBmsy<- Projection$BvBmsy
    
    PolicyMatrix$FvFmsy<- Projection$FvFmsy
    
    PolicyMatrix$Year<- RecentStockData$Year+(1:ProjectionTime)
    
    PolicyMatrix$Profits<- Projection$Profits
    
    PolicyMatrix$Policy<- Policies[p]
    
    PolicyMatrix$MarginalCost<- cost
    
    TempMat[counter:(counter+-1+(dim(PolicyMatrix)[1])),]<- I(PolicyMatrix)
#     
     counter<- (counter+(dim(PolicyMatrix)[1]))
    
    
  } # close policies loop
  
  return(TempMat)
} #Close function