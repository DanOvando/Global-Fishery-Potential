
McIterations<- function(i,PossibleParams,PolicyStorage,Stocks,ErrorSize)
{
  
  Grab<- sample(1:dim(PossibleParams)[1],1,replace=T)
  
  PossParams<- PossibleParams[Grab,]
  
  PolicyFuncs<- subset(PolicyStorage,IdOrig==Stocks[i])
  
  RecentStockData<- subset(ProjectionData,IdOrig==Stocks[i] & Year==BaselineYear)
  
  Price<- RecentStockData$Price[1] *rlnorm(1,0,ErrorSize)
  
  CatchSharePrice<- CatchSharePrice  *rlnorm(1,0,ErrorSize)
  
  CatchShareCost<- CatchShareCost  *rlnorm(1,0,ErrorSize)
  
  BOA<- pmin(1.99,RecentStockData$BvBmsyOpenAccess[1] *rlnorm(1,0,ErrorSize))
  
  
  MSY<- PossParams$MSY
  
  
  r<- PossParams$r
  FStatusQuo<- PossParams$FinalFvFmsy
  BStatusQuo<- PossParams$FinalBvBmsy
  IsCatchShare<- RecentStockData$CatchShare[1]
  c_num <-  Price*(2-BOA)*BOA*MSY*2^beta
  
  c_den = ((2-BOA)*r)^beta
  
  cost = c_num/c_den
  
  if(IsCatchShare==1) # adjust prices and costs for catch share fisheries before dynamic optimization
  {
    Price<-Price*CatchSharePrice
    
    Data$Price[Where]<-Price
    
    cost<-cost*CatchShareCost
  }
  
  MsyProfits = Price*MSY - cost*(r/2)^beta
  
  Policies<- colnames(PolicyFuncs)
  
  Policies<- Policies[!(Policies %in% c('IdOrig','b'))]
  
  bvec<- PolicyFuncs$b
  
  for (j in 1:length(Policies))
  {
    Where<- which(colnames(PolicyFuncs)==Policies[j])
    eval(parse(text=paste(Policies[j],'Policy<- PolicyFuncs[,',Where,']',sep='')))    
  }
  
  
  
  StatusQuoFForeverPolicy<- FStatusQuo*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])  
  
  StatusQuoBForeverPolicy<- (2-BStatusQuo)*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])  
  
  StatusQuoOpenAccessPolicy<- FStatusQuo
  
  browser()
  for (p in 1:length(Policies))
  {
    
    eval(parse(text=paste('Policy<-',Policies[p],'Policy',sep=''))) 
    
    Projection<- Sim_Forward(FStatusQuo,BStatusQuo,Policies[p],Policy,IsCatchShare,bvec,BStatusQuo,ProjectionTime,Price,MSY,cost,r,beta)
    
    Projection$IdOrig<- Stocks[i]
    
    Projection$Country<- RecentStockData$Country[1]
    
    Projection$Dbase<- RecentStockData$Dbase[1]
    
    Projection$MSY<- MSY
    
    Projection$r<- r
    
    Projection$Price<- Price
    
    Projection$Cost<- cost
    
    Projection$BOA<- BOA
    
    Projection$Year<- RecentStockData$Year+(0:(ProjectionTime))
    
    Projection$SciName<- RecentStockData$SciName
    
    Projection$CommName<- RecentStockData$CommName
    
    Projection$IdLevel<- RecentStockData$IdLevel
    
    Projection$Policy<- Policies[p]
    
    Projection$SpeciesCatName<- RecentStockData$SpeciesCatName
    
    Projection$Iteration<- k
    
return(Projection)
}
    
}