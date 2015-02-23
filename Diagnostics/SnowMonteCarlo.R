SnowMonteCarlo<- function(Iterations,Stocks,ProjectionData,CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
{
  OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
  {
    
    if (t==1)
    {
      f=f
    }
    if (t>1)
    {
      f<- pmin(10,max(f+omega*(pi/MsyProfits),.0000001))
    }
    return(f)
  }
  
  Sim_Forward= function(FStatusQuo,BStatusQuo,Policy,Policies,IsCatchShare,bvec,b0,Time,p,MSY,c,r,beta)
  {  
    
    FindF<- function(i,Stocks,CurrentB,Policy,Policies)
    {
      StockPol<- Policies[Policies$IdOrig==Stocks[i],]
      
      NextF<- approx(StockPol$b,StockPol[,Policy],CurrentB[i])$y
      return(NextF)
    }
    
    
    b = matrix(0,Time+1,length(FStatusQuo))
    f = b
    pi = b
    y = b
    b[1,] = b0;
    if (Policy=='StatusQuoOpenAccess'){f[1,]<- FStatusQuo}
    if (Policy=='CatchShare') # apply price cost effects of catch share policy to non-catch share stocks
    {
      p[IsCatchShare==0]<- p[IsCatchShare==0]*CatchSharePrice
      
      c[IsCatchShare==0]<- c[IsCatchShare==0]*CatchShareCost
    }
    
    if(Policy=='StatusQuoOpenAccess') # revert previously applied price and cost effects of catch share fisheries for Open Access policy
    {
      p[IsCatchShare==1]<-p[IsCatchShare==1]/CatchSharePrice
      c[IsCatchShare==1]<-c[IsCatchShare==1]/CatchShareCost
    }
    
    MsyProfits<- MSY*p-c*(r/2)^beta
    
    Omega<- 0.1
    
    PastF<- FStatusQuo
    
    for (t in 1:(Time+1))
    {
      
      if (Policy!='StatusQuoOpenAccess'){ f[t,]<- sapply(1:length(Stocks),FindF,Stocks=Stocks,CurrentB=b[t,],Policy,Policies)}
      if (Policy=='StatusQuoOpenAccess')
      {
        
        f[t,]=OpenAccessFleet(PastF,pi[t-1,],t,Omega,MsyProfits)
        PastF<- f[t,]
      }
      if (t==1)
      {
        f[t,]<- FStatusQuo
        b[t,]<- BStatusQuo
      }
      pi[t,] = p*MSY*f[t,]*b[t,] - c*(f[t,]*r/2)^beta
      y[t,] = MSY*f[t,]*b[t,]
      if (t<Time+1)
      {b[t+1,] =max(min(bvec), b[t,] + r*b[t,]*(1-b[t,]/2) - r/2*b[t,]*f[t,])}
    }
    
    colnames(f)<- Stocks
    colnames(b)<- Stocks
    colnames(y)<- Stocks
    colnames(pi)<- Stocks
    
    fFlat<- melt(f)
    colnames(fFlat)<- c('Year','IdOrig','Value')
    fFlat$Metric<- 'FvFmsy'
    
    bFlat<- melt(b)
    colnames(bFlat)<- c('Year','IdOrig','Value')
    bFlat$Metric<- 'BvBmsy'
    
    yFlat<- melt(y)
    colnames(yFlat)<- c('Year','IdOrig','Value')
    yFlat$Metric<- 'Catch'
    
    piFlat<- melt(pi)
    colnames(piFlat)<- c('Year','IdOrig','Value')
    piFlat$Metric<- 'Profits'
    
    Projection<- rbind(fFlat,bFlat,yFlat,piFlat)
    
    Projection$Year<- Projection$Year+(BaselineYear-1)
    
    subset(Projection,Year==BaselineYear | Year==max(Year))
    
    return(Projection)
  }
  
  
  McIterations<- function(k,Iterations,Index,PossibleParams,PolicyStorage,Stocks,ErrorSize)
  {
    
    #     Index<- matrix(NA,nrow=length(Stocks),ncol=1)
    #     
    #     for (i in 1:length(Stocks))
    #     {
    #       Index[i,1]<- sample(which(PossibleParams$IdOrig==Stocks[i]),1,replace=T)
    #     }
    #     
    PossParams<- PossibleParams[Index[,k],]
    
    #     Grab<- sample(1:dim(PossibleParams)[1],1,replace=T)
    #     
    #     PossParams<- PossibleParams[Grab,]
    #     
    #     PolicyFuncs<- subset(PolicyStorage,IdOrig==Stocks[i])
    
    PolicyFuncs<- PolicyStorage[PolicyStorage$IdOrig %in% Stocks,]
    
    
    #     RecentStockData<- subset(ProjectionData,IdOrig==Stocks[i] & Year==BaselineYear)
    
    #     RecentStockData<- ProjectionData[ProjectionData$IdOrig=Stocks[i] & ProjectionData$Year==BaselineYear,]
    
    RecentStockData<- ProjectionData[ProjectionData$IdOrig %in% Stocks & ProjectionData$Year==BaselineYear,]
    
    
    #     Price<- RecentStockData$Price[1] *rlnorm(1,0,ErrorSize)
    
    Price<- RecentStockData$Price * rlnorm(dim(RecentStockData)[1],0,ErrorSize)
    
    CatchSharePrice<- CatchSharePrice  *rlnorm(1,0,ErrorSize)
    
    CatchShareCost<- CatchShareCost  *rlnorm(1,0,ErrorSize)
    
    #     BOA<- pmin(1.99,RecentStockData$BvBmsyOpenAccess[1] *rlnorm(1,0,ErrorSize))
    BOA<- pmin(1.99,RecentStockData$BvBmsyOpenAccess *rlnorm(dim(RecentStockData)[1],0,.1))
    
    MSY<- PossParams$MSY
    
    r<- PossParams$r
    FStatusQuo<- PossParams$FinalFvFmsy
    BStatusQuo<- PossParams$FinalBvBmsy
    #     IsCatchShare<- RecentStockData$CatchShare[1]
    IsCatchShare<- RecentStockData$CatchShare
    
    c_num <-  Price*(2-BOA)*BOA*MSY*2^beta
    
    c_den = ((2-BOA)*r)^beta
    
    cost = c_num/c_den
    
    #     if(IsCatchShare==1) # adjust prices and costs for catch share fisheries before dynamic optimization
    #     {
    Price[IsCatchShare==1]<-Price[IsCatchShare==1]*CatchSharePrice
    
    #       Data$Price[Where]<-Price
    
    cost[IsCatchShare==1]<-cost[IsCatchShare==1]*CatchShareCost
    #     }
    
    MsyProfits = Price*MSY - cost*(r/2)^beta
    
    PossParams$Price<- Price
    
    PossParams$Cost<- cost
    
    PossParams$MsyProfits<- MsyProfits
    
    PossParams$BOA<- BOA
    
    Policies<- colnames(PolicyFuncs)
    
    Policies<- Policies[!(Policies %in% c('IdOrig','b'))]
    
    bvec<- PolicyFuncs$b
    
    ProjectionMat<- NULL
    
    cc<- 0
    
    for (p in 1:length(Policies))
    {
      
      cc<- cc+1
      #       eval(parse(text=paste('Policy<-',Policies[p],'Policy',sep=''))) 
      
      Projection<- Sim_Forward(FStatusQuo,BStatusQuo,Policies[p],PolicyFuncs,IsCatchShare,bvec,BStatusQuo,ProjectionTime,Price,MSY,cost,r,beta)
      
      Projection<- join(Projection,PossParams[,c('IdOrig','MSY','r','K','Price','Cost','MsyProfits','BOA')],by='IdOrig',match='first')
      
      Projection<- join(Projection,RecentStockData[,c('IdOrig','Country','Dbase','SciName','CommName','IdLevel','SpeciesCatName')],by='IdOrig',match='first')        
      
      Projection$Policy<- Policies[p]
      
      Projection$Iteration<- k
      
      ProjectionMat<- rbind(ProjectionMat,Projection)
      #       
      #       ProjectionMat[cc:(cc-1+dim(Projection)[1]),]<- as.matrix(Projection)
      #       
      #       cc<- (cc-1)+dim(Projection)[1]
      #       
    }
    PercDone<- round(100*(k/Iterations),2)
    
    write.table(paste(PercDone, '% done with Monte Carlo',sep=''), file = 'MonteCarloProgess.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    
    return(ProjectionMat)
  }
  
  
  ErrorVars<- c('Price','BOA')
  
  PossibleParams<- CatchMSYPossibleParams
  
  Index<- matrix(NA,nrow=length(Stocks),ncol=Iterations)
  
  for (i in 1:length(Stocks))
  {
    Index[i,]<- sample(which(PossibleParams$IdOrig==Stocks[i]),Iterations,replace=T)
  }
  Projections<- mclapply(1:Iterations,McIterations,mc.cores=NumCPUs,Index=Index,Iterations=Iterations,PossibleParams=PossibleParams,PolicyStorage=PolicyStorage,Stocks=Stocks,ErrorSize=ErrorSize)
  
  Projections<- ldply(Projections)
  
  return(Projections)
} #Close Function