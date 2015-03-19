SnowMonteCarlo<- function(Iterations,Stocks,ProjectionData,CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
{
  
  Sim_Forward= function(FStatusQuo,BStatusQuo,Policy,Policies,IsCatchShare,bvec,b0,Time,p,MSY,c,g,phi,beta)
  {  
    #Function to move each fishery forward in time simultaneously
    FindF<- function(Stocks,CurrentB,Policy,Policies,BCount) #Function that finds f for each fishery by it's closeest match from bvec
    {
      
      BvecMat=matrix(Policies$b,nrow=BCount,ncol=length(Stocks))
      
      CurrentBMat<- matrix(rep(CurrentB,BCount),nrow=BCount,ncol=length(CurrentB),byrow=T)
      
      FVecMat<- matrix(Policies[,Policy],nrow=BCount,ncol=length(CurrentB))
      
      BDiff=(BvecMat-CurrentBMat)^2
      
      ClosestB<- apply(BDiff, 2, function(x) max(which(x == min(x, na.rm = TRUE))))
      #Find the closest value in bvec for each fishery
      
      FFun= function(x,FVecMat,ClosestB)
      {
        y=FVecMat[ClosestB[x],x]
      }
      
      NextF<- sapply(1:length(Stocks),FFun,FVecMat=FVecMat,ClosestB=ClosestB)    #Calculate next f based on current b 
      #       NextF<- FVecMat[ClosestB,]
      #       NextF<- approx(StockPol$b,StockPol[,Policy],CurrentB[i])$y
      return(NextF)
    }
    
    OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
    {
      #Change f in response to profits
      if (t==1)
      {
        f=f
      }
      if (t>1)
      {
        f<- pmin(4,pmax(f+omega*(pi/MsyProfits),.0000001))
      }
      return(f)
    }
    
    b = matrix(0,Time+1,length(FStatusQuo))
    f = b
    pi = b
    y = b
    b[1,] = b0;
    BCount=ddply(Policies,c('IdOrig'),summarize,NumB=length(b))
    BCount=unique(BCount$NumB)
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
    
    MsyProfits<- MSY*p-c*(g)^beta
    
    Omega<- .125
    
    PastF<- FStatusQuo
    
    for (t in 1:(Time+1))
    {
      if (Policy!='StatusQuoOpenAccess')
      { 
        #         browser()
        #         f[t,]<- system.time(lapply(1:length(Stocks),FindF,Stocks=Stocks,CurrentB=b[t,],Policy,Policies))
        f[t,]<- (FindF(Stocks=Stocks,CurrentB=b[t,],Policy,Policies,BCount))
      }
      
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
      pi[t,] = p*MSY*f[t,]*b[t,] - c*(f[t,]*g)^beta
      y[t,] = MSY*f[t,]*b[t,]
      if (t<Time+1)
      {
#         b[t+1,] =pmax(min(bvec), b[t,] + r*b[t,]*(1-b[t,]/2) - r/2*b[t,]*f[t,])
        b[t+1] =pmax(min(bvec), b[t,] + ((phi+1)/phi)*g*b[t,]*(1-b[t,]^phi/(phi+1)) - g*b[t,]*f[t,])
        
      }
    }
    colnames(f)<- Stocks
    colnames(b)<- Stocks
    colnames(y)<- Stocks
    colnames(pi)<- Stocks
    
    fFlat<- melt(f)
    colnames(fFlat)<- c('Year','IdOrig','FvFmsy')
    #     fFlat$Metric<- 'FvFmsy'
    
    bFlat<- melt(b)
    colnames(bFlat)<- c('Year','IdOrig','BvBmsy')
    #     bFlat$Metric<- 'BvBmsy'
    
    yFlat<- melt(y)
    colnames(yFlat)<- c('Year','IdOrig','Catch')
    #     yFlat$Metric<- 'Catch'
    
    piFlat<- melt(pi)
    colnames(piFlat)<- c('Year','IdOrig','Profits')
    #     piFlat$Metric<- 'Profits'
    
    Projection<- data.frame(fFlat,bFlat[,'BvBmsy'],yFlat[,'Catch'],piFlat[,'Profits'])
    
    colnames(Projection)<- c('Year','IdOrig','FvFmsy','BvBmsy','Catch','Profits')
    
    #     browser()
    #     quartz()
    #     ggplot(data=Projection,aes(x=BvBmsy,y=FvFmsy))+geom_line()+facet_wrap(~IdOrig)
    #     
    #     quartz()
    #     ggplot(data=Projection,aes(x=Year,y=BvBmsy))+geom_line()+facet_wrap(~IdOrig)
    #     
    Projection$Year<- Projection$Year+(BaselineYear-1)
    
    Projection<- subset(Projection,Year==BaselineYear | Year==max(Year))
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
    BOA<- pmin(1.99,RecentStockData$BvBmsyOpenAccess *rlnorm(dim(RecentStockData)[1],0,ErrorSize))
    
    MSY<- PossParams$MSY
    
    g<- PossParams$g
    
    phi<- PossParams$phi
    
    FStatusQuo<- PossParams$FinalFvFmsy
    BStatusQuo<- PossParams$FinalBvBmsy
    #     IsCatchShare<- RecentStockData$CatchShare[1]
    IsCatchShare<- RecentStockData$CatchShare
    
    FOA<- ((phi+1)/phi)*(1-BOA^phi/(phi+1))
    
    c_num <-  Price*FOA*BOA*MSY
    
    c_den = (g*FOA)^beta
    
    cost = c_num/c_den
    
    #     Price*MSY*BOA*(2-BOA)-cost*(2-BOA*(r/2))^beta
    
    #     if(IsCatchShare==1) # adjust prices and costs for catch share fisheries before dynamic optimization
    #     {
    Price[IsCatchShare==1]<-Price[IsCatchShare==1]*CatchSharePrice
    
    #       Data$Price[Where]<-Price
    
    cost[IsCatchShare==1]<-cost[IsCatchShare==1]*CatchShareCost
    #     }
    
    MsyProfits = Price*MSY - cost*(g)^beta
    
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
      
      Projection<- Sim_Forward(FStatusQuo,BStatusQuo,Policies[p],PolicyFuncs,IsCatchShare,bvec,BStatusQuo,ProjectionTime,Price,MSY,cost,g,phi,beta)
      
      Projection<- join(Projection,PossParams[,c('IdOrig','MSY','g','phi','K','Price','Cost','MsyProfits','BOA')],by='IdOrig',match='first')
      
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