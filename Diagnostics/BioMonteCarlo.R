BioMonteCarlo<- function(Iterations,Stocks,ProjectionData,BiomassData,MsyData,CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
{
  
  Sim_Forward= function(FStatusQuo,BStatusQuo,Stocks,Policy,Policies,IsCatchShare,bvec,b0,Time,p,MSY,c,g,phi,beta)
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
    
    Omega<- 0.1
    
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
        b[t+1,] =pmax(min(bvec), b[t,] + ((phi+1)/phi)*g*b[t,]*(1-b[t,]^phi/(phi+1)) - g*b[t,]*f[t,])
        
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
    #     if (any(b==0))
    #     {
    #       browser()
    #     }
    #  
    Projection$Year<- Projection$Year+(BaselineYear-1)
    
    Projection<- ddply(Projection,c('IdOrig'),mutate,NPV=cumsum(Profits*(1+0.05)^-(Year-BaselineYear)))
    
#     Projection<- subset(Projection,Year==BaselineYear | Year==max(Year))
    return(Projection)
  }
  
  
  McIterations<- function(k,Iterations,BioError,ProjectionData,MsyData,BiomassData,BaselineYear,PolicyStorage,Stocks,ErrorSize)
  {
    
    
    #     PossParams<- PossibleParams[Index[,k],]
    CurrentBio<- BioError[,k]
    
    PolicyFuncs<- PolicyStorage[PolicyStorage$IdOrig %in% Stocks,]
        
    RecentStockData<- ProjectionData[ProjectionData$IdOrig %in% Stocks & ProjectionData$Year==BaselineYear,]
    
    RecentStockData$Price<- RecentStockData$Price * rlnorm(dim(RecentStockData)[1],0,ErrorSize)
    
    CatchSharePrice<- CatchSharePrice  *rlnorm(1,0,ErrorSize)
    
    CatchShareCost<- CatchShareCost  *rlnorm(1,0,ErrorSize)
    
    RecentStockData$BOA<- pmin(0.9*((RecentStockData$phi+1)^(1/(RecentStockData$phi))),RecentStockData$BvBmsyOpenAccess *rlnorm(dim(RecentStockData)[1],0,ErrorSize))
    
    RecentStockData$MSY<- RecentStockData$MSY * rlnorm(length(Stocks),0,ErrorSize)
    
    RecentStockData$g<- RecentStockData$g * rlnorm(length(Stocks),0,ErrorSize)
    
    #     phi<- RecentStockData$phi
    
    RecentStockData$BvBmsy<- pmin(2.5,RecentStockData$BvBmsy * CurrentBio)
    
    RecentStockData$FvFmsy<- (RecentStockData$Catch/RecentStockData$MSY)/RecentStockData$BvBmsy
    
    IsCatchShare<- RecentStockData$CatchShare
    
    RecentStockData$FOA<- ((RecentStockData$phi+1)/RecentStockData$phi)*(1-RecentStockData$BOA^RecentStockData$phi/(RecentStockData$phi+1))
    
    c_num <-  RecentStockData$Price*RecentStockData$FOA*RecentStockData$BOA*RecentStockData$MSY
    
    c_den = (RecentStockData$g*RecentStockData$FOA)^beta
    
    RecentStockData$cost = c_num/c_den
    
    RecentStockData$Price[IsCatchShare==1]<- RecentStockData$Price[IsCatchShare==1]*CatchSharePrice
    
    
    RecentStockData$ cost[IsCatchShare==1]<- RecentStockData$cost[IsCatchShare==1]*CatchShareCost
    
    
    RecentStockData$MsyProfits = RecentStockData$Price*RecentStockData$MSY - RecentStockData$cost*(RecentStockData$g)^beta
    
    Policies<- colnames(PolicyFuncs)
    
    Policies<- Policies[!(Policies %in% c('IdOrig','b'))]
    
    bvec<- PolicyFuncs$b
    
    ProjectionMat<- NULL
    
    cc<- 0
    
    for (p in 1:length(Policies))
    {
      
      cc<- cc+1
      Projection<- Sim_Forward(FStatusQuo=RecentStockData$FvFmsy,BStatusQuo=RecentStockData$BvBmsy,
                               Policy=Policies[p],Policies=PolicyFuncs,IsCatchShare=IsCatchShare,bvec=bvec,
                               Time=ProjectionTime,b0=RecentStockData$BvBmsy,p=RecentStockData$Price
                               ,MSY=RecentStockData$MSY,c=RecentStockData$cost,g=RecentStockData$g,phi=RecentStockData$phi,
                               beta=beta,Stocks=Stocks)
              
      Projection<- join(Projection,RecentStockData[,c('IdOrig','Country','Dbase','SciName','CommName','IdLevel','SpeciesCatName','CatchShare','MSY','g','phi','Price','cost','MsyProfits','BOA')],by='IdOrig',match='first')
            
      Projection$Policy<- Policies[p]
      
      Projection$Iteration<- k

      ProjectionMat<- rbind(ProjectionMat,Projection)
      
    } #Close policies loop
    PercDone<- round(100*(k/Iterations),2)
    
    write.table(paste(PercDone, '% done with Monte Carlo',sep=''), file = 'MonteCarloProgess.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    
    ProjectionMat<-BuildPolicyBAUs(ProjectionMat,BaselineYear)
    
    ProjectionMat$Bmsy<- ProjectionMat$MSY/ProjectionMat$g
    
    ProjectionMat$Biomass<- ProjectionMat$BvBmsy * ProjectionMat$Bmsy
    
    SpeciesSummary<- ddply(ProjectionMat,c('Policy','SpeciesCatName','Year'),summarize,b25=quantile(BvBmsy,0.25),
                           f75=mean(FvFmsy,0.75)) %>% 
                             mutate(GroupName=paste(Policy,SpeciesCatName,Year,sep='-'))
    
    
    NEIs<- subset(ProjectionData,IdLevel=='Neis' & Policy !='Historic') %>%
      mutate(GroupName=paste(Policy,SpeciesCatName,Year,sep='-'),RunName=paste(Policy,IdOrig,sep='-')) %>% 
        join(select(SpeciesSummary,GroupName,b25,f75),by='GroupName') %>%
        mutate(NewProfits=MSY*Price*b25*f75-MarginalCost*(f75*g)^1.3) %>%
      select(RunName,IdOrig,Country,Year,Policy,b25,f75,Biomass,Catch,MSY,NewProfits) %>%
      rename(BvBmsy=b25,FvFmsy=f75,Profits=NewProfits)
    
      
      
    NEIs<- ddply(NEIs,c('RunName'),mutate,NPV=cumsum(Profits*(1+.05)^-(Year-2012))) %>% 
            select(-RunName) %>%
            mutate(IdLevel='Nei')

    Species<- select(ProjectionMat,IdOrig,Country,Year,Policy,BvBmsy,FvFmsy,Biomass,Catch,MSY,Profits,NPV) %>%
      mutate(IdLevel='Species')
    
  
    BioMonte<- rbind(NEIs,Species)
    
    BioMonte$Iteration<- k
    return(BioMonte)
  } #Close McIterations
  
  PossibleParams<- subset(ProjectionData,IdOrig %in% PolicyStorage$IdOrig)
  
  ErrorVars<- c('Price','BOA')
  #   PossibleParams<- CatchMSYPossibleParams
  
  NumStocks<- length(unique(PossibleParams$IdOrig))  
  
  BioError<- replicate(Iterations,runif(Stocks,.5,1.5))
  
Projections<- mclapply(1:Iterations,McIterations,mc.cores=NumCPUs,BioError=BioError,Iterations=Iterations,
                         ProjectionData=ProjectionData,MsyData=MsyData,BiomassData=BiomassData,BaselineYear=BaselineYear,
                         PolicyStorage=PolicyStorage,Stocks=Stocks,ErrorSize=ErrorSize)
Projections<- ldply(Projections)
  
  return(Projections)
} #Close Function