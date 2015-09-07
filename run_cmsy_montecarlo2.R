run_cmsy_montecarlo<- function(Iterations,Stocks,projdata,
                               CatchMSYPossibleParams,PolicyStorage,
                               ErrorSize,NumCPUs = 1,BaselineYear = 2012, CatchSharePrice = 1.31,
                               CatchShareCost = 0.77,elastic_demand = T, elasticity = -0.9, sp_group_demand = F, Discount = 0)
{
  
  Sim_Forward= function(Stocks,Policy,Policies,IsCatchShare,bvec,b0,pre_f,pre_profits,Time = 38,p,MSY,c,g,phi,beta,omega,CatchSharePrice,
                        CatchShareCost)
  {
    #Function to move each fishery forward in time simultaneously
    FindF<- function(Stocks,CurrentB,Policy,Policies,BCount) #Function that finds f for each fishery by it's closeest match from bvec
    {
      
      current_status <- data.frame(Stocks,CurrentB,original_order = 1:length(Stocks),stringsAsFactors = F) %>%
        arrange((Stocks))
      
      sorted_policies <- Policies %>%
        arrange(IdOrig,b)
      
      CurrentBMat<- matrix(rep(current_status$CurrentB,BCount),nrow=BCount,ncol=length(CurrentB),byrow=T)
      
      colnames(CurrentBMat) <- current_status$Stocks
      
      BvecMat <- matrix(Policies$b,nrow=BCount,ncol=length(Stocks))
      
      colnames(BvecMat) <- unique(Policies$IdOrig)
      
      FVecMat<- matrix(Policies[,Policy],nrow=BCount,ncol=length(CurrentB),byrow=F)
      
      colnames(FVecMat) <- unique(Policies$IdOrig)
      #       BDiff=(BvecMat-CurrentBMat)^2
      
      BDiff=(BvecMat-CurrentBMat)
      
      BDiff[BDiff > 0] <- NA
      
      ClosestB<- apply(BDiff, 2, function(x) min(which(x == max(x, na.rm = TRUE))))
      
      ClosestB<- data.frame(unique(Policies$IdOrig),ClosestB) #find index of closest B
      
      b_real <- current_status$CurrentB
      
      bdex <- ClosestB$ClosestB
      
      b_a <- BvecMat[bdex,1]
      
      b_maxed <- bdex >= BCount
      
      next_bdex <- bdex
      
      next_bdex[b_maxed == F] <- pmin(BCount,next_bdex + 1)[b_maxed == F]
      
      b_b <- BvecMat[next_bdex,1]
      
      #Find the closest value in bvec for each fishery
      
      FFun= function(x,FVecMat,ClosestB,Stocks)
      {
        #                 Which<- (ClosestB[,1]==Stocks[x])
        y=FVecMat[ClosestB[x],x]
      }
      
      f_a<- sapply(1:length(Stocks),FFun,FVecMat=FVecMat,ClosestB=bdex,Stocks=Stocks)    #Calculate next f based on current b
      
      f_b<- sapply(1:length(Stocks),FFun,FVecMat=FVecMat,ClosestB=next_bdex,Stocks=Stocks)    #Calculate next f based on current b
      
      f_b[b_maxed] <- f_a[b_maxed]
      
      slope = (f_b - f_a)/(b_b - b_a)
      
      f_star <- slope*b_real + (f_a-slope*b_a)
      
      f_star[b_maxed] <- f_b[b_maxed]
      
      f_star <- f_star[order((current_status$original_order))]
      
      return(f_star)
    }
    
    OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
    {
      #Function to adjust f in response to prior profits
      #     if (t==1)
      #     {
      #       f=f
      #     }
      #     if (t>1)
      #     {
      f<- pmin(4,pmax(f+omega*(pi/MsyProfits),.0001))
      # }
      return(f)
    }
    b = matrix(0,Time,length(pre_f))
    
    f = b
    pi = b
    y = b
    b[1,] = b0
    
    BCount<- Policies %>%
      group_by(IdOrig) %>%
      summarize(NumB=length(b))
    
    BCount=unique(BCount$NumB)
    if (Policy=='CatchShare') # apply price cost effects of catch share policy to non-catch share stocks
    {
      p[IsCatchShare==0]<- (p*CatchSharePrice )[IsCatchShare==0]
      
      c[IsCatchShare==0]<- (c*CatchShareCost)[IsCatchShare==0]
    }
    
    if(Policy=='StatusQuoOpenAccess') # revert previously applied price and cost effects of catch share fisheries for Open Access policy
    {
      p[IsCatchShare==1]<-(p/CatchSharePrice)[IsCatchShare==1]
      c[IsCatchShare==1]<- (c/CatchShareCost)[IsCatchShare==1]
    }
    
    MsyProfits<- MSY*p-c*(g)^beta
    
    PastF<- pre_f
    
    PastPi <- pre_profits
    
    for (t in 1:(Time))
    {
      if (Policy!='StatusQuoOpenAccess')
      {
        f[t,]<- (FindF(Stocks=Stocks,
                       CurrentB=pmin(max(Policies$b),pmax(min(Policies$b),b[t,])),Policy,Policies,BCount))
      }
      
      if (Policy=='StatusQuoOpenAccess')
      {
        f[t,]=OpenAccessFleet(PastF,PastPi,t,omega = omega,MsyProfits)
        PastF<- f[t,]
      }
      
      pi[t,] = p*MSY*f[t,]*b[t,] - c*(f[t,]*g)^beta
      PastPi = pi[t,]
      y[t,] = MSY*f[t,]*b[t,]
      
      if (t<Time)
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
    Projection$Year<- Projection$Year+(BaselineYear)
    #     Projection <- subset(Projection, Year <=2050)
    #     Projection$Year<- Projection$Year+(BaselineYear-1)
    
    Projection$IdOrig <- as.character(Projection$IdOrig)
    #     Projection<- ddply(Projection,c('IdOrig'),mutate,NPV=cumsum(Profits*(1+0.05)^-(Year-BaselineYear)))
    
    Projection<- Projection %>%
      group_by(IdOrig) %>%
      mutate(NPV=cumsum(Profits*(1+Discount)^-(Year-BaselineYear)))
    
    #     Projection<- subset(Projection,Year==BaselineYear | Year==max(Year))
    return(Projection)
  }
  
  
  McIterations<- function(k,Iterations,Index,PossibleParams,PolicyStorage,Stocks,ErrorSize,base_omega = 0.1,base_beta = 1.3,
                          elastic_demand = F, sp_group_demand = F, Discount = 0,lower_unif = 0.75, upper_unif = 1.25)
  {
    
    PossParams<- PossibleParams[Index[,k],]
    
    PolicyFuncs<- PolicyStorage[PolicyStorage$IdOrig %in% Stocks,]
    
    RecentStockData<- ProjectionData[ProjectionData$IdOrig %in% Stocks & ProjectionData$Year==BaselineYear,]
    
    Price<- RecentStockData$Price * rlnorm(dim(RecentStockData)[1],0,ErrorSize)
    
    beta <-  runif(dim(RecentStockData)[1],0.75*base_beta,1.25*base_beta)
    
    omega <-  runif(dim(RecentStockData)[1],0.75*base_omega,1.25*base_omega)
    
    CatchSharePrice<- CatchSharePrice  *rlnorm(1,0,ErrorSize)
    
    CatchShareCost<- CatchShareCost  *rlnorm(1,0,ErrorSize)
    
    BOA<- pmin(0.9*((RecentStockData$phi+1)^(1/(RecentStockData$phi))),RecentStockData$BvBmsyOpenAccess *rlnorm(dim(RecentStockData)[1],0,ErrorSize))
    
    MSY<- PossParams$MSY
    
    g<- PossParams$g
    
    phi<- PossParams$phi
    
    FStatusQuo<- PossParams$FinalFvFmsy
    
    BStatusQuo<- PossParams$FinalBvBmsy
    
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
    
    PossParams$Bmsy <- with(PossParams,K/((phi+1)^(1/phi)))
    
    Policies<- colnames(PolicyFuncs)
    
    Policies<- Policies[!(Policies %in% c('IdOrig','b'))]
    
    bvec<- PolicyFuncs$b
    
    ProjectionMat<- NULL
    
    cc<- 0
    
    for (p in 1:length(Policies))
    {
      
      cc<- cc+1
      
      Projection<- Sim_Forward(FStatusQuo = FStatusQuo,BStatusQuo = BStatusQuo,Policy = Policies[p],
                               Policies =  PolicyFuncs,IsCatchShare = IsCatchShare,bvec = bvec,b0 = BStatusQuo,
                               p = Price,MSY = MSY,c = cost, g = g,phi = phi,beta = beta, omega = omega)
      
      Projection<- join(Projection,PossParams[,c('IdOrig','MSY','g','phi','K','Price','Cost','MsyProfits','BOA','Bmsy')],by='IdOrig',match='first')
      
      Projection<- join(Projection,RecentStockData[,c('IdOrig','Country','Dbase','SciName','CommName','IdLevel','SpeciesCatName','CatchShare')],by='IdOrig',match='first')        
      
      Projection$Policy<- Policies[p]
      
      Projection$Iteration<- k
      
      Projection$Biomass <- Projection$BvBmsy * Projection$Bmsy
      
      ProjectionMat<- rbind(ProjectionMat,Projection)
      
    } #Close policies loop
    
    ProjectionMat$DiscProfits<- ProjectionMat$Profits * (1+Discount)^-(ProjectionMat$Year-BaselineYear)
    
    ProjectionMat$MarginalCost <- ProjectionMat$Cost
    
    Historic <- subset(ProjectionMat, Policy == 'CatchShare' & Year == 2012)
    
    Historic$Policy <- 'Historic'
    
    ProjectionMat <- rbind(ProjectionMat, Historic)
    
    PercDone<- round(100*(k/Iterations),2)
    
    write.table(paste(PercDone, '% done with Monte Carlo',sep=''), file = 'MonteCarloProgess.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    #     ProjectionMat$Biomass <- (ProjectionMat$BvBmsy * ProjectionMat$Bmsy)
    
    
    ProjectionMat <- BuildPolicyBAUs(ProjectionData = ProjectionMat,BaselineYear = BaselineYear,elastic_demand = elastic_demand, elasticity = -0.7,
                                     Discount = Discount,sp_group_demand = sp_group_demand )
    
    
    return(ProjectionMat)
  } #Close McIterations
  
  
  ErrorVars<- c('Price','BOA')
  
  PossibleParams<- CatchMSYPossibleParams
  
  Index<- matrix(NA,nrow=length(Stocks),ncol=Iterations)
  
  for (i in 1:length(Stocks))
  {
    Index[i,]<- sample(which(PossibleParams$IdOrig==Stocks[i]),Iterations,replace=T)
  }
  
  Projections<- mclapply(1:Iterations,McIterations,mc.cores=NumCPUs,Index=Index,
                         Iterations=Iterations,PossibleParams=PossibleParams,
                         PolicyStorage=PolicyStorage,Stocks=Stocks,ErrorSize=ErrorSize,elastic_demand = elastic_demand,
                         sp_group_demand = sp_group_demand)
  
  Projections<- ldply(Projections)
  
  return(Projections)
} #Close Function