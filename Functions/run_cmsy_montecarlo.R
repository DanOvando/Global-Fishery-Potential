run_cmsy_montecarlo<- function(Iterations,Stocks,projdata,PolicyStorage,CatchMSYPossibleParams,ErrorSize,NumCPUs = 1,BaselineYear = 2012,
                                   CatchSharePrice = 1.31,CatchShareCost = 0.77,elastic_demand = F,sp_group_demand = F,
                                   Discount = 0, elasticity = -0.9)
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
  
  
  
  McIterations<- function(k,Iterations,projdata,
                          BaselineYear,PolicyStorage,Stocks,
                          Spec_ISSCAAP,base_omega = 0.1, base_beta = 1.3,
                          lower_unif = 0.75, upper_unif = 1.25,Index,PossibleParams)
  {
    
    PossParams<- PossibleParams[Index[,k],]
    PolicyFuncs<- PolicyStorage[PolicyStorage$IdOrig %in% Stocks,]
    
    RecentStockData<- projdata[projdata$IdOrig %in% Stocks & projdata$Year==BaselineYear,]
    
    RecentStockData$MSY<- PossParams$MSY
    
    RecentStockData$g<- PossParams$g
    
    RecentStockData$phi<- PossParams$phi
    
    RecentStockData$Price<- RecentStockData$Price * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$beta <- base_beta * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$omega <- base_omega * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    CatchSharePrice<- CatchSharePrice  * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    CatchShareCost<- CatchShareCost  * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$BOA<- pmin(0.9*((RecentStockData$phi+1)^(1/(RecentStockData$phi))),RecentStockData$BvBmsyOpenAccess * runif(dim(RecentStockData)[1],lower_unif,upper_unif))
    
    RecentStockData$k <-  (RecentStockData$MSY * (RecentStockData$phi + 1)^(1/ RecentStockData$phi)) / RecentStockData$g
    
    RecentStockData$Bmsy <-  RecentStockData$MSY/ RecentStockData$g
    
    RecentStockData$FvFmsy<- pmin(6,(RecentStockData$Catch/RecentStockData$MSY)/RecentStockData$BvBmsy)
    
    IsCatchShare<- RecentStockData$CatchShare
    
    RecentStockData$FOA<- ((RecentStockData$phi+1)/RecentStockData$phi)*(1-RecentStockData$BOA^RecentStockData$phi/(RecentStockData$phi+1))
    
    RecentStockData$cost <- RecentStockData$MarginalCost
    
    c_num <-  RecentStockData$Price*RecentStockData$FOA*RecentStockData$BOA*RecentStockData$MSY
    
    c_den = (RecentStockData$g*RecentStockData$FOA)^RecentStockData$beta
    
    RecentStockData$cost = (c_num/c_den)
    
    RecentStockData$MsyProfits = RecentStockData$Price*RecentStockData$MSY - RecentStockData$cost*(RecentStockData$g)^RecentStockData$beta
   
     bvec<- PolicyFuncs$b
    
     year_one_bvbmsy <- pmin(max(bvec),pmax(min(bvec), with(RecentStockData,BvBmsy + ((phi+1)/phi)*g*BvBmsy*(1-BvBmsy^phi/(phi+1)) - g*BvBmsy*FvFmsy)))
     
#     year_one_bvbmsy <- year_one_bvbmsy * CurrentBio
    
    RecentStockData$Profits <- RecentStockData$Price*RecentStockData$Catch - RecentStockData$cost*(RecentStockData$g*RecentStockData$FvFmsy)^RecentStockData$beta 
    
    
    Policies<- colnames(PolicyFuncs)
    
    Policies<- Policies[!(Policies %in% c('X','IdOrig','b'))]
    
    
    ProjectionMat<- NULL
    
    cc<- 0
    
    for (p in 1:length(Policies))
    {
      cc<- cc+1
      Projection<- Sim_Forward(
        Policy=Policies[p],Policies=PolicyFuncs,IsCatchShare=IsCatchShare,bvec=bvec,
        b0=year_one_bvbmsy,pre_f = RecentStockData$FvFmsy,pre_profits = RecentStockData$Profits,p=RecentStockData$Price
        ,MSY=RecentStockData$MSY,c=RecentStockData$cost,g=RecentStockData$g,phi=RecentStockData$phi,
        beta=RecentStockData$beta,omega = RecentStockData$omega,Stocks=RecentStockData$IdOrig,
        CatchSharePrice = CatchSharePrice,CatchShareCost = CatchShareCost)
      
      Projection<- join(Projection,RecentStockData[,c('IdOrig','Country','Dbase','SciName','CommName','IdLevel','SpeciesCatName','CatchShare','MSY','g','k','phi','Price','cost','MsyProfits','BOA','beta','omega')],by='IdOrig',match='first')
      
      Projection$Policy<- Policies[p]
      if (Policies[p]=='CatchShare') # apply price cost effects of catch share policy to non-catch share stocks
      {
        
        Projection$Price[Projection$CatchShare == 0]<- (Projection$Price*CatchSharePrice)[Projection$CatchShare == 0]
        
        Projection$cost[Projection$CatchShare == 0]<- (Projection$cost*CatchShareCost)[Projection$CatchShare == 0]
      }
      
      if(Policies[p] == 'StatusQuoOpenAccess') # revert previously applied price and cost effects of catch share fisheries for Open Access policy
      {
        
        Projection$Price[Projection$CatchShare == 1]<- (Projection$Price/CatchSharePrice)[Projection$CatchShare == 1]
        
        Projection$cost[Projection$CatchShare == 1]<- (Projection$cost/CatchShareCost)[Projection$CatchShare == 1]
      }
      
      Projection$Iteration<- k
      
      ProjectionMat<- rbind(ProjectionMat,Projection)
    } #Close policies loop
    
    
    PercDone<- round(100*(k/Iterations),2)
    
    write.table(paste(PercDone, '% done with Monte Carlo',sep=''), file = 'MonteCarloProgess.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    
    ProjectionMat$Bmsy<- ProjectionMat$MSY/ProjectionMat$g
    
    ProjectionMat$Biomass<- ProjectionMat$BvBmsy * ProjectionMat$Bmsy
    
    ProjectionMat$MarginalCost<- ProjectionMat$cost
    
    Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
    
    RecentStockData$Iteration <- 0
    
    Historic <- RecentStockData[,colnames(ProjectionMat)]
    
    ProjectionMat <- subset(ProjectionMat, Year > BaselineYear) %>%
      rbind(Historic)
    
    ProjectionMat$DiscProfits<- ProjectionMat$Profits * (1+Discount)^-(ProjectionMat$Year-BaselineYear)
    
    ProjectionMat <- BuildPolicyBAUs(ProjectionData = ProjectionMat,BaselineYear = BaselineYear,elastic_demand = elastic_demand, elasticity = elasticity,
                                Discount = Discount,sp_group_demand = sp_group_demand,beta = ProjectionMat$beta,
                                omega = ProjectionMat$omega)
    
    ProjectionMat$Iteration<- k
    #     BioMonte<- subset(BioMonte,Year==2012 | Year==2013 | Year==2050)
    return(ProjectionMat)
  } #Close McIterations
  
  ErrorVars<- c('Price','BOA')
  
  PossibleParams<- CatchMSYPossibleParams
  
  Index<- matrix(NA,nrow=length(Stocks),ncol=Iterations)
  
  for (i in 1:length(Stocks))
  {
    Index[i,]<- sample(which(PossibleParams$IdOrig==Stocks[i]),Iterations,replace=T)
  }
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes

  Projections<- mclapply(1:Iterations,McIterations,Iterations=Iterations,
                         projdata=projdata,Index = Index,PossibleParams=PossibleParams,BaselineYear=BaselineYear,
                         PolicyStorage=PolicyStorage,Stocks=Stocks,Spec_ISSCAAP=Spec_ISSCAAP,
                         lower_unif = 0.75,upper_unif = 1.25,mc.cores = NumCPUs,mc.cleanup = T)
  
  
  Projections<- ldply(Projections)
  return(Projections)
} #Close Function