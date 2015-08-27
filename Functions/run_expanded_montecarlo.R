run_expanded_montecarlo<- function(Iterations,Stocks,ProjectionData,BiomassData,MsyData,
                                   PolicyStorage,ErrorSize,NumCPUs = 1,BaselineYear = 2012,
                                   CatchSharePrice = 1.3,CatchShareCost = 0.77,ResultFolder,elastic_demand = F,sp_group_demand = F,
                                   Discount = 0, elasticity = -0.9)
{
  
  Sim_Forward= function(FStatusQuo,BStatusQuo,Stocks,Policy,Policies,IsCatchShare,bvec,b0,Time = 38,p,MSY,c,g,phi,beta,omega)
  {  
    #Function to move each fishery forward in time simultaneously
    FindF<- function(Stocks,CurrentB,Policy,Policies,BCount) #Function that finds f for each fishery by it's closeest match from bvec
    {
      BvecMat=matrix(Policies$b,nrow=BCount,ncol=length(Stocks))
      
      CurrentBMat<- matrix(rep(CurrentB,BCount),nrow=BCount,ncol=length(CurrentB),byrow=T)
      
      FVecMat<- matrix(Policies[,Policy],nrow=BCount,ncol=length(CurrentB),byrow=F)
      
      BDiff=(BvecMat-CurrentBMat)^2
      
      ClosestB<- apply(BDiff, 2, function(x) max(which(x == min(x, na.rm = TRUE))))
      
      ClosestB<- data.frame(unique(Policies$IdOrig),ClosestB)
      
      #Find the closest value in bvec for each fishery
      
      FFun= function(x,FVecMat,ClosestB,Stocks)
      {
        #         Which<- (ClosestB[,1]==Stocks[x])
        y=FVecMat[ClosestB[x,2],x]
      }
      NextF<- sapply(1:length(Stocks),FFun,FVecMat=FVecMat,ClosestB=ClosestB,Stocks=Stocks)    #Calculate next f based on current b 
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
    #     BCount=ddply(Policies,c('IdOrig'),summarize,NumB=length(b))
    
    BCount<- Policies %>%
      group_by(IdOrig) %>%
      summarize(NumB=length(b))
    
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
    
    #     Omega<- 0.1
    
    PastF<- FStatusQuo
    
    for (t in 1:(Time+1))
    {
      if (Policy!='StatusQuoOpenAccess')
      { 
        #         browser()
        #         f[t,]<- system.time(lapply(1:length(Stocks),FindF,Stocks=S
        f[t,]<- (FindF(Stocks=Stocks,CurrentB=b[t,],Policy,Policies,BCount))
      }
      
      if (Policy=='StatusQuoOpenAccess')
      {
        
        f[t,]=OpenAccessFleet(PastF,pi[t-1,],t,omega = omega,MsyProfits)
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
    
    Projection$IdOrig <- as.character(Projection$IdOrig)
    #     Projection<- ddply(Projection,c('IdOrig'),mutate,NPV=cumsum(Profits*(1+0.05)^-(Year-BaselineYear)))
    
    Projection<- Projection %>%
      group_by(IdOrig) %>%
      mutate(NPV=cumsum(Profits*(1+Discount)^-(Year-BaselineYear)))
    
    #     Projection<- subset(Projection,Year==BaselineYear | Year==max(Year))
    return(Projection)
  }
  
  
  McIterations<- function(k,Iterations,BioError,ProjectionData,MsyData,BiomassData,
                          BaselineYear,PolicyStorage,Stocks,ErrorSize,Spec_ISSCAAP,base_omega = 0.1, base_beta = 1.3)
  {
    
    
    #     PossParams<- PossibleParams[Index[,k],]
    CurrentBio<- BioError[,k]
    
    PolicyFuncs<- PolicyStorage[PolicyStorage$IdOrig %in% Stocks,]
    
    RecentStockData<- ProjectionData[ProjectionData$IdOrig %in% Stocks & ProjectionData$Year==BaselineYear,]
    
    RecentStockData$Price<- RecentStockData$Price * rlnorm(dim(RecentStockData)[1],0,ErrorSize)
    
    #     RecentStockData$beta <- base_beta * rlnorm(dim(RecentStockData)[1],0,ErrorSize)
    #     
    #     RecentStockData$omega <- base_omega * rlnorm(dim(RecentStockData)[1],0,ErrorSize)
    #     
    RecentStockData$beta <-  runif(dim(RecentStockData)[1],0.75*base_beta,1.25*base_beta)
    
    RecentStockData$omega <-  runif(dim(RecentStockData)[1],0.75*base_omega,1.25*base_omega)
    
    
    CatchSharePrice<- CatchSharePrice  *rlnorm(1,0,ErrorSize)
    
    CatchShareCost<- CatchShareCost  *rlnorm(1,0,ErrorSize)
    
    RecentStockData$BOA<- pmin(0.9*((RecentStockData$phi+1)^(1/(RecentStockData$phi))),RecentStockData$BvBmsyOpenAccess *rlnorm(dim(RecentStockData)[1],0,ErrorSize))
    
    RecentStockData$MSY<- RecentStockData$MSY * rlnorm(length(Stocks),0,ErrorSize)
    
    RecentStockData$g<- RecentStockData$g * rlnorm(length(Stocks),0,ErrorSize)
    
    RecentStockData$k <-  (RecentStockData$MSY * (RecentStockData$phi + 1)^(1/ RecentStockData$phi)) / RecentStockData$g
    
    RecentStockData$Bmsy <-  RecentStockData$MSY/ RecentStockData$g
    
    #     phi<- RecentStockData$phi
    
    RecentStockData$BvBmsy<- pmin(2.5,RecentStockData$BvBmsy* CurrentBio)
    
    RecentStockData$FvFmsy<- pmin(6,(RecentStockData$Catch/RecentStockData$MSY)/RecentStockData$BvBmsy)
    
    IsCatchShare<- RecentStockData$CatchShare
    
    RecentStockData$FOA<- ((RecentStockData$phi+1)/RecentStockData$phi)*(1-RecentStockData$BOA^RecentStockData$phi/(RecentStockData$phi+1))
    
    c_num <-  RecentStockData$Price*RecentStockData$FOA*RecentStockData$BOA*RecentStockData$MSY
    
    c_den = (RecentStockData$g*RecentStockData$FOA)^RecentStockData$beta
    
    RecentStockData$cost = c_num/c_den
    
    RecentStockData$Price[IsCatchShare==1]<- RecentStockData$Price[IsCatchShare==1]*CatchSharePrice
    
    
    RecentStockData$ cost[IsCatchShare==1]<- RecentStockData$cost[IsCatchShare==1]*CatchShareCost
    
    
    RecentStockData$MsyProfits = RecentStockData$Price*RecentStockData$MSY - RecentStockData$cost*(RecentStockData$g)^RecentStockData$beta
    
    Policies<- colnames(PolicyFuncs)
    
    Policies<- Policies[!(Policies %in% c('IdOrig','b'))]
    
    bvec<- PolicyFuncs$b
    
    ProjectionMat<- NULL
    
    cc<- 0
    #     RecentStockData<- subset(RecentStockData,IdOrig=='10-FAO-37-57')
    for (p in 1:length(Policies))
    {
      
      cc<- cc+1
      
      Projection<- Sim_Forward(FStatusQuo=RecentStockData$FvFmsy,BStatusQuo=RecentStockData$BvBmsy,
                               Policy=Policies[p],Policies=PolicyFuncs,IsCatchShare=IsCatchShare,bvec=bvec,
                               b0=RecentStockData$BvBmsy,p=RecentStockData$Price
                               ,MSY=RecentStockData$MSY,c=RecentStockData$cost,g=RecentStockData$g,phi=RecentStockData$phi,
                               beta=RecentStockData$beta,omega = RecentStockData$omega,Stocks=Stocks)
      Projection<- join(Projection,RecentStockData[,c('IdOrig','Country','Dbase','SciName','CommName','IdLevel','SpeciesCatName','CatchShare','MSY','g','k','phi','Price','cost','MsyProfits','BOA','beta')],by='IdOrig',match='first')
      
      Projection$Policy<- Policies[p]
      #       a=subset(Projection,IdOrig=='SPRFMO-CHTRACCH-1950-2010-RICARD')
      #       
      Projection$Iteration<- k
      
      ProjectionMat<- rbind(ProjectionMat,Projection)
      
    } #Close policies loop
    PercDone<- round(100*(k/Iterations),2)
    
    write.table(paste(PercDone, '% done with Monte Carlo',sep=''), file = 'MonteCarloProgess.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    
    ProjectionMat$Bmsy<- ProjectionMat$MSY/ProjectionMat$g
    
    ProjectionMat$Biomass<- ProjectionMat$BvBmsy * ProjectionMat$Bmsy
    
    
    
    #     SpeciesSummary<- ddply(ProjectionMat,c('Policy','SpeciesCatName','Year'),summarize,b25=quantile(BvBmsy,0.25,na.rm=T),
    #                            f75=quantile(FvFmsy,0.75,na.rm=T)) %>% 
    #       mutate(GroupName=paste(Policy,SpeciesCatName,Year,sep='-'))
    
    #     ProjectionMat$k<- NA
    
    ProjectionMat$MarginalCost<- ProjectionMat$cost
    
    Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
    
    RecentStockData$Iteration <- 0
    
    Historic <- RecentStockData[,colnames(ProjectionMat)]
    
    ProjectionMat <- subset(ProjectionMat, Year > BaselineYear) %>%
      rbind(Historic)
    
    NEIs<- NearestNeighborNeis(BiomassData,MsyData,ProjectionMat,BaselineYear,
                               ResultFolder = ResultFolder,Spec_ISSCAAP = Spec_ISSCAAP)
    
    NEIs<- NEIs$ProjNeis[,colnames(NEIs$ProjNeis) %in% colnames(ProjectionMat)]
    
    NEIs<- NEIs %>%
      ungroup() %>%
    subset(is.finite(BvBmsy)) %>%
      mutate(GroupName=paste(Policy,SpeciesCatName,Year,sep='-'),RunName=paste(Policy,IdOrig,sep='-')) %>% 
      dplyr::select(RunName,IdOrig,CommName,SciName,Country,Year,Policy,BvBmsy,FvFmsy,Biomass,Catch,MSY,Profits,Dbase,CatchShare,SpeciesCatName,Price,g,Bmsy,phi,k,MarginalCost)
    
    
    
    NEIs<- NEIs %>%
      group_by(RunName) %>%
      mutate(NPV=cumsum(Profits*(1+.05)^-(Year-2012))) %>% 
      ungroup() %>%
      dplyr::select(-RunName) %>%
      mutate(IdLevel='Neis')
    
    Species<- dplyr::select(ProjectionMat,IdOrig,CommName,SciName,Country,Year,Policy,BvBmsy,FvFmsy,Biomass,Catch,MSY,Profits,Dbase,CatchShare,NPV,SpeciesCatName,Price,g,k,Bmsy,phi,MarginalCost) %>%
      mutate(IdLevel='Species')
    
    BioMonte<- rbind(NEIs,Species)
    
    BioMonte$DiscProfits<- BioMonte$Profits * (1+Discount)^-(BioMonte$Year-BaselineYear)
    
    
    #     BioMonte<-BuildPolicyBAUs(BioMonte,BaselineYear)
    #     
#     BioMonte <- subset(BioMonte,Policy != 'Historic')
#     
#     Historic <- subset(ProjectionData, Policy == 'Historic' & Year == 2012) %>%
#       dplyr::select(IdOrig,Country,Year,Policy,BvBmsy,FvFmsy,Biomass,Catch,MSY,Profits,Dbase,CatchShare,NPV,SpeciesCatName,Price,g,Bmsy,phi,MarginalCost,IdLevel)
#     
#     Historic$DiscProfits<- Historic$Profits * (1+Discount)^-(Historic$Year-BaselineYear)
#     
#     
#     #     Historic$Policy <- 'Historic'
#     
#     BioMonte <- rbind(BioMonte, Historic)

    BioMonte<-BuildPolicyBAUs(BioMonte,BaselineYear,elastic_demand = elastic_demand, elasticity = elasticity,
                              Discount = Discount,sp_group_demand = sp_group_demand )
    
    BioMonte$Iteration<- k
    BioMonte<- subset(BioMonte,Year==2012 | Year==2013 | Year==2050)
    return(BioMonte)
  } #Close McIterations
  
  PossibleParams<- subset(ProjectionData,IdOrig %in% PolicyStorage$IdOrig)
  
  ErrorVars<- c('Price','BOA')
  #   PossibleParams<- CatchMSYPossibleParams
  
  NumStocks<- length(unique(PossibleParams$IdOrig))  
  
  BioError<- replicate(Iterations,runif(Stocks,0.5,1.5))
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
  
  Projections<- mclapply(1:Iterations,McIterations,BioError=BioError,Iterations=Iterations,
                         ProjectionData=ProjectionData,MsyData=MsyData,BiomassData=BiomassData,BaselineYear=BaselineYear,
                         PolicyStorage=PolicyStorage,Stocks=Stocks,ErrorSize=ErrorSize,Spec_ISSCAAP=Spec_ISSCAAP,mc.cores=NumCPUs)
  Projections<- ldply(Projections)
  
  return(Projections)
} #Close Function