run_expanded_montecarlo<- function(Iterations,Stocks,projdata,
                                   PolicyStorage,CatchMSYPossibleParams,ErrorSize,NumCPUs = 1,BaselineYear = 2012,
                                   CatchSharePrice = 1.31,CatchShareCost = 0.77,ResultFolder,elastic_demand = F,sp_group_demand = F,
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
                          BaselineYear,PolicyStorage,Stocks,ErrorSize,
                          Spec_ISSCAAP,base_omega = 0.1, base_beta = 1.3,
                          lower_unif = 0.75, upper_unif = 1.25)
  {
    

    PolicyFuncs<- PolicyStorage[PolicyStorage$IdOrig %in% Stocks,]
    
    RecentStockData<- projdata[projdata$IdOrig %in% Stocks & projdata$Year==BaselineYear,]
    
    CurrentBio<- runif(dim(RecentStockData)[1],lower_unif,upper_unif) #BioError[,k]

    RecentStockData$BvBmsy<- RecentStockData$BvBmsy* CurrentBio
    
    
    RecentStockData$Price<- RecentStockData$Price * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$beta <- base_beta * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    #
    RecentStockData$omega <- base_omega * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    #
    #     RecentStockData$beta <-  runif(dim(RecentStockData)[1],1.3,1.3)
    #     RecentStockData$beta <-  runif(dim(RecentStockData)[1],1,1.6)
    
    #     RecentStockData$omega <-  runif(dim(RecentStockData)[1],0.75*base_omega,1.25*base_omega)
    #     RecentStockData$omega <-  runif(dim(RecentStockData)[1],base_omega,base_omega)
    CatchSharePrice<- CatchSharePrice  * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    CatchShareCost<- CatchShareCost  * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$BOA<- pmin(0.9*((RecentStockData$phi+1)^(1/(RecentStockData$phi))),RecentStockData$BvBmsyOpenAccess * runif(dim(RecentStockData)[1],lower_unif,upper_unif))
    
    #     RecentStockData$BOA<- RecentStockData$BvBmsyOpenAccess 
    
    
    RecentStockData$MSY<- RecentStockData$MSY * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$g<- RecentStockData$g * runif(dim(RecentStockData)[1],lower_unif,upper_unif)
    
    RecentStockData$k <-  (RecentStockData$MSY * (RecentStockData$phi + 1)^(1/ RecentStockData$phi)) / RecentStockData$g
    
    RecentStockData$Bmsy <-  RecentStockData$MSY/ RecentStockData$g
    
    #     phi<- RecentStockData$phi
    
    
    #         RecentStockData$FvFmsy<- (RecentStockData$Catch/RecentStockData$MSY)/RecentStockData$BvBmsy
    
    #     RecentStockData$BvBmsy<- pmin(2.5,RecentStockData$BvBmsy* CurrentBio)
    #     
    #     RecentStockData$FvFmsy<- pmin(6,(RecentStockData$Catch/RecentStockData$MSY)/RecentStockData$BvBmsy)
    #     
    
    IsCatchShare<- RecentStockData$CatchShare
    
    RecentStockData$FOA<- ((RecentStockData$phi+1)/RecentStockData$phi)*(1-RecentStockData$BOA^RecentStockData$phi/(RecentStockData$phi+1))
    
    RecentStockData$cost <- RecentStockData$MarginalCost
    
    RecentStockData$MsyProfits = RecentStockData$Price*RecentStockData$MSY - RecentStockData$cost*(RecentStockData$g)^RecentStockData$beta
    
    bvec<- PolicyFuncs$b
    
    year_one_bvbmsy <- pmin(max(bvec),pmax(min(bvec), with(RecentStockData,BvBmsy + ((phi+1)/phi)*g*BvBmsy*(1-BvBmsy^phi/(phi+1)) - g*BvBmsy*FvFmsy)))
    
    year_one_bvbmsy <- year_one_bvbmsy * CurrentBio
    
    RecentStockData$Profits <- RecentStockData$Price*RecentStockData$Catch - RecentStockData$cost*(RecentStockData$g*RecentStockData$FvFmsy)^RecentStockData$beta 
    
    Policies<- colnames(PolicyFuncs)
    
    Policies<- Policies[!(Policies %in% c('X','IdOrig','b'))]
    
    
    ProjectionMat<- NULL
    
    cc<- 0
    #     RecentStockData<- subset(RecentStockData,IdOrig=='10-FAO-37-57')
    
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
        
        #         c[IsCatchShare==0]<- (c*CatchShareCost)[IsCatchShare==0]
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
    
    RecentStockData$Iteration <- 0
    
    Historic <- RecentStockData[,colnames(ProjectionMat)]
    
    ProjectionMat <- subset(ProjectionMat, Year > BaselineYear) %>%
      rbind(Historic)
    
    nei <- subset(projdata, IdLevel == 'Neis' & Policy %in% unique(ProjectionMat$Policy))
    
    nei_ids <- unique(nei$IdOrig)
    
    nei_type_table<-unique(nei[c("CommName", "SciName","SpeciesCatName")]) # find unique combinations of nei stocks  
    
    nei_type_table$TaxonLevel<-NA
    
    nei_type_table$TaxonLevel[grepl("spp",nei_type_table$SciName)==T]<-"Genus"  
    
    nei_type_table$TaxonLevel[grepl("spp",nei_type_table$SciName)==F]<-"Non-Genus"
    
    nei_types <-unique(nei_type_table$SciName)
    
    species_types <- read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
    nei_lookup_table <- lapply(1:length(nei_types),make_nei_lookup,nei_type_table = nei_type_table,species_types = species_types) %>% ldply()
    
    years <- unique(ProjectionMat$Year)
    
#     starttime <- proc.time()
    comp_nei_lookup <- list()
    for (n in 1:length(nei_types)){
      nei_type <- nei_types[n]
      
      compstocks <- nei_lookup_table$compstocks[nei_lookup_table$NeiCat == nei_type]
      
      results<- ProjectionMat %>%
        subset(SciName %in% compstocks) %>%
        group_by(Year,Policy) %>%
        summarize(BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),
                  MedianG=median(g,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T)
                  ,MedianCost=median(MarginalCost,na.rm=T),MedianBeta = median(beta, na.rm = T)
                  ,JStocks=length(unique(IdOrig)),VarBvBmsy=var(BvBmsy,na.rm=T),VarFvFmsy=var(FvFmsy,na.rm=T),
                  MedianBOA = median(BOA, na.rm = T))
      
      out <- data.frame(nei_type,results) %>%
        rename(SciName = nei_type )
      
      comp_nei_lookup[[n]] <- out
    }
    comp_stock_status <- ldply(comp_nei_lookup)
    
    comp_stock_status$compname <- with(comp_stock_status,paste(SciName,Year,Policy))
    
    comp_stock_status <- dplyr::select(comp_stock_status,-SciName,-Year,-Policy)
    
    nei$compname <- with(nei,paste(SciName,Year,Policy))
    
    nei <- plyr::join(nei,comp_stock_status,by = 'compname')
    
    nei$BvBmsy <- nei$BvBmsy25
    
    nei$FvFmsy <- nei$FvFmsy75
    
    nei$beta <- nei$MedianBeta
    
    nei$omega <- base_omega
    
    nei$BOA <- nei$MedianBOA
    
    nei$Price <- nei$MedianPrice
    
    nei$g <- nei$MedianG
    
    nei <- nei[,!(colnames(nei) %in% colnames(comp_stock_status))]
    
    nei$Iteration <- k
    
    nei$MsyProfits <- NA
    nei2<- nei %>%
      ungroup() %>%
      group_by(IdOrig) %>%
      mutate(NewMSY= (Catch/(BvBmsy*FvFmsy))[Policy=='Historic' & Year==2012] ) %>%
      dplyr::select(-MSY,-Catch,-Profits,-Biomass,-Bmsy) %>%
      ungroup() %>%
      rename(MSY = NewMSY) %>%
      mutate( Catch = MSY * BvBmsy * FvFmsy, #this is changing 2012 catch
FOA = (((phi+1)/phi)*(1-BOA^phi/(phi+1))),
              c_num =  Price*FOA*BOA*MSY,
              c_den = (g*FOA)^beta) %>%
#               MarginalCost = c_num/c_den) %>%
      mutate(Bmsy = MSY/g, Biomass = BvBmsy * Bmsy) %>%
      mutate(Profits = (Price * MSY * BvBmsy * FvFmsy) -  
               MarginalCost * (FvFmsy * g)^beta)

    nei2$cost <- nei2$MarginalCost
    
    nei <- nei2[,colnames(ProjectionMat)]
    
#     show(proc.time() - starttime)
    
    Species<- dplyr::select(ProjectionMat,IdOrig,CommName,SciName,Country,Year,Policy,BvBmsy,FvFmsy,Biomass,Catch,MSY,Profits,Dbase,
                            CatchShare,NPV,SpeciesCatName,Price,g,k,Bmsy,phi,MarginalCost,beta,omega) %>%
      mutate(IdLevel='Species')

    nei <- dplyr::select(nei,IdOrig,CommName,SciName,Country,Year,Policy,BvBmsy,FvFmsy,Biomass,
                         Catch,MSY,Profits,Dbase,CatchShare,NPV,SpeciesCatName,Price,g,k,Bmsy,phi,MarginalCost,beta,omega) %>%
      mutate(IdLevel = 'Neis')
    nei$beta <- base_beta
    
    BioMonte<- rbind(nei,Species)
    
    BioMonte$DiscProfits<- BioMonte$Profits * (1+Discount)^-(BioMonte$Year-BaselineYear)
    
#     BioMonte <- subset(BioMonte, IdOrig != 'NWWG-CAPEICE-1977-2010-NEUBAUER')
    
    BioMonte <- BuildPolicyBAUs(BioMonte,BaselineYear,elastic_demand = elastic_demand, elasticity = elasticity,
                                Discount = Discount,sp_group_demand = sp_group_demand,beta = BioMonte$beta,
                                omega = BioMonte$omega)
    
    BioMonte$Iteration<- k
    #     BioMonte<- subset(BioMonte,Year==2012 | Year==2013 | Year==2050)
    return(BioMonte)
  } #Close McIterations
  
  PossibleParams<- subset(projdata,IdOrig %in% PolicyStorage$IdOrig)
  
  ErrorVars<- c('Price','BOA')
  #   PossibleParams<- CatchMSYPossibleParams
  
  NumStocks<- length(unique(PossibleParams$IdOrig))
#   BioError<- replicate(Iterations,runif(length(Stocks),0.75,1.25))
  
    Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
  
  Projections<- mclapply(1:Iterations,McIterations,Iterations=Iterations,
                       projdata=projdata,BaselineYear=BaselineYear,
                       PolicyStorage=PolicyStorage,Stocks=Stocks,ErrorSize=ErrorSize,Spec_ISSCAAP=Spec_ISSCAAP,
                       lower_unif = 1,upper_unif = 1,mc.cores = NumCPUs,mc.cleanup = T)
  
  #   Projections<- mclapply(1:Iterations,McIterations,BioError=BioError,Iterations=Iterations,
  #                          ProjectionData=ProjectionData,BaselineYear=BaselineYear,
  #                          PolicyStorage=PolicyStorage,Stocks=Stocks,ErrorSize=ErrorSize,Spec_ISSCAAP=Spec_ISSCAAP,mc.cores=NumCPUs)
  #   
  
  Projections<- ldply(Projections)
  return(Projections)
} #Close Function