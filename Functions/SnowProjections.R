# SnowProjections<- function(s)
SnowProjections<- function(s,Data,BaselineYear,Stocks,IdVar,bvec,Discount,tol,beta,CatchSharePrice,CatchShareCost,Policies,ProjectionTime,TempStockMatrix,StatusQuoPolicy)
  
{
  ######################################
  # Run Dynamic Optimization --------------------------------------------------
  # Solves for optimal policy function f (as function of bvec) given model parameters.
  # last input argument "tol" is convergence tolerance (use tol=.01)
  ######################################
  
  RunDynamicOpt2= function(MSY,r,p,c,beta,disc,bvec,tol)
  {
    
    # MSY<- RecentStockData$MSY
    # r<- RecentStockData$r
    # p<- RecentStockData$Price
    # c<- cost
    # beta<- Beta
    # disc<- Discount
    # bvec<- bvec
    # tol<- tol
    #   
    
    delta= 1/(1+disc) #Discount parameter
    t=0
    
    f1= matrix(1,length(bvec),1)
    Vnew= matrix(0,length(bvec),1)
    diff= 10*tol
    
    while (t<4 | diff>tol)
    {
      t= t+1
      V= Vnew
      oldf1= f1
      for (i in 1:length(bvec))
      {
        b= bvec[i]
        if(i==1)
        {guess= 1}
        else
        {guess= f1[i-1]}
        
        FishOut= optim(par=guess,fn=GFRM_funR,lower=0.0001,upper=1.99,b=b,p=p,MSY=MSY,c=c,r=r,beta=beta,V=V,bvec=bvec,delta=delta,method="L-BFGS-B")
        
        Vnew[i]= -FishOut$value
        f1[i]= FishOut$par
        
        
      } #Close bvec loop
      
      diff= sum(abs(f1-oldf1))
      if (t>200)
      {
        diff<- tol
        write.table(paste(  'Fishery ',Stocks[s],' is stuck',sep=''), file = 'Optimization Fail Log.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
      }
    }# Close while loop
    
    #     write.table(paste(  'Number of Trys is ', t,sep=''), file = 'Optimization Testing.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    
    
    
    return(list(Policy=f1))
    
  } #Close function
  
  ######################################
  # Internal Optimization Function 
  # To be used with RunDynamicOptimization2.R
  # Gives (negative) value function value for value function iteration code--------------------------------------------------
  ######################################
  
  GFRM_funR= function(f,b,p,MSY,c,r,beta,V,bvec,delta)
  {  
    profit= p*MSY*f*b - c*(f*r/2)^beta
    
    bnext= max(min(bvec),b + r*b*(1-b/2) - r/2*b*f)
    bnext=min(max(bvec),bnext)
    out= approx(bvec,t(V),bnext) #spline(bvec,V,xout=bnext)
    Vnext= out$y
    
    negout= -(profit + delta*Vnext)
    return(negout)
  }  
  
  ######################################
  # Forward Simulation of Policy Function
  # Inputs: polcy function, duration of simulation, and other model parameters
  # Outputs: variables over time (f, b, yield (y), profit (pi)--------------------------------------------------
  ######################################
  
  OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
  {
    
    if (t==1)
    {
      f=f
    }
    if (t>1)
    {
      f<- max(f+omega*(pi/MsyProfits),.0001)
    }
    return(f)
  }
  
  Sim_Forward= function(Policy,fpolicy,IsCatchShare,bvec,b0,Time,p,MSY,c,r,beta,delta)
  {  
    b = matrix(0,Time,1)
    f = b
    pi = b
    y = b
    b[1] = b0;
    if (Policy=='StatusQuoOpenAccess'){f[1]<- fpolicy}
    if (Policy=='CatchShare' & IsCatchShare==0)
    {
      p<- p*CatchSharePrice
      
      c<- c*CatchShareCost
    }
    
    MsyProfits<- MSY*p-c*(r/2)^beta
    
    Omega<- 0.1
    
    PastF<- f[1]
    
    for (t in 1:Time)
    {
      if (Policy!='StatusQuoOpenAccess'){ f[t] = approx(bvec,fpolicy,b[t])$y}
      if (Policy=='StatusQuoOpenAccess')
      {
        if(IsCatchShare==1) # revert price and cost effects of catch share fisheries for Open Access policy
        {
          p<-p/CatchSharePrice
          c<-c/CatchShareCost
        }
        f[t]=OpenAccessFleet(PastF,pi[t-1],t,Omega,MsyProfits)
        PastF<- f[t]
      }
      pi[t] = p*MSY*f[t]*b[t] - c*(f[t]*r/2)^beta
      y[t] = MSY*f[t]*b[t]
      if (t<Time)
      {b[t+1] =max(min(bvec), b[t] + r*b[t]*(1-b[t]/2) - r/2*b[t]*f[t])}
    }
    
    Projection<- data.frame(f,b,y,pi)
    
    colnames(Projection)<- c('FvFmsy','BvBmsy','Yields','Profits')
    
    return(Projection)
  }
  
  #   sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
  
  counter<- 1
  
  #   show(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''))
  
  write.table(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''), file = 'Projection Analysis Progress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
  
  
  
  TempMat<- TempStockMatrix
  
  Where<- Data[,IdVar]==Stocks[s]
  
  #   Where<- Data[,IdVar]== '11776-FAO-67-31'
  
  StockData<- Data[Where,]
  
  if(max(StockData$BvBmsy)>max(bvec))
  {
    maxb<-max(StockData$BvBmsy)
    
    bvec<-seq(from=0.00000001,to=maxb+0.1,by=0.1)
  }
  
  RecentStockData<-  StockData[dim(StockData)[1],]
  
  Price<- RecentStockData$Price
  MSY<- RecentStockData$MSY
  BOA<- RecentStockData$BvBmsyOpenAccess
  r<- RecentStockData$r
  FStatusQuo<- RecentStockData$FvFmsy
  IsCatchShare<-RecentStockData$CatchShare
  
  #     FStatusQuo<- ((RecentStockData$Catch)/MSY)/RecentStockData$BvBmsy
  
  FStatusQuo[is.na(FStatusQuo)]<- 0
  
  Where<- Data[,IdVar]==Stocks[s]
  
  c_num <-  Price*(2-BOA)*BOA*MSY*2^beta
  
  c_den = ((2-BOA)*r)^beta
  
  cost = c_num/c_den
  
  Data$MarginalCost[Where]<- cost 
  
  if(IsCatchShare==1) # adjust prices and costs for catch share fisheries before dynamic optimization
  {
    Price<-Price*CatchSharePrice
    
    Data$Price[Where]<-Data$Price[Where]*CatchSharePrice
    
    cost<-cost*CatchShareCost
    
    Data$MarginalCost[Where]<-Data$MarginalCost[Where]*CatchShareCost
  }

  MsyProfits = Price*MSY - cost*(r/2)^beta
  
  OptPolicy<-  RunDynamicOpt2(MSY,r,Price,cost,beta,Discount,bvec,tol)$Policy
  
  # Only apply catch share economic effects to non-catch share stocks. Should make Opt and CatchShare Identical for CS stocks
  if(IsCatchShare==0)
  {
    CatchSharePolicy<-  RunDynamicOpt2(MSY,r,CatchSharePrice*Price,CatchShareCost*cost,beta,Discount,bvec,tol)$Policy
  }
  
  if(IsCatchShare==1)
  {
    CatchSharePolicy<-  RunDynamicOpt2(MSY,r,Price,CatchShareCost*cost,beta,Discount,bvec,tol)$Policy
  }
  
  FoodPolicy<-  RunDynamicOpt2(MSY,r,1,0,beta,0,bvec,tol)$Policy
  
  StatusQuoFForeverPolicy<- FStatusQuo*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])  
  
  StatusQuoBForeverPolicy<- (2-RecentStockData$BvBmsy)*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])  
  
  FmsyPolicy<- matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])
  
  CloseDownPolicy<- bvec
  
  CloseDownPolicy[bvec<1]<- 0 
  
  CloseDownPolicy[bvec>=1]<- 1 
  
  StatusQuoOpenAccessPolicy<- FStatusQuo
  
  
  for (p in 1:length(Policies))
  {
    
    eval(parse(text=paste('Policy<-',Policies[p],'Policy',sep=''))) 
        
    Projection<- Sim_Forward(Policies[p],Policy,IsCatchShare,bvec,RecentStockData$BvBmsy,ProjectionTime,Price,MSY,cost,r,beta,delta)
    
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