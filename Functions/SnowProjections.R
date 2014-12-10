# SnowProjections<- function(s)
SnowProjections<- function(s,Data,BaselineYear,Stocks,IdVar,bvec,Discount,tol,beta,CatchSharePrice,CatchShareCost,Policies,ProjectionTime,TempStockMatrix)
  
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
      
    }# Close while loop
    
    
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
  
  Sim_Forward= function(Policy,fpolicy,bvec,b0,T,p,MSY,c,r,beta,delta)
  {  
    b = matrix(0,T,1)
    f = b
    pi = b
    y = b
    
    b[1] = b0;
    
    if (Policy=='CatchShare')
    {
      p<- p*CatchSharePrice
      
      c<- c*CatchShareCost
    }
    
    for (t in 1:T)
    {
      f[t] = approx(bvec,fpolicy,b[t])$y
      pi[t] = p*MSY*f[t]*b[t] - c*(f[t]*r/2)^beta
      y[t] = MSY*f[t]*b[t]
      if (t<T)
      {b[t+1] = b[t] + r*b[t]*(1-b[t]/2) - r/2*b[t]*f[t]}
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