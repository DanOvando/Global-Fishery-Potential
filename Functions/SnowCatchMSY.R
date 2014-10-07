######################################
#Calculate MSY--------------------------------------------------
#This code runs CatchMSY on fisheries
###################################### 

  SnowCatchMSY<- function(s)
#  SnowCatchMSY<- function(s,Data,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,n,NumCPUs,CatchMSYTrumps,stock_id,IdVar)    
{
  
  RanCMSY<- FALSE
  require(zoo)
  require(plyr)
  sraMSY  <-function(theta, N) #CatchMSY guts
  {
    # theta=parbound
    # N=n
    #This function conducts the stock reduction
    #analysis for N trials
    #args:
    #  theta - a list object containing:
    #  	r (lower and upper bounds for r)
    #		k (lower and upper bounds for k)
    #		lambda (limits for current depletion)
    
    #     theta=parbound
    #     N=n
    #     r=parbound$r
    #     k=parbound$k
    #     lambda=parbound$lambda
    with(as.list(theta), 
{
  # c(0.05,0.5)
  # ri = exp(runif(N, log(.05), log(0.5)))  ## get N values between r[1] and r[2], assign to ri
  ri = exp(runif(N, log(r[1]), log(r[2])))  ## get N values between r[1] and r[2], assign to ri
  ki = exp(runif(N, log(k[1]), log(k[2])))  ## get N values between k[1] and k[2], assing to ki
  itheta=cbind(r=ri,k=ki, lam1=lambda[1],lam2=lambda[2], sigR=sigR) ## assign ri, ki, and final biomass range to itheta
  M = apply(itheta,1,.schaefer) ## call Schaefer function with parameters in itheta
  i=1:N
  ## prototype objective function
  get.ell=function(i) M[[i]]$ell
  get.bvbmsy=function(i) M[[i]]$bvbmsy
  get.bvbmsyell=function(i) M[[i]]$bvbmsyell
  
  
  ell = sapply(i, get.ell) 
  bvbmsy = sapply(i, get.bvbmsy) 
  bvbmsyell = sapply(i, get.bvbmsyell) 
  return(list(r=ri,k=ki, ell=ell,bvbmsy=bvbmsy,bvbmsyell=bvbmsyell))	
})
  }

.schaefer  <- function(theta) # Schaefer model
{
  with(as.list(theta), {  ## for all combinations of ri & ki
    bt=vector()
    ell = 0  ## initialize ell
    for (j in startbt)
    {
      if(ell == 0) 
      {
        bt[1]=j*k*exp(rnorm(1,0, sigR))  ## set biomass in first year
        for(i in 1:(nyr-1)) ## for all years in the time series
        {
          xt=rnorm(1,0, sigR)
          bt[i+1]=(bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
        }
        
        #Bernoulli likelihood, assign 0 or 1 to each combination of r and k
        ell = 0
        #         show(paste('k is',k))
        #         show(paste('lam1 is',lam1))
        #         show(paste('lam2 is',lam2))
        bvbmsyell<- rep(0,length(bt))
        if(bt[nyr]/k>=lam1 && bt[nyr]/k <=lam2 && min(bt,na.rm=T) > 0 && max(bt,na.rm=T) <=k && bt[which(yr==interyr)]/k>=interbio[1] && bt[which(yr==interyr)]/k<=interbio[2]) 
        {ell = 1
         bvbmsyell<-rep(1,length(bt))
        }
      }  
    }
    return(list(ell=ell,bvbmsy=2*bt/k,bvbmsyell=bvbmsyell))
    
    
  })
}

stock<- (stock_id[s])

Data<- Data[Data$IdOrig==stock,]

CatchYears<- (Data$Year*as.numeric(is.na(Data$Catch)==F))

CatchYears[CatchYears==0]<- NA

FirstCatchYear<- which(Data$Year==min(CatchYears,na.rm=T))[1]

LastCatchYear<- which(Data$Year==max(CatchYears,na.rm=T))[1]

Data<- Data[FirstCatchYear:LastCatchYear,]



if (any(Data$HasRamFvFmsy))
{
  RamFvFmsy<- Data$FvFmsy
}

Where<- Data[,IdVar]==stock

show(paste(round(100*(s/length(stock_id)),2),'% done with CatchMSY',sep=''))

yr   <- Data$Year[(Data[,IdVar])==stock]

ct   <- (Data$Catch[(Data[,IdVar])==stock])  ## assumes that catch is given in tonnes, transforms to 1'000 tonnes

bio<- pmin(1,Data$BvBmsy[Data[,IdVar]==stock]/2) #pull out bvbmsy (transposed to B/K)

bioerror<- Data$BvBmsySD[Where]/2

  bioerror[is.na(bioerror)]<- CommonError/2

if (sum(ct,na.rm=T)>0 & sum(bio,na.rm=T)>0& length(LastCatchYear)>0 & length(ct) >1)
{
  

  ct<- na.approx(ct)
      
  if(Smooth==1){ct<- runmed(ct,3)}
  
  res  <- (Data$Res[(Data[,IdVar])==stock])[1] ## resilience from FishBase, if needed, enable in PARAMETER SECTION
  
  if(is.na(res)){res<- 0.5}
  
  for (i in 1){
    start_r  <- if(res == "Very low"){c(0.015, 0.1)}
    else if(res == "Low") {c(0.05,0.5)}
    else if(res == "High") {c(0.6,1.5)}
    else {c(0.2,1)} ## Medium, or default if no res is found  
  }
  
  nyr  <- length(yr)    ## number of years in the time series
  
  # cat("\n","Stock",stock,"\n")
  flush.console()
  
  ## PARAMETER SECTION
  
  start_k     <- c(max(ct,na.rm=T),50*max(ct,na.rm=T)) ## default for upper k e.g. 100 * max catch
  ## startbio 	<- c(0.8,1)   ## assumed biomass range at start of time series, as fraction of k

  startbio    <- pmin(1,pmax(0,c(qnorm(0.25,bio[1],bioerror[1]),qnorm(0.75,bio[1],bioerror[1]))))
  
  
#   startbio    <- pmin(1,c((1-ErrorSize)*bio[1],(1+ErrorSize)*bio[1]))
  
  if (is.na(bio[1]) | bio[1]==0)
  {
    startbio    <- if(ct[1]/max(ct,na.rm=T) < 0.5) {c(0.5,0.9)} else {c(0.3,0.6)} ## use for batch processing #SUB IN BVBMSY VALUES
  }
  
  interyr 	<- median(1:length(yr))   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available #SUB IN INTERMIN YEAR
  
#   interbio 	<- pmin(1,c((1-ErrorSize)*bio[interyr],(1+ErrorSize)*bio[interyr])) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available

interbio   <-  pmin(1,pmax(0,c(qnorm(0.25,bio[interyr],bioerror[interyr]),qnorm(0.75,bio[interyr],bioerror[interyr])))) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available


if (is.na(bio[interyr]) | bio[interyr]==0)
  {
    interbio 	<- c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
  }
  
  interyr<- yr[interyr]
  
#   finalbio    <- pmin(1,c((1-ErrorSize)*bio[nyr],(1+ErrorSize)*bio[nyr]))
finalbio    <- pmin(1,pmax(0,c(qnorm(0.25,bio[nyr],bioerror[nyr]),qnorm(0.75,bio[nyr],bioerror[nyr]))))


  if (is.na(bio[nyr]) | bio[nyr]==0)
  {
    finalbio    <- if(ct[nyr]/max(ct,na.rm=T) > 0.5) {c(0.3,0.7)} else {c(0.01,0.4)} ## use for batch processing #SET TO KNOWN B/BMSY RANGE
    
  }
  
  #       sigR        <- 0.0      ## process error; 0 if deterministic model; 0.05 reasonable value? 0.2 is too high
  startbt     <- seq(startbio[1], startbio[2], by = 0.05) ## apply range of start biomass in steps of 0.05	
  parbound <- list(r = start_r, k = start_k, lambda = finalbio, sigR)
  
  if (Display==1)
  {
    cat("Last year =",max(yr),", last catch =",ct[nyr],"\n")
    cat("Resilience =",res,"\n")
    cat("Process error =", sigR,"\n")
    cat("Assumed initial biomass (B/k) =", startbio[1],"-", startbio[2], " k","\n")
    cat("Assumed intermediate biomass (B/k) in", interyr, " =", interbio[1],"-",interbio[2]," k","\n")
    cat("Assumed final biomass (B/k) =", parbound$lambda[1],"-",parbound$lambda[2]," k","\n")
    cat("Initial bounds for r =", parbound$r[1], "-", parbound$r[2],"\n")
    cat("Initial bounds for k =", format(parbound$k[1], digits=3), "-", format(parbound$k[2],digits=3),"\n")
  }
  flush.console()
  
  ## MAIN
  
  R1 = sraMSY(parbound, n)  
  
  
  ## Get statistics on r, k, MSY and determine new bounds for r and k
  r1 	<- R1$r[R1$ell==1]
  k1 	<- R1$k[R1$ell==1]
  
  msy1  <- r1*k1/4
  mean_msy1 <- exp(mean(log(msy1))) 
  max_k1a  <- min(k1[r1<1.1*parbound$r[1]],na.rm=T) ## smallest k1 near initial lower bound of r
  max_k1b  <- max(k1[r1*k1/4<mean_msy1],na.rm=T) ## largest k1 that gives mean MSY
  max_k1 <- if(max_k1a < max_k1b) {max_k1a} else {max_k1b}
  
  if(length(r1)<10) {
    cat("Too few (", length(r1), ") possible r-k combinations, check input parameters","\n")
    flush.console()
  }
  
  if(length(r1)>=10) {
    
    ## set new upper bound of r to 1.2 max r1
    parbound$r[2] <- 1.2*max(r1)
    ## set new lower bound for k to 0.9 min k1 and upper bound to max_k1 
    parbound$k 	  <- c(0.9 * min(k1), max_k1)
    
    if (Display==1)	
    {
      cat("First MSY =", format(mean_msy1, digits=3),"\n")
      cat("First r =", format(exp(mean(log(r1))), digits=3),"\n")
      cat("New upper bound for r =", format(parbound$r[2],digits=2),"\n")	
      cat("New range for k =", format(parbound$k[1], digits=3), "-", format(parbound$k[2],digits=3),"\n")
    }
    
    ## Repeat analysis with new r-k bounds
    R1 = sraMSY(parbound, n)
    
    ## Get statistics on r, k and msy
    r = R1$r[R1$ell==1]
    k = R1$k[R1$ell==1]
    bvbmsy1<- R1$bvbmsy[,R1$bvbmsyell[1,]==1]
    time_bvbmsy<- (apply(bvbmsy1,1,function(x) exp(mean(log(x)))))
    
    mean_bvbmsy<- mean(apply(bvbmsy1,1,function(x) exp(mean(log(x)))))
    LogSD_bvbmsy<- mean(apply(bvbmsy1,1,function(x) (sd(log(x)))))
    
    
    
    msy = r * k / 4
    Fmsy<- r/2
    
    
    mean_ln_msy = mean(log(msy))
    
    #         Data$MSY[Where]<- mean(msy,na.rm=T)
    
    Data$RanCatchMSY[Where]<- TRUE
    
    Data$MSY[Where]<- exp(mean(log(msy)))
    
    Data$r[Where]<- exp(mean(log(r),na.rm=T))
    
    Data$k[Where]<- exp(mean(log(k),na.rm=T))
    
    Data$MSYLogSd[Where]<- (sd(log(msy)))
    
    Data$rLogSd[Where]<- (sd(log(r),na.rm=T))
    
    Data$KLogSd[Where]<- (sd(log(k),na.rm=T))
    
    Data$CatchMSYBvBmsy[Where]<- time_bvbmsy
    
    if (CatchMSYTrumps==T)
    {
      Data$BvBmsy[Where]<- time_bvbmsy
    }
    
    Data$CatchMSYBvBmsy_LogSd[Where]<- LogSD_bvbmsy
    
    Data$FvFmsy[Where]<- (Data$Catch[Where]/Data$MSY[Where])/Data$BvBmsy[Where]
    
    ## plot MSY over catch data
    if (NumCPUs==1)
    {
    par(mfcol=c(2,3))
    plot(yr, ct, type="l", ylim = c(0, max(ct)), xlab = "Year", ylab = "Catch (MT)", main = stock)
    abline(h=exp(mean(log(msy))),col="red", lwd=2)
    abline(h=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
    abline(h=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
    
    hist(r, freq=F, xlim=c(0, 1.2 * max(r)), main = "")
    abline(v=exp(mean(log(r))),col="red",lwd=2)
    abline(v=exp(mean(log(r))-2*sd(log(r))),col="red")
    abline(v=exp(mean(log(r))+2*sd(log(r))),col="red")
    
    plot(r1, k1, xlim = start_r, ylim = start_k, xlab="r", ylab="k (MT)")
    
    hist(k, freq=F, xlim=c(0, 1.2 * max(k)), xlab="k (MT)", main = "")
    abline(v=exp(mean(log(k))),col="red", lwd=2)	
    abline(v=exp(mean(log(k))-2*sd(log(k))),col="red")
    abline(v=exp(mean(log(k))+2*sd(log(k))),col="red")
    
    plot(log(r), log(k),xlab="ln(r)",ylab="ln(k)")
    abline(v=mean(log(r)))
    abline(h=mean(log(k)))
    abline(mean(log(msy))+log(4),-1, col="red",lwd=2)
    abline(mean(log(msy))-2*sd(log(msy))+log(4),-1, col="red")
    abline(mean(log(msy))+2*sd(log(msy))+log(4),-1, col="red")
    
    hist(msy, freq=F, xlim=c(0, 1.2 * max(c(msy))), xlab="MSY (MT)",main = "")
    abline(v=exp(mean(log(msy))),col="red", lwd=2)
    abline(v=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
    abline(v=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
    }
    if (Display==1)
    {
      cat("Possible combinations = ", length(r),"\n")
      cat("geom. mean r =", format(exp(mean(log(r))),digits=3), "\n")
      cat("r +/- 2 SD =", format(exp(mean(log(r))-2*sd(log(r))),digits=3),"-",format(exp(mean(log(r))+2*sd(log(r))),digits=3), "\n")
      cat("geom. mean k =", format(exp(mean(log(k))),digits=3), "\n")
      cat("k +/- 2 SD =", format(exp(mean(log(k))-2*sd(log(k))),digits=3),"-",format(exp(mean(log(k))+2*sd(log(k))),digits=3), "\n")
      cat("geom. mean MSY =", format(exp(mean(log(msy))),digits=3),"\n")
      cat("MSY +/- 2 SD =", format(exp(mean_ln_msy - 2 * sd(log(msy))),digits=3), "-", format(exp(mean_ln_msy + 2 * sd(log(msy))),digits=3), "\n")
      
    }
    
    bio<- bio*2 #Replace B/K with B/Bmsy
    
    FvFmsy<- (ct[length(ct)])/(bio[length(bio)]*msy)
    
    RealRuns<- length(r)
    
    RanCMSY<- TRUE
    
  } #Close if r1 is greater than 10
  
} #Close if there is catch loop


# Data$r[is.na(Data$r)]<- mean(Data$r,na.rm=T)

if (any(Data$HasRamFvFmsy))
{
  Data$FvFmsy<- RamFvFmsy
}

# if(NumCPUs>1)
# {
# sfCat(paste(round(100*(s/length(stock_id))),'% Done',sep="\n"))
# }
return(list(CatchMSY=Data,RanCatchMSY=RanCMSY)) 
} #Close function


