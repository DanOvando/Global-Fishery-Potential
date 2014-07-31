######################################
#Calculate MSY--------------------------------------------------
#This code runs CatchMSY on fisheries
###################################### 

RunCatchMSY<- function(Data,ExcludeSmallPelagics,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,n,SampleLength,NoNei)
{
  
  #   Data<- GlobalStatus$Data
  #   
  #   ExcludeSmallPelagics<- 1
  #   
  #   ErrorSize<- 0.85 #The amount of error to serach over CatchMSY terms
  #   
  #   sigR<- 0 #Process Error Size
  #   
  #   Smooth<- 0 #Marks whether to smooth catch history
  #   
  #   Display<- 0 #Display running outputs
  #   
  #   runCatchMSY<- 0 #run CatchMSY or rely on saved results
  #   
  #   BestValues<- 1 # 1 subs in RAM F/Fmsy and MSY values where possible
  #   
  #   ManualFinalYear<- 0 #Set year you want to run all analyses for
  #   
  #   n <- 200  ## number of iterations, e.g. 100000
  #   
  #   SampleLength<- 100 # Number of subsampled bootstraps 
  #   
  #   NoNEI<- 1 #Set to 1 to omit all nei listing
  
  set.seed(999)  ## for same random sequence
  #  require(hacks)
  
  outfile  <- "CatchMSY_Output.csv"
  outfile2  <- "Clean_CatchMSY_Output.csv"
  
  
  if (ExcludeSmallPelagics==1)
  {
    Data<- Data[Data$SpeciesCatName!='Herrings, sardines, anchovies',]
  }
  
  
  # Data$res<- as.numeric(levels(Data$res))[Data$res]
  
  # Data$Srini_MSY<- 10^(0.8458*log10(Data$MaxCatch)+0.3777) #Calculate Srinivassen MSY
  # 
  # Data$Cos_MSY<- 1.78*10^(-.8644+1.0976*log10(Data$MaxCatch)) #Calculate Costello JEM MSY
  
  stock_id <- unique((Data[,IdVar])) 
  ## stock_id <- "cod-2224" ## for selecting individual stocks
  
  Data$FirstYear<- 0
  
  Data$FinalYear<- 0
  
  TotalResults<- NULL
  
  SampleResults<- NULL
  
  #   for (i in 1:length(stock_id))
  #   {
  #     
  #     Where<- Data[,IdVar]==stock_id[i]
  #     
  #     # filter(Data$ct[Where],rep(1,2),method='convolution',sides=2)
  #     
  #     Year1<- which(Data[,IdVar]==stock_id[i] & is.na(Data$Catch)==F)[1]
  #     
  #     Temp<- which(Data[,IdVar]==stock_id[i] & is.na(Data$Catch)==F & is.na(Data$BvBmsy)==F)
  #     
  #     FinalYear<- Temp[length(Temp)]
  #     
  #     Data$FirstYear[Year1]<- 1
  #     
  #     Data$FinalYear[FinalYear]<- 1
  #   }
  #   
  
  TotalResults<- NULL
  
  SampleResults<- NULL
  
  rMatrix<- as.data.frame(matrix(NA,nrow=length(stock_id),ncol=(SampleLength+3)))
  colnames(rMatrix)<- c('stock','bvbmsy','catch',paste('it',1:SampleLength,sep=''))
  
  kMatrix<- as.data.frame(matrix(NA,nrow=length(stock_id),ncol=(SampleLength+3)))
  colnames(kMatrix)<- c('stock','bvbmsy','catch',paste('it',1:SampleLength,sep=''))
  
  msyMatrix<- as.data.frame(matrix(NA,nrow=length(stock_id),ncol=(SampleLength+3)))
  colnames(msyMatrix)<- c('stock','bvbmsy','catch',paste('it',1:SampleLength,sep=''))
  
  fMatrix<- as.data.frame(matrix(NA,nrow=length(stock_id),ncol=(SampleLength+3)))
  colnames(fMatrix)<- c('stock','bvbmsy','catch',paste('it',1:SampleLength,sep=''))
  
  
  pdf(file=paste(FigureFolder,'Catch-MSY Diagnostics.pdf',sep=''))
  
  ## Loop through stocks
  for(s in 1:length(stock_id)) 
  {	
    stock<- (stock_id[s])
    
    Where<- Data[,IdVar]==stock
    
    show(paste(round(100*(s/length(stock_id)),2),'% done',sep=''))
    yr   <- Data$Year[(Data[,IdVar])==stock]
    ct   <- (Data$Catch[(Data[,IdVar])==stock])  ## assumes that catch is given in tonnes, transforms to 1'000 tonnes
    
    FirstCatchYear<- which(is.na(ct)==F)[1]
    
    LastYear<- which(is.na(ct)==F)
    
    LastCatchYear<- LastYear[length(LastYear)]
    
    
    bio<- pmin(1,Data$BvBmsy[Data[,IdVar]==stock]/2) #pull out bvbmsy (transposed to B/K)
    
    #     FvFmsy<- Data$FvFmsy[Data[,IdVar]==stock] #RAM fvfmsy is available
    
    #     RAM_MSY<- Data$RAM_MSY[Data[,IdVar]==stock] #RAM MSY if available
    
    if (sum(ct,na.rm=T)>0 & sum(bio,na.rm=T)>0& length(LastCatchYear)>0 & length(ct) >1)
    {
      
      yr<- yr[FirstCatchYear: LastCatchYear]
      ct<- ct[FirstCatchYear: LastCatchYear]
      
      bio<- bio[FirstCatchYear:LastCatchYear]
      
      if(Smooth==1){ct<- runmed(ct,3)}
      
      #       res  <- (Data$res[as.character(Data[,IdVar])==stock])[1] ## resilience from FishBase, if needed, enable in PARAMETER SECTION
      
      res<- 'Medium'
      
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
      startbio    <- pmin(1,c((1-ErrorSize)*bio[1],(1+ErrorSize)*bio[1]))
      
      if (is.na(bio[1]) | bio[1]==0)
      {
        startbio    <- if(ct[1]/max(ct,na.rm=T) < 0.5) {c(0.5,0.9)} else {c(0.3,0.6)} ## use for batch processing #SUB IN BVBMSY VALUES
      }
      
      interyr 	<- median(1:length(yr))   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available #SUB IN INTERMIN YEAR
      
      
      # interyr 	<- yr[2]   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available #SUB IN INTERMIN YEAR
      # interbio 	<- c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
      
      interbio 	<- pmin(1,c((1-ErrorSize)*bio[interyr],(1+ErrorSize)*bio[interyr])) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
      if (is.na(bio[interyr]) | bio[interyr]==0)
      {
        interbio 	<- c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
      }
      
      interyr<- yr[interyr]
      
      ## finalbio 	<- c(0.8, 0.9) ## biomass range after last catches, as fraction of k
      # finalbio    <- if(ct[nyr]/max(ct) > 0.5) {c(0.3,0.7)} else {c(0.01,0.4)} ## use for batch processing #SET TO KNOWN B/BMSY RANGE
      
      finalbio    <- pmin(1,c((1-ErrorSize)*bio[nyr],(1+ErrorSize)*bio[nyr]))
      
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
        msy = r * k / 4
        mean_ln_msy = mean(log(msy))
        
        #         Data$MSY[Where]<- mean(msy,na.rm=T)
        Data$MSY[Where]<- exp(mean(log(msy)))
        
        Data$r[Where]<- exp(mean(log(r),na.rm=T))
        
        Data$k[Where]<- exp(mean(log(k),na.rm=T))
        
        Data$MSYLogSd[Where]<- (sd(log(msy)))
        
        Data$rLogSd[Where]<- (sd(log(r),na.rm=T))
        
        Data$KLogSd[Where]<- (sd(log(k),na.rm=T))
        
        ## plot MSY over catch data
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
        
        ## Write results into outfile, in append mode (no header in file, existing files will be continued)
        output = data.frame(as.character(stock), sigR, startbio[1], startbio[2], interbio[1], interbio[2], finalbio[1], finalbio[2], min(yr), max(yr), res, max(ct), ct[1], ct[nyr], length(r), exp(mean(log(r))), sd(log(r)), min(r), quantile(r,0.05), quantile(r,0.25), median(r), quantile(r,0.75), quantile(r,0.95), max(r), exp(mean(log(k))), sd(log(k)), min(k), quantile(k, 0.05), quantile(k, 0.25), median(k), quantile(k, 0.75), quantile(k, 0.95), max(k), exp(mean(log(msy))), sd(log(msy)), min(msy), quantile(msy, 0.05), quantile(msy, 0.25), median(msy), quantile(msy, 0.75), quantile(msy, 0.95), max(msy)) 
        
        #         output2<- rbind(output2,output)
        write.table(output, file = outfile, append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
        
        bio<- bio*2 #Replace B/K with B/Bmsy
        
        FvFmsy<- (ct[length(ct)])/(bio[length(bio)]*msy)
        
        RealRuns<- length(r)
        
        #         if (BestValues==1)
        #         {
        #           if((is.na(RAM_F[length(RAM_F)])==F)) 
        #           {
        #             FvFmsy<- rep(RAM_F[length(RAM_F)],RealRuns)
        #           }
        #           
        #           if(sum(is.na(RAM_MSY)==F)>0) 
        #           {
        #             
        #             msy<-  rep(RAM_MSY[1],RealRuns)
        #           }
        #           
        #         }
        #         
        
        
        
        
        ### Bootstrap Samples###
        
        # if (RealRuns>=SampleLength)
        # {
        
        SampleRuns<- sample(1:RealRuns, SampleLength,replace=T)
        
        SampleResults2<- data.frame(rep(stock, SampleLength),rep(bio[length(yr)], SampleLength),rep(yr[length(yr)], SampleLength),1: SampleLength,FvFmsy[SampleRuns],msy[SampleRuns]*1000,r[SampleRuns],k[SampleRuns]*1000,1000*rep(ct[length(ct)], SampleLength))
        
        SampleResults<- rbind(SampleResults,SampleResults2)
        
        rMatrix[s,1:3]<- data.frame(as.character(stock),bio[length(yr)],1000*ct[length(yr)], stringsAsFactors=FALSE)
        rMatrix[s,4:dim(rMatrix)[2]]<- r[SampleRuns]
        
        kMatrix[s,1:3]<- data.frame(stock,bio[length(yr)],1000*ct[length(yr)],stringsAsFactors=FALSE)
        kMatrix[s,4:dim(rMatrix)[2]]<- 1000*k[SampleRuns]
        
        msyMatrix[s,1:3]<- data.frame(stock,bio[length(yr)],1000*ct[length(yr)],stringsAsFactors=FALSE)
        msyMatrix[s,4:dim(rMatrix)[2]]<- 1000*msy[SampleRuns]
        
        fMatrix[s,1:3]<- data.frame(stock,bio[length(yr)],1000*ct[length(yr)],stringsAsFactors=FALSE)
        fMatrix[s,4:dim(rMatrix)[2]]<- FvFmsy[SampleRuns]
        
        # }
        
        
      } #Close if r1 is greater than 10
    } #Close if there is catch loop
  }  ## End of stock loop, get next stock or exit
  
  dev.off()
  
  HasData<- is.na(rMatrix$stock)==F
  
  rMatrix<- rMatrix[HasData,]
  kMatrix<- kMatrix[HasData,]
  msyMatrix<- msyMatrix[HasData,]
  fMatrix<- fMatrix[HasData,]
  #   
  #   colnames(TotalResults)<- c('stock','bvbmsy','finalyear','iteration','fvfmsy','msy','r','K','finalcatch')
  #   
  #   colnames(SampleResults)<- c('stock','bvbmsy','finalyear','iteration','fvfmsy','msy','r','K','finalcatch')
  #   
  
  CountryMsy<- ddply(Data[is.na(Data$MSY)==F,],c('Country','Year'),summarize,CurrentCatch= sum(Catch,na.rm=T),MSY=sum(MSY,na.rm=T),TotalGain=sum(MSY,na.rm=T)-sum(Catch,na.rm=T),
                     PercGain=(100*(sum(MSY,na.rm=T)/sum(Catch,na.rm=T)-1)),MedianBvBmsy=median(BvBmsy,na.rm=T),PercMissing=100*(sum(is.na(MSY))/length(MSY)))
  
  PercGainOrder<- order(CountryMsy$PercGain,decreasing=T)
  
  CountryMsy<- CountryMsy[PercGainOrder,]
  
  return(list(Data=Data,CountryMsy=CountryMsy,rMatrix=rMatrix,kMatrix=kMatrix,msyMatrix=msyMatrix,fMatrix=fMatrix,MoreResults=SampleResults)) 
} #Close function

