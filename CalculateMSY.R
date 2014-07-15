rm(list=ls())
# set.seed(999)  ## for same random sequence
# require(hacks)

## Read Data for stock, year=yr, catch=ct, and resilience=res. Expects space delimited file with header yr  ct and years in integer and catch in real with decimal point
## For example
## stock	res	 yr     ct       
## cap-icel	Medium 1984  1234.32 

### Controls ###

ExcludeSmallPelagics<- 1

ErrorSize<- 0.85 #The amount of error to serach over CatchMSY terms

Smooth<- 0 #Marks whether to smooth catch history

Display<- 0 #Display running outputs

runCatchMSY<- 0 #run CatchMSY or rely on saved results

BestValues<- 1 # 1 subs in RAM F/Fmsy and MSY values where possible

ManualFinalYear<- 0 #Set year you want to run all analyses for

n <- 200  ## number of iterations, e.g. 100000

SampleLength<- 100 # Number of subsampled bootstraps 

NoNEI<- 1 #Set to 1 to omit all nei listing


## filename <- "RAM_MSY.csv"
setwd('/Users/danovando/Desktop/Bren/SFG Work/Will Fish Recovery Increase Food')
filename <- "FOR ANALYSIS_V2_Final Yield Increase Database.csv"
# filename <- "Unassessed and Assessed Fishery Data for Catch MSY.csv"

outfile  <- "CatchMSY_Output.csv"
outfile2  <- "Clean_CatchMSY_Output.csv"

BatchNames<- 'No Forage Fish Version'
FigureFolder<- paste('Figures/',BatchNames,sep='')
ResultFolder<-  paste('Results/',BatchNames,sep='')
dir.create(ResultFolder)
dir.create(FigureFolder)

RawData <- read.csv(filename, header=T)

RawData$Price<- 1000

if (ExcludeSmallPelagics==1)
{
	RawData<- RawData[RawData$spcat!='Herrings, sardines, anchovies',]
}

head(RawData)

RawData$F.Fmsy.Source<- as.character(levels(RawData$F.Fmsy.Source))[RawData$F.Fmsy.Source]

RawData$MSY.Source<- as.character(levels(RawData$MSY.Source))[RawData$MSY.Source]

RawData$MSY<- as.numeric(RawData$MSY)

RAMF<- (RawData$F.Fmsy.Source=='RAM Legacy')

RAMMSY<- (RawData$MSY.Source=='RAM Legacy')


RawData$RAMF<- RawData$FvFmsy * RAMF

RawData$RAMMSY<- RawData$MSY * RAMMSY


RawData$RAMF[RAMF==F]<- NA

RawData$RAMMSY[RAMMSY==F]<- NA

if (NoNEI==1) #remove NEI stocks
{
	Where<- grepl('nei',RawData$CommName) | RawData$CommName	==0
	RawData<- RawData[Where==F,]
}


cdat<- data.frame(RawData$ID, RawData$Year, RawData$Landings, RawData$BvBmsy, RawData$r,RawData$MaxC,RawData$RAMF,RawData$RAMMSY,RawData$RAM)

colnames(cdat)<- c('stock','yr','ct','bvbmsy','res','MaxCatch','RAM_FvFmsy','RAM_MSY','RAM')
head(cdat)
output2<- NULL

# cdat$res<- as.numeric(levels(cdat$res))[cdat$res]

cdat$Srini_MSY<- 10^(0.8458*log10(cdat$MaxCatch)+0.3777) #Calculate Srinivassen MSY

cdat$Cos_MSY<- 1.78*10^(-.8644+1.0976*log10(cdat$MaxCatch)) #Calculate Costello JEM MSY

stock_id <- unique(as.character(cdat$stock)) 
## stock_id <- "cod-2224" ## for selecting individual stocks

cdat$FirstYear<- 0

cdat$FinalYear<- 0

for (i in 1:length(stock_id))
{
	Where<- cdat$stock==stock_id[i]
	
	# filter(cdat$ct[Where],rep(1,2),method='convolution',sides=2)
	
	Year1<- which(cdat$stock==stock_id[i] & is.na(cdat$ct)==F)[1]
		
	Temp<- which(cdat$stock==stock_id[i] & is.na(cdat$ct)==F & is.na(cdat$bvbmsy)==F)

	FinalYear<- Temp[length(Temp)]
	
	WhereMean<- cdat$stock==stock_id[i] & cdat$yr>=2006
	
	cdat$MeanCatch[Where]<- mean(cdat$ct[WhereMean],na.rm=T)
	
	cdat$FirstYear[Year1]<- 1
	
	cdat$FinalYear[FinalYear]<- 1
}

RecentCatch<- cdat$MeanCatch[cdat$FinalYear==1]

RecentRam<- cdat$RAM[cdat$FinalYear==1]

CatchOrder<- order(RecentCatch,decreasing=T)

pdf(file=paste(FigureFolder,'/Current Catch Distribution.pdf',sep=''),width=6,height=4)
plot(cumsum(RecentCatch[CatchOrder]),type='b',col=1+RecentRam[CatchOrder],pch=19,xlab='Stock',ylab='Cumulative Catch (tons)',bty='n')
legend('right',pch=19,col=c(1,2),legend=c('Non-RAM','RAM'))
dev.off()

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

## FUNCTIONS

sim_HB<- function(rvec,Kvec,MSYvec,bstart,Time,PolicyFlag) #Chris' projection model
{
	# %This function simulates for T periods and produces time series (plus CIs)
# %for raw harvest and raw biomass over time. Kvec, rvec, and MSYvec are Jx1 column
# %vectors
# %PolicyFlag =
# % 1 if close until Bmsy, then Fmsy (equil = 1.0)
# % 2 if F/Fmsy = .8 forever (equil = 1.2)
# % 3 if F/Fmsy = .66*B/Bmsy (equil = 1.2)
 
# %The simulation below produces J "scaled" harvests and J "scaled" biomasses
# rvec<- rMatrix[1,4:SampleLength+3]
# Kvec<- kMatrix[1,4:SampleLength+3]
# MSYvec<- msyMatrix[1,4:SampleLength+3]
# bstart<- rMatrix$bvbmsy[1]
# Time<- 50
# PolicyFlag<- 1

J <-  length(rvec)

b<- as.data.frame(matrix(NA,nrow=J,ncol=Time))
h<- as.data.frame(matrix(NA,nrow=J,ncol=Time))
f<- as.data.frame(matrix(NA,nrow=J,ncol=Time))

Htime<- as.data.frame(matrix(NA,nrow=J,ncol=Time))

b[,1] = bstart

for (t in 1:Time)
{
    if (PolicyFlag==1){ f[,t] <- 0 + 1*as.numeric(b[,t]>1)}
    if (PolicyFlag==2){ f[,t] <- .8}
    if (PolicyFlag==3){ f[,t]<- 0.666*b[,t]}
   
    h[,t] <- as.numeric(b[,t]*rvec*f[,t]/2) #scaled harvest term
    Htime[,t] <- as.numeric((2*h[,t]/rvec)*MSYvec)
    b[,t+1] <- as.numeric(b[,t]+ rvec*b[,t]*(1-b[,t]/2) - h[,t])
}
 
#Next, convert the "scaled" h and b to raw H and B

Bmsy_vec<- Kvec/2;

Btime <- b[,1:Time]*matrix(rep(as.numeric(Bmsy_vec),Time),nrow=J,ncol=Time)


return(list(Btime=Btime,B=b,Htime=Htime,H=h))

}


.schaefer	<- function(theta) # Schaefer model
{
	with(as.list(theta), {  ## for all combinations of ri & ki
		bt=vector()
		ell = 0  ## initialize ell
		for (j in startbt)
		{
			if(ell == 0) 
			{
				bt[1]=j*k*exp(rnorm(1,0, sigR))  ## set biomass in first year
				for(i in 1:nyr) ## for all years in the time series
				{
					xt=rnorm(1,0, sigR)
					bt[i+1]=(bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
				}
		
				#Bernoulli likelihood, assign 0 or 1 to each combination of r and k
				ell = 0
				# show(paste('k is',k))
				# show(paste('lam1 is',lam1))
				# show(paste('lam2 is',lam2))
				
				if(bt[nyr+1]/k>=lam1 && bt[nyr+1]/k <=lam2 && min(bt) > 0 && max(bt) <=k && bt[which(yr==interyr)]/k>=interbio[1] && bt[which(yr==interyr)]/k<=interbio[2]) 
				ell = 1
			}	
		}
		return(list(ell=ell))
		
		
	})
}

sraMSY	<-function(theta, N) #CatchMSY guts
{
	# theta=parbound
	# N=n
	#This function conducts the stock reduction
	#analysis for N trials
	#args:
	#	theta - a list object containing:
	#		r (lower and upper bounds for r)
	#		k (lower and upper bounds for k)
	#		lambda (limits for current depletion)
	
	
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
		ell = sapply(i, get.ell) 
		return(list(r=ri,k=ki, ell=ell))	
	})
}


if (runCatchMSY==1)
{
## Loop through stocks
for(s in 1:length(stock_id)) 
{	
	stock<- (stock_id[s])
	show(paste(round(100*(s/length(stock_id)),2),'% done',sep=''))
	yr   <- cdat$yr[as.character(cdat$stock)==stock]
	ct   <- as.numeric(cdat$ct[as.character(cdat$stock)==stock])/1000  ## assumes that catch is given in tonnes, transforms to 1'000 tonnes
	
	YearSpan<- cdat$FirstYear[cdat$stock==stock]+cdat$FinalYear[cdat$stock==stock]
	
	bio<- pmin(1,cdat$bvbmsy[cdat$stock==stock]/2) #pull out bvbmsy (transposed to B/K)
	
	RAM_F<- cdat$RAM_FvFmsy[cdat$stock==stock] #RAM fvfmsy is available
	
	RAM_MSY<- cdat$RAM_MSY[cdat$stock==stock] #RAM MSY if available
	
	FirstCatchYear<- which(YearSpan==1)[1] #Take a whole bunch of steps to chop data as needed

if (ManualFinalYear==0)
{
	FinalCatchYear<- which(YearSpan==1)[2]
	Flag<- 0
} # Close if (ManualFinalYear==0)
	
	if (ManualFinalYear>0)
	{
		
	FinalCatchYear<- NA
	if (max(yr,na.rm=T)>= ManualFinalYear)
	{
		FinalCatchYear<- which(yr== ManualFinalYear)
		
		if (is.na(ct[FinalCatchYear]))
		{
			Flag<- 1
		}
		
	}
} #close (ManualFinalYear>0)

	
	
	if (sum(ct,na.rm=T)>0 & sum(bio,na.rm=T)>0 & Flag==0 & length(FinalCatchYear)>0)
	{
	
	yr<- yr[FirstCatchYear: FinalCatchYear]
	ct<- ct[FirstCatchYear: FinalCatchYear]
	
	bio<- bio[FirstCatchYear:FinalCatchYear]
	RAM_F<- RAM_F[FirstCatchYear:FinalCatchYear]
	RAM_MSY<- RAM_MSY[FirstCatchYear:FinalCatchYear]

	if(Smooth==1){ct<- runmed(ct,3)}
	
	# res  <- unique(as.numeric((as.character(cdat$res[as.character(cdat$stock)==stock])))) ## resilience from FishBase, if needed, enable in PARAMETER SECTION

	res  <- (cdat$res[as.character(cdat$stock)==stock])[1] ## resilience from FishBase, if needed, enable in PARAMETER SECTION

	if(is.na(res)){res<- 0.5}
	
	nyr  <- length(yr)    ## number of years in the time series
	
	msy11<- cdat$Srini_MSY[cdat$stock==stock][1]

	msy2<- cdat$Cos_MSY[cdat$stock==stock][1]

	
# cat("\n","Stock",stock,"\n")
flush.console()
		
## PARAMETER SECTION

## If resilience is to be used, delete ## in rows 1-4 below and set ## in row 5	below
# start_r  <- if(res == "Very low"){c(0.015, 0.1)}
            # else if(res == "Low") {c(0.05,0.5)}
            # else if(res == "High") {c(0.6,1.5)}
            # else {c(0.2,1)} ## Medium, or default if no res is found
            
start_r  <- c((1-ErrorSize)*res,(1+ ErrorSize)*res)
                        	
## start_r     <- c(0.5,1.5)  ## disable this line if you use resilience
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
 
sigR        <- 0.0      ## process error; 0 if deterministic model; 0.05 reasonable value? 0.2 is too high

startbt     <- seq(startbio[1], startbio[2], by = 0.05) ## apply range of start biomass in steps of 0.05	
parbound <- list(r = start_r, k = start_k, lambda = finalbio, sigR)

if (Display==1)
{
cat("Last year =",max(yr),", last catch =",1000*ct[nyr],"\n")
cat("Resilience =",res,"\n")
cat("Process error =", sigR,"\n")
cat("Assumed initial biomass (B/k) =", startbio[1],"-", startbio[2], " k","\n")
cat("Assumed intermediate biomass (B/k) in", interyr, " =", interbio[1],"-",interbio[2]," k","\n")
cat("Assumed final biomass (B/k) =", parbound$lambda[1],"-",parbound$lambda[2]," k","\n")
cat("Initial bounds for r =", parbound$r[1], "-", parbound$r[2],"\n")
cat("Initial bounds for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")
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
	cat("First MSY =", format(1000*mean_msy1, digits=3),"\n")
	cat("First r =", format(exp(mean(log(r1))), digits=3),"\n")
	cat("New upper bound for r =", format(parbound$r[2],digits=2),"\n")	
	cat("New range for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")
}

## Repeat analysis with new r-k bounds
R1 = sraMSY(parbound, n)

## Get statistics on r, k and msy
r = R1$r[R1$ell==1]
k = R1$k[R1$ell==1]
msy = r * k / 4
mean_ln_msy = mean(log(msy))
pdf(file=paste(FigureFolder,'/',stock,'.pdf',sep=''))
	## plot MSY over catch data
	par(mfcol=c(2,3))
	plot(yr, ct, type="l", ylim = c(0, max(ct)), xlab = "Year", ylab = "Catch (1000 t)", main = stock)
	abline(h=exp(mean(log(msy))),col="red", lwd=2)
	abline(h=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
	abline(h=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
		
	hist(r, freq=F, xlim=c(0, 1.2 * max(r)), main = "")
	abline(v=exp(mean(log(r))),col="red",lwd=2)
	abline(v=exp(mean(log(r))-2*sd(log(r))),col="red")
	abline(v=exp(mean(log(r))+2*sd(log(r))),col="red")
	
	plot(r1, k1, xlim = start_r, ylim = start_k, xlab="r", ylab="k (1000t)")
	
	hist(k, freq=F, xlim=c(0, 1.2 * max(k)), xlab="k (1000t)", main = "")
	abline(v=exp(mean(log(k))),col="red", lwd=2)	
	abline(v=exp(mean(log(k))-2*sd(log(k))),col="red")
	abline(v=exp(mean(log(k))+2*sd(log(k))),col="red")


	plot(log(r), log(k),xlab="ln(r)",ylab="ln(k)")
	abline(v=mean(log(r)))
	abline(h=mean(log(k)))
	abline(mean(log(msy))+log(4),-1, col="red",lwd=2)
	abline(mean(log(msy))-2*sd(log(msy))+log(4),-1, col="red")
	abline(mean(log(msy))+2*sd(log(msy))+log(4),-1, col="red")

	hist(msy, freq=F, xlim=c(0, 1.2 * max(c(msy,msy11/1000,msy2/1000))), xlab="MSY (1000t)",main = "")
	abline(v=exp(mean(log(msy))),col="red", lwd=2)
	abline(v=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
	abline(v=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
	abline(v=msy11/1000,col='black') #Srini
	abline(v=msy2/1000,col='blue') #Costello

	dev.off()
	if (Display==1)
	{
	cat("Possible combinations = ", length(r),"\n")
	cat("geom. mean r =", format(exp(mean(log(r))),digits=3), "\n")
	cat("r +/- 2 SD =", format(exp(mean(log(r))-2*sd(log(r))),digits=3),"-",format(exp(mean(log(r))+2*sd(log(r))),digits=3), "\n")
	cat("geom. mean k =", format(1000*exp(mean(log(k))),digits=3), "\n")
	cat("k +/- 2 SD =", format(1000*exp(mean(log(k))-2*sd(log(k))),digits=3),"-",format(1000*exp(mean(log(k))+2*sd(log(k))),digits=3), "\n")
	cat("geom. mean MSY =", format(1000*exp(mean(log(msy))),digits=3),"\n")
	cat("MSY +/- 2 SD =", format(1000*exp(mean_ln_msy - 2 * sd(log(msy))),digits=3), "-", format(1000*exp(mean_ln_msy + 2 * sd(log(msy))),digits=3), "\n")

	}

## Write results into outfile, in append mode (no header in file, existing files will be continued)
output = data.frame(stock, sigR, startbio[1], startbio[2], interbio[1], interbio[2], finalbio[1], finalbio[2], min(yr), max(yr), res, max(ct), ct[1], ct[nyr], length(r), exp(mean(log(r))), sd(log(r)), min(r), quantile(r,0.05), quantile(r,0.25), median(r), quantile(r,0.75), quantile(r,0.95), max(r), exp(mean(log(k))), sd(log(k)), min(k), quantile(k, 0.05), quantile(k, 0.25), median(k), quantile(k, 0.75), quantile(k, 0.95), max(k), exp(mean(log(msy))), sd(log(msy)), min(msy), quantile(msy, 0.05), quantile(msy, 0.25), median(msy), quantile(msy, 0.75), quantile(msy, 0.95), max(msy)) 

output2<- rbind(output2,output)
write.table(output, file = outfile, append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)

bio<- bio*2 #Replace B/K with B/Bmsy

cdat$CatchMSY_MSY[cdat$stock==stock]<- 1000*exp(mean(log(msy)))

FvFmsy<- (ct[length(ct)])/(bio[length(bio)]*msy)

RealRuns<- length(r)

if (BestValues==1)
{
	if((is.na(RAM_F[length(RAM_F)])==F)) 
	{
		FvFmsy<- rep(RAM_F[length(RAM_F)],RealRuns)
		}

	if(sum(is.na(RAM_MSY)==F)>0) 
	{
		
		msy<-  rep(RAM_MSY[1]/1000,RealRuns)
	}

}


TotalResults2<- data.frame(rep(stock, RealRuns),rep(bio[length(yr)], RealRuns),rep(yr[length(yr)],RealRuns),1: RealRuns,FvFmsy,msy*1000,r,k*1000,1000*rep(ct[length(ct)], RealRuns))


TotalResults<- rbind(TotalResults,TotalResults2)


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


HasData<- is.na(rMatrix$stock)==F

rMatrix<- rMatrix[HasData,]
kMatrix<- kMatrix[HasData,]
msyMatrix<- msyMatrix[HasData,]
fMatrix<- fMatrix[HasData,]

colnames(TotalResults)<- c('stock','bvbmsy','finalyear','iteration','fvfmsy','msy','r','K','finalcatch')

colnames(SampleResults)<- c('stock','bvbmsy','finalyear','iteration','fvfmsy','msy','r','K','finalcatch')

} #CLose if MSY loop

if (runCatchMSY==0)
{
load(paste(ResultFolder,'/Completed Workspace.rdata',sep=''))
}


### 


### Diagnostic Plots ###

pdf(file=paste(FigureFolder,'/MSY Boxplot.pdf',sep=''))
boxplot(log10(data.frame(cdat$Srini_MSY,cdat$Cos_MSY,cdat$CatchMSY_MSY)),outline=T,notch=T,names=c('Srinivasen','Costello','CatchMSY'),ylab='Log10 MSY')
dev.off()

pdf(file=paste(FigureFolder,'/RawMSY Boxplot.pdf',sep=''))
boxplot((data.frame(cdat$Srini_MSY,cdat$Cos_MSY,cdat$CatchMSY_MSY)),outline=T,notch=T,names=c('Srinivasen','Costello','CatchMSY'),ylab='MSY')
dev.off()

pdf(file=paste(FigureFolder,'/Srinivasen vs Costello.pdf',sep=''))
plot(log(cdat$Srini_MSY),log(cdat$Cos_MSY),xlab='Log Srinivasen MSY',ylab='Log Costello MSY')
abline(a=0,b=1)
dev.off()

pdf(file=paste(FigureFolder,'/Srinivasen vs CatchMSY.pdf',sep=''))
plot(log(cdat$CatchMSY_MSY),log(cdat$Srini_MSY),ylab='Log Srinivasen MSY',xlab='Log CatchMSY MSY')
abline(a=0,b=1)
dev.off()

pdf(file=paste(FigureFolder,'/Costello vs CatchMSY.pdf',sep=''))
plot(log(cdat$CatchMSY_MSY),log(cdat$Cos_MSY),ylab='Log Costello MSY',xlab='Log CatchMSY MSY')
abline(a=0,b=1)
dev.off()


pdf(file=paste(FigureFolder,'/Raw Srinivasen vs Costello.pdf',sep=''))
plot((cdat$Srini_MSY),(cdat$Cos_MSY),xlab='Srinivasen MSY',ylab='Costello MSY')
abline(a=0,b=1)
dev.off()

pdf(file=paste(FigureFolder,'/Raw Srinivasen vs CatchMSY.pdf',sep=''))
plot((cdat$CatchMSY_MSY),(cdat$Srini_MSY),ylab='Srinivasen MSY',xlab='CatchMSY MSY')
abline(a=0,b=1)
dev.off()

pdf(file=paste(FigureFolder,'/Raw Costello vs CatchMSY.pdf',sep=''))
plot((cdat$CatchMSY_MSY),(cdat$Cos_MSY),ylab='Costello MSY',xlab='CatchMSY MSY')
abline(a=0,b=1)
dev.off()

# pdf(file=paste(FigureFolder,'/CatchMSY vs. RAM.pdf',sep=''))
# plot((cdat$CatchMSY_MSY),(1000*cdat$RAM_MSY),ylab='Costello MSY',xlab='CatchMSY MSY')
# abline(a=0,b=1)
# dev.off()


### Paper Figures ###

### Make Kobe Plot ###
ids<- unique(TotalResults$stock)

MeanKobe<- as.data.frame(matrix(NA,nrow=length(ids),ncol=7))

colnames(MeanKobe)<- c('stock','bvbmsy','fvfmsy','botf','topf','msy','ram')

SriniMSY<- NULL

CostelloMSY<- NULL
for (i in 1:length(ids))
{
	Where<- TotalResults$stock==ids[i]
	
	TempData<- TotalResults[Where,]
	
	InRam<- cdat$RAM[cdat$stock==as.character(ids[i])][1]

	if (sum(is.na(TempData$fvfmsy)==F)>0)
	{
	Fs<- sort(TempData$fvfmsy)
	}
	else
	{
		Fs<- TempData$fvfmsy
	}
	
	TopLoc<- floor(.975*length(Fs))
	
	BotLoc<- ceiling(.025*length(Fs))
	
	TopF<- Fs[TopLoc]
	
	BotF<- Fs[BotLoc]
	
	MeanKobe[i,]<- data.frame(ids[i],TempData$bvbmsy[1],mean(TempData$fvfmsy,na.rm=T),BotF,TopF,mean(TempData$msy,na.rm=T),InRam)
	
}

MeanKobe<- MeanKobe[MeanKobe$bvbmsy<1,]
MeanKobe$fvfmsy<- pmin(4,MeanKobe$fvfmsy)
MeanKobe$error<- MeanKobe$topf-MeanKobe$botf

library(lattice)

pdf(file=paste(FigureFolder,'/KobePlot Sized by MSY.pdf',sep=''))
plot( MeanKobe$fvfmsy ~ MeanKobe$bvbmsy,xlim=c(0,2),bty='n',col=MeanKobe$ram+1,xlab='B/Bmsy',ylab='FvFmsy',cex=log(MeanKobe$msy)/10,pch=19)
legend('topright',legend=c('Non-RAM','RAM'),pch=19,col=1:2,bty='n')
abline(a=2,b=-1,lty=2)
abline(h=1,v=1)
dev.off()

pdf(file=paste(FigureFolder,'/KobePlot Sized by FError.pdf',sep=''))
plot( MeanKobe$fvfmsy ~ MeanKobe$bvbmsy,xlim=c(0,2),bty='n',col=MeanKobe$ram+1,xlab='B/Bmsy',ylab='FvFmsy',cex=(MeanKobe$error),pch=19)
legend('topright',legend=c('Non-RAM','RAM'),pch=19,col=1:2,bty='n')
abline(a=2,b=-1,lty=2)
abline(h=1,v=1)
dev.off()


### Cumulative Distribution Graph ###

#For CatchMSY
ItMSYs<- sort(TotalResults$msy[TotalResults$iteration==1 & TotalResults$bvbmsy<1],decreasing=T)

CumMSYs<- cumsum(ItMSYs)

pdf(file=paste(FigureFolder,'/ Cumulative CatchMSY Contributions.pdf',sep=''))
plot(CumMSYs,type='b',bty='n',ylab='Cumulative MSY',xlab='Stock',pch=19)
dev.off()

#For Costello

CostelloMSY<- cdat$Cos_MSY[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T]

CostelloRAM<- cdat$RAM[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T]

CMSYOrder<- order(CostelloMSY,decreasing=T)


CumMSYs<- cumsum(CostelloMSY[CMSYOrder])

pdf(file=paste(FigureFolder,'/ Cumulative CostelloMSY Contributions.pdf',sep=''))
plot(CumMSYs,type='b',bty='n',ylab='Cumulative MSY',xlab='Stock',col=CostelloRAM[CMSYOrder]+1,pch=19)
legend('right',legend=c('Non-RAM','RAM'),pch=19,col=1:2,bty='n')
dev.off()


#For Srini

SriniMSY<- cdat$Srini_MSY[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T]

SriniRAM<- cdat$RAM[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T]

SriniMSYOrder<- order(SriniMSY,decreasing=T)


CumMSYs<- cumsum(SriniMSY[SriniMSYOrder])

pdf(file=paste(FigureFolder,'/ Cumulative SriniMSY Contributions.pdf',sep=''))
plot(CumMSYs,type='b',bty='n',ylab='Cumulative MSY',xlab='Stock',col= SriniRAM[SriniMSYOrder]+1,pch=19)
legend('right',legend=c('Non-RAM','RAM'),pch=19,col=1:2,bty='n')
dev.off()


### Make Yield Histograms ###

BelowBMSY<- which(msyMatrix$bvbmsy<1)

TotalMSY<- (colSums(msyMatrix[BelowBMSY,4:dim(msyMatrix)[2]],na.rm=T))

CurrentCatch_CMSY<- sum(msyMatrix$catch[BelowBMSY])

RealMSYData<- unique(cbind(as.character(cdat$stock),as.numeric(cdat$RAM_MSY)))

TotalResults2<- merge(TotalResults,RealMSYData,by.x=1,by.y=1)

CurrentCatch_Regression<- sum(TotalResults2$finalcatch[TotalResults2$iteration==1 & TotalResults2$bvbmsy<1 & is.na(TotalResults2$V2)] ,na.rm=T)

TotalCostelloMSY<- sum(cdat$Cos_MSY[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T],na.rm=T)

TotalSriniMSY<- sum(cdat$Srini_MSY[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T],na.rm=T)

TotalCurrentCatch<- sum(cdat$ct[cdat$FinalYear==1 & cdat$bvbmsy <1 & is.na(cdat$bvbmsy)==F & is.na(cdat$RAM_MSY)==T],na.rm=T)


MaxYear<- max(cdat$yr)

if (ManualFinalYear>0)
{
	MaxYear<- ManualFinalYear
}

# CurrentCatch<- sum(cdat$ct[cdat$yr==MaxYear],na.rm=T)

CatchMSY_RecoveyYieldChange<- 100*(TotalMSY/CurrentCatch_CMSY-1)

Sri_RecoveyYieldChange<- 100*(TotalSriniMSY/TotalCurrentCatch - 1)

Costello_RecoveyYieldChange<- 100*(TotalCostelloMSY/TotalCurrentCatch - 1)



pdf(file=paste(FigureFolder,'/ Percent Change in Overfished Yields Histogram.pdf',sep=''))
hist(CatchMSY_RecoveyYieldChange,xlab='% Change from Current Overfished Catch',xlim=c(-100,300),main=NULL,col='dodgerblue')
abline(v= Sri_RecoveyYieldChange,col=2,lty=2,lwd=2)
abline(v= Costello_RecoveyYieldChange,col=3,lty=2,lwd=2)
abline(v=0,col=1,lty=1,lwd=4)
dev.off()

pdf(file=paste(FigureFolder,'/Overfished MSY Histogram.pdf',sep=''))
hist(TotalMSY/1000,xlab='MSY (1000tons) of Overfished Fisheries',main=NULL,xlim=c(.75* TotalSriniMSY/1000,1.25*(TotalCostelloMSY/1000)))
abline(v= TotalSriniMSY/1000,col=2,lty=2,lwd=2)
abline(v=TotalCostelloMSY/1000,col=3,lty=2,lwd=2)
dev.off()


### Make Biomass Histograms ###

biomassMatrix<- kMatrix$bvbmsy[BelowBMSY]* (kMatrix[BelowBMSY,4:dim(kMatrix)[2]]/2) #Calculate Real Biomass

CurrentBiomass<- (colSums(biomassMatrix,na.rm=T))

FutureBiomass<- colSums(kMatrix[BelowBMSY,4:dim(kMatrix)[2]]/2,na.rm=T)

BiomassChange<- FutureBiomass-CurrentBiomass

PercentBiomassChange<- 100*(FutureBiomass/CurrentBiomass-1)


pdf(file=paste(FigureFolder,'/Percent Change in Overfished Biomass Histogram.pdf',sep=''))
hist(PercentBiomassChange,xlab='% Change in Overfished Biomass',main=NULL,col='dodgerblue')
dev.off()

pdf(file=paste(FigureFolder,'/Raw Change in Oversifhed Biomass Histogram.pdf',sep=''))
hist(BiomassChange/1000,xlab='Raw Change in Overfished Biomass (1000 tons)',main=NULL,col='dodgerblue')
dev.off()

### Save Outputs ###
 
write.csv(output2, file = paste(ResultFolder,'/',outfile2,sep=''))
write.csv(cdat, file = paste(ResultFolder,'/','NewInputData.csv',sep=''))
write.csv(TotalResults, file = paste(ResultFolder,'/','TotalResults.csv',sep=''))
write.csv(SampleResults, file = paste(ResultFolder,'/','SampleResults.csv',sep=''))

write.csv(cdat, file = paste(ResultFolder,'/','cdat.csv',sep=''))


write.csv(cdat, file = paste(ResultFolder,'/','cdat.csv',sep=''))


# SampleResults$fvfmsy[is.na(SampleResults$fvfmsy)]<- -999

# SampleResults$bvbmsy[is.na(SampleResults$bvbmsy)]<- -999


boMean<- aggregate(bvbmsy~stock,data=SampleResults,FUN=function(x) c(mean=mean(x)))

# foMean<- aggregate(fvfmsy~stock,data=SampleResults,FUN=function(x) c(mean=mean(x)))
msyMean<- aggregate(msy~stock,data=SampleResults,FUN=function(x) c(mean=mean(x)))

CatchMean<- aggregate(finalcatch~stock,data=SampleResults,FUN=function(x) c(mean=mean(x)))

foMean<- (CatchMean[,2]/msyMean[,2])/boMean[,2]

rMean<- aggregate(r~stock,data=SampleResults,FUN=function(x) c(mean=mean(x)))

SpcatQ<- read.csv(' Species Groups Lower Quartiles 2009 to 2009.csv')

ProjectionOutput<- cbind(boMean,foMean,CatchMean[,2],msyMean[,2],rMean[,2])
colnames(ProjectionOutput)<- c('stock','b0','f0','catch0','msy','mean_r')

RawData$Price<- 1000

Temp<- data.frame(RawData$ID, RawData$SciName, RawData$CommName,RawData$spcat,RawData$Country,RawData$Price)

Temp<- unique(Temp)

TotalProjectionOutput<- merge(ProjectionOutput,Temp,by.x=1,by.y=1,all.y=F,all.x=F,sort=F)
dim(TotalProjectionOutput)

SpcatTemp<- merge(data.frame(TotalProjectionOutput$stock, TotalProjectionOutput$RawData.spcat),SpcatQ,by.x=2,by.y=1,all.y=F,sort=F)

TotalProjectionOutput <- merge(TotalProjectionOutput, SpcatTemp,by.x=1,by.y=2,all.y=F,all.x=T,sort=F)

TotalProjectionOutput$TotalProjectionOutput.RawData.spcat<- NULL

colnames(TotalProjectionOutput)<- c(colnames(ProjectionOutput),'SciName','CommName','Spcat','Country','Price','boa')

write.csv(TotalProjectionOutput, file = paste(ResultFolder,'/','TotalProjectionOutput.csv',sep=''),row.names=F)


write.csv(rMatrix[is.na(rMatrix$stock)==F,], file = paste(ResultFolder,'/','rMatrix.csv',sep=''))
write.csv(kMatrix[is.na(rMatrix$stock)==F,], file = paste(ResultFolder,'/','kMatrix.csv',sep=''))
write.csv(msyMatrix[is.na(rMatrix$stock)==F,], file = paste(ResultFolder,'/','msyMatrix.csv',sep=''))
write.csv(fMatrix[is.na(rMatrix$stock)==F,], file = paste(ResultFolder,'/','fMatrix.csv',sep=''))




save.image(file=paste(ResultFolder,'/Completed Workspace.rdata',sep=''))



