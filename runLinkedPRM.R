
 runLinkedPRM<- function(catchhistory,lifehistory,year,theta)
 {	
### runLinkedPRM ###
#By Dan Ovando

#This code allows the user to calcualte estimates of median B/Bmsy for a group of fisheries, using the methods described in Costello et al. 2012, by inputting landings and life history data. See Included help document for detailed instructions. LinkedPRM is linked to the fishbase database compiled for Costello et al. 2012


###############################
### Load in and prepare required data ###
###############################
chist=catchhistory
custom_lh=lifehistory

if (is.factor(chist$catch))
{
	chist$catch<- as.numeric(levels(chist$catch))[chist$catch] 
}

if (is.factor(chist$sname))
{
	chist$sname<- as.character(levels(chist$sname))[chist$sname]
}

load('mpack.rdata') #load in regression coefficients and required data for retrandformatino bias
cf<- mpack$cf #model coefficients
lh<- mpack$lh #life history data from fishbase
lh$sname<- tolower(lh$sname) #scientific names
residser<- mpack$residser #series of residuals from regression 
stdbin<- mpack$stdbin #bins of standard deviations from regression
testvariance<- mpack$testvariance #Artifact 
# source('transbias.R') #code for apply retransformation bias
# source('coreprep.R') # code to prepare data for transbias


###################################
### Prepare Data for Regression ###
###################################

rpack<- as.data.frame(matrix(NA,nrow=1,ncol=dim(cf)[2])) #make blank object to store results in
colnames(rpack)=colnames(cf)
rpack$id<- NA
rpack$spcat<- NA

fs<- unique(chist$id) #fishery id numbers
originalspcat<- matrix(NA,nrow=length(fs),ncol=2)
dc<- 0
for (f in 1:length(fs)) #loop over each fishery 
{
temp<- chist[chist$id==fs[f],] #find catch history
trpack<- as.data.frame(matrix(NA,nrow=dim(temp)[1],ncol=dim(cf)[2]))
colnames(trpack)=colnames(cf)
cmax<- max(temp$catch,na.rm=T) #max recorded landings
temp$catch<- temp$catch/cmax #scale landings by max
#chop early fishing history out (catch <.15 max catch)
start<- which(temp$catch>.15)[1] #find beginning of catch history
if (is.na(start)==T)
{
	start<- 0
}
temp<- temp[start:dim(temp)[1],] #shorten catch history to appropriate length
missing_tally<- sum(is.na(temp$catch))/length(temp$catch) #find amount of missing data
c<- 0
if (missing_tally>.1 | length(temp$catch)<7 | start==0)
{
	warning(paste('FISHERY',fs[f],'DOES NOT HAVE ENOUGH CATCH HISTORY TO RUN'))
}


if (missing_tally<=.1 & length(temp$catch)>=7 & start>0) #If there is enought catch data, continue data preperation
{
	#Fill in missing data by interpolation
	interp<- approx(temp$catch,xout=which(is.na(temp$catch)==T))
	temp$catch[interp$x]<- interp$y
		for (t in 1:(dim(temp)[1]-4)) #loop over each fisheries timline and fill in catch data
		{
			c<- c+1
			trpack[c,2]<- (dim(temp)[1]-3)-t #calculate inverse age
			trpack[c,3:7]<- temp$catch[(t+4)-(4:0)] #create lagged catch
			trpack[c,8]<- which(temp$catch==1)[1] #calculate time to max catch
			trpack[c,9]<- sum(temp$catch[4:6])/sum(temp$catch[1:3]) #slope over the first few years
			trpack[c,10]<- cmax #max catch
			trpack[c,11]<- mean(temp$catch) #average catch
			trpack[c,12]<- temp$catch[t+4]/max(temp$catch[1:(t+3)]) #ratio of current harvest to prior maximum
			trpack[c,13]<- temp$year[t+4] #current year
		}#close loop over t
	#Fill in life history data from fishbase	
	lh_match=match(tolower(temp$sname[1]),lh$sname) #find matching species names
	spcat=NA
	trpack[,26]=1 #create space for constant

	if (is.na(lh_match)==F)
	{
	lh_data=lh[lh_match,] #pull out matching data
	spcat=lh_data$spcat #identify the species category
	
	#Add in life history
	trpack[,27]=lh_data$maxl
	trpack[,28]=lh_data$vbk
	trpack[,29]=lh_data$area
	trpack[,30]=lh_data$agem
	trpack[,31]=lh_data$temp
	}
if (is.numeric(custom_lh)==F) #If there is a file for custom researched life history, sub data in here
	{	
		tlh<- custom_lh[custom_lh$id==fs[f],]
		tlh$id<- NULL
		custom_spcat<- tlh$spcat
		tlh$spcat<- NULL
		if (is.na(custom_spcat)==F)
		{
			spcat=custom_spcat
		}
		what<- colnames(tlh)[is.na(tlh)==F]	
		if (length(what)>0) #if there is any data
		{
			for (w in 1:length(what))
			{
				where<- colnames(trpack)==what[w]
				where2<- colnames(tlh)==what[w]
				trpack[,where]<- tlh[,where2]		
			} #close loop over custom variables that are present
		}
		}	
	if (is.na(spcat)==F)
	{
	catlist<- c(32,33,34,35,36,37,42,43,44,45,52,55)	#list of possible species categories
	if (spcat!=31 & sum(spcat==catlist)==0) #if the species isn't the dropped category and isn't in the list, find closest match
	{
		diff<- which(abs(catlist-spcat)==min(abs(catlist-spcat)))[1]
		dc=dc+1
		originalspcat[dc,]<- c(fs[f],spcat) #store original species category
		spcat<- catlist[diff]
	}
	tempfix<- matrix(rep(as.numeric(catlist==spcat),dim(trpack)[1]),nrow=dim(trpack)[1],ncol=length(catlist),byrow=T) #create fixed effects
	trpack[,14:25]<- tempfix
	
	### Identify which model each fishery runs on ###
	bool<- (as.matrix(is.na(cf))) #mark present variables in each dataset
	bool<- (bool[,2:dim(bool)[2]])
	struct<- t(is.na(trpack[1,2:dim(trpack)[2]]))
	w<- (bool %*% struct)
	whichmodel<- which(w==max(w))[1] #Identify which model each fishery runs on 	
	if (spcat>=40) #ignore, a trial invertebrate method
	{
		whichmodel<- 7
	}
	trpack$id<- fs[f]
	trpack$model<- whichmodel	
	trpack$spcat<- spcat
	rpack<- rbind(rpack,trpack) #stack fisheries on top of each other
	
	} #close if spcat is real

		if (is.na(spcat)==T)
		{
			warning(paste('FISHERY ID#',fs[f],'NEEDS A CUSTOM SPECIES CATEGORY, NOT INCLUDED'))
		}
	

	} #close if over missing_tally
} #close loop over fisheries

##################################################################
### Apply regresion coefficients and run retransformation bias ###
##################################################################

if (colSums(is.na(rpack))[1]!=dim(rpack)[1])
{
	

rpack<- rpack[is.na(rpack$inv_age)==F,] #remove extra blanks
### Remove Invertebrates etc. ###
inverts<- rpack$model==7
rpack<- rpack[inverts==F,]
###Predict B/Bmsy ###
idtemp<- rpack$id
cattemp<- rpack$spcat
rpack$id<- NULL
rpack$spcat<- NULL

mods<- unique(rpack$model) #Models needed to run
bvbtemp<- NA
for (m in 1:length(mods)) #loop over models
{
	where=rpack$model==mods[m] #find location of model m
	ptemp=as.matrix(rpack[where,2:dim(rpack)[2]]) #pull out parameters
	ptemp[is.na(ptemp)]=0 #convert nans to 0
	ctemp=t(cf[mods[m],2:dim(cf)[2]]) #model coefficients
	ctemp[is.na(ctemp)]=0 #convert nans to 0
	bvbtemp[where]=exp( ptemp %*% ctemp ) #multiply coefficients by parameters
}
rpack$bvb=bvbtemp #put raw bvbmsy values in
rpack$spcat=cattemp 
rpack$id=idtemp
### Apply  retransformation transbias ###
rpack$model[rpack$model==7]=99 #mark inverts
rcore=coreprep(idtemp,rpack$model,rpack$year,rpack$bvb) #prepare data for transbias
corr=transbias(rcore,residser,stdbin,1000,.9) #pass to transbias, code that performs retransformatino bias
corr$ind=cbind(corr$ind,rpack$bvb)
colnames(corr$ind)=c('id','years','mean','top','bot','rawbvb') #store raw bvbmsy values

##################################################################
### Prepare Results ###
##################################################################

### Replace modified species categories for regression with appropriate species categories ###

if (sum(is.na(originalspcat[,1]))!=dim(originalspcat)[1])
{
originalspcat<- originalspcat[is.na(originalspcat[,1])==0,]
for (o in 1:dim(originalspcat)[1])
{
	wo<- rpack$id==originalspcat[o,1]
	rpack$spcat[wo]<- originalspcat[o,2]
}
}
### Locate data that you want to process for summary ###
if (year=='LAST')
{
	where<- corr$med$year==max(corr$med$year)
	whereland<- rpack$year==max(rpack$year)
	fyear<- max(corr$med$year)
}
if (is.numeric(year))
{
	where<- corr$med$year %in% year
	whereland<- rpack$year %in% year
	fyear<- year
}


### Prepare summary data ###

summary<- as.data.frame(matrix(NA,nrow=sum(where),ncol=12))
colnames(summary)=c('year','nof','bvb','bci','tci','lnd','of','lof','uof','yi','fyi','bi')

summary$year<- corr$med$year[where]
summary$nof<- sum(corr$ind$years==fyear)
summary$bvb<- corr$med$med[where]
summary$bci<- corr$med$bot[where]
summary$tci<- corr$med$top[where]
summary$lnd<- sum(rpack$sH_t[whereland]*rpack$hmax[whereland])
summary$of<- corr$over$med[where]
summary$lof<- corr$over$bot[where]
summary$uof<- corr$over$top[where]

ucurrent<- (2-summary$bvb)+theta*(1.8-(2-summary$bvb))
bfuture<- 2-ucurrent

summary$yi<- 100*(1/(ucurrent*summary$bvb)-1)
summary$fyi<- 100*(1/(ucurrent*bfuture)-1)
summary$bi<- 100*(1/summary$bvb-1)

colnames(summary)=c('YEAR','NUMBER OF FISHERIES','BVBMSY','LOWCI','HIGHCI','TOTAL LANDINGS','PROBABILITY OVERFISHED','bottom95_overfished','top95_overfished','CURRENT PERCENT YIELD CHANGE','FUTURE PERCENT YIELD CHANGE','PERCENT BIOMASS CHANGE')

out<- list(data=rpack,result=corr,summary=summary)
} #close if there are any results
if (colSums(is.na(rpack))[1]==dim(rpack)[1]) #IF there are no results
{

warning('Not enough data to run any species; make sure you have at least supplied species categories')
out='Not enough data to run any species; make sure you have at least supplied species categories'

}

### Spit out results ###
return(out)
}

transbias<- function(core,residser,stdbin,J,bin)
{
#Function for applying retransformation bias routine described in Costello et al. 2012
#core: the core data passed to function
# residser: an alternative way to run the method, not currently used
# stdbin: bins of standard deviations from the regression over time
# J: number of iterations
# bin: bin betwen small vs large, always leave as 0.9
####################
### Perpare Data ###
####################	
# require(matlab)

testvariance<- 0
stdbin$high[is.na(stdbin$high)]<- 0
stdbin$low[is.na(stdbin$low)]<- 0
originalstd<- stdbin

um<- sort(unique(core$mod)) #unique models in core

core$bvb<- exp(core$bvb) #exponentiate raw bvbmsy values 

years<- sort(unique(core$years)) #unique years

fs<- sort(unique(core$id)) #unique fishery ids

bt<- matrix(NA,nrow=dim(core)[1],ncol=1) #blank matrix for results

bt[core$bvb>=bin,1]<- 1 #mark bin locations

bt[core$bvb<bin,1]<- 0 #mark bin locations

core<- cbind(core,bt)

core<- as.data.frame(core)

indtemp<- matrix(NA,nrow=dim(core)[1],ncol=5) #Matrix for individual fishery results

indtemp<- as.data.frame(indtemp)

colnames(indtemp)=c('id','years','mean','top','bot')

colnames(core)=c('id','mod','years','bvb','bin') #storage matrix

c<- matrix(NA,nrow=length(years),ncol=4)
c<- as.data.frame(c)	
colnames(c)=c('year','med','top','bot')


bin<- matrix(NA,nrow=3,ncol=4)
bin<- as.data.frame(bin)
colnames(bin)=c('<4','4to8','8to12','>12')
rownames(bin)=c('med','bot','top')


collapsed<- matrix(NA,nrow=length(years),ncol=4)
colnames(collapsed)=c('year','med','bot','top')
collapsed<- as.data.frame(collapsed)

under<- matrix(NA,nrow=length(years),ncol=4)
colnames(under)=c('year','med','bot','top')
under<- as.data.frame(under)

over<- matrix(NA,nrow=length(years),ncol=4)
colnames(over)=c('year','med','bot','top')
over<- as.data.frame(over)

####################################
###Perform retransformation bias ###
####################################

	for (y in 1:length(years)) #loop over years
	{
		
		where<- core$years==years[y] #find year locations
		
		tcore<- core[where,] #temporary core
		
		jstore<- matrix(NA,nrow=dim(tcore)[1],ncol=J) #blank matrix for bootstrap
		
		for (m in 1:length(um)) #loop over models
		{
			
			stdbin<- originalstd #reset stdbin
			if (um[m]==99)
			{
				stdbin<- inv_stdbin #ignore this, for invertebrate investigation
			}
			
			whereh<- tcore$mod==um[m] & tcore$bin==1 & is.na(tcore$bvb)==F	#find fisheries in high bin		
			wherel<- tcore$mod==um[m] & tcore$bin==0 & is.na(tcore$bvb)==F #find fisheries in low bin	
			
			
			### Series of steps to deal with some time issues ###
			if (years[y]<=max(stdbin$years) & years[y]>=1955)
			{
				wheres<- stdbin$years==years[y] & stdbin$mod==um[m]
				
				highstd<- stdbin$high[wheres]
				
				lowstd<- stdbin$low[wheres]		
			}
			if (years[y]<min(stdbin$years))
			{
				wheres<- stdbin$years==min(stdbin$years) & stdbin$mod==um[m]
				
				highstd<- stdbin$high[wheres]
				
				lowstd<- stdbin$low[wheres]			
			}
					if (years[y]>max(stdbin$years))
			{
				wheres<- stdbin$years==max(stdbin$years) & stdbin$mod==um[m]
				
				highstd<- stdbin$high[wheres]
				
				lowstd<- stdbin$low[wheres]			
				
			}
		
		 if (testvariance==1) #Tool for doing some diagnostics
		 {
		 	highstd<- highstd*vartest
		 	lowstd<- lowstd*vartest
		 }
		
		
		### Apply error terms ###
		
			jstd<- rnorm(sum(whereh,na.rm=T)*J)
			dim(jstd)<- c(sum(whereh,na.rm=T),J)
			jstore[whereh,]<- rep(log(tcore$bvb[whereh]),J) +highstd*jstd			
			jstd<- rnorm(sum(wherel,na.rm=T)*J)
			dim(jstd)<- c(sum(wherel,na.rm=T),J)
			jstore[wherel,]<- rep(log(tcore$bvb[wherel]),J) +lowstd*jstd

		}
		
		
		### Store Results ###
		
		### % collapsed###
		ctemp<- exp(jstore)<=.2
		ctemp<- colSums(ctemp,na.rm=T)/sum(where)
		csort<- sort(ctemp)
		ctop<- csort[ceiling(0.975*length(csort))]
		cbot<- csort[ceiling(0.025*length(csort))]
		cmed<- median(csort,na.rm=T)
		
		### % B > Bmsy ###
		otemp<- exp(jstore)<1
		otemp<- colSums(otemp,na.rm=T)/sum(where)
		osort<- sort(otemp)
		otop<- osort[ceiling(0.975*length(osort))]
		obot<- osort[ceiling(0.025*length(osort))]
		omed<- median(osort,na.rm=T)
		
		### % B<Bmsy ###
		utemp<- exp(jstore)>=1
		utemp<- colSums(utemp,na.rm=T)/sum(where)
		usort<- sort(utemp)
		utop<- usort[ceiling(0.975*length(usort))]
		ubot<- usort[ceiling(0.025*length(usort))]
		umed<- median(usort,na.rm=T)


		### % in different bins ###
		
		b1temp<- exp(jstore)<=0.4
		b1temp<- colSums(b1temp,na.rm=T)/sum(where)
		b1sort<- sort(b1temp)
		b1top<- b1sort[ceiling(0.975*length(b1sort))]
		b1bot<- b1sort[ceiling(0.025*length(b1sort))]
		b1med<- median(b1sort,na.rm=T)

		b2temp<- exp(jstore)<=0.8 & exp(jstore)>0.4
		b2temp<- colSums(b2temp,na.rm=T)/sum(where)
		b2sort<- sort(b2temp)
		b2top<- b2sort[ceiling(0.975*length(b2sort))]
		b2bot<- b2sort[ceiling(0.025*length(b2sort))]
		b2med<- median(b2sort,na.rm=T)

		b3temp<- exp(jstore)<=1.2 & exp(jstore)>0.8
		b3temp<- colSums(b3temp,na.rm=T)/sum(where)
		b3sort<- sort(b3temp)
		b3top<- b3sort[ceiling(0.975*length(b3sort))]
		b3bot<- b3sort[ceiling(0.025*length(b3sort))]
		b3med<- median(b3sort,na.rm=T)

		b4temp<- exp(jstore)>1.2
		b4temp<- colSums(b4temp,na.rm=T)/sum(where)
		b4sort<- sort(b4temp)
		b4top<- b4sort[ceiling(0.975*length(b4sort))]
		b4bot<- b4sort[ceiling(0.025*length(b4sort))]
		b4med<- median(b4sort,na.rm=T)		

		
		### Store results ###
		
		collapsed[y,]<- c(years[y],cmed,cbot,ctop)

		over[y,]<- c(years[y],omed,obot,otop)

		under[y,]<- c(years[y],umed,ubot,utop)
				
		jtemp<- apply(exp(jstore),2,median,na.rm=T) #calculate median of each column
		
		itemp<- apply(exp(jstore),1,median,na.rm=T) # Return B/Bmsu for individua' fishery (different than group)
		
		isort<- t(apply(exp(jstore),1,sort))
		
		top<- isort[,ceiling(0.975*dim(isort)[2])]

		bot<- isort[,ceiling(0.025*dim(isort)[2])]
		
		###Store results for individual fisheries###
		indtemp[where,1]<- core$id[where]
		indtemp[where,2]<- years[y]
		indtemp[where,3]<- itemp
		indtemp[where,4]<- top
		indtemp[where,5]<- bot
		
		jsort<- sort(jtemp) #Sort medians
	
		bin[,1]<- c(b1med,b1bot,b1top)
		bin[,2]<- c(b2med,b2bot,b2top)
		bin[,3]<- c(b3med,b3bot,b3top)
		bin[,4]<- c(b4med,b4bot,b4top)


		med<- mean(jsort,na.rm=T) #mean of medians
		top<- jsort[ceiling(0.975*length(jsort))] #upper CI
		bot<- jsort[ceiling(0.025*length(jsort))] #lower CI
		
		c[y,]<- c(years[y],med,top,bot)
		
	}

output<- list(med=c,ind=indtemp,collapsed=collapsed,over=over,under=under,bin=bin,dist=jsort)
return(output)	
}
coreprep<- function(id,mod,years,bvb)
{
	t<- cbind(id,mod,years,bvb)
	colnames(t)=c('id','mod','years','bvb')
	core<- as.data.frame(t)
	core$bvb<- log(core$bvb)
	return(core)	
}

