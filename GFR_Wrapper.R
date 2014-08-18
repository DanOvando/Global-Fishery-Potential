
######################################
#Global Fishery Recovery Wrapper--------------------------------------------------
# This code executes the steps in the Global Fishery Recovery program
######################################

# Source Functions --------------------------------------------------------

sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

# Read in and process data ------------------------------------------------------------

if (file.exists(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))==F)
{
  
  source('Database_Build.r') #Build Tyler's database
  
  # fulldata<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))
  
  FullData<- fulldata
  
  rm(fulldata)
  
  CleanedData<- MaidService(FullData)
  
  DroppedStocks<- CleanedData$DroppedStocks
  
  FullData<- CleanedData$CleanedData
  
  
  FullData<- FindFishbase(FullData)
  
  rm(CleanedData)
  
  write.csv(file=paste(ResultFolder,'Raw Compiled Database.csv',sep=''),FullData)
  
  write.csv(file=paste(ResultFolder,'Omitted Stocks.csv',sep=''),DroppedStocks)
  
}
if (file.exists(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))){FullData<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))}

# FullData$SpeciesCatName<- as.factor( FullData$SpeciesCatName)

FullData$ReferenceBiomass[FullData$ReferenceBiomass==0]<- NA

ModelNames<- names(Regressions)

for (m in 1:length(ModelNames))
{
  
  eval(parse(text=paste('FullData$',ModelNames[m],'Marker<- FALSE',sep='')))
  
  eval(parse(text=paste('FullData$',ModelNames[m],'Prediction<- NA',sep='')))
}

# Where<- FullData[,'AgeMat']==0 | is.na(FullData[,'AgeMat'])
# 
# FullData[Where,'AgeMat']<- NA

SofiaData<-  FullData[FullData$Dbase=='SOFIA',]

RamData<- FullData[FullData$Dbase=='RAM',]

RamData$BvBmsy[RamData$BvBmsy>OutlierBvBmsy]<- NA

FaoData<- FullData[FullData$Dbase=='FAO',]

FaoData<- LumpFisheries(FaoData,SpeciesCategoriesToLump)

# 
FaoIdSample<- sample(unique(FaoData[,IdVar]),500,replace=FALSE)
# # # 
FaoData<- FaoData[FaoData[,IdVar] %in% FaoIdSample,]

show('Raw Data Processed')

# Create synthetic stocks -------------------------------------------------

if (GroupMethod=='All')
{
  Groups<- unique(FullData$SpeciesCatName,na.rm=T)
  
  Groups<- Groups[is.na(Groups)==F]
}
if (GroupMethod=='Nei')
{
  Groups<- unique(FaoData$SpeciesCatName[ (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F])
}

SyntheticData<- StitchFish(RamData,IdVar,Groups,GroupSamples,Iterations) 

SyntheticData$BvBmsy[SyntheticData$BvBmsy>OutlierBvBmsy]<- NA

show('Synthetic Stocks Created')

for (m in 1:length(ModelNames))
{
  
  eval(parse(text=paste('SyntheticData$',ModelNames[m],'Marker<- FALSE',sep='')))
  
  eval(parse(text=paste('SyntheticData$',ModelNames[m],'Prediction<- NA',sep='')))
  
}

# Prepare data for regression ---------------------------------------------

library(proftools)

#  Rprof()

RamData<- FormatForRegression(RamData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)#Add resgression data to database

SyntheticData<- FormatForRegression(SyntheticData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)

SofiaData<- SofiaData[is.na(SofiaData$Catch)==F,]

SofiaData<- FormatForRegression(SofiaData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)#Add resgression data to database


if (file.exists(paste(ResultFolder,'FaoData.Rdata',sep=''))==F)
{
  
  FaoData<- FormatForRegression(FaoData,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)
  
  save(file=paste(ResultFolder,'FaoData.Rdata',sep=''),FaoData)
}
if (file.exists(paste(ResultFolder,'FaoData.Rdata',sep='')))
{
  load(paste(ResultFolder,'FaoData.Rdata',sep=''))
}
show('Data prepared for regression')


# Rprof(NULL)
#  RProfData<- readProfileData('Rprof.out')
#  flatProfile(RProfData,byTotal=TRUE)

# Run regressions ---------------------------------------------------------

RealModels<- RunRegressions(RamData,Regressions,'Real Stocks')

RealModelFactorLevels<- NULL

Models<- names(Regressions)

show('Regressions Run')

# Process Regressions -----------------------------------------------------

## Determine species category levels that were used in each model run

TempOmitted<- NULL
for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  eval(parse(text=paste('RealModelFactorLevels$',Model,'<- RealModels$',Model,'$xlevels$SpeciesCatName',sep='')))
}

RamData<- InsertFisheryPredictions(RamData,RealModels) #Add fishery predictions back into main dataframe

RealModelSdevs<- CreateSdevBins(RealModels,RamData,TransbiasBin)

NeiRegressions<- list()

NeiRegressions$M6<- Regressions$M6

NeiRegressions$M7<- Regressions$M7

NeiModels<- RunRegressions(SyntheticData,NeiRegressions,'Synthetic Stocks')

NeiModelFactorLevels<- NULL

for (m in 1:length(names(Regressions)))
{
  Model<- names(Regressions)[m]
  
  eval(parse(text=paste('NeiModelFactorLevels$',Model,'<- NeiModels$',Model,'$xlevels$SpeciesCatName',sep='')))
  
}

SyntheticData<- InsertFisheryPredictions(SyntheticData,NeiModels) #Add fishery predictions back into main dataframe

NeiModelSdevs<- CreateSdevBins(NeiModels,SyntheticData,TransbiasBin)

# Prepare data for regression application ---------------------------------

WhereFaoNeis<- (grepl('nei',FaoData$CommName) | grepl('spp',FaoData$SciName)) & grepl('not identified',FaoData$SpeciesCatName)==F #Find unassessed NEIs

WhereFaoMarineFish<- grepl('not identified',FaoData$SpeciesCatName)

FaoSpeciesLevel<- FaoData[WhereFaoNeis==F & WhereFaoMarineFish==F ,] #Fao stocks named to the species level

FaoNeiLevel<- FaoData[WhereFaoNeis,] #fao species named to the nei or spp level

FaoMarineFishLevel<- FaoData[WhereFaoMarineFish,] #completely unidentified marine goo

TempLevel<- NULL

TempModel<- NULL

show('Regressions Processed')

# Prep for dummy species categories  ----------------------------------------

AllPossible<- unique(data.frame(FullData$SpeciesCatName,FullData$SpeciesCat))

colnames(AllPossible)<- c('SpeciesCatNames','SpeciesCat')

RamPossibleCats<- unique(RamData$SpeciesCatName)

FaoSpeciesPossibleCats<- unique(FaoSpeciesLevel$SpeciesCatName)

FaoNeiPossibleCats<- unique(FaoNeiLevel$SpeciesCatName)

FaoMarineFishPossibleCats<- unique(FaoMarineFishLevel$SpeciesCatName)


# Apply regressions -------------------------------------------------------

Models<- Models[Models!='M7']

for (m in 1:length(Models)) #Apply models to species level fisheries
{
  
  TempModelName<- Models[m]
  
  eval(parse(text=paste('TempLevel<- RealModelFactorLevels$',TempModelName,sep='')))
  
  eval(parse(text=paste('TempModel<- RealModels$',TempModelName,sep='')))
  
  ProxyCats<- AssignNearestSpeciesCategory(FaoSpeciesLevel,TempLevel,AllPossible)
  
  Predictions<- predict(TempModel,ProxyCats$Data)
  
  eval(parse(text=paste('FaoSpeciesLevel$',TempModelName,'Prediction<- Predictions',sep='')))    
}

TempLevel<- NeiModelFactorLevels$M6 

ProxyCats<- AssignNearestSpeciesCategory(FaoNeiLevel,TempLevel,AllPossible)$Data

Predictions<- predict(NeiModels$M6,ProxyCats) #Apply nei model

FaoNeiLevel$M6Prediction<- Predictions

NotIdentifiedPredictions<- predict(NeiModels$M7,FaoMarineFishLevel) #Apply unidentified fish model

FaoMarineFishLevel$M7Prediction<- NotIdentifiedPredictions

show('Regressions Applied')

# Clean and process predictions and data ---------------------------------------

if (IncludeNEIs==1)
{
  PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel,FaoNeiLevel,FaoMarineFishLevel) #Bind all data back together
}
if (IncludeNEIs==0)
{
  PredictedData<- rbind(RamData,SofiaData,FaoSpeciesLevel) #Bind all data back together
}
BiomassColumns<- grepl('BvBmsy',colnames(PredictedData)) | grepl('Prediction',colnames(PredictedData))

BioNames<- colnames(PredictedData)[BiomassColumns]

HasBiomass<- rowSums(is.na(PredictedData[,BiomassColumns]))<length(BioNames)

BiomassData<- PredictedData[HasBiomass,] #Only store fisheries that have some form of biomass estimates

MissingData<- PredictedData[HasBiomass==F & PredictedData$Dbase=='FAO',]

AvailableBio<- (BiomassData[,BiomassColumns])

AvailableBioMarker<- matrix(rep((1:dim(AvailableBio)[2]),dim(AvailableBio)[1]), dim(AvailableBio)[1],dim(AvailableBio)[2],byrow=TRUE)

AvailableBioMarker<- AvailableBioMarker*(is.na(AvailableBio)==F)

AvailableBioMarker[AvailableBioMarker==0]<- NA

BestModel<- apply(AvailableBioMarker,1,min,na.rm=T)

BestBio<- NULL
for (b in 1:dim(AvailableBio)[1])
{
  BestBio[b]<- AvailableBio[b,BestModel[b]]
}

BestBio[BestModel==1]<- log(BestBio[BestModel==1])

BestModelnames<- c('RAM',ModelNames)

BestModelNames<- BestModelnames[sort(unique(BestModel))]

BestModel<- as.factor((BestModel))

levels(BestModel)<- BestModelNames

BiomassData$BestModel<- BestModel

BiomassData$BestBio<- BestBio

BiomassData$CommName<- as.character((BiomassData$CommName))

BiomassData$SciName<- as.character((BiomassData$SciName))

BiomassData$SpeciesCatName<- as.character(BiomassData$SpeciesCatName)

WhereNeis<- (grepl('nei',BiomassData$CommName) | grepl('spp',BiomassData$SciName)) & grepl('not identified',BiomassData$SpeciesCatName)==F & (BiomassData$Dbase=='FAO') #Find unassessed NEIs

WhereUnidentified<- grepl('not identified',BiomassData$SpeciesCatName)

WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level

BiomassData$IdLevel[WhereNeis]<- 'Neis'

BiomassData$IdLevel[WhereUnidentified]<- 'Unidentified'

BiomassData$IdLevel[WhereSpeciesLevel]<- 'Species'

show('Results Processed')

BiomassData<- AssignEconomicData(BiomassData)

# Analyze Current Status --------------------------------------------------

GlobalStatus<- AnalyzeFisheries(BiomassData,'Global Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)

USA<- BiomassData[BiomassData$Country=='USA' |BiomassData$Country=='United States of America' ,]

USAStatus<- AnalyzeFisheries(USA,'USA Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)

RAMStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='RAM',],'RAM Status','Year',1950:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)

# FAOStatus<- AnalyzeFisheries(BiomassData[BiomassData$Dbase=='FAO',],'FAO Status','Year',2000:2010,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
# 
# IndonesiaStatus<- AnalyzeFisheries(BiomassData[BiomassData$Country=='Indonesia',],'Indonesia Status','Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
# 
# CanadaStatus<- AnalyzeFisheries(BiomassData[BiomassData$Country=='Canada',],'Canada Status','Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)

# Calculate MSY -----------------------------------------------------------
sigR<- 0

CatchMSYresults<- RunCatchMSY(GlobalStatus$Data,ExcludeSmallPelagics,ErrorSize,sigR,Smooth,Display,BestValues,ManualFinalYear,n,SampleLength)

MsyData<- CatchMSYresults$Data

pdf(file=paste(FigureFolder,'Catch MSY vs PRM BvBmsy predictions.pdf',sep=''))
xyplot(  CatchMSYBvBmsy ~ BvBmsy | Dbase,data=MsyData,xlab='PRM BvBmsy',ylab='CMSY BvBmsy',panel=function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
})
dev.off()

MedianWorked<- median(MsyData$BvBmsy[is.na(MsyData$MSY)==F])

MedianFailed<- median(MsyData$BvBmsy[is.na(MsyData$MSY)])

CurrentCatch<- sum(MsyData$Catch[MsyData$Year==2010 & is.na(MsyData$MSY)==F],na.rm=T)

CurrentCatch2<- sum(MsyData$Catch[MsyData$Year==2010],na.rm=T)

MsyData<- MsyData[is.na(MsyData$MSY)==F,]

MsyData$r[is.na(MsyData$r)]<- mean(MsyData$r,na.rm=T)

MsyData$PercentGain<- 100*(MsyData$MSY/MsyData$Catch-1)

FutureMSY<- sum(MsyData$MSY[MsyData$Year==2010],na.rm=T)

GlobalPercentChange<- 100*(FutureMSY/CurrentCatch-1)

IndoCatch<- (PredictedData[PredictedData$Country=='Indonesia',])

MsyData$Country[MsyData$Country=='United States of America']<- 'USA'

CountryMsy<- ddply(MsyData[MsyData$Year==2010,],c('Country'),summarize,CurrentCatch= sum(Catch,na.rm=T),MSY=sum(MSY,na.rm=T),TotalGain=sum(MSY,na.rm=T)-sum(Catch,na.rm=T),
                   PercGain=(100*(sum(MSY,na.rm=T)/sum(Catch,na.rm=T)-1)),MedianBvBmsy=median(BvBmsy,na.rm=T),PercMissing=100*(sum(is.na(MSY))/length(MSY)))

PercGainOrder<- order(CountryMsy$PercGain,decreasing=T)

CountryMsy<- CountryMsy[PercGainOrder,]

write.csv(file=paste(ResultFolder,'Country Rankings.csv',sep=''),CountryMsy)

# Run projection analysis -------------------------------------------------
# 
# MsyData$Price<- 1000
# 
# MsyData$BvBmsyOpenAccess<- 0.25

BaselineYear<- 2009

MsyData$Price[is.na(MsyData$Price)]<- mean(MsyData$Price,na.rm=T)

ProjectionData<- RunProjection(MsyData,BaselineYear)

ProjectionData$Country[ProjectionData$Country=='United States of America']<- 'USA'

BiomassData$Country[BiomassData$Country=='United States of America']<- 'USA'

FullData$Country[FullData$Country=='United States of America']<- 'USA'

WhereNeis<- (grepl('nei',FullData$CommName) | grepl('spp',FullData$SciName)) & grepl('not identified',FullData$SpeciesCatName)==F #Find unassessed NEIs

WhereUnidentified<- grepl('not identified',FullData$SpeciesCatName)

WhereSpeciesLevel<- WhereNeis==F & WhereUnidentified==F #Fao stocks named to the species level

FullData$IdLevel[WhereNeis]<- 'Neis'

FullData$IdLevel[WhereUnidentified]<- 'Unidentified'

FullData$IdLevel[WhereSpeciesLevel]<- 'Species'

Policies<- unique(ProjectionData$Policy)

## This is where you need to calculate actual biomass for each fishery

ProjectionData$Biomass<- (ProjectionData$BvBmsy* (2* ProjectionData$MSY/ProjectionData$r))

if (OverFishedOnly==1)
{
  
  CurrentlyOverfished<- ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & ProjectionData$BvBmsy<1]
  ProjectionData<- ProjectionData[ProjectionData$IdOrig %in% CurrentlyOverfished ,]
  
}


for (c in 1:length(CountriesToRun))
{
  
  BaselineYear<- 2009
  
  show(CountriesToRun[c])
  if (CountriesToRun[c]=='Global'){
    FullData_CountryLocater<-  FullData$Country %in% unique(FullData$Country)
    Biomass_CountryLocater<-  BiomassData$Country %in% unique(BiomassData$Country)
    Proj_CountryLocater<- ProjectionData$Country %in% unique(ProjectionData$Country)
  }
  else if (CountriesToRun[c]=='Parties to the Nauru Agreement')
  {
    Biomass_CountryLocater<- BiomassData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
    Proj_CountryLocater<- ProjectionData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
    FullData_CountryLocater<- FullData$Country %in% c('Papua New Guinea','Marshall Islands', 'Solomon Islands', 'Kiribati','Federated States of Micronesia','Tuvalu','Palau','Nauru') 
    
  }
  else if (CountriesToRun[c]=='EU')
  {
    Biomass_CountryLocater<- BiomassData$Country %in% EUCountries
    Proj_CountryLocater<- ProjectionData$Country %in% EUCountries
    FullData_CountryLocater<- FullData$Country %in% EUCountries
    
  }
  else
  {
    Biomass_CountryLocater<- BiomassData$Country==CountriesToRun[c] 
    Proj_CountryLocater<- ProjectionData$Country==CountriesToRun[c] 
    FullData_CountryLocater<- FullData$Country==CountriesToRun[c] 
    
  }
  
  if(sum(Biomass_CountryLocater,na.rm=T)>0 & sum(Proj_CountryLocater,na.rm=T)>0)
  {
    
    # Analyze Time Trends  ----------------------------------------------------------
    
    BiomassStatus<- AnalyzeFisheries(BiomassData[Biomass_CountryLocater,],paste(CountriesToRun[c],' Status',sep=''),'Year',2000:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
    
    TempProjectionData<- ProjectionData[Proj_CountryLocater,]
    
    TempProjectionData$DiscProfits<- TempProjectionData$Profits * (1+Discount)^-(TempProjectionData$Year-BaselineYear)
    
    Baseline<- ddply(subset(TempProjectionData,Year==BaselineYear & Policy=='Historic'),c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                     TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                     MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),NumberOfStocks=length(unique(IdOrig))
                     ,DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    BaselineMarker<- as.data.frame(matrix(NA,nrow=length(unique(TempProjectionData$Policy))-1,ncol=dim(Baseline)[2]))
    
    colnames(BaselineMarker)<- colnames(Baseline)
    
    BaselineMarker[,c(1,3:dim(Baseline)[2])]<- Baseline[,c(1,3:dim(Baseline)[2])]
    
    BaselineMarker$Policy<- Policies[Policies!='Historic']
    
    TimeTrend<- ddply(TempProjectionData,c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                      TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),
                      NumberOfStocks=length(unique(IdOrig)),DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    TimeTrend<- rbind(TimeTrend,BaselineMarker)
    
    YearOrder<- order(TimeTrend$Year)
    
    TimeTrend<- TimeTrend[YearOrder,]
    
    TimeTrend$PercChangeTotalProfits<-100*(TimeTrend$TotalProfits/Baseline$TotalProfits-1)
    
    TimeTrend$PercChangeTotalCatch<-100*(TimeTrend$TotalCatch/Baseline$TotalCatch-1)
    
    TimeTrend$PercChangeTotalBiomass<-100*(TimeTrend$TotalBiomass/Baseline$TotalBiomass-1)
    
    TimeTrend$PercChangeMedianProfits<-100*(TimeTrend$MedianProfits/Baseline$MedianProfits-1)
    
    TimeTrend$PercChangeMedianCatch<-100*(TimeTrend$MedianCatch/Baseline$MedianCatch-1)
    
    TimeTrend$PercChangeMedianBiomass<-100*(TimeTrend$MedianBvBmsy/Baseline$MedianBvBmsy-1)
    
    #     TimeTrend$PercChangeFromSQMedianBiomass<-100*(TimeTrend$MedianBvBmsy/TimeTrend$MedianBvBmsy[TimeTrend$Policy=='SQ']-1)
    
    Cumulatives<- ddply(TimeTrend[TimeTrend$Year>BaselineYear,],c('Policy'),summarize,NPV=sum(DiscProfits,na.rm=T),Food=sum(TotalCatch,na.rm=T),Fish=sum(TotalBiomass,na.rm=T))
    
    CumeNumbers<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbers<- 100*(t(t(CumeNumbers)/CumeNumbers[which(Cumulatives$Policy=='SQ'),])-1)
    
    Cumulatives[,2:dim(Cumulatives)[2]]<- CumeNumbers
    
    Cumulatives<- Cumulatives[Cumulatives$Policy!='SQ' & Cumulatives$Policy!='Historic',]
    
    FinalYear<- TimeTrend[TimeTrend$Year==max(TimeTrend$Year),]  
    
    FinalYear$PercChangeFromSQMedianBiomass<- 100*(FinalYear$MedianBvBmsy/FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ']-1)
    
    
    # Analyze Database Composition --------------------------------------------
    
    pdf(file=paste(FigureFolder,CountriesToRun[c],' Database Analysis.pdf',sep='')) 
    
    AvailableDatabaseTallies<- ddply(FullData[FullData_CountryLocater & FullData$Year==BaselineYear,],c('Year','Dbase'),summarize,Catch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
    
    BlankTallies<- as.data.frame(matrix(0,nrow=length(AvailableDatabaseTallies$Dbase),ncol=4))
    
    colnames(BlankTallies)<- c('Year','Dbase','Catch','Fisheries')

    BlankTallies$Year<- BaselineYear

    BlankTallies$Dbase<- AvailableDatabaseTallies$Dbase
    
    AccountedForDatabaseTallies<- ddply(TempProjectionData[TempProjectionData$Year==BaselineYear,],c('Year','Dbase'),summarize,Catch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
        
    BlankTallies[BlankTallies$Dbase %in% AccountedForDatabaseTallies$Dbase,3:4]<- AccountedForDatabaseTallies[,3:4]
    
    UnaccountedForDatabaseTallies<-  as.data.frame(t(colSums(AvailableDatabaseTallies[,3:4] -BlankTallies[,3:4])))
    
    AccountedForDatabaseTallies$Used<- 'Analyzed'
    
    AvailableDatabaseTallies$Used<- 'Available'
    
    DatabaseStuff<- rbind(AccountedForDatabaseTallies,AvailableDatabaseTallies)
    
    print(dotplot(Catch ~ Dbase | Used,data=DatabaseStuff,cex=2))
    
    print(dotplot(Fisheries ~Dbase | Used,data=DatabaseStuff,cex=2))
    
    (pie(c(AccountedForDatabaseTallies$Catch,UnaccountedForDatabaseTallies$Catch),labels=c(AccountedForDatabaseTallies$Dbase,'Unknown')))
    
    AccountedForDatabaseTallies<- ddply(TempProjectionData[TempProjectionData$Year==BaselineYear,],c('Year','IdLevel'),summarize,Catch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
        
    AvailableDatabaseTallies<- ddply(FullData[FullData_CountryLocater & FullData$Year==BaselineYear,],c('Year','IdLevel'),summarize,Catch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
    
    AccountedForDatabaseTallies$Used<- 'Analyzed'
    
    AvailableDatabaseTallies$Used<- 'Available'
    
    SpeciesStuff<- rbind(AccountedForDatabaseTallies,AvailableDatabaseTallies)
    
    print(dotplot(Catch ~ IdLevel | Used,data=SpeciesStuff,cex=2))
    
    print(dotplot( Fisheries ~ IdLevel  | Used,data=SpeciesStuff,cex=2))
    
    dev.off()
    
    # Save and Print Results --------------------------------------------------
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Policy Projections.csv',sep=''),TimeTrend)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Baseline.csv',sep=''),Baseline)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Biomass Status.csv',sep=''),BiomassStatus$Data)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' Raw Projection Data.csv',sep=''),TempProjectionData)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Current Year Data.csv',sep=''),FinalYear)
    
    write.csv(file=paste(ResultFolder,CountriesToRun[c],' From Business As Usual Data.csv',sep=''),Cumulatives)
    
    
    BaselineYear<- 2005
    
    pdf(file=paste(FigureFolder,CountriesToRun[c],' Trajectories.pdf',sep='')) 
    
    print(barchart(NPV ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in NPV from Business as Usual'))
    
    print(barchart(Food ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in Total Food from Business as Usual'))
    
    print(barchart(Fish ~ Policy,data=Cumulatives,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change in Total Fish from Business as Usual'))
    
    print( barchart(PercChangeTotalProfits ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Profits'))
    
    print(barchart(PercChangeTotalCatch ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Food'))
    
    print(a<- barchart(PercChangeTotalBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Fish'))
    
    print(barchart(PercChangeMedianBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Current Median Biomass'))
    
    print(barchart(PercChangeFromSQMedianBiomass ~ Policy,data=FinalYear,col=terrain.colors(length(Policies)),origin=0,ylab= ' % Change from Business as Usual Median Biomass'))
    
    print(xyplot( PercChangeTotalProfits ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,ylab='% Change from Current Total Profits',type='l',lwd=4,auto.key=T,aspect='fill'))
    
    print(xyplot( PercChangeTotalCatch ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Total Catch'))
    
    print(xyplot( PercChangeTotalBiomass ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Total Biomass'))
    
    print(xyplot( PercChangeMedianProfits ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,ylab='% Change in Median Profits',type='l',lwd=4,auto.key=T,aspect='fill'))
    
    print(xyplot( PercChangeMedianCatch ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='% Change from Current Median Catch'))
    
    print(xyplot( MedianBvBmsy ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='Median B/Bmsy'))
    
    print(xyplot( MedianFvFmsy ~ Year,data=TimeTrend[TimeTrend$Year>=BaselineYear,],groups=Policy,type='l',lwd=4,auto.key=T,aspect='fill',ylab='Median F/Fmsy'))
    
    dev.off()
    
  } #Close if
} #Close Country Trajectory Analysis 





# Scale and Analyze Results -----------------------------------------------

# Publish in Science ------------------------------------------------------


save.image(file=paste(ResultFolder,'Global Fishery Recovery Results.rdata',sep=''))


