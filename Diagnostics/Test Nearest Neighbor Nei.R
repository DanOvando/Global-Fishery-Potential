# Test Nearest Neighbor Nei ------------------------------------------------------------


#Pull in RAM data once again

load('Results/Scratch/Data/Global Fishery Recovery Results.rdata')


RamProj<- ProjectionData[ProjectionData$Dbase=='RAM',]

RamMsy<- MsyData[MsyData$Dbase=='RAM',]

RamMsy$Dbase<- 'FAO'

RamMsy$CommName<- 'nei'

RamMsy$BvBmsy<- 999

RamMsy$RanCatchMSY<- FALSE


YearSpan<- ddply(RamMsy,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T),MinYear=min(Year,na.rm=T),HasBase=any(Year==2000))

DropIt<- YearSpan$IdOrig[YearSpan$HasBase==F]

RamMsy<- RamMsy[(RamMsy$IdOrig %in% DropIt)==F,]

FakeNeis<- NearestNeighborNeis(BiomassData,RamMsy,RamProj,2000) #Run Nearest Neighbor NEI analysis



PrmStatus<- AnalyzeFisheries(PrmPred,'Scratch CMSY Diagnostics','Year',min(PrmPred$Year):max(PrmPred$Year),RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)$Data

RamMsy<- ddply(PrmStatus,c('IdOrig'),summarize,RamMsy=mean(MSY,na.rm=T))
