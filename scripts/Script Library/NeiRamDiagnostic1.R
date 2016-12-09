
# 1) convert RAM stocks into "nei" stocks
# 2) loop over new "nei" stocks and run through NearestNeighborNei and AnalyzeFisheries
#     - must change Dbase from "RAM" to "FAO"
#     - 

RamNeiDiag<-MsyData

RamNeiDiag<-RamNeiDiag[RamNeiDiag$Dbase=='RAM',] # subset ram data out of msy data to make "fake nei" stocks

# RamNeiDiag<-RamNeiDiag[is.na(RamNeiDiag$RegionFAO)==F,] FIGURE out why RAM has NAs for several US fisheries when not run on my machine

RamStatus<-unique(RamNeiDiag[c("IdOrig","Year","SciName","BvBmsy","FvFmsy")]) # make new data table to keep original BvBmsy and FvFmsy values



RamSampleIds<-unique(RamNeiDiag[c("SciName","RegionFAO")])

RamSampleIds$Genus<-NA

ProjDataDiag<-ProjectionData[ProjectionData$IdLevel=="Species",]

for(e in 1:nrow(RamSampleIds))
{ 
  RamSampleIds$Genus[e]<-unlist(str_split(unique(RamSampleIds$SciName[e]),pattern=" "))[1]
  
  show(e)
}


ids<-unique(RamStatus$IdOrig)

# make changes to make ram stocks appear as nei's
RamNeiDiag$Dbase<-"FAO" # convert Dbase to FAO
RamNeiDiag$BvBmsy<-NA # remove true values
RamNeiDiag$CommName<-paste(RamNeiDiag$CommName,"nei",sep=" ") # add nei to CommName
RamNeiDiag$RanCatchMSY<-FALSE # change to FALSE

# function to convert SciName into "Genus spp" for NearestNeighborNEI to recognize
MakeGenusNei<-function(x) 
{
  genus<-unlist(str_split(x,pattern=" "))[1]
  
  genus<-paste(genus,"spp",sep=" ")
  
  return(genus)
}

RamNeiDiag$SciName<-sapply(RamNeiDiag$SciName,MakeGenusNei) # apply function to data subset

ProjDataDiag<-ProjectionData[ProjectionData$IdLevel=="Species",]

### Loop over new nei stocks
# pdf()

for(b in 1:length(ids))
{
  show(paste(b/length(ids)*100,"% done with Jacknife",sep=""))
  
  RamNei<-RamNeiDiag[RamNeiDiag$IdOrig==ids[b],] # create msy data subset for stock to pass to NearestNeighbor
  
  RamTrue<-MsyData$BvBmsy[MsyData$IdOrig==ids[b]] # true RAM Status
  
  TempProjData<-ProjDataDiag[!(ProjDataDiag$IdOrig %in% ids[b]),]
  
  if(max(RamNei$Year)>=BaselineYear)
  {
  TempNeiData<-NearestNeighborNeis(BiomassData,RamNei,TempProjData,BaselineYear)
  
  TempNeiBiomassData<-TempNeiData$BiomassNeis
  
  Predicted<-exp(TempNeiBiomassData$BvBmsy)
  
  TestResults<-data.frame(RamTrue,Predicted)
  
  if(b==1){TestResultsFinal<-TestResults}
  if(b>1){TestResultsFinal<-rbind(TestResultsFinal,TestResults)}
#   TestResults<-data.frame(TempNeiBiomassData$Year,RamTrue,exp(TempNeiBiomassData$BvBmsy))
  }
#   colnames(TestResults)<-c("Year","True","Estimated")

#   TempNeiStatus<-AnalyzeFisheries(TempNeiBiomassData,'Year',2005:2011,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
}

#Run quick diagnostic of Nei results
pdf(file=paste(FigureFolder,'NEI Method vs Observed RAM Status.pdf',sep=''))
print(xyplot(  RamTrue ~ Predicted, data=TestResultsFinal,xlab='RAM Status',ylab='Predicted BvBmsy',
              main="Jacknife Resampling w/ Individual RAM Stocks", panel=function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a=0,b=1,lty=2)
}))
dev.off()

