######################################
#Stitch Fisheries--------------------------------------------------
# This code creates synthetic neis out of input data
######################################

StitchFishJackknife<- function(Data,IdVar,Level,Groups,GroupSamples,Iterations)
{
  
  #   Data<- RamOriginal
  #   Groups<-unique(Data$SpeciesCatName)
  
  if (length(GroupSamples)==1)
  {
    GroupSamples<- rep(GroupSamples,length(Groups)) #Allows for different sample numbers per grouping category
  }
  
  StitchMat<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]+2))
  
  colnames(StitchMat)<- c(colnames(Data),'IDs','NumStocks') # create additional variables to store the number of stocks composing each nei and their ids
  
  CandidateStocks<- is.na(Data$BvBmsy)==F & is.infinite(Data$BvBmsy)==F & is.na(Data$Biomass)==F & is.na(Data$Bmsy)==F & is.infinite(Data$Biomass)==F & is.infinite(Data$Bmsy)==F
  
  Data<- Data[CandidateStocks,]
  
  for (g in 1:length(Groups))
  {
    show(paste(g/length(Groups)*100,'% done with Groups',sep=''))
    
    if (any(Data[,Level]==Groups[g]))
    {    
      GroupData<- Data[Data[,Level]==Groups[g],]
      
      GroupIds<- unique(GroupData[,IdVar])
      
      GroupsIds<- GroupIds[is.na(GroupIds)==F]
      
      AvailableSamples<- length(GroupIds)
      
      GroupSample<- min(GroupSamples[g],AvailableSamples) # determine whether the available samples are smaller than the GroupSample size
      
      
      for (i in 1:Iterations)
      {
        
        for(j in 1:GroupSample) # create a synthetic nei using 1:GroupSample number of stocks
        
        {
          
        Samples<- sample(GroupIds,j,replace=FALSE)
        
        WhereSamples<- GroupData[,IdVar] %in% Samples
        
        SampleData<- GroupData[WhereSamples,]
        
        SampleYears<- min(SampleData$Year,na.rm=T):max(SampleData$Year,na.rm=T)
        
        TempStitchMat<- as.data.frame(matrix(NA,nrow=length(SampleYears),ncol=dim(Data)[2]))
        
        colnames(TempStitchMat)<- colnames(Data)
        
        StitchCatch<- matrix(NA,nrow=length(SampleYears),ncol=1)
        
        StitchBio<- matrix(NA,nrow=length(SampleYears),ncol=1)
        
        StitchBmsy<- matrix(NA,nrow=length(SampleYears),ncol=1)
        
        for (y in 1:length(SampleYears))
        {
          
          StitchCatch[y]<- sum(SampleData$Catch[SampleData$Year==SampleYears[y]],na.rm=T)
          
          StitchBio[y]<- sum(SampleData$Biomass[SampleData$Year==SampleYears[y]],na.rm=T)
          
          StitchBmsy[y]<- sum(SampleData$ReferenceBiomass[SampleData$Year==SampleYears[y]],na.rm=T)
          
          
        }
        
        StitchId<- paste(Groups[g],'Stitch',as.character(i),as.character(j),sep='_')
        
        TempStitchMat[,IdVar]<- StitchId
        
        TempStitchMat[,Level]<- Groups[g]
        
        TempStitchMat$Year<- SampleYears
        
        TempStitchMat$NumStocks<-j
        
        TempStitchMat$IDs<-paste(Samples,collapse='_')
        
        TempStitchMat$Catch<-  StitchCatch
        
        TempStitchMat$Biomass<-  StitchBio
        
        TempStitchMat$Bmsy<-  StitchBmsy
        
        TempStitchMat$BvBmsy<-  TempStitchMat$Biomass/TempStitchMat$Bmsy
        
        
        StitchMat<- rbind(StitchMat,TempStitchMat)
        } #Close GroupSamples loop
      } #Close iterations loop
    } #Close if any groups loop
  } #Close groups loop
  StitchMat<- StitchMat[is.na(StitchMat[,IdVar])==F,]
  return(SynthNeis=StitchMat)
} #Close function