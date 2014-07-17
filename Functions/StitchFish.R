######################################
#Stitch Fisheries--------------------------------------------------
# This code creates synthetic neis out of input data
######################################

StitchFish<- function(Data,IdVar,Groups,GroupSamples,Iterations)
{
  
  if (length(GroupSamples)==1)
  {
    GroupSamples<- rep(GroupSamples,length(Groups)) #Allows for different sample numbers per grouping category
  }
  
  StitchMat<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]))
  
  colnames(StitchMat)<- colnames(Data)
  
  for (g in 1:length(Groups))
  {
    
    GroupData<- Data[Data$SpeciesCat==Groups[g],]
    
    GroupIds<- unique(GroupData[,IdVar])
    
    GroupsIds<- GroupIds[is.na(GroupIds)==F]
    
    AvailableSamples<- length(GroupIds)
    
    GroupSample<- min(GroupSamples[g],AvailableSamples)
  
    
    for (i in 1:Iterations)
    {
           
      Samples<- sample(GroupIds,GroupSample,replace=FALSE)
      
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
        
        StitchBmsy[y]<- sum(SampleData$Bmsy[SampleData$Year==SampleYears[y]],na.rm=T)
        
        
      }
      
      StitchId<- paste('Group',Groups[g],'_Stitch',i,sep='')
        
      TempStitchMat[,IdVar]<- StitchId

      TempStitchMat$SpeciesCat<- Groups[g]

     TempStitchMat$Year<- SampleYears
     
     
      TempStitchMat$Catch<-  StitchCatch

      TempStitchMat$Biomass<-  StitchBio

      TempStitchMat$Bmsy<-  StitchBmsy

      TempStitchMat$BvBmsy<-  TempStitchMat$Biomass/TempStitchMat$Bmsy
      
      
      StitchMat<- rbind(StitchMat,TempStitchMat)
    } #Close iterations loop
    
  } #Close groups loop
  
  return(StitchMat)
} #Close function