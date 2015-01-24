######################################
#Stitch Fisheries--------------------------------------------------
# This code creates synthetic neis out of input data
######################################

LumpFisheries<- function(Data,GroupsToGroup)
{
  
#   Data<- FaoData
#   
#   GroupsToGroup<- SpeciesCategoriesToLump
 
  StitchMat<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]))
  
  colnames(StitchMat)<- colnames(Data)
  
  StitchIds<-as.data.frame(matrix(NA,nrow=0,ncol=2))
  
  colnames(StitchIds)<-c('LumpedID','StockIDs')
  
  CandidateStocks<- (Data$SpeciesCatName %in% GroupsToGroup) & Data$Dbase=='FAO'
  
  KeepData<- Data[CandidateStocks==F,]
  
  Data<- Data[CandidateStocks,]
  
  for (g in 1:length(GroupsToGroup))
  {
    
    
    if (any(Data$SpeciesCatName==GroupsToGroup[g]))
    {    
      
      GroupData<- Data[Data$SpeciesCatName==GroupsToGroup[g],]
      
      FaoRegs<- unique(GroupData$RegionFAO)
      
      for (f in 1:length(FaoRegs))
      {
        
        GroupRegData<- GroupData[GroupData$RegionFAO==FaoRegs[f],]
        
        GroupRegSpecies<- unique(GroupRegData$SciName)
        
        for (s in 1:length(GroupRegSpecies))
        {
          
          GroupRegSpeciesData<- GroupRegData[GroupRegData$SciName==GroupRegSpecies[s],]
          
          GroupYears<- sort(unique(GroupRegSpeciesData$Year))
          
          GroupIds<-unique(GroupRegSpeciesData$IdOrig)
          
          TempStitch<- as.data.frame(matrix(NA,nrow=length(GroupYears),ncol=dim(Data)[2]))
          
          TempStitchIds<-as.data.frame(matrix(NA,nrow=1,ncol=2))
          
          BioData<- GroupRegSpeciesData[1,]
          
          colnames(TempStitch)<- colnames(Data)
          
          colnames(TempStitchIds)<-colnames(StitchIds)
          
          TempStitch[,2:dim(TempStitch)[2]]<- BioData[2:dim(BioData)[2]]
         
          TempStitch$Year<- GroupYears
          
          TempStitch$IdOrig<- paste('Lumped-',TempStitch$CommName[1],'-FaoRegion',FaoRegs[f],sep='')

          TempStitch$Country<- 'Multinational'
          
          TempStitchIds$LumpedID<-paste('Lumped-',TempStitch$CommName[1],'-FaoRegion',FaoRegs[f],sep='')
          
          TempStitchIds$StockIDs<-paste(GroupIds,collapse='_')
          for (y in 1:length(GroupYears))
          {
            
            TempStitch$Catch[y]<- sum(GroupRegSpeciesData$Catch[GroupRegSpeciesData$Year==GroupYears[y]],na.rm=T)
            
            if (sum(is.na(GroupRegSpeciesData$Catch[GroupRegSpeciesData$Year==GroupYears[y]])==F)==0){TempStitch$Catch[y]<- NA}
            
          }
          
          StitchMat<- rbind(StitchMat,TempStitch)
          
          StitchIds<-rbind(StitchIds,TempStitchIds)
          
        } #Close species loop
        
      } #Close FAO Regions Loop
        
    } #Close if statement
    
  }# Close species group loop
       
  StitchedData<- rbind(KeepData,StitchMat)
    
return(list(StitchedData=StitchedData,StitchIds=StitchIds))
} #Close function