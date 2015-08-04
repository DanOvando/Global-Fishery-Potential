
CatchTracker<-function(RawData,MissingData,FullData,)
{
  FAO<-ddply(RawData[RawData$Dbase=='FAO' & RawData$Year==2012 & !(RawData$SpeciesCatName %in% SpeciesCategoriesToOmit),],c('Country'),summarize,
             TotalCatch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))

  OurData<-ddply(UnlumpedProjectionData[UnlumpedProjectionData$Year==2012,],c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
  
  # Calculate total amount of catch lost by stocks with inadequate data
  
  Missing<-ddply(MissingData[MissingData$Year==2012,],c('Country'),summarize,MDTotalCatch=sum(Catch,na.rm=T),MDFisheries=length(unique(IdOrig)))
  
  Dropped<-RawData[RawData$IdOrig %in% DroppedStocks$IdOrig,]
  
  Dropped<-ddply(Dropped[Dropped$Year==2012,],c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalFisheries=length(unique(IdOrig,na.rm=T)))
  
  for(a in 1:nrow(OurData))
  {
    whereF<-match(OurData$Country[a],FAO$Country)
    
    whereM<-match(OurData$Country[a],Missing$Country)
    
    if(is.na(whereF)==F) 
    {
      OurData$FaoCatch[a]<-FAO$TotalCatch[whereF]
    
      OurData$FaoFisheries[a]<-FAO$Fisheries[whereF]
      
      rm(whereF)
    }
    
    if(is.na(whereM)==F) 
    {
      OurData$MissingCatch[a]<-Missing$MDTotalCatch[whereM]
      
      OurData$MissingFisheries[a]<-Missing$MDFisheries[whereM]
      
      rm(whereM)
    }
  }
  
  OurData$CatchAccounted<-apply(OurData[,c('TotalCatch','MissingCatch')],1,function(x) sum(x,na.rm=T))
  
  OurData$FisheriesAccounted<-apply(OurData[,c('Fisheries','MissingFisheries')],1,function(x) sum(x,na.rm=T))
  
  OurData$PercCovered<-(OurData$CatchAccounted/OurData$FaoCatch)*100

  OurData$PercFisheriesCovered<-(OurData$FisheriesAccounted/OurData$FaoFisheries)*100
  
  ggplot(OurData[OurData$Country!='Multinational',],aes(x=PercCovered)) +
    geom_density()
  
  
  overlap<-ddply(RawData[RawData$Dbase=='FAO' & !(RawData$SpeciesCatName %in% SpeciesCategoriesToOmit),],c('Year'),summarize,
                 TotalCatch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
  
  after<-ddply(FullData[FullData$Dbase=='FAO',],c('Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalFisheries=length(unique(IdOrig)))

  overlap$CatchRemaining<-after$TotalCatch
  
  overlap$CatchRemoved<-overlap$TotalCatch-overlap$CatchRemaining
  
  ram<-ddply(RawData[FullData$Dbase=='RAM' & FullData$Year>=1950 & FullData$Year<=2012 ,],c('Year'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  overlap$RamCatch<-ram$TotalCatch
  
  overlapplot<-melt(overlap,id.vars=c('Year')) 
    
  ggplot(overlapplot[overlapplot$variable %in% c('CatchRemoved','RamCatch'),],aes(x=Year,y=value,color=variable)) +
    geom_line()
  
  
  
  
  
  
  
  
  
  
}