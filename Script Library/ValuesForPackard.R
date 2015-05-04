##########################################
##
## Script to calculate in-text values for 
## Packard status database report
##
###########################################

# DataL<-ProjectionData
# DataU<-UnlumpedProjectionData

PackardReport<-function(DataL,DataU,BaselineYear)
{
  
  # Several overall values to reference
  totalstocks<-length(unique(DataL$IdOrig[DataL$Year==BaselineYear])) # Total number of Fisheries
  
  globalcatch<-80000000
  
  ramspecies<-length(unique(DataL$SciName[DataL$Dbase=='RAM']))
  
  ## Number of fisheries and total catch in each dataset
  DbSummary<-function(data,BaselineYear,totalstocks,globalcatch)
    {
      temp<-ddply(data[data$Year==BaselineYear,],c('Year'),summarize,Fisheries=length(unique(IdOrig,na.rm=T)),
        TotalCatch=sum(Catch,na.rm=T),MedB=median(BvBmsy,na.rm=T),MeanB=mean(BvBmsy,na.rm=T),MedF=median(FvFmsy,na.rm=T),MeanF=mean(FvFmsy,na.rm=T))
      
      temp$PercTotal<-100*(temp$TotalCatch/globalcatch)
      
      temp$PercUnderOne<-length(unique(data$IdOrig[data$Year==BaselineYear & data$BvBmsy<1]))/temp$Fisheries
      
      temp$PercFOverOne<-length(unique(data$IdOrig[data$Year==BaselineYear & data$FvFmsy>1]))/temp$Fisheries
      
      print(temp)

    }
  
  # Build table of summary stats for following subsets: 1) All stocks; 2) RAM; 3a) FAO; 3b) FAO (species level); 3c) FAO (nei level); 4) SOFIA; 5) Catch Shares 
  
  DbResults<-DbSummary(data=DataL,BaselineYear,totalstocks,globalcatch)
  
  subsets<-c('All Data','RAM','FAO','FAO (Sp)','FAO (NEI)','SOFIA','Catch Shares')
  
  DbResults[1:length(subsets),]<-NA
  
  DbResults$Subset[1:length(subsets)]<-c(subsets)
  
  DbResults[DbResults$Subset=='All Data',colnames(DbResults)!='Subset']<-DbSummary(data=DataL,BaselineYear,totalstocks,globalcatch)
  
  DbResults[DbResults$Subset=='RAM',colnames(DbResults)!='Subset']<-DbSummary(data=DataL[DataL$Dbase=='RAM',],BaselineYear,totalstocks,globalcatch)
  
  DbResults[DbResults$Subset=='FAO',colnames(DbResults)!='Subset']<-DbSummary(data=DataL[DataL$Dbase=='FAO',],BaselineYear,totalstocks,globalcatch)
  
  DbResults[DbResults$Subset=='SOFIA',colnames(DbResults)!='Subset']<-DbSummary(data=DataL[DataL$Dbase=='SOFIA',],BaselineYear,totalstocks,globalcatch)
  
  DbResults[DbResults$Subset=='FAO (Sp)',colnames(DbResults)!='Subset']<-DbSummary(data=DataL[DataL$Dbase=='FAO' & DataL$IdLevel=='Species',],BaselineYear,totalstocks,globalcatch)
  
  DbResults[DbResults$Subset=='FAO (NEI)',colnames(DbResults)!='Subset']<-DbSummary(data=DataL[DataL$Dbase=='FAO' & DataL$IdLevel=='Neis',],BaselineYear,totalstocks,globalcatch)
  
  DbResults[DbResults$Subset=='Catch Shares',colnames(DbResults)!='Subset']<-DbSummary(data=DataL[DataL$CatchShare==1,],BaselineYear,totalstocks,globalcatch)
  
  return(DbResults)
  
}