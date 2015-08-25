################################################
##
## Function to unlump fisheries for projections
##
################################################

# For each lumped stock, the projected catch, profits, and biomass are unlumped and attributed to the countries that contribute to that stock.
# The percent attributed to each country is based on the % of catch each country was responsible for over a 5 year period (2008 - 2012)
# This code only runs during CountriesToRun. When evaluating individual fisheries, the stock will remained lumped

# StitchIds<-CleanedData$StitchIds
# Data<-ProjectionData
# YearsBack<-4

UnlumpFisheries<-function(Data,RawData,BaselineYear,YearsBack,StitchIds)
{
  
  data<-Data[grepl('Lumped',Data$IdOrig),]
  
  ids<-unique(data$IdOrig)
  
  UnlumpedFaoData<-list()
  
  for(a in 1:length(ids))
  {
    ## calculate the percent of 5 year total catch contributed by each stitched fishery 
    tempdata<-data[data$IdOrig %in% ids[a],] # subset stitched data
    
    stids<-StitchIds$StockIDs[StitchIds$LumpedID==ids[a]] # find ids of stocks that were stitched
    
    stids<-unlist(str_split(stids,pattern='_')) # separate stocks
    
    raw<-RawData[(RawData$IdOrig %in% stids) & RawData$Year<=BaselineYear& RawData$Year>=(BaselineYear-YearsBack),c('IdOrig','Country','Year','CommName','Catch')] # pull out pre stitched catch data
    
#     t<-ddply(raw,c('Year'),summarize,TotalCatch=sum(Catch,na.rm=T)) # calculate total catch each year

    t<- raw %>%
      group_by(Year) %>%
      summarize(TotalCatch=sum(Catch,na.rm=T)) # calculate total catch each year
    
        
    raw$YearTotal<-t$TotalCatch # add total catch
    
#     percent<-ddply(raw,c('IdOrig','Country'),summarize,Percent=sum(Catch,na.rm=T)/sum(YearTotal,na.rm=T)) # calculate percent of 5 year total catch
    
    percent<- raw %>% 
      group_by(IdOrig,Country) %>%
      summarize(Percent=sum(Catch,na.rm=T)/sum(YearTotal,na.rm=T)) # calculate percent of 5 year total catch
    
      
    stids<-unique(percent$IdOrig[(percent$Percent>0)])
    
    ## create new stocks for country upside analysis and apply percentages to the projections of the lumped stock
    lumpproj<-tempdata
    
    UnLumped<-data.frame(matrix(nrow=0,ncol=ncol(lumpproj)))

    colnames(UnLumped)<-colnames(lumpproj)
    for(b in 1:length(stids))
    {
      # calculate projection data results for projected policies
      unlumpproj<-lumpproj
      
      unlumpproj$Country<-unique(RawData$Country[RawData$IdOrig==stids[b]])
      
      unlumpproj$IdOrig<-stids[b]
      
      unlumpproj$Catch<-unlumpproj$Catch*percent$Percent[percent$IdOrig==stids[b]]
      
      unlumpproj$Biomass<-unlumpproj$Biomass*percent$Percent[percent$IdOrig==stids[b]]
      
      unlumpproj$MSY<-unlumpproj$MSY*percent$Percent[percent$IdOrig==stids[b]]
      
      unlumpproj$k<-unlumpproj$k*percent$Percent[percent$IdOrig==stids[b]]
      
      unlumpproj$Profits<-unlumpproj$Profits*percent$Percent[percent$IdOrig==stids[b]]
      
      unlumpproj$DiscProfits<-unlumpproj$DiscProfits*percent$Percent[percent$IdOrig==stids[b]]
      
      UnLumped<-rbind(UnLumped,unlumpproj)
      
    } # close stids loop
    
    UnlumpedFaoData[[a]]<-UnLumped
    
    show(a)
  
  } # close lumped stock loop
  
  # flatten to one dataframe
  UnLumpedFaoData<-ldply(UnlumpedFaoData, data.frame) 
  
  FinalData<-Data[!(grepl('Lumped',Data$IdOrig)),]

  FinalData<-rbind(UnLumpedFaoData,FinalData)

  return(FinalData)
  
}  
  
