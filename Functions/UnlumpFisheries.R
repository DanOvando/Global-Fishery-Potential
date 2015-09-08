################################################
##
## Function to unlump fisheries for projections
##
################################################

# For each lumped stock, the projected catch, profits, and biomass are unlumped and attributed to the countries that contribute to that stock.
# The percent attributed to each country is based on the % of catch each country was responsible for over a 5 year period (2008 - 2012)
# This code only runs during CountriesToRun. When evaluating individual fisheries, the stock will remained lumped

# StitchIds<-CleanedData$StitchIds
# Data<-UnlumpedData
# ProjData<-ProjectionData
# YearsBack<-4

UnlumpFisheries<-function(Data,ProjData,RawData,BaselineYear,YearsBack,StitchIds)
{
  
  data<-Data[grepl('Lumped',Data$IdOrig),]
  
  ramdata<-subset(ProjData,Dbase=='RAM' & Year<=BaselineYear,c('IdOrig','Country','RegionFAO','Year','SciName','CommName','Catch'))
  
  ids<-unique(data$IdOrig)
  
  UnlumpedFaoData<-list()
  
  for(a in 1:length(ids))
  {
    # subset stitched data
    tempdata<-data[data$IdOrig %in% ids[a],] 
    
    # identify max year in the temp data (2050 means the stock was projected)
    maxyr<-max(tempdata$Year)
    
    # find ids of stocks that were stitched
    stids<-StitchIds$StockIDs[StitchIds$LumpedID==ids[a]] 
    
    # separate stocks
    stids<-unlist(str_split(stids,pattern='_')) 
    
    # pull out pre stitched catch data
    raw1<-RawData[(RawData$IdOrig %in% stids),c('IdOrig','Country','RegionFAO','Year','SciName','CommName','Catch')] 
    
    # Check to see if any unlumped stocks would duplicate a RAM stock accidentally
    check1<-left_join(raw1,ramdata,by=c('Country','Year','SciName','RegionFAO'))
    
    # find the years and stocks that overlap in unlumped
    fuckers<-unique(check1[is.na(check1$IdOrig.y)==F,c('IdOrig.x','Year')])
    
    fuckers$pastename <- paste(fuckers$IdOrig.x,fuckers$Year,sep = '-')
    
    # remove these overlapping stock/year combinations. The remaining total catch should exactly match what's in the lumped fishery
    check<-subset(check1,is.na(check1$IdOrig.y))
    
    # resubset raw and stids to only include the stock(s) remaining in check
    stids<-unique(check$IdOrig.x)
    
    raw<-subset(raw1,IdOrig %in% check$IdOrig.x)
    
    raw$pastename <- paste(raw$IdOrig, raw$Year, sep = '-')
    
    raw<-subset(raw,!(pastename %in% fuckers$pastename))

    # calculate percent of historical timeseries to use for historical data
    percentPast<-raw %>%
      ungroup() %>%
      group_by(Year) %>%
      mutate(YearTotal=sum(Catch,na.rm=T)) %>%
      ungroup() %>%
      group_by(Year,IdOrig,Country) %>%
      summarize(Percent=sum(Catch,na.rm=T)/sum(YearTotal,na.rm=T))
    
    # calculate percent of 5 year total catch to use for projections
    if(maxyr>BaselineYear)
    {
      percentFuture<-raw %>%
        filter(!(IdOrig %in% fuckers$IdOrig.x) & Year <= BaselineYear & Year >=(BaselineYear-YearsBack)) %>%
        group_by(Year) %>%
        mutate(YearTotal=sum(Catch,na.rm=T)) %>%
        ungroup() %>%
        group_by(IdOrig,Country) %>%
        summarize(Percent=sum(Catch,na.rm=T)/sum(YearTotal,na.rm=T))
      
      # find ids with projected data to loop over
      futurestids<-unique(percentFuture$IdOrig[is.na(percentFuture$Percent)==F & percentFuture$Percent>0])
      
      # show sum of percents (This should always equal 1 or zero)
      # show(sum(percentFuture$Percent,na.rm=T))
    }
    
    # test percentages
    test<-percentPast %>%
      ungroup() %>%
      group_by(Year) %>%
      summarize(TotalPerc=sum(Percent,na.rm=T))
    
    # show(unique(test$TotalPerc))
  
    ## create new stocks for country upside analysis and apply percentages to the projections of the lumped stock
    lumpproj<-tempdata
    
    # initialize dataframes to store historical and projected results
    UnLumpedHist<-data.frame(matrix(nrow=0,ncol=ncol(lumpproj)))
    colnames(UnLumpedHist)<-colnames(lumpproj)
    
    UnLumpedProj<-data.frame(matrix(nrow=0,ncol=ncol(lumpproj)))
    colnames(UnLumpedProj)<-colnames(lumpproj)
    
    # loop over all stitched ids and unlump historical
    
    # pull out historical timeseries and apply past percentages
    for(b in 1:length(stids))
    {
      # if it's an overlapping fucker only use the non-overlapping timeframe
      if(stids[b] %in% fuckers$IdOrig.x)
      {
        fuckeryrs<-unique(fuckers$Year[fuckers$IdOrig.x==stids[b]])
        
        unlumphist<-subset(lumpproj,Year<=BaselineYear & !(Year %in% fuckeryrs))
      }
      
      # if not, use all historical data
      if(!(stids[b] %in% fuckers$IdOrig.x)) { unlumphist<-subset(lumpproj,Year<=BaselineYear) }
      
      unlumphist$Country<-unique(percentPast$Country[percentPast$IdOrig==stids[b]])
      
      unlumphist$IdOrig<-stids[b]
      
      # Join past percentages with historical data and apply to all relevent categories
      unlumphist<-left_join(unlumphist,percentPast,by=c('Year','IdOrig','Country'))
      
      unlumphist$Catch<-unlumphist$Catch*unlumphist$Percent
      
      unlumphist$Biomass<-unlumphist$Biomass*unlumphist$Percent
      
      unlumphist$MSY<-unlumphist$MSY*unlumphist$Percent
      
      unlumphist$k<-unlumphist$k*unlumphist$Percent
      
      unlumphist$Profits<-unlumphist$Profits*unlumphist$Percent
      
      unlumphist$DiscProfits<-unlumphist$DiscProfits*unlumphist$Percent
      
      unlumphist<- unlumphist %>%
        dplyr::select(-Percent)
    
      UnLumpedHist<-rbind(UnLumpedHist,unlumphist)
      
#       a=unlumphist$Catch
#       b=raw$Catch[raw$Year %in% unlumphist$Year & raw$IdOrig==stids[b]]
    }
    
      # if(nrow(unlumpproj)>0 & is.na(percentFuture$Percent[percentFuture$IdOrig==stids[b]])==F)
      # if((nrow(percentFuture)>0 & is.na(percentFuture$Percent[percentFuture$IdOrig==stids[b]])==F) & stids[b] %in% percentFuture$IdOrig) & percentFuture$Percent[percentFuture$IdOrig==stids[b]]>0)
      
    if(maxyr>BaselineYear & length(futurestids)>0)
    {
      for(b in 1:length(futurestids))
        {
          # copy lumped stock's projected data
          unlumpproj<-subset(lumpproj,Year>BaselineYear)
          
          unlumpproj$Country<-unique(percentFuture$Country[percentFuture$IdOrig==futurestids[b]])
          
          unlumpproj$IdOrig<-futurestids[b]
          
          unlumpproj$Catch<-unlumpproj$Catch*percentFuture$Percent[percentFuture$IdOrig==futurestids[b]]
          
          unlumpproj$Biomass<-unlumpproj$Biomass*percentFuture$Percent[percentFuture$IdOrig==futurestids[b]]
          
          unlumpproj$MSY<-unlumpproj$MSY*percentFuture$Percent[percentFuture$IdOrig==futurestids[b]]
          
          unlumpproj$k<-unlumpproj$k*percentFuture$Percent[percentFuture$IdOrig==futurestids[b]]
          
          unlumpproj$Profits<-unlumpproj$Profits*percentFuture$Percent[percentFuture$IdOrig==futurestids[b]]
          
          unlumpproj$DiscProfits<-unlumpproj$DiscProfits*percentFuture$Percent[percentFuture$IdOrig==futurestids[b]]
          
          UnLumpedProj<-rbind(UnLumpedProj,unlumpproj)
        }
    }
      # UnLumped<-rbind(UnLumped,unlumphist,unlumpproj)
      show(a)
      
      All<-rbind(UnLumpedHist,UnLumpedProj)
      
      UnlumpedFaoData[[a]]<-All
    
    # show(a)
  
  } # close lumped stock loop
  
  # flatten to one dataframe
  UnLumpedFaoData<-ldply(UnlumpedFaoData, data.frame) 
  
#   testAll<-inner_join(UnLumpedFaoData,RawData[,c('IdOrig','Year','Catch')],by=c('IdOrig','Year'))
#   plot(testAll$Catch.x,testAll$Catch.y)
  
  
  FinalData<-Data[!(grepl('Lumped',Data$IdOrig)),]

  FinalData<-rbind(UnLumpedFaoData,FinalData)

  return(FinalData)
  
}  
  
