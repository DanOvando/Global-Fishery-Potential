###########################################
##
## This function produces summary tables of
## all stocks included in Projection data
## as well as aggregated by country
##
###########################################



StockAndCountrySummary<-function(UnlumpedProjectionData,StitchIds,BaselineYear)
{
  # Create list of stocks
  
  StockList<-UnlumpedProjectionData[UnlumpedProjectionData$Year==BaselineYear & is.na(UnlumpedProjectionData$MSY)==F & UnlumpedProjectionData$CanProject==T,
                                    c('IdOrig','Country','SpeciesCatName','IdLevel','CommName','SciName','Dbase','CatchShare','Year','Catch','MSY','BvBmsy',
                                      'FvFmsy','r','k')]
  
  # find ids of all lumped stocks
  
  stitchIds<-list()
  
  for(a in 1:nrow(StitchIds))
  {  
    ids<-unlist(str_split(StitchIds$StockIDs[a],pattern='_'))
    
    tempIds<-data.frame(as.character(ids))
    
    colnames(tempIds)<-c('StockId')
    
    stitchIds[[a]]<-tempIds

#     show(a)
  }
  
  stitchIds<-ldply(stitchIds)
  
  stitchIds$StockId<-as.character(stitchIds$StockId)
  
  # Indicate which stocks were previously lumped
  
  StockList$WasLumped[StockList$IdOrig %in% stitchIds$StockId]<-TRUE
  
  # Indicate which stocks are currently B/Bmsy < 1
  
  StockList$OverFished[StockList$BvBmsy<1]<-TRUE
  
  StockList$OverFishing[StockList$FvFmsy>1]<-TRUE
  
  # Order by country
  
  StockList<-StockList[with(StockList,order(Country)),]
  
  write.csv(StockList,file=paste(ResultFolder,'GFR Projection Stock List.csv',sep=''))

  # Pull out just RAM stocks

  ram<-StockList[StockList$Dbase=='RAM',]
  
  write.csv(ram,file=paste(ResultFolder,'GFR Projection RAM Stock List.csv',sep=''))

  ### Aggregate by Country-----------------------------------------------------------------
  
  # Calculate stats in our data
  
  CountrySummary<-ddply(StockList,c('Country'),summarize,Stocks=length(unique(IdOrig)),TotalMSY=sum(MSY,na.rm=T),TotalCatch=sum(Catch,na.rm=T),
                        MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),PercBvBmsyBelowOne=100*(sum(OverFished,na.rm=T)/length(unique(IdOrig))),
                        PercFvFmsyAboveOne=100*(sum(OverFishing,na.rm=T))/length(unique(IdOrig)))
  
  # calculate number of NEI fisheries
  
  CountryNeiSummary<-ddply(StockList[StockList$IdLevel=='Neis',], c('Country'), summarize, NeiStocks=length(unique(IdOrig)),NeiCatch=sum(Catch,na.rm=T),NeiMSY=sum(MSY,na.rm=T),
        NeiMedianStatus=median(BvBmsy,na.rm=T))
  
  CountryNeiSummary<-CountryNeiSummary[with(CountryNeiSummary,order(-NeiMSY)),]
  
  CountrySummary$NeiCatch<- NA
  
  CountrySummary$NeiStocks<- NA
  
  # Add nei data to country summary
  for(b in 1:nrow(CountrySummary))
  {
    whereN<-match(CountrySummary$Country[b],CountryNeiSummary$Country)
    
    if(is.na(whereN)==F) 
    {
      CountrySummary$NeiCatch[b]<-CountryNeiSummary$NeiCatch[whereN]
      
      CountrySummary$NeiStocks[b]<-CountryNeiSummary$NeiStock[whereN]
    }
    rm(whereN)
  }
  
  # Calculate stats in FAO data
  FAO<-ddply(RawData[RawData$Dbase=='FAO' & RawData$Year==2012 & !(RawData$SpeciesCatName %in% SpeciesCategoriesToOmit),],c('Country'),summarize,
             TotalCatch=sum(Catch,na.rm=T),Fisheries=length(unique(IdOrig)))
  

  CountrySummary<-CountrySummary[order(-CountrySummary$TotalMSY),]
  
  # Add fao data to country summary
  
  CountrySummary$FaoCatch<-NA
  CountrySummary$FaoFisheries<-NA
  
  for(a in 1:nrow(CountrySummary))
  {
    whereF<-match(CountrySummary$Country[a],FAO$Country)
    
    if(is.na(whereF)==F) 
    {
      CountrySummary$FaoCatch[a]<-FAO$TotalCatch[whereF]
      
      CountrySummary$FaoFisheries[a]<-FAO$Fisheries[whereF]
    }
    rm(whereF)
  }
  
  # Calculate what we account for
  
  CountrySummary$PercCatchCovered<-(CountrySummary$TotalCatch/CountrySummary$FaoCatch)*100
  
  CountrySummary$PercFisheriesCovered<-(CountrySummary$Stocks/CountrySummary$FaoFisheries)*100
  
  write.csv(CountrySummary,file=paste(ResultFolder,'Country Data Summary.csv',sep=''))
  
  ### Plot coverage statistics
  
  # Percent coverage all countries
  
  percs<-CountrySummary[CountrySummary$Country!='Multinational',c('Country','TotalMSY','PercCatchCovered','PercFisheriesCovered')]
  
  percs<-melt(percs,id.vars=c('Country'))
  
  pdf(file=paste(FigureFolder,'Country Coverage Density Plot.pdf',sep=''))
  
  print(ggplot(percs[percs$variable!='TotalMSY',],aes(x=value,fill=variable)) +
    geom_density(alpha=0.6) +
    theme(text=element_text(size=18)) +
    labs(x='% in Database Relative to FAO',title='Country Coverage (All Countries)'))
  
  dev.off()
  
  # Percent coverage countries with MSY > 1,000,000
  
  msyIds<-unique(CountrySummary$Country[CountrySummary$Country!='Multinational' & CountrySummary$TotalMSY>100000])
  
  pdf(file=paste(FigureFolder,'Country Coverage MSY Over 100K Density Plot.pdf',sep=''))
  
  print(ggplot(percs[percs$Country %in% msyIds & percs$variable!='TotalMSY',],aes(x=value,fill=variable)) +
          geom_density(alpha=0.6) +
          theme(text=element_text(size=18)) +
          labs(x='% in Database Relative to FAO',title='Country Coverage (Country MSY > 100,000)'))
  
  dev.off()

  return(list(CountrySummary=CountrySummary,StockList=StockList))
}

  
