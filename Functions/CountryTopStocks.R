##########################################################
##
## Produce summary table of results for top fisheries in
## major fishing countries
##
##########################################################

# Values are for top stocks in a country according to total catch in 2012
# 'All Stocks' values are medians for B/Bmsy, F/Fmsy, total for MSY, and harvest weighted price

# NumberOfCountries<-20
# NumberOfStocks<-5

CountryTopStocks<-function(UnlumpedProjectionData,BaselineYear,NumberOfStocks,NumberOfCountries,Discount,ResultFolder)
{
  
  # Find top countries in Unlumped data
  top<-ddply(UnlumpedProjectionData[UnlumpedProjectionData$Year==2012,],c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T))

  top<-top[with(top,order(-TotalMSY)),]
  
  top<-top[!(top$Country %in% c('Multinational','High Seas Tuna and Billfish')),]
  
  if(is.numeric(NumberOfCountries))
  {
    top<-top[1:NumberOfCountries,]
    
    cntrys<-top$Country
  }

  # If making table for all countries, use unique countries
  if(NumberOfCountries=='All')
  {
    cntrys<-unique(UnlumpedProjectionData$Country[UnlumpedProjectionData$Year==2012])
  }
  
  # Create empty list to fill
  TopStocks<-list()
  
  # Loop over cntrys and extract price, b, f, and msy, as well as Profits, Biomass, and Harvest for Baseline, BAU, and Catch Share Three
  
  for(a in 1:length(cntrys))
  { 
#     show(a)
    # subset to country stocks
    tempC<-UnlumpedProjectionData[UnlumpedProjectionData$Country==cntrys[a],]
    
    # find total results and save 2012, BAU, and CS Three results
    tempAll<-ddply(tempC,c('Year','Policy'),summarize,TotalStocks=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),
                   TotalBiomass=sum(Biomass,na.rm=T),TotalNPV=sum(NPV,na.rm=T),TotalMSY=sum(MSY,na.rm=T),MedB=median(BvBmsy,na.rm=T),MedF=median(FvFmsy,na.rm=T))
    
    tempAll<-tempAll[tempAll$Year==2012 | (tempAll$Year==max(UnlumpedProjectionData$Year) & tempAll$Policy %in% c('Business As Usual','Catch Share Three')),
                     c('Policy','MedB','MedF','TotalCatch','TotalProfit','TotalBiomass','TotalNPV','TotalMSY')]
    
    tempAll$Annuity<-tempAll$TotalNPV*(Discount/(1-(1+Discount)^(-1*max(UnlumpedProjectionData$Year-BaselineYear))))
    
    tempAll$IdOrig<-rep('All Stocks')
    
    # find baseline year results
    tempBase<-tempC[tempC$Year==2012,]
    
    tempBase<-tempBase[with(tempBase,order(-Catch)),]
    
    tempBase<-tempBase[1:NumberOfStocks,c('IdOrig','Dbase','CatchShare','CommName','BvBmsy','FvFmsy','MSY','Price','Profits','Catch','Biomass')]
    
    colnames(tempBase)<-c('IdOrig','Dbase','CatchShare','CommName','BvBmsy','FvFmsy','MSY','Price','Profits_2012','Catch_2012','Biomass_2012')
    
    # find catch weighted average price
    tempBase$PxC<-tempBase$Price*tempBase$Catch_2012
    
    tempPrice<-sum(tempBase$PxC,na.rm=T)/sum(tempBase$Catch_2012)
    
    tempBase$PxC<-NULL
    
    ## find BAU and Catch Share Three results for same stocks
    ids<-unique(tempBase$IdOrig)

    # BAU
    tempBAU<-tempC[(tempC$IdOrig %in% ids) & tempC$Policy=='Business As Usual' & tempC$Year==max(UnlumpedProjectionData$Year),
                    c('IdOrig','NPV','Catch','Biomass')]
    
    colnames(tempBAU)<-c('IdOrig','NPV_BAU','Catch_BAU','Biomass_BAU')

    # CS3
    tempCS3<-tempC[(tempC$IdOrig %in% ids) & tempC$Policy=='Catch Share Three' & tempC$Year==max(UnlumpedProjectionData$Year),
                   c('IdOrig','NPV','Catch','Biomass')]
    
    colnames(tempCS3)<-c('IdOrig','NPV_CatchShareThree','Catch_CatchShareThree','Biomass_CatchShareThree')
     
    # bind tables together
    tempFinal<-join(tempBase,tempBAU,by='IdOrig',type='full',match='all')
    
    tempFinal<-join(tempFinal,tempCS3,by='IdOrig',type='full',match='all')

    tempFinal[nrow(tempFinal)+1,c('IdOrig','BvBmsy','FvFmsy','MSY','Profits_2012','Catch_2012','Biomass_2012')]<-tempAll[tempAll$Policy=='Historic',c('IdOrig','MedB','MedF','TotalMSY','TotalProfit','TotalCatch','TotalBiomass')]
    
    tempFinal[nrow(tempFinal),c('NPV_BAU','Catch_BAU','Biomass_BAU')]<-tempAll[tempAll$Policy=='Business As Usual',c('Annuity','TotalCatch','TotalBiomass')]

    tempFinal[nrow(tempFinal),c('NPV_CatchShareThree','Catch_CatchShareThree','Biomass_CatchShareThree')]<-tempAll[tempAll$Policy=='Catch Share Three',c('Annuity','TotalCatch','TotalBiomass')]

    tempFinal$Price[nrow(tempFinal)]<-tempPrice
    
    tempFinal$Country<-rep(cntrys[a])
    
    tempFinal<-tempFinal[,c('Country','IdOrig','Dbase','CatchShare','CommName','BvBmsy','FvFmsy','MSY','Price','Profits_2012','Catch_2012','Biomass_2012','NPV_BAU','Catch_BAU','Biomass_BAU','NPV_CatchShareThree','Catch_CatchShareThree','Biomass_CatchShareThree')]
    
    colnames(tempFinal)<-c('Country','IdOrig','Dbase','CatchShare','CommName','BvBmsy','FvFmsy','MSY','Price','Profits_2012','Catch_2012','Biomass_2012','Annuity_BAU','Catch_BAU','Biomass_BAU','Annuity_CatchShareThree','Catch_CatchShareThree','Biomass_CatchShareThree')
    # add to main list
    TopStocks[[a]]<-tempFinal
    
  } # close cntrys loop
  
  TopStocks<-ldply(TopStocks)
  
  # Calculate global totals by policy and add to table
  global<-ddply(UnlumpedProjectionData[UnlumpedProjectionData$Year %in% c(2012,2050),],c('Year','Policy'),summarize,TotalStocks=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),
                TotalBiomass=sum(Biomass,na.rm=T),TotalNPV=sum(NPV,na.rm=T),TotalMSY=sum(MSY,na.rm=T),MedB=median(BvBmsy,na.rm=T),MedF=median(FvFmsy,na.rm=T))
  
  global$Annuity<-global$TotalNPV*(Discount/(1-(1+Discount)^(-1*max(UnlumpedProjectionData$Year-BaselineYear))))
  
  ## Create new row for global values
  g<-TopStocks[1,]
  
  g[1,]<-NA
  
  g[1,c('Country','IdOrig','Dbase','CatchShare','CommName')]<-c('Global','All Stocks',NA,NA,NA)
  
  g[1,c('BvBmsy','FvFmsy','MSY','Profits_2012','Catch_2012','Biomass_2012')]<-global[global$Policy=='Historic',c('MedB','MedF','TotalMSY','TotalProfit','TotalCatch','TotalBiomass')]
  
  g[1,c('Annuity_BAU','Catch_BAU','Biomass_BAU')]<-global[global$Policy=='Business As Usual',c('Annuity','TotalCatch','TotalBiomass')]
  
  g[1,c('Annuity_CatchShareThree','Catch_CatchShareThree','Biomass_CatchShareThree')]<-global[global$Policy=='Catch Share Three',c('Annuity','TotalCatch','TotalBiomass')]
  
  # find catch weighted average price
  gp<-UnlumpedProjectionData[UnlumpedProjectionData$Year==2012,c('Catch','Price')]
  
  gp$PxC<-gp$Price*gp$Catch
  
  gp$GlobalPrice<-sum(gp$PxC,na.rm=T)/sum(gp$Catch,na.rm=T)
  
  tempGlobalPrice<-sum(gp$PxC,na.rm=T)/sum(gp$Catch,na.rm=T)
  
  g[1,c('Price')]<-unique(gp$GlobalPrice)
  
  # Bind global row to top of TopStocks dataframe
  TopStocks<-rbind(g,TopStocks)

  # Write csv
  write.csv(TopStocks,file=paste(ResultFolder,'Country Results for Top Stocks.csv',sep=''))

  return(TopStocks)
}