##########################################################
##
## Produce summary table of results for top fisheries in
## major fishing countries
##
##########################################################

# Values are for top stocks in a country according to total catch in 2012
# 'All Stocks' values are medians for B/Bmsy, F/Fmsy, total for MSY, and harvest weighted price

# DataU<-UnlumpedProjectionData
# DataL<-ProjectionData
# NumberOfCountries<-'All'
# NumberOfStocks<-'All'
# Policies<-c('Business As Usual','Business As Usual Pessimistic','Catch Share Three','CatchShare','Fmsy Three','Fmsy')
# FileName<-'Country Results All Stocks'

CountryTopStocks<-function(DataU,DataL,BaselineYear,Policies,NumberOfStocks,NumberOfCountries,Discount,ResultFolder,FileName)
{
  
  # Find top countries in Unlumped data
  top<-ddply(DataU[DataU$Year==BaselineYear,],c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T))

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
    cntrys<-unique(DataU$Country[DataU$Year==BaselineYear])
  }
  
  # Create empty list to fill
  TopStocks<-list()
  
  # Loop over cntrys and extract price, b, f, and msy, as well as Profits, Biomass, and Harvest for Baseline, BAU, and Catch Share Three
  
  for(a in 1:length(cntrys))
  { 
    show(a)
    # subset to country stocks
    tempC<-DataU[DataU$Country==cntrys[a],]
    
    # find total results and save 2012, BAU, and CS Three results
    tempAll<-ddply(tempC,c('Year','Policy'),summarize,TotalStocks=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),
                   TotalBiomass=sum(Biomass,na.rm=T),TotalNPV=sum(NPV,na.rm=T),TotalMSY=sum(MSY,na.rm=T),MedB=median(BvBmsy,na.rm=T),MedF=median(FvFmsy,na.rm=T))
    
    tempAll<-tempAll[tempAll$Year==BaselineYear | (tempAll$Year==max(DataU$Year)),
                     c('Policy','MedB','MedF','TotalCatch','TotalProfit','TotalBiomass','TotalNPV','TotalMSY')]
    
    tempAll$Annuity<-tempAll$TotalNPV*(Discount/(1-(1+Discount)^(-1*max(DataU$Year-BaselineYear))))
    
    tempAll$IdOrig<-rep('All Stocks')  
    
    # find baseline year results
    tempBase<-tempC[tempC$Year==BaselineYear,]
    
    tempBase<-tempBase[with(tempBase,order(-Catch)),]
    
    # find catch weighted average price across stocks
    tempBase$PxC<-tempBase$Price*tempBase$Catch
    
    tempPrice<-sum(tempBase$PxC,na.rm=T)/sum(tempBase$Catch)
    
    tempBase$PxC<-NULL
    
    # If NumberOfStocks=='All', find number of stocks for that country, other wise set Stocks to NumberOfStocks
    if(NumberOfStocks=='All')
    {
      Stocks<-nrow(tempBase)
    }
    
    if(is.numeric(NumberOfStocks))
    {
      Stocks<-NumberOfStocks
    }
    
    tempBase<-tempBase[1:Stocks,c('IdOrig','Dbase','CatchShare','CommName','BvBmsy','FvFmsy','MSY','Price','Profits','Catch','Biomass')]
    
    colnames(tempBase)<-c('IdOrig','Dbase','CatchShare','CommName','BvBmsy','FvFmsy','MSY','Price','Profits_Today','Catch_Today','Biomass_Today')
    
    ## find BAU and Catch Share Three results for same stocks
    ids<-unique(tempBase$IdOrig)
    
    for(b in 1:length(Policies))
    {
      # subset data to policy
      tempPol<-tempC[(tempC$IdOrig %in% ids) & tempC$Policy==Policies[b] & tempC$Year==max(DataU$Year),
                     c('IdOrig','NPV','Profits','Catch','Biomass')]
      
      tempPol$Annuity<-tempPol$NPV*(Discount/(1-(1+Discount)^(-1*max(DataU$Year-BaselineYear))))
      
      tempPol<-tempPol[,c('IdOrig','NPV','Annuity','Profits','Catch','Biomass')]
            
      # set colnames for each policy
      if(Policies[b]=='Business As Usual') { colnames(tempPol)[!colnames(tempPol)=='IdOrig']<-paste(colnames(tempPol)[!colnames(tempPol)=='IdOrig'],'BAU',sep='_') }
      if(Policies[b]=='Business As Usual Pessimistic') { colnames(tempPol)[!colnames(tempPol)=='IdOrig']<-paste(colnames(tempPol)[!colnames(tempPol)=='IdOrig'],'BAUPessimistic',sep='_') }
      if(Policies[b]=='Catch Share Three') { colnames(tempPol)[!colnames(tempPol)=='IdOrig']<-paste(colnames(tempPol)[!colnames(tempPol)=='IdOrig'],'CS3',sep='_') }
      if(Policies[b]=='CatchShare') { colnames(tempPol)[!colnames(tempPol)=='IdOrig']<-paste(colnames(tempPol)[!colnames(tempPol)=='IdOrig'],'CS',sep='_') }
      if(Policies[b]=='Fmsy Three') { colnames(tempPol)[!colnames(tempPol)=='IdOrig']<-paste(colnames(tempPol)[!colnames(tempPol)=='IdOrig'],'Fmsy3',sep='_') }
      if(Policies[b]=='Fmsy') { colnames(tempPol)[!colnames(tempPol)=='IdOrig']<-paste(colnames(tempPol)[!colnames(tempPol)=='IdOrig'],'Fmsy',sep='_') }
      
      # join results for each policy as new columns
      if(b==1)
      {
        tempFinal<-join(tempBase,tempPol,by='IdOrig',type='full',match='all')
      }
      
      if(b>1)
      {
        tempFinal<-join(tempFinal,tempPol,by='IdOrig',type='full',match='all')
      }
    } # close loop
    
    # Add country totals
    tempFinal[nrow(tempFinal)+1,c('IdOrig','BvBmsy','FvFmsy','MSY','Profits_Today','Catch_Today','Biomass_Today')]<-tempAll[tempAll$Policy=='Historic',c('IdOrig','MedB','MedF','TotalMSY','TotalProfit','TotalCatch','TotalBiomass')]
    
    for(c in 1:length(Policies))
    {
      # find colnames of results to fill for each policy
      if(Policies[c]=='Business As Usual') { cols<-c('NPV_BAU','Annuity_BAU','Profits_BAU','Catch_BAU','Biomass_BAU') }
      if(Policies[c]=='Business As Usual Pessimistic') { cols<-c('NPV_BAUPessimistic','Annuity_BAUPessimistic','Profits_BAUPessimistic','Catch_BAUPessimistic','Biomass_BAUPessimistic') }
      if(Policies[c]=='Catch Share Three') { cols<-c('NPV_CS3','Annuity_CS3','Profits_CS3','Catch_CS3','Biomass_CS3') }
      if(Policies[c]=='CatchShare') { cols<-c('NPV_CS','Annuity_CS','Profits_CS3','Catch_CS','Biomass_CS') }
      if(Policies[c]=='Fmsy Three') { cols<-c('NPV_Fmsy3','Annuity_Fmsy3','Profits_Fmsy3','Catch_Fmsy3','Biomass_Fmsy3') }
      if(Policies[c]=='Fmsy') { cols<-c('NPV_Fmsy','Annuity_Fmsy','Profits_Fmsy','Catch_Fmsy','Biomass_Fmsy') }
      
      tempFinal[nrow(tempFinal),cols]<-tempAll[tempAll$Policy==Policies[c],c('TotalNPV','Annuity','TotalProfit','TotalCatch','TotalBiomass')]
    }
    
    # add country catch-weighted price
    tempFinal$Price[nrow(tempFinal)]<-tempPrice
    
    # add country name
    tempFinal$Country<-rep(cntrys[a])
    
    # store in main list
    TopStocks[[a]]<-tempFinal
    
  } # close cntrys loop
  
  TopStocks<-ldply(TopStocks)
  
  # Calculate global totals by policy and add to table
  global<-ddply(DataL[DataL$Year %in% c(BaselineYear,max(DataL$Year)),],c('Year','Policy'),summarize,TotalStocks=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalProfit=sum(Profits,na.rm=T),
                TotalBiomass=sum(Biomass,na.rm=T),TotalNPV=sum(NPV,na.rm=T),TotalMSY=sum(MSY,na.rm=T),MedB=median(BvBmsy,na.rm=T),MedF=median(FvFmsy,na.rm=T))
  
  global$Annuity<-global$TotalNPV*(Discount/(1-(1+Discount)^(-1*max(DataL$Year-BaselineYear))))
  
  # Add global totals
  g<-TopStocks[1,]
  
  g[1,]<-NA
  
  g[1,c('Country','IdOrig','Dbase','CatchShare','CommName')]<-c('Global','All Stocks',NA,NA,NA)
  
  g[1,c('BvBmsy','FvFmsy','MSY','Profits_Today','Catch_Today','Biomass_Today')]<-global[global$Policy=='Historic',c('MedB','MedF','TotalMSY','TotalProfit','TotalCatch','TotalBiomass')]
  
  
  for(d in 1:length(Policies))
  {
    # find colnames of results to fill for each policy
    if(Policies[d]=='Business As Usual') { cols<-c('NPV_BAU','Annuity_BAU','Profits_BAU','Catch_BAU','Biomass_BAU') }
    if(Policies[d]=='Business As Usual Pessimistic') { cols<-c('NPV_BAUPessimistic','Annuity_BAUPessimistic','Profits_BAUPessimistic','Catch_BAUPessimistic','Biomass_BAUPessimistic') }
    if(Policies[d]=='Catch Share Three') { cols<-c('NPV_CS3','Annuity_CS3','Profits_CS3','Catch_CS3','Biomass_CS3') }
    if(Policies[d]=='CatchShare') { cols<-c('NPV_CS','Annuity_CS','Profits_CS','Catch_CS','Biomass_CS') }
    if(Policies[d]=='Fmsy Three') { cols<-c('NPV_Fmsy3','Annuity_Fmsy3','Profits_Fmsy3','Catch_Fmsy3','Biomass_Fmsy3') }
    if(Policies[d]=='Fmsy') { cols<-c('NPV_Fmsy','Annuity_Fmsy','Profits_Fmsy','Catch_Fmsy','Biomass_Fmsy') }
    
    g[1,cols]<-global[global$Policy==Policies[d],c('TotalNPV','Annuity','TotalProfit','TotalCatch','TotalBiomass')]
  }

  # find catch weighted average price
  gp<-DataL[DataL$Year==2012,c('Catch','Price')]
  
  gp$PxC<-gp$Price*gp$Catch
  
  gp$GlobalPrice<-sum(gp$PxC,na.rm=T)/sum(gp$Catch,na.rm=T)
  
  tempGlobalPrice<-sum(gp$PxC,na.rm=T)/sum(gp$Catch,na.rm=T)
  
  g[1,c('Price')]<-unique(gp$GlobalPrice)
  
  # Bind global row to top of TopStocks dataframe
  TopStocks<-rbind(g,TopStocks)
  
  # Write csv
  write.csv(TopStocks,file=paste(ResultFolder,FileName,'.csv',sep=''))

  return(TopStocks)
}