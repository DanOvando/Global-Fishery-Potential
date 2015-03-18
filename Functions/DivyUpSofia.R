

DivyUpSofia<-function(ProjectionData,RawData)
{
  ########################################################
  ##
  ## Function to distribute benefits from SOFIA stocks to 
  ## component countries
  ##
  ########################################################
  
  # Protocol:
  # Separate the individual and multinational SOFIA stocks
  # For each multinational SOFIA fishery in ProjectionData
  #
  # 1) Subset raw FAO data to include all countries in the SOFIA stock and their respective catch records for 2000-2009
  # 2) Calculate percentage of catch each country contributed to total catch in last 5 years of time series (2005-2009)
  #      - When a match can't be found for a given country, save the id of that SOFIA stock to reference later
  # 3) Redistibute benefits using percentages
  
  Data<-ProjectionData
  
  
  ### Subset out SOFIA results and raw FAO data to calculate percentages-------------------------------------------
  sofia<-Data[Data$Dbase=='SOFIA',]
  
  sofia$RegionFAO[sofia$IdOrig=='307-SOFIA-35-37']<-'34'
  
  # summary table of SOFIA stocks to reference
  sofiaRef<-unique(sofia[c('IdOrig','Country','CommName','SciName','SpeciesCatName','RegionFAO')])
    
  # get unique sofia ids
  sofiaIds<-unique(sofiaRef$IdOrig)
  
  # find number of countries in each sofia stock
  sofiaRef$NumCntrys<-NA
  
  for(a in 1:length(sofiaIds))
  {
    sofiaRef$NumCntrys[a]<-length(unlist(str_split(sofiaRef$Country[a],pattern=',')))
  }
    
  # Find individual country sofia stocks
  sofiaIndivIds<-sofiaRef$IdOrig[sofiaRef$NumCntrys==1]

  # save ProjectionData results for individual sofia stocks
  sofiaIndiv<-sofia[sofia$IdOrig %in% sofiaIndivIds,]
  
  # subset sofiaRef to only include multinational stocks that need unlumping
  sofiaRef<-sofiaRef[sofiaRef$NumCntrys>1,]
  
  # remove multinations sofia stocks from original projection data and add unlumped versions back in at end of function
  sofia<-sofia[!(sofia$IdOrig %in% sofiaRef$IdOrig),]
  
  # get unique sofia ids for remaining multinational stocks
  sofiaIds<-unique(sofiaRef$IdOrig)
  
  # use last five years of sofia data for finding average in fao data
  yrs<-c(2005:2009)
  
  # pull out raw FAO data for same years
  f<-RawData[RawData$Dbase=='FAO' & (RawData$Year %in% yrs),]
  
  # create empty list to fill with unlumped data and one for sofia stocks where there are discrepencies between countries in SOFIA and FAO
  
  DivySofia<-list()
  
  MissingSofiaCntry<-c('List of Sofia Stocks Where A Country Cannot be Found in FAO')
  
### Loop over stocks, calculate percentages, and distribute benefits--------------------------------------------------------------

  for(n in 1:length(sofiaIds))
  {
    show(n)
    # SciName of stock
    sci<-sofiaRef$SciName[sofiaRef$IdOrig==sofiaIds[n]]
    
    cntrys<-unlist(str_split(sofiaRef$Country[sofiaRef$IdOrig==sofiaIds[n]],pattern=', '))
    
    # Fao region (unlist in case there are multiple)
    regs<-unlist(str_split(sofiaRef$RegionFAO[sofiaRef$IdOrig==sofiaIds[n]],pattern=','))
    
    # Subset fao to sci name and regions that match multinational stock
    tempF<-f[f$SciName==sci & (f$Country %in% cntrys) & (f$RegionFAO %in% regs),
             c('IdOrig','Country','Year','SciName','SpeciesCatName','Catch')]
    
    # calculate percentages for each country
    cntryPercs<-ddply(tempF,c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T))
    
    cntryPercs$PercOfTotal<- cntryPercs$TotalCatch/sum(cntryPercs$TotalCatch,na.rm=T)
    
    if(length(unique(cntryPercs$Country))<length(cntrys))
    {
      MissingSofiaCntry<-append(MissingSofiaCntry,sofiaIds[n],after=length(MissingSofiaCntry))
      
      print('Missing Stock')
    }
    
    # re-create cntrys with only countries that are found in FAO
    cntrys<-unique(cntryPercs$Country)
    
    # subset ProjectionData for that multinational sofia stock 
    tempSProj<-Data[Data$IdOrig==sofiaIds[n],]  
    
    # create empty dataframe to fill
    NewSofia<-data.frame(matrix(nrow=0,ncol=ncol(tempSProj)))
    
    colnames(NewSofia)<-colnames(tempSProj)

    # loop over countries 
    for(d in 1:length(cntrys))
    {      
      # calculate projection data results for projected policies
      unlumpSofia<-tempSProj
      
      unlumpSofia$Country<-cntrys[d]
      
      # create new IdOrig that combines original sofia ID with country name
      unlumpSofia$IdOrig<-paste(sofiaIds[n],cntrys[d],sep='_') 
      
      unlumpSofia$Catch<-unlumpSofia$Catch*cntryPercs$PercOfTotal[cntryPercs$Country==cntrys[d]]
      
      unlumpSofia$Biomass<-unlumpSofia$Biomass*cntryPercs$PercOfTotal[cntryPercs$Country==cntrys[d]]
      
      unlumpSofia$MSY<-unlumpSofia$MSY*cntryPercs$PercOfTotal[cntryPercs$Country==cntrys[d]]
      
      unlumpSofia$k<-unlumpSofia$k*cntryPercs$PercOfTotal[cntryPercs$Country==cntrys[d]]
      
      unlumpSofia$Profits<-unlumpSofia$Profits*cntryPercs$PercOfTotal[cntryPercs$Country==cntrys[d]]
      
      unlumpSofia$DiscProfits<-unlumpSofia$DiscProfits*cntryPercs$PercOfTotal[cntryPercs$Country==cntrys[d]]
      
      NewSofia<-rbind(NewSofia,unlumpSofia)
      
    } # close country loop
   
    # fill list with unlumped results for sofia stock
    DivySofia[[n]]<-NewSofia
    
  } # close sofia stock loop

# flatten unlumped sofia list
DivySofia<-ldply(DivySofia)

# Combine results for country-level and multinational sofia stocks
UnlumpSofia<-rbind(sofiaIndiv,DivySofia)

# Combine Sofia data with rest of ProjectionData

Data<-Data[!(Data$Dbase=='SOFIA'),]

FinalData<-rbind(Data,UnlumpSofia)

# return ProjectionData with unlumped SOFIA data
show(dim(FinalData))

show(sum(FinalData$Catch,na.rm=T))

return(FinalData)

}

# # test 
# 
# ddply(sofia[sofia$Policy=='Opt',],c('Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),Stocks=length(unique(IdOrig)))
# 
# ddply(UnlumpSofia[UnlumpSofia$Policy=='Opt',],c('Year'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),Stocks=length(unique(IdOrig)))
