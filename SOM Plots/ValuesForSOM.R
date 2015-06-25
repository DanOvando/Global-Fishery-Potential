##########################################################
##
## Values, Summary tables, and misc figures for SOM
##
##########################################################

ValuesForSOM<-function(ProjectionData,SpeciesCategoriesToLump)
{
  
  # ISSCAAP categories, codes, and number of stocks------------------------------------
  
  isscaap<-ddply(ProjectionData[ProjectionData$Year==2012,],c('SpeciesCatName','SpeciesCat'),summarize,
                 TotalStocks=length(unique(IdOrig)))
  
  isscaap$Grouped[isscaap$SpeciesCatName %in% SpeciesCategoriesToLump]<-'Yes'
  isscaap$Grouped[!(isscaap$SpeciesCatName %in% SpeciesCategoriesToLump)]<-'No'
  
  isscaap<-isscaap[with(isscaap,order(SpeciesCat)),]
  
  write.csv(paste(ResultFolder,'SOM ISSCAAP Table.csv'))
  
  # Regression Coeffients and significance levels--------------------------------------
  
  m1<-RegressionResultsAllRam$Models[coefficients]
  m1$coefficients
  
  # Total landings, MSY, and k for current year----------------------------------------
  
  ddply(UnlumpedProjectionData[UnlumpedProjectionData$Year==2012 & is.na(UnlumpedProjectionData$MSY)==F & UnlumpedProjectionData$CanProject==T,],c('Year','Policy'),summarize,
        TotalFisheries=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalK=sum(k,na.rm=T),MeanPrice=mean(Price,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
  
  ddply(ProjectionData[ProjectionData$Year==2012 & is.na(ProjectionData$MSY)==F & ProjectionData$CanProject==T,],c('Year','Policy'),summarize,
        TotalFisheries=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalK=sum(k,na.rm=T),MeanPrice=mean(Price,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
  
  ddply(ProjectionData[ProjectionData$Dbase=='RAM' & ProjectionData$Year==2012 & is.na(ProjectionData$MSY)==F & ProjectionData$CanProject==T,],c('Year','Policy'),summarize,
        TotalFisheries=length(unique(IdOrig)),TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T),TotalK=sum(k,na.rm=T),MeanPrice=mean(Price,na.rm=T),TotalProfit=sum(Profits,na.rm=T))
    
  # Biomass and NPV by policy in final year----------------------------------------------
  
  upside<-UpsideAllStocks$CountryUpsides
  
  upside<-ddply(upside,c('Policy'),summarize, FinalBiomass=sum(TotalBiomass,na.rm=T),FinalNPV=sum(TotalNPV,na.rm=T),
        FinalSQNPV=sum(TotalNPVSQ,na.rm=T),FinalYield=sum(TotalCatch,na.rm=T))
  
  # Median status by ISSCAAP in current year------------------------------------------------
  
  statIsscaap<-ddply(ProjectionData[ProjectionData$CanProject==T,],c('Year','Policy','SpeciesCatName'),summarize,MedianStatus=median(BvBmsy,na.rm=T))
  
  ggplot(statIsscaap,aes(x=Year,y=MedianStatus,color=Policy)) +
    geom_line(size=2) +
    facet_wrap(~SpeciesCatName,scales='free')
  
  
}