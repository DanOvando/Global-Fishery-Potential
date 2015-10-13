##################################################################---------------------------------------------------------------------
#
# Upside Calculator and Scenario Summarizer
#
# Tyler
# 7/17/2015
#
# Code description: Code calculates upsides and organizes results from rawdata to match the scenarios/policies described in Costello
# manuscript
#
##################################################################---------------------------------------------------------------------

# Variables for testing
# DataU<-subset(UnlumpedProjectionData,Year %in% c(2012:2050))
# BaselineYear<-2012
# Level<-'Global'

### Initialize function --------------------------------------------------------------------------------

ScenarioResults<-function(DataU,BaselineYear,Level)
{
  
  ### Define internal functions to use --------------------------------------------------------------------------------
  
  Upside<-function(df)
  {
    # Define perc upside function
    PercChange<- function(A,B)
    {
      PC<- ((A-B)/(B))*100*sign(B)
      
      PC[B<=0 & (A-B)>0]<- 999
      
      PC[B<=0 & (A-B)<=0]<- -999
      
      return(PC)
    }
    
    # Calculate abs upsides
    df$AbsChangeCatch<-df$TotalCatch-df$BaselineCatch
    df$AbsChangeBiomass<-df$TotalBiomass-df$BaselineBiomass
    df$AbsChangeProfits<-df$TotalProfits-df$BaselineProfits
    
    # Calculate perc upsides
    df$PercChangeCatch<-PercChange(df$TotalCatch,df$BaselineCatch)
    df$PercChangeBiomass<-PercChange(df$TotalBiomass,df$BaselineBiomass)
    df$PercChangeProfits<-PercChange(df$TotalProfits,df$BaselineProfits)
    
    # Calculate abs upsides from BAU
    df$AbsChangeCatchBAU<-df$TotalCatch-df$TotalCatchBAU
    df$AbsChangeBiomassBAU<-df$TotalBiomass-df$TotalBiomassBAU
    df$AbsChangeProfitsBAU<-df$TotalProfits-df$TotalProfitsBAU
    
    # Calculate perc upsides from BAU
    df$PercChangeCatchBAU<-PercChange(df$TotalCatch,df$TotalCatchBAU)
    df$PercChangeBiomassBAU<-PercChange(df$TotalBiomass,df$TotalBiomassBAU)
    df$PercChangeProfitsBAU<-PercChange(df$TotalProfits,df$TotalProfitsBAU)
    
    return(df)
  }
  
  ### Calculate totals for all subsets --------------------------------------------------------------------------------
    
  if(Level=='Country')
  {
    totals<- DataU %>%
      filter(Year>=BaselineYear) %>%
      group_by(Country,Policy,Year) %>%
      summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
    base<-totals %>%
      ungroup() %>%
      filter(Year==BaselineYear) %>%
      select(-TotalMSY,-Year,-Policy,-Fisheries) %>%
      rename(BaselineBvBmsy=MedianBvBmsy,BaselineFvFmsy=MedianFvFmsy,BaselineCatch=TotalCatch,BaselineBiomass=TotalBiomass,BaselineProfits=TotalProfits)
    
    # Join upsides and baseline data
    upsides<-inner_join(totals,base,by='Country') %>%
      tbl_df()
  }

  if(Level=='SpeciesCatName')
  {
      totals<- DataU %>%
        filter(Year>=BaselineYear) %>%
        group_by(SpeciesCatName,Policy,Year) %>%
        summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                  TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
      base<-totals %>%
        ungroup() %>%
        filter(Year==BaselineYear) %>%
        select(-TotalMSY,-Year,-Policy,-Fisheries) %>%
        rename(BaselineBvBmsy=MedianBvBmsy,BaselineFvFmsy=MedianFvFmsy,BaselineCatch=TotalCatch,BaselineBiomass=TotalBiomass,BaselineProfits=TotalProfits)
      
      # Join upsides and baseline data
      upsides<-inner_join(totals,base,by='SpeciesCatName') %>%
        tbl_df()
  }
    
  if(Level=='Global')
  {
    totals<- DataU %>%
      filter(Year>=BaselineYear) %>%
      group_by(Policy,Year) %>%
      summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
    
    totals$Country<-'Global'
    
    base<-totals %>%
      ungroup() %>%
      filter(Year==BaselineYear) %>%
      select(-TotalMSY,-Year,-Fisheries) %>%
      rename(BaselineBvBmsy=MedianBvBmsy,BaselineFvFmsy=MedianFvFmsy,BaselineCatch=TotalCatch,BaselineBiomass=TotalBiomass,BaselineProfits=TotalProfits)
    
    # Join upsides and baseline data
    upsides<-inner_join(totals,base,by='Country') %>%
      tbl_df()
  }

  # Split into 'Conservation Concern' and 'All Stocks' scenarios
    CC<- subset(upsides,Policy %in% c('Catch Share Three','Fmsy Three'))
    
    CC$Scenario<-'Conservation Concern'
    
    CC$Policy[CC$Policy=='Catch Share Three']<-'RBFM'
    
    CC$Policy[CC$Policy=='Fmsy Three']<-'Fmsy'
    
    ALL<-subset(upsides,Policy %in% c('CatchShare','Fmsy'))
    
    ALL$Scenario<-'All Stocks'
    
    ALL$Policy[ALL$Policy=='CatchShare']<-'RBFM'
    
    # Subset respective BAU policies and add to scenarios
    CCBAU<-subset(upsides,Policy=='Business As Usual') %>%
      select(Country,Policy,Year,MedianBvBmsy,MedianFvFmsy,TotalCatch,TotalBiomass,TotalProfits) %>%
      rename(PolicyBAU=Policy,BvBmsyBAU=MedianBvBmsy,FvFmsyBAU=MedianFvFmsy,TotalCatchBAU=TotalCatch,TotalBiomassBAU=TotalBiomass,TotalProfitsBAU=TotalProfits)
    
    ALLBAU<-subset(upsides,Policy=='Business As Usual Pessimistic') %>%
      select(Country,Policy,Year,MedianBvBmsy,MedianFvFmsy,TotalCatch,TotalBiomass,TotalProfits) %>%
      rename(PolicyBAU=Policy,BvBmsyBAU=MedianBvBmsy,FvFmsyBAU=MedianFvFmsy,TotalCatchBAU=TotalCatch,TotalBiomassBAU=TotalBiomass,TotalProfitsBAU=TotalProfits)
    
    # Join BAU results with scenario upsides
    CC<-inner_join(CC,CCBAU,by=c('Country','Year'))
    
    ALL<-inner_join(ALL,ALLBAU,by=c('Country','Year'))
    
    if(Level=='Global')
    {
      CC<-inner_join(CC,CCBAU,by=c('Year'))
      
      ALL<-inner_join(ALL,ALLBAU,by=c('Year'))
    }
    
    Scenarios<-rbind(CC,ALL)
    
    ### Calculate upsides relative to BAU --------------------------------------------------------------------------------
    Scenarios<-Upside(Scenarios)
    
    write.csv(Scenarios,file=paste(ResultFolder,Level,'_Scenario_Results_Final.csv',sep=''))
    
    return(Scenarios)
}





