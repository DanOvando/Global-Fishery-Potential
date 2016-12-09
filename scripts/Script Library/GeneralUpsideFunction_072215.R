##################################################################---------------------------------------------------------------------
#
# Flexible Upside Calculator Function
#
# Tyler Clavelle
# 7/20/2015
#
# Code description: This code contains a general function for calculating upsides based on aggregating criteria specified by a grouping
# variable
#
# *** FUNCTION CURRENTLY ONLY WORKS FOR WHEN GROUPING IS SET TO COUNTRY
#
##################################################################---------------------------------------------------------------------

# Variables for testing
# DataU<-UnlumpedProjectionData
# Grouping<-'Country'

UpsideFunction(DataU,Grouping)
{

  # Convert to tbl_df and subset data to include relevant columns
  DataU<- DataU %>%
    tbl_df() %>%
    select(Dbase,IdOrig,Country,RegionFAO,SciName,CommName,SpeciesCatName,Year,Policy,BvBmsy,FvFmsy,MSY,Catch,Profits,Biomass,Price)
  
  # Calculate totals aggregated by grouping variables
  if(Grouping=='Country')
  {
    Upsides<- DataU %>%
      tbl_df() %>%
      group_by(Country,Policy,Year) %>%
      summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
  }

  if(Grouping=='IdOrig')
  {
  Upsides<- DataU %>%
    tbl_df() %>%
    rename(MedianBvBmsy=BvBmsy,MedianFvFmsy=FvFmsy,TotalMSY=MSY,
           TotalCatch=Catch, TotalBiomass=Biomass,TotalProfits=Profits,MeanPrice=Price)
  }
      
    # Subset baseline year 
    Base<-Upsides %>%
      tbl_df() %>%
      filter(Year==BaselineYear) %>%
      ungroup() %>%
      select(-TotalMSY,-Year,-Policy) %>%
      rename(BaselineBvBmsy=MedianBvBmsy,BaselineFvFmsy=MedianFvFmsy,BaselineCatch=TotalCatch,BaselineBiomass=TotalBiomass,BaselineProfits=TotalProfits)
    
    # Join upsides and baseline data
    if(Grouping=='Country') { Upsides<-inner_join(Upsides,Base,by=c('Country')) }
    if(Grouping=='IdOrig') { Upsides<-inner_join(Upsides,Base,by=c('IdOrig')) }
    
    ### Calculate upsides relative to today (Baseline) --------------------------------------------------------------------------------
    
    # Define perc upside function
    PercChange<- function(A,B)
    {
      PC<- ((A-B)/(B))*100*sign(B)
      
      PC[B<=0 & (A-B)>0]<- 999
      
      PC[B<=0 & (A-B)<=0]<- -999
      
      return(PC)
    }
    
    # Calculate abs upsides
    Upsides$AbsChangeCatch<-Upsides$TotalCatch-Upsides$BaselineCatch
    Upsides$AbsChangeBiomass<-Upsides$TotalBiomass-Upsides$BaselineBiomass
    Upsides$AbsChangeProfits<-Upsides$TotalProfits-Upsides$BaselineProfits
    
    # Calculate perc upsides
    Upsides$PercChangeCatch<-PercChange(Upsides$TotalCatch,Upsides$BaselineCatch)
    Upsides$PercChangeBiomass<-PercChange(Upsides$TotalBiomass,Upsides$BaselineBiomass)
    Upsides$PercChangeProfits<-PercChange(Upsides$TotalProfits,Upsides$BaselineProfits)
    
    ### Rename policies and build scenarios for calculating upsides relative to BAU ---------------------------------------------------
    
    # Split into 'Conservation Concern' and 'All Stocks' scenarios
    CC<- subset(Upsides,Policy %in% c('Catch Share Three','Fmsy Three'))
    
    CC$Scenario<-'Conservation Concern'
    
    CC$Policy[CC$Policy=='Catch Share Three']<-'RBFM'
    
    CC$Policy[CC$Policy=='Fmsy Three']<-'Fmsy'
    
    ALL<-subset(Upsides,Policy %in% c('CatchShare','Fmsy'))
    
    ALL$Scenario<-'All Stocks'
    
    ALL$Policy[ALL$Policy=='CatchShare']<-'RBFM'
    
    # Subset respective BAU policies and add to scenarios
    CCBAU<-subset(Upsides,Policy=='Business As Usual') %>%
      select(Country,Policy,Year,MedianBvBmsy,MedianFvFmsy,TotalCatch,TotalBiomass,TotalProfits) %>%
      rename(PolicyBAU=Policy,BvBmsyBAU=MedianBvBmsy,FvFmsyBAU=MedianFvFmsy,TotalCatchBAU=TotalCatch,TotalBiomassBAU=TotalBiomass,TotalProfitsBAU=TotalProfits)
    
    ALLBAU<-subset(Upsides,Policy=='Business As Usual Pessimistic') %>%
      select(Country,Policy,Year,MedianBvBmsy,MedianFvFmsy,TotalCatch,TotalBiomass,TotalProfits) %>%
      rename(PolicyBAU=Policy,BvBmsyBAU=MedianBvBmsy,FvFmsyBAU=MedianFvFmsy,TotalCatchBAU=TotalCatch,TotalBiomassBAU=TotalBiomass,TotalProfitsBAU=TotalProfits)
    
    # Join BAU results with scenario upsides
    CC<-inner_join(CC,CCBAU,by=c('Country','Year'))
    
    ALL<-inner_join(ALL,ALLBAU,by=c('Country','Year'))
    
    Scenarios<-rbind(CC,ALL)
    
    ### Calculate upsides relative to BAU --------------------------------------------------------------------------------
    
    # Calculate abs upsides from BAU
    Scenarios$AbsChangeCatchBAU<-Scenarios$TotalCatch-Scenarios$TotalCatchBAU
    Scenarios$AbsChangeBiomassBAU<-Scenarios$TotalBiomass-Scenarios$TotalBiomassBAU
    Scenarios$AbsChangeProfitsBAU<-Scenarios$TotalProfits-Scenarios$TotalProfitsBAU
    
    # Calculate perc upsides from BAU
    Scenarios$PercChangeCatchBAU<-PercChange(Scenarios$TotalCatch,Scenarios$TotalCatchBAU)
    Scenarios$PercChangeBiomassBAU<-PercChange(Scenarios$TotalBiomass,Scenarios$TotalBiomassBAU)
    Scenarios$PercChangeProfitsBAU<-PercChange(Scenarios$TotalProfits,Scenarios$TotalProfitsBAU)
    
    # Subset to final year of projection and RBFM policy
    Scenarios2050<-Scenarios %>%
      tbl_df() %>%
      filter(Year==2050 & Policy=='RBFM') %>%
      select(-MeanPrice.y,-Fisheries.y)
    
    # Reorder columns to have Policy and Scenario adjacent
    Scenarios2050<-Scenarios2050[c(1,2,23,3:22,24:35)]
    
    # save data
    write.csv(Scenarios2050,file=paste(ResultFolder,'RBFM_Upside_ByScenario.csv',sep=''))
    
    # Test calculations match global totals in paper - They do
    Scenarios2050 %>%
      group_by(Scenario) %>%
      summarize(TotalBiomass=sum(TotalBiomass,na.rm=T),TotalProfits=sum(TotalProfits,na.rm=T))
    
  return(Scenarios2050)
  
}