##################################################################---------------------------------------------------------------------
#
# Nearshore Stock Classification
#
# Tyler
# 7/17/2015
#
# Code description: Methodology/Function for classifying stocks in the Upside database as nearshore 
#
##################################################################---------------------------------------------------------------------

# Variables for testing
# DataU<-UnlumpedProjectionData
# DataR<-RawData
# DepthRangeMax<-200

### Function

ClassifyNearshore<-function(DataU,DataR,DepthRangeMax)
{
  ### Read in FishBase data
  Fishbase<-read.csv('Data/Fishbase_Environment.csv',stringsAsFactors=F)
  
  # Define invertebrate categories
  inverts<-c('Abalones, winkles, conchs','Clams, cockles, arkshells','Crabs, sea-spiders','Lobsters, spiny-rock lobsters','Mussels',
    'Oysters','Scallops, pectens','Sea-urchins and other echinoderms','Shrimps, prawns','Miscellaneous marine crustaceans',
    'Squids, cuttlefishes, octopuses','King crabs, squat-lobsters','Miscellaneous aquatic invertebrates',
    'Horseshoe crabs and other arachnoids')
  
  ### Find unique stocks and add in Fishbase data where available
  
  # Unique stocks
  FFstocks<-unique(DataU[,c('IdOrig','Country','Dbase','SciName','CommName','SpeciesCatName','RegionFAO')])

  # Create column to indicate nearshore status
  FFstocks$Nearshore<-NA
  
  # join fishbase habitat data with stock list
  FFstocks<-left_join(FFstocks,Fishbase[,c('SciName','Habitat','DepthRangeMax')],by='SciName')
  
  # classify non-fish ISSCAAP categories as nearshore
  FFstocks$Nearshore[FFstocks$SpeciesCatName %in% c('Abalones, winkles, conchs','Clams, cockles, arkshells','Crabs, sea-spiders','Lobsters, spiny-rock lobsters','Mussels',
                                                    'Oysters','Scallops, pectens','Sea-urchins and other echinoderms','Shrimps, prawns','Miscellaneous marine crustaceans',
                                                    'Squids, cuttlefishes, octopuses','King crabs, squat-lobsters','Miscellaneous aquatic invertebrates',
                                                    'Horseshoe crabs and other arachnoids')] <- 'Nearshore'
  
  # Classify 'Miscellaneous coastal fishes' as nearshore
  FFstocks$Nearshore[FFstocks$SpeciesCatName %in% c('Miscellaneous coastal fishes','Miscellaneous diadromous fishes')]<-'Nearshore'
  

  # Classify stocks based on Fishbase habitat type
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$Habitat %in% c('reef-associated','pelagic-neritic')]<-'Nearshore'
  
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$Habitat %in% c('bathydemersal','bathypelagic','benthopelagic','pelagic-oceanic')]<-'Offshore'
  
  # For stocks identified as "demersal" on Fishbase, classify stocks as Nearshore based on a depth range of <=200
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$Habitat=='demersal' & FFstocks$DepthRangeMax<=DepthRangeMax]<-'Nearshore'
  
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$Habitat=='demersal' & FFstocks$DepthRangeMax>=DepthRangeMax]<-'Offshore'
  
  # Classify remaining
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$SpeciesCatName %in% c('Tunas, bonitos, billfishes','Sharks, rays, chimaeras','Miscellaneous pelagic fishes',
                                                                                'Miscellaneous demersal fishes', 'Cods, hakes, haddocks','Sturgeons, paddlefishes')]<-'Offshore'
  
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$SpeciesCatName %in% c('Flounders, halibuts, soles','Herrings, sardines, anchovies','Shads','Carps, barbels and other cyprinids',
                                                                                'Salmons, trouts, smelts')]<-'Nearshore'
  
  # Classify any stocks from RAM database as "Offshore"
  FFstocks$Nearshore[FFstocks$Dbase=='RAM']<-'Offshore'
  
  # Add nearshore classification to FF dataset
  DataU<-left_join(DataU,FFstocks[,c('IdOrig','Country','SciName','RegionFAO','Nearshore')],by=c('IdOrig','Country','SciName','RegionFAO'))
  
  ### Calculate upsides relative to Today --------------------------------------------------------------------------------
  
  # Percent upside function
  PercChange<- function(A,B)
  {
    PC<- ((A-B)/(B))*100*sign(B)
    
    PC[B<=0 & (A-B)>0]<- 999
    
    PC[B<=0 & (A-B)<=0]<- -999
    
    return(PC)
  }
  
  # Subsets to calculate upsides for
  sets<-c('All','Nearshore','Offshore')
  
  FinalData<-list()
  
  for(a in 1:length(sets))
  {
    
    # Calculate totals by country, policy, and year
    FFupsides<- DataU %>%
      group_by(Country,Policy,Year) %>%
      summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
    
    # Subset to nearshore when appropriate
    if(sets[a]=='Nearshore')
    {
      FFupsides<- DataU %>%
        filter(Nearshore=='Nearshore') %>%
        group_by(Country,Policy,Year) %>%
        summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                  TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
    }
    
    # Subset to offshore when appropriate
    if(sets[a]=='Offshore')
    {
      FFupsides<- DataU %>%
        filter(Nearshore=='Offshore') %>%
        group_by(Country,Policy,Year) %>%
        summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                  TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
    }
    
    # Subset baseline year 
    Base<-FFupsides %>%
      filter(Year==BaselineYear) %>%
      ungroup() %>%
      select(-TotalMSY,-Year,-Policy) %>%
      rename(BaselineBvBmsy=MedianBvBmsy,BaselineFvFmsy=MedianFvFmsy,BaselineCatch=TotalCatch,BaselineBiomass=TotalBiomass,BaselineProfits=TotalProfits)
    
    # Join upsides and baseline data
    FFupsides<-inner_join(FFupsides,Base,by=c('Country'))
    
    # Calculate abs upsides
    FFupsides$AbsChangeCatch<-FFupsides$TotalCatch-FFupsides$BaselineCatch
    FFupsides$AbsChangeBiomass<-FFupsides$TotalBiomass-FFupsides$BaselineBiomass
    FFupsides$AbsChangeProfits<-FFupsides$TotalProfits-FFupsides$BaselineProfits
    
    # Calculate perc upsides
    FFupsides$PercChangeCatch<-PercChange(FFupsides$TotalCatch,FFupsides$BaselineCatch)
    FFupsides$PercChangeBiomass<-PercChange(FFupsides$TotalBiomass,FFupsides$BaselineBiomass)
    FFupsides$PercChangeProfits<-PercChange(FFupsides$TotalProfits,FFupsides$BaselineProfits)
    
    ### Calculate upsides relative to BAU --------------------------------------------------------------------------------
    
    # Split into 'Conservation Concern' and 'All Stocks' scenarios
    CC<- subset(FFupsides,Policy %in% c('Catch Share Three','Fmsy Three'))
    
    CC$Scenario<-'Conservation Concern'
    
    CC$Policy[CC$Policy=='Catch Share Three']<-'RBFM'
    
    CC$Policy[CC$Policy=='Fmsy Three']<-'Fmsy'
    
    ALL<-subset(FFupsides,Policy %in% c('CatchShare','Fmsy'))
    
    ALL$Scenario<-'All Stocks'
    
    ALL$Policy[ALL$Policy=='CatchShare']<-'RBFM'
    
    # Subset respective BAU policies and add to scenarios
    CCBAU<-subset(FFupsides,Policy=='Business As Usual') %>%
      select(Country,Policy,Year,MedianBvBmsy,MedianFvFmsy,TotalCatch,TotalBiomass,TotalProfits) %>%
      rename(PolicyBAU=Policy,BvBmsyBAU=MedianBvBmsy,FvFmsyBAU=MedianFvFmsy,TotalCatchBAU=TotalCatch,TotalBiomassBAU=TotalBiomass,TotalProfitsBAU=TotalProfits)
    
    ALLBAU<-subset(FFupsides,Policy=='Business As Usual Pessimistic') %>%
      select(Country,Policy,Year,MedianBvBmsy,MedianFvFmsy,TotalCatch,TotalBiomass,TotalProfits) %>%
      rename(PolicyBAU=Policy,BvBmsyBAU=MedianBvBmsy,FvFmsyBAU=MedianFvFmsy,TotalCatchBAU=TotalCatch,TotalBiomassBAU=TotalBiomass,TotalProfitsBAU=TotalProfits)
    
    # Join BAU results with scenario upsides
    CC<-inner_join(CC,CCBAU,by=c('Country','Year'))
    
    ALL<-inner_join(ALL,ALLBAU,by=c('Country','Year'))
    
    Scenarios<-rbind(CC,ALL)
    
    # Calculate abs upsides from BAU
    Scenarios$AbsChangeCatchBAU<-Scenarios$TotalCatch-Scenarios$TotalCatchBAU
    Scenarios$AbsChangeBiomassBAU<-Scenarios$TotalBiomass-Scenarios$TotalBiomassBAU
    Scenarios$AbsChangeProfitsBAU<-Scenarios$TotalProfits-Scenarios$TotalProfitsBAU
    
    # Calculate perc upsides from BAU
    Scenarios$PercChangeCatchBAU<-PercChange(Scenarios$TotalCatch,Scenarios$TotalCatchBAU)
    Scenarios$PercChangeBiomassBAU<-PercChange(Scenarios$TotalBiomass,Scenarios$TotalBiomassBAU)
    Scenarios$PercChangeProfitsBAU<-PercChange(Scenarios$TotalProfits,Scenarios$TotalProfitsBAU)
    
    Scenarios$Subset<-sets[a]
    
    FinalData[[a]]<-Scenarios
    
    print(a)
  }
  
  FinalData<-bind_rows(FinalData)
  
  # write csv of profit upside by Country for all fisheries, nearshore, and offshore classifications
  NearImportance<-FinalData %>%
    tbl_df() %>%
    filter(Year==2050 & Policy=='RBFM' & Scenario=='Conservation Concern') %>%
    select(Country,Subset,AbsChangeProfits) %>%
    spread(Subset,AbsChangeProfits) 
  
  FinalData %>%
    tbl_df() %>%
    filter(Year==2050 & Policy=='RBFM' & Scenario=='Conservation Concern' & Subset=='All') %>%
    select(Country,Policy,AbsChangeProfits) %>%
    spread(Policy,AbsChangeProfits)
  
  write.csv(NearImportance,file=paste(ResultFolder,'Nearshore Importance.csv',sep=''))
  
  ### Plot upsides results for nearshore stocks --------------------------------------------------------------------------------
  
  BPtheme<-function(base_size=12)
  {
    theme(
      text=element_text(size=14),
      panel.background =  element_rect(fill = "white", colour = NA),
      panel.border =      element_rect(fill = NA, colour="grey50"), 
      panel.grid.major =  element_line(colour = "grey60", size = 0.2),
      panel.grid.minor =  element_line(colour = "grey78", size = 0.5)
    )
  }
  
  FinalData %>%
    tbl_df() %>%
    filter(Year==2050 & Policy=='RBFM' & Scenario=='All Stocks') %>%
    select(Country,Subset,AbsChangeProfits) %>%
    spread(Subset,AbsChangeProfits) %>%
    group_by(Country) %>%
    summarize(ProfitsRatio=Nearshore/Offshore) %>%
    ggplot(aes(x=ProfitsRatio)) +
    geom_density() +
    coord_cartesian(xlim=c(-100,100))

 p1A<-FinalData %>%
    tbl_df() %>%
    filter(Year==2050 & Policy=='RBFM' & Scenario=='All Stocks') %>%
    select(Country,Subset,TotalProfits) %>%
    spread(Subset,TotalProfits) %>%
    group_by(Country) %>%
    mutate(NearPerc=Nearshore/All,OffPerc=Offshore/All) %>%
    select(Country,NearPerc,OffPerc) %>%
    gather('Metric','Value',2:3) %>%
    filter(is.na(Value)==F) %>%
    ggplot(aes(x=Value,fill=Metric)) +
    geom_density(alpha=0.6) +
    scale_fill_discrete(labels=c('Nearshore','Offshore')) +
    labs(x='Percent of 2050 Profits',fill='Subset',title='Distribution 2050 Profits Obtained from Nearshore and Offshore\n Fisheries Across Countries') +
    BPtheme()
  
 p1B<-FinalData %>%
    tbl_df() %>%
    filter(Year==2050 & Policy=='RBFM' & Scenario=='All Stocks') %>%
    select(Country,Subset,TotalProfits) %>%
    spread(Subset,TotalProfits) %>%
    summarize(TotalAll=sum(All,na.rm=T),TotalNear=sum(Nearshore,na.rm=T),TotalOff=sum(Offshore,na.rm=T)) %>%
    mutate(Nearshore=TotalNear/TotalAll,Offshore=TotalOff/TotalAll) %>%
    select(Nearshore,Offshore) %>%
    gather('Metric','Value',1:2) %>%
    filter(is.na(Value)==F) %>%
    ggplot(aes(x=Metric,y=Value,fill=Metric)) +
    geom_bar(stat='identity',alpha=0.6) +
    scale_fill_discrete(labels=c('Nearshore','Offshore')) +
    labs(x='Subset',fill='Subset',y='Percent of Global Profits in 2050',title='Percent of Global Profits in 2050 Obtained\n from Nearshore and Offshore Fisheries') +
    BPtheme()
 
 pdf(paste(FigureFolder,'Nearshore Importance.pdf',sep=''))
 grid.arrange(p1A,p1B)
 dev.off()
 
 ### Calculate percent of Marine fishes nei for each country --------------------------------------------------------------------------------

 # Calculate total catch and percent of total catch from RawData for "Marine fishes not identified" and "Tunas, bonitos, billfishes" categories    
 IssueCats<-DataR %>%
   tbl_df() %>%
   filter(Dbase=='FAO' & Year==2012) %>%
   group_by(Country,SpeciesCatName) %>%
   summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
   group_by(Country) %>%
   mutate(AllCatch=sum(TotalCatch,na.rm=T)) %>%
   ungroup() %>%
   mutate(PercentOfTotal=100*(TotalCatch/AllCatch)) %>%
   filter(SpeciesCatName %in% c('Marine fishes not identified','Tunas, bonitos, billfishes') & AllCatch>0) %>%
   arrange(-AllCatch)
 
 # Write csv
 write.csv(IssueCats,paste(ResultFolder,'Catch Fractions for Unidentified and Tuna Categories.csv',sep=''))
   
   p2<-ggplot(IssueCats,aes(x=PercentOfTotal,fill=SpeciesCatName)) +
   geom_density(alpha=0.6) +
   scale_fill_discrete(labels=c('Marine fishes not identified','Tunas, bonitos, billfishes')) +
   labs(x='Percent of Total Catch for Country',fill='ISSCAAP') +
   BPtheme()
   
   pdf(paste(FigureFolder,'RawData Catch of Marine fishes nei and Tunas.pdf',sep=''))
   print(p2)
   dev.off()
   
   
   test<-DataU %>%
     tbl_df() %>%
     filter(Year==2012 & SpeciesCatName %in% inverts) %>%
     select(IdOrig,Country,CommName,SciName,SpeciesCatName,BvBmsy,FvFmsy,Catch,Profits,Biomass) %>%
     arrange(-Profits) %>%
     filter(Profits>10000000)
   
   pdf(paste(FigureFolder,'Density plot of Invert Stock Profits.pdf',sep=''))  
   print(ggplot(test,aes(x=Profits)) +
     geom_density() +
     coord_cartesian(xlim=c(-10000000:100000000)))
   dev.off()
  
   write.csv(test,paste(ResultFolder,'Important Nonfish Stocks for Nearshore Classification.csv',sep=''))
    
   return(FinalData)
  
}