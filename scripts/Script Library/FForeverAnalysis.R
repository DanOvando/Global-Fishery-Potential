##################################################################---------------------------------------------------------------------
#
# Upside Analysis for Rare Fish Forever Countries
#
# Tyler Clavelle
# 6/26/2015
#
# Code description: This code provides upside estimates using results from Costello et al. 2015 for Fish Forever Countries (Belize, 
# Brazil, Indonesia, Philippines, and Mozambique)
#
##################################################################---------------------------------------------------------------------

# FFCntrys<-c('Brazil','Belize','Philippines','Indonesia','Mozambique')
# Data<- UnlumpedProjectionData
# Compare<-UnlumpedUpsideAllStocks$CountryUpsides
# Compare<- Compare %>%
#   filter(Country %in% FFCntrys & Policy %in% c('CatchShare','Catch Share Three'))

FishForeverCntry<-function(Data,FFCntrys,BaselineYear)
{
#   library(plyr)
#   library(tidyr)
#   library(dplyr)
#   library(stringr)
  
  # Subset data to include Fish Forever Countries and relevant columns
  FF<- Data %>%
    tbl_df() %>%
    filter(Country %in% FFCntrys) %>%
    select(Dbase,IdOrig,Country,RegionFAO,SciName,CommName,SpeciesCatName,Year,Policy,BvBmsy,FvFmsy,MSY,Catch,Profits,Biomass,Price)
  
  # read in FishBase data
  Fishbase<-read.csv('Data/Fishbase_Environment.csv',stringsAsFactors=F)
  
  # find unique stocks
  FFstocks<-unique(FF[,c('Country','SciName','SpeciesCatName','Dbase','RegionFAO')])
  
  # create column for nearshore
  FFstocks$Nearshore<-NA
  
  # join fishbase habitat data with stock list
  FFstocks<-left_join(FFstocks,Fishbase[,c('SciName','Habitat','DepthRangeMax')],by='SciName')
  
  # classify non-fish ISSCAAP categories as nearshore
  FFstocks$Nearshore[FFstocks$SpeciesCatName %in% c('Abalones, winkles, conchs','Clams, cockles, arkshells','Crabs, sea-spiders','Lobsters, spiny-rock lobsters','Mussels',
                                                    'Oysters','Scallops, pectens','Sea-urchins and other echinoderms','Shrimps, prawns','Miscellaneous marine crustaceans',
                                                    'Squids, cuttlefishes, octopuses')] <- 'Nearshore'
  
  # Classify 'Miscellaneous coastal fishes' as nearshore
  FFstocks$Nearshore[FFstocks$SpeciesCatName %in% c('Miscellaneous coastal fishes','Miscellaneous diadromous fishes')]<-'Nearshore'
  
  
#   spp<-unique(FFstocks$SciName[grepl('spp',FFstocks$SciName)])
#   
#   for(a in 1:length(spp))
#   {
#     genus<-unlist(str_split(spp[a],pattern=' '))[1]
#     
#     # stocks<-Spec_ISSCAAP[grepl(genus,Spec_ISSCAAP$Species_AFSIS),]
#     
#     temp<-Fishbase[grepl(genus,Fishbase$Genus),c('Genus','Habitat')] %>%
#       group_by(Habitat) %>%
#       summarize(Count=length(Genus))
#     
#     class<-temp$Habitat[temp$Count==max(temp$Count)]
#     
#     if(length(class)>0){FFstocks$Habitat[FFstocks$SciName==spp[a]]<-class}
#   }
  
  # Classify stocks based on Fishbase habitat type
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$Habitat %in% c('reef-associated','pelagic-neritic','demersal')]<-'Nearshore'
  
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$Habitat %in% c('bathydemersal','bathypelagic','benthopelagic','pelagic-oceanic')]<-'Offshore'
  
  # Classify remaining
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$SpeciesCatName %in% c('Tunas, bonitos, billfishes','Sharks, rays, chimaeras','Miscellaneous pelagic fishes',
                                                                               'Miscellaneous demersal fishes', 'Cods, hakes, haddocks')]<-'Offshore'
  
  FFstocks$Nearshore[is.na(FFstocks$Nearshore) & FFstocks$SpeciesCatName %in% c('Flounders, halibuts, soles','Herrings, sardines, anchovies','Shads')]<-'Nearshore'
  
  # Classify any stocks from RAM database as "Offshore"
  FFstocks$Nearshore[FFstocks$Dbase=='RAM']<-'Offshore'
  
  # Add nearshore classification to FF dataset
  FF<-left_join(FF,FFstocks[,c('Country','SciName','RegionFAO','Nearshore')],by=c('Country','SciName','RegionFAO'))
  
  # Generate percent upside function
  PercChange<- function(A,B)
  {
    PC<- ((A-B)/(B))*100*sign(B)
    
    PC[B<=0 & (A-B)>0]<- 999
    
    PC[B<=0 & (A-B)<=0]<- -999
    
    return(PC)
  }
  
  ### Calculate upsides relative to Today --------------------------------------------------------------------------------
  
  sets<-c('All Fisheries','Nearshore','Offshore')
  
  FinalData<-list()
  
  for(a in 1:length(sets))
  {
  
  # Calculate totals by country, policy, and year
  FFupsides<- FF %>%
    group_by(Country,Policy,Year) %>%
    summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
              TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
  
  # Subset to nearshore when appropriate
  if(sets[a]=='Nearshore')
  {
    FFupsides<- FF %>%
      filter(Nearshore=='Nearshore') %>%
      group_by(Country,Policy,Year) %>%
      summarize(Fisheries=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                TotalCatch=sum(Catch,na.rm=T), TotalBiomass=sum(Biomass,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MeanPrice=mean(Price,na.rm=T))
  }
  
  # Subset to offshore when appropriate
  if(sets[a]=='Offshore')
  {
    FFupsides<- FF %>%
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
  
  }
  
  FinalData<-ldply(FinalData)
  
  write.csv(FinalData,file=paste(ResultFolder,'FF Country Upside Full Data.csv',sep=''))
  
  ### Create any desired data subsets --------------------------------------------------------------------------------
  
  rbfm<- FinalData %>%
    filter(Policy=='RBFM' & Year==2020 & Subset!='Offshore') %>%
    select(Country,Policy,Scenario,Subset,Year,Fisheries.x,TotalMSY,MeanPrice.x,TotalCatch,BaselineCatch,TotalCatchBAU,TotalProfits,BaselineProfits,TotalProfitsBAU,
           AbsChangeProfits,PercChangeProfits,AbsChangeProfitsBAU,PercChangeProfitsBAU) %>%
    group_by(Country,Policy,Scenario,Subset,Year) 
  
  ### Plot --------------------------------------------------------------------------------

  plotdata<- FinalData %>%
    filter(Year==2050 & Policy=='RBFM' & Scenario=='All Stocks')
  
  write.csv(plotdata,file=paste(ResultFolder,'FF Country Upside Data.csv',sep=''))
  
  plotdata %>%
    filter(Subset=='All Fisheries') %>%
    ggplot(aes(x=PercChangeBiomass,y=PercChangeProfits,size=TotalMSY,color=Country)) +
    geom_point() +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    facet_wrap(~Scenario)
  
  Scenarios %>%
    filter(Policy=='') %>%
    ggplot(aes(x=AbsChangeCatch,y=AbsChangeProfits,size=AbsChangeBiomass,color=Country)) +
    geom_point() +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    facet_wrap(Scenario~Policy)
  
  
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
   
    
  )
  
pdf(file=paste(FigureFolder,'Abs_Change_Profit_FF.pdf',sep=''),height=8,width=8)
print(
  plotdata %>%
    # filter(Scenario=='All Stocks') %>%
    ggplot(aes(x=Country,y=AbsChangeProfits/1000000000,fill=Subset)) +
    geom_bar(stat='identity',position='dodge') +
    scale_fill_brewer(palette = "Greens") +
    labs(y='Absolute Change in Annual Profit ($ Billion)') +
    BPtheme()
)
dev.off()

pdf(file=paste(FigureFolder,'Percent_Change_Profit_FF.pdf',sep=''),height=8,width=8)
print(  
  plotdata %>%
    # filter(Scenario=='All Stocks') %>%
    ggplot(aes(x=Country,y=PercChangeProfits,fill=Subset)) +
    geom_bar(stat='identity',position='dodge') +
    coord_cartesian(ylim=c(0,750)) +
    scale_fill_brewer(palette = "Greens") +
    labs(y='Percent Change in Annual Profit') +
    BPtheme()
)
dev.off()

pdf(file=paste(FigureFolder,'Abs_Change_Biomass_FF.pdf',sep=''),height=8,width=8)
print(  
  plotdata %>%
    # filter(Scenario=='All Stocks') %>%
    ggplot(aes(x=Country,y=AbsChangeBiomass/1000000,fill=Subset)) +
    geom_bar(stat='identity',position='dodge') +
    scale_fill_brewer(palette = "Blues") +
    labs(y='Absolute Change in Biomass (Millions, MT)') +
    BPtheme()
)
dev.off()

pdf(file=paste(FigureFolder,'Percent_Change_Biomass_FF.pdf',sep=''),height=8,width=8)
print(  
  plotdata %>%
    # filter(Scenario=='All Stocks') %>%
    ggplot(aes(x=Country,y=PercChangeBiomass,fill=Subset)) +
    geom_bar(stat='identity',position='dodge') +
    coord_cartesian(ylim=c(0,200)) +
    scale_fill_brewer(palette = "Blues") +
    labs(y='Percent Change in Biomass') +
    BPtheme()
)
dev.off()
  
}



pdf(file=paste(FigureFolder,'Upsides_ByCountry_FF.pdf',sep=''),height=8,width=8)

for(b in 1:length(FFCntrys))
{
 Temp<- plotdata %>%
  filter(Country==FFCntrys[b]) %>%
   select(Country,Subset,AbsChangeProfits,PercChangeProfits,AbsChangeBiomass,PercChangeBiomass) %>%
  gather('Metric','Value',3:6)
 
 print(
 ggplot(Temp,aes(x=Country,y=Value,fill=Subset)) +
   geom_bar(stat='identity',position = 'dodge') +
   facet_wrap(~Metric,scales='free') +
   labs(title=paste('Upside Metrics for ',FFCntrys[b],sep=''))
 )
}
dev.off()


