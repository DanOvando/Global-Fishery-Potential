######################################################
##
## Function to, for each NEI stock, find comparable species level  
## stocks and apply the median values of interest to the NEI stock
##
######################################################

NearestNeighborNeis<- function(BiomassData,MsyData,ProjData,BaselineYear)
{
  
#     Data<- MsyData
  
#     ProjData<- ProjectionData
  
#   MsyData<- RamMsy
  
#   ProjData<- RamProj
  
#   BaselineYear<- 2011
#   
  data(fishbase)  
   
  #Pull out NEI fisheries
  NEIs<-MsyData[MsyData$Dbase!='RAM' & MsyData$RanCatchMSY==F & ((grepl("nei",MsyData$CommName,ignore.case=T)) | (grepl("nei",MsyData$CommName,ignore.case=T) & (is.infinite(MsyData$BvBmsy)==T | MsyData$BvBmsy==999)) | (grepl("spp",MsyData$SciName) & grepl("not identified",MsyData$SpeciesCatName) & MsyData$Dbase=="FAO")),]
  
  FinalYear<- ddply(NEIs,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T))
  
  DropIt<- FinalYear$IdOrig[FinalYear$MaxYear<BaselineYear]
  
  
  # Prepare NEI data for nearest neighbot analysis --------------------------
  
  NEIs<- NEIs[(NEIs$IdOrig %in% DropIt)==F,]
  
  NEIs$MarginalCost<- NA
  
  NEIs$Policy<- NA
  
  NEIs$Profits= NA
  
  ShortNEIs<- NEIs
  
  ShortNEIs$Policy<- 'Historic'
  
  NEIs<- ExtendTimeSeries(NEIs,max(ProjData$Year))
  
  Pols<- unique(ProjData$Policy)
  
  LongPols<- Pols
  
  Pols<- Pols[Pols!='Historic']
  
  LongNeis<- NEIs
  
  LongNeis$Policy<- Pols[1]
  
  for (p in 2:length(Pols))
  {
    
    TempNeis<- NEIs
    
    TempNeis$Policy<- Pols[p]
    
    LongNeis<- rbind(LongNeis,TempNeis)
    
  }
  
  NEIs<- rbind(ShortNEIs,LongNeis)
  
  SpeciesLevel<-ProjData[!(ProjData$IdOrig %in% unique(NEIs$IdOrig)),] 
  
  # Find comparison stocks --------------------------
  
  VarsToFill<-c("BvBmsy","FvFmsy", "r", "k","Price","MarginalCost")
  
  NeiStats<-unique(NEIs[c("SciName","RegionFAO","SpeciesCatName")]) # find unique combinations of nei stocks  
  NeiStats$TaxonLevel<-NA
  
  NeiStats$SciName<-gsub("\\(.*\\)","",NeiStats$SciName) # delete anything within parentheses in SciName
  NeiStats$SciName<-gsub(",.*$","",NeiStats$SciName) # delete anything after a comma in SciName
  
  NeiSciNames<-unique(NeiStats$SciName)
    
  for (j in 1:length(NeiSciNames))
  {
    
    show(paste( round(100*(j/length(NeiSciNames))), '% Done with NeiSciNames',sep=''))
    
    where<- NeiStats$SciName==NeiSciNames[j]
    
    if(grepl("spp",as.character(NeiSciNames[j]))==T) 
    {NeiStats$TaxonLevel[where]<-"Genus"
     
    } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Genus"))))==2) 
    {NeiStats$TaxonLevel[where]<-"Genus"
     
    } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Family"))))==2) 
    {NeiStats$TaxonLevel[where]<-"Family"
     
    } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Class"))))==2) 
    {NeiStats$TaxonLevel[where]<-"Class"
     
    } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Order"))))==2) 
    {NeiStats$TaxonLevel[where]<-"Order"}
    
  } # close NeiSciNames loop
  
  # subset out non-fish nei stocks
  
  NonFishNeis<-c( "Miscellaneous marine molluscs"   ,    "Squids, cuttlefishes, octopuses"   ,  "Miscellaneous marine crustaceans" ,  
                  "Miscellaneous aquatic invertebrates" ,"Crabs, sea-spiders"   ,  "Sea-urchins and other echinoderms" , 
                  "Shrimps, prawns"      ,               "Lobsters, spiny-rock lobsters")
  
  NonFish<-NeiStats[(NeiStats$SpeciesCatName %in% NonFishNeis),]
    
  NeiStats<-NeiStats[is.na(NeiStats$TaxonLevel)==F,]
#   Rprof()
  for (m in 1:nrow(NeiStats))
  {
    
    show(paste( round(100*(m/nrow(NeiStats))), '% Done with NeiStats',sep=''))
    
    
    if(NeiStats$TaxonLevel[m]=="Genus")
    {
      Genus<-unlist(str_split(NeiStats$SciName[m],pattern=" "))[1]
      
      WhereFish<-which_fish(as.character(Genus),using=NeiStats$TaxonLevel[m]) # use passed data to find matches in fishbase data
      
    } else {WhereFish<-which_fish(as.character(NeiStats$SciName[m]),using=NeiStats$TaxonLevel[m])}
    
    SubsetNames<-fish_names(fish.data[WhereFish], name=c("ScientificName")) # create a vector of matched names
    
    ComparisonStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% SubsetNames) & SpeciesLevel$Year>(BaselineYear-1) &
                                     grepl((NeiStats$RegionFAO[m]),SpeciesLevel$RegionFAO ) & is.na(SpeciesLevel$RegionFAO)==F,]
    
    if(nrow(ComparisonStocks)>0) # consider changing this to instead be a requirement for a number of J comparison stocks
    {
      
      for (p in 1:length(LongPols))
      {
        results<-ddply(ComparisonStocks[ComparisonStocks$Policy==LongPols[p],],c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                       MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
        
        for (b in 1:nrow(results))
        {
          WhereNei<- NEIs$SciName==NeiStats$SciName[m] & grepl((NeiStats$RegionFAO[m]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(SpeciesLevel$RegionFAO)==F & NEIs$Policy==LongPols[p]
          
          NEIs[WhereNei,VarsToFill]<-results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")]
          NEIs$CanProject[WhereNei]<- TRUE
          
        }
      } #Close Policy Loop
    }
  } # close NeiStats loop
  
# Rprof(NULL)
#  RProfData<- readProfileData('Rprof.out')
#  flatProfile(RProfData,byTotal=TRUE)


  # repeat process of finding comparable stocks for NonFish  nei stocks
  
  for (m in 1:nrow(NonFish))
  {
    
    show(paste( round(100*(m/nrow(NonFish))), '% Done with NonFish',sep=''))
    
    NonFishCompStocks<-SpeciesLevel[SpeciesLevel$SpeciesCatName==NonFish$SpeciesCatName[m] &  grepl((NeiStats$RegionFAO[m]),SpeciesLevel$RegionFAO ) & is.na(SpeciesLevel$RegionFAO)==F,]
    
    if(nrow(NonFishCompStocks)>0)
    {
      for (p in 1:length(LongPols))
      {
        NonFishResults<-ddply(NonFishCompStocks[NonFishCompStocks$Policy==LongPols[p],],c("Year"),summarize, MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                              MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
        
        for (b in 1:nrow(NonFishResults))
        {
          WhereNei<- NEIs$SciName==NonFish$SciName[m] &  grepl((NonFish$RegionFAO[m]),NEIs$RegionFAO ) & is.na(NEIs$RegionFAO)==F & NEIs$Year==NonFishResults$Year[b] & NEIs$Policy==LongPols[p]
          
          NEIs[WhereNei,VarsToFill]<- results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")] 
          
          NEIs$CanProject[WhereNei]<- TRUE
        } # close results loop
      }
    } # close NonFish loop
  }
  # loop over NEIs and calculate MSY using catch in the BaselineYear and the median BvBmsy and FvFmsy values from above
  # then calculate projected catch using this MSY value and the same BvBmsy and FvFmsy values
  
  NEIs<- NEIs[NEIs$CanProject==T,]
  
  Stocks<- unique(NEIs$IdOrig)
  
  for (p in 1:length(Pols))
  {
    
    
    for( s in 1:length(Stocks))
    {
      show(paste( round(100*(p/length(Pols))), '% Done with Policy Loop-  ', round(100*(s/length(Stocks))), '% Done with Stocks in Policies Loop',sep=''))
      
      Where<- NEIs$IdOrig==Stocks[s] & NEIs$Policy==Pols[p]
      
      WhereBase<- NEIs$IdOrig==Stocks[s]  & NEIs$Policy=='Historic' & NEIs$Year==BaselineYear

      WhereHistoric<- NEIs$IdOrig==Stocks[s]  & NEIs$Policy=='Historic' 
      
      msy<-NEIs$Catch[WhereBase]/(NEIs$BvBmsy[WhereBase]*NEIs$FvFmsy[WhereBase])
      
      NEIs$MSY[Where]<- msy

      NEIs$MSY[WhereHistoric]<- msy[1]
      
      NEIs$Catch[Where]<- NEIs$MSY[Where]*(NEIs$BvBmsy[Where]*NEIs$FvFmsy[Where])
      
      c_num <-  NEIs$Price[Where]*(2-NEIs$BvBmsyOpenAccess[Where])*NEIs$BvBmsyOpenAccess[Where]*NEIs$MSY[Where]*2^beta
      
      c_den = ((2-NEIs$BvBmsyOpenAccess[Where])*NEIs$r[Where])^beta
      
      cost = c_num/c_den
      
      NEIs$MarginalCost[Where]<- cost

      NEIs$MarginalCost[WhereHistoric]<- cost[1]
      
      NEIs$Profits[Where]<- NEIs$Price[Where]*NEIs$MSY[Where]*(NEIs$BvBmsy[Where]*NEIs$FvFmsy[Where])-NEIs$MarginalCost[Where]*(NEIs$FvFmsy[Where]*NEIs$r[Where]/2)^beta
    
      } # close stock loop
  } # close policy loop
  
  Biomass<- NEIs[,colnames(NEIs) %in% colnames(BiomassData)]
  
  Biomass$BvBmsy<- log(Biomass$BvBmsy)
  
  return(list(ProjNeis=NEIs,BiomassNeis=Biomass))
} # close function
