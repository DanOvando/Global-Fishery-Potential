######################################################
##
## Function to, for each NEI stock, find comparable species level  
## stocks and apply the median values of interest to the NEI stock
##
######################################################

NearestNeighborNeis<- function(BiomassData,MsyData,ProjData,BaselineYear)
{
  
  #     Data<- MsyData
  
  #         ProjData<- ProjectionData
  
  #   MsyData<- RamMsy
  
  #   ProjData<- RamProj
  
  #   BaselineYear<- 2011
  #   
  data(fishbase)  
  
  #Pull out NEI fisheries
  NEIs<-MsyData[MsyData$Dbase!='RAM' & MsyData$RanCatchMSY==F & ((grepl("nei",MsyData$CommName,ignore.case=T)) | (grepl("nei",MsyData$CommName,ignore.case=T) & (is.infinite(MsyData$BvBmsy)==T | MsyData$BvBmsy==999)) | (grepl("spp",MsyData$SciName) & grepl("not identified",MsyData$SpeciesCatName) & MsyData$Dbase=="FAO")),]
  
  FinalYear<- ddply(NEIs,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T))
  
  FinalProjYear<- ddply(ProjectionData,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T))
  
  DropItProj<- FinalProjYear$IdOrig[FinalProjYear$MaxYear<BaselineYear]
  
  DropIt<- FinalYear$IdOrig[FinalYear$MaxYear<(BaselineYear)]
  
  # Prepare NEI data for nearest neighbot analysis --------------------------
  
  NEIs<- NEIs[(NEIs$IdOrig %in% DropIt)==F,]
  
  NEIs$MarginalCost<- NA
  
  NEIs$Policy<- NA
  
  NEIs$Profits= NA
  
  ShortNEIs<- NEIs
  
  ShortNEIs$Policy<- 'Historic'
  
  
  sfInit( parallel=Parel, cpus=NumCPUs,slaveOutfile="NeiExtendTimeSeriesProgress.txt" )
  
  BaselineYear<- max(ProjData$Year,na.rm=T)
  
  Data<- NEIs
  sfExport('Data','BaselineYear')
  
  Stocks<- (unique(NEIs$IdOrig))
  ExtendResults <- (sfClusterApplyLB(1:(length(Stocks)), ExtendTimeSeries))      
  sfStop()
  rm(Data)
  
  NEIs <- ldply (ExtendResults, data.frame)
  
  #   NEIs<- ExtendTimeSeries(NEIs,max(ProjData$Year))
  
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
  
  SpeciesLevel<-ProjData[!(ProjData$IdOrig %in% unique(NEIs$IdOrig)) & !(ProjData$IdOrig %in% DropItProj),] 
  
  # Find comparison stocks --------------------------
  
  VarsToFill<-c("BvBmsy","FvFmsy", "r", "k","Price","MarginalCost")
  
  NeiStats<-unique(NEIs[c("CommName", "SciName","RegionFAO","SpeciesCatName")]) # find unique combinations of nei stocks  
  NeiStats$TaxonLevel<-NA
  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==T]<-"Genus"  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==F]<-"Non-Genus"
  
  for(a in 1:nrow(NeiStats))
  {
    
    rm(compstocks)
    
    show(paste( round(100*(a/nrow(NeiStats))), '% Done with NeiStats',sep=''))
    
    if(NeiStats$TaxonLevel[a]=="Genus") # find scientific names for all genus level nei stocks
    {
      
      Genus<-unlist(str_split(NeiStats$SciName[a],pattern=" "))[1] # pull out genus
      
      WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # search for species names in that genus
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp]) # pull out species names
      
    } 
    
    if(NeiStats$TaxonLevel[a]=="Non-Genus" & (NeiStats$SciName[a] %in% Spec_ISSCAAP$Family)) # determine if non-genus stock is a family name
    {  
      family<-NeiStats$SciName[a] # if so find species within that family
      
      WhereComp<-Spec_ISSCAAP$Family==family
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp])
    }
    
    if(NeiStats$TaxonLevel[a]=="Non-Genus" & (tolower(NeiStats$SciName[a]) %in% tolower(Spec_ISSCAAP$Order))) # determine if non-genus stock is an order name
    {  
      
      order<-toupper(NeiStats$SciName[a]) # order of nei stock (translate to uppercase to match sheet)
      
      WhereComp<-Spec_ISSCAAP$Order==order
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp])
    }
    
    if(exists('compstocks'))
    {
      ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks  &
                                       grepl((NeiStats$RegionFAO[a]),SpeciesLevel$RegionFAO ) & is.na(SpeciesLevel$RegionFAO)==F,]
      
      if(nrow(ComparisonStocks)>0)
      {
        
        for (p in 1:length(LongPols))
        {
          results<-ddply(ComparisonStocks[ComparisonStocks$Policy==LongPols[p],],c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                         MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
          
          for (b in 1:nrow(results))
          {
            WhereNei<- NEIs$SciName==NeiStats$SciName[a] & grepl((NeiStats$RegionFAO[a]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(NEIs$RegionFAO)==F & NEIs$Policy==LongPols[p]
            
            NEIs[WhereNei,VarsToFill]<-results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")]
            NEIs$CanProject[WhereNei]<- TRUE
          } 
        } # Close Policy loop
      } # Close ComparisonStocks if
    } # Close compstocks if
  } # Close NeiStats loop
  
  Biomass<- NEIs[NEIs$Policy=='Historic',colnames(NEIs) %in% colnames(BiomassData)]
  
  Biomass$BvBmsy<- log(Biomass$BvBmsy)
  
  return(list(ProjNeis=NEIs,BiomassNeis=Biomass))
} # close function
