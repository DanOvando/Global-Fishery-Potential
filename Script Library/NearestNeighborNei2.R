
###############

data(fishbase)  

#Pull out NEI fisheries
NEIs<-MsyData[MsyData$Dbase!='RAM' & MsyData$RanCatchMSY==F & ((grepl("nei",MsyData$CommName,ignore.case=T)) | (grepl("nei",MsyData$CommName,ignore.case=T) & (is.infinite(MsyData$BvBmsy)==T | MsyData$BvBmsy==999)) | (grepl("spp",MsyData$SciName) & grepl("not identified",MsyData$SpeciesCatName) & MsyData$Dbase=="FAO")),]


NeiIds<- unique(NEIs$IdOrig)

SampleIds<- sample(NeiIds,SubSample*length(NeiIds),replace=FALSE)
# # # 
NEIs<-  NEIs[! NEIs[,IdVar] %in% SampleIds,]


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

NeiStats<-unique(NEIs[c("CommName", "SciName","RegionFAO","SpeciesCatName")]) # find unique combinations of nei stocks  
NeiStats$TaxonLevel<-NA

NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==T]<-"Genus"  
NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==F]<-"Non-Genus"

for(a in 1:nrow(NeiStats))
{
  
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
  
  if(length(compstocks)>0)
  {
  ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks & SpeciesLevel$Year>(BaselineYear-1) &
                                   grepl((NeiStats$RegionFAO[a]),SpeciesLevel$RegionFAO ) & is.na(SpeciesLevel$RegionFAO)==F,]
  
  if(nrow(ComparisonStocks)>0)
  {
    
    for (p in 1:length(LongPols))
    {
      results<-ddply(ComparisonStocks[ComparisonStocks$Policy==LongPols[p],],c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                     MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
      
      for (b in 1:nrow(results))
      {
        WhereNei<- NEIs$SciName==NeiStats$SciName[a] & grepl((NeiStats$RegionFAO[a]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(NeiStats$RegionFAO)==F & NEIs$Policy==LongPols[p]
        
        NEIs[WhereNei,VarsToFill]<-results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")]
        NEIs$CanProject[WhereNei]<- TRUE
      } 
    } # Close Policy loop
    } # Close ComparisonStocks if
  } # Close compstocks if
} # Close NeiStats loop

