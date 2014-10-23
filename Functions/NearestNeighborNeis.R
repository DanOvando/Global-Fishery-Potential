######################################################
##
## Function to, for each NEI stock, find comparable species level  
## stocks and apply the median values of interest to the NEI stock
##
######################################################

NearestNeighborNeis<- function(BiomassData,MsyData,ProjData,BaselineYear)
{
  
  #     Data<- MsyData
  
#           ProjData<- ProjectionData
  
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
  
  OrigBaselineYear<- BaselineYear
  
  BaselineYear<- max(ProjData$Year,na.rm=T)
  
  Stocks<- (unique(NEIs$IdOrig))
  
  ExtendResults <- (mclapply(1:(length(Stocks)), ExtendTimeSeries,mc.cores=NumCPUs,NEIs,BaselineYear))      
  
  BaselineYear<- OrigBaselineYear
  
  
#   sfInit( parallel=Parel, cpus=NumCPUs,slaveOutfile="NeiExtendTimeSeriesProgress.txt" )
  
  
#   Data<- NEIs
#   sfExport('Data','BaselineYear')
  
#   ExtendResults <- (sfClusterApplyLB(1:(length(Stocks)), ExtendTimeSeries))      
#   sfStop()
#   rm(Data)
  
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

#   NeiStats<-unique(NEIs[c("CommName", "SciName","RegionFAO","SpeciesCatName")]) # find unique combinations of nei stocks  
  NeiStats<-unique(NEIs[c("CommName", "SciName","SpeciesCatName")]) # find unique combinations of nei stocks  


NeiStats$TaxonLevel<-NA
  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==T]<-"Genus"  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==F]<-"Non-Genus"
  
JStocks<-NA# fill this vector with the number of Jstocks for each nei group 
VarBvBmsy<-NA # fill this vector with the variance in BvBmsy for each nei group 
VarFvFmsy<-NA # fill this vector with the variance in FvFmsy for each nei group
Year<-NA

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
      
      ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks,] # pulling comparable stocks from the globe
                                         
#       ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks  &
#                                        grepl((NeiStats$RegionFAO[a]),SpeciesLevel$RegionFAO ) & is.na(SpeciesLevel$RegionFAO)==F,]
#       
      if(nrow(ComparisonStocks)>0)
      {
        
        for (p in 1:length(LongPols))
        {
          results<-ddply(ComparisonStocks[ComparisonStocks$Policy==LongPols[p],],c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                         MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)),VarBvBmsy=var(BvBmsy,na.rm=T),VarFvFmsy=var(FvFmsy,na.rm=T))
          
          Year<-append(Year,results$Year,after=length(Year))
          JStocks<-append(JStocks,results$JStocks,after=length(JStocks))
          VarBvBmsy<-append(VarBvBmsy,results$VarBvBmsy,after=length(VarBvBmsy))
          VarFvFmsy<-append(VarFvFmsy,results$VarFvFmsy,after=length(VarFvFmsy))
          
          for (b in 1:nrow(results))
          {
#             WhereNei<- NEIs$SciName==NeiStats$SciName[a] & grepl((NeiStats$RegionFAO[a]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(NEIs$RegionFAO)==F & NEIs$Policy==LongPols[p]
            WhereNei<- NEIs$SciName==NeiStats$SciName[a] & NEIs$Year==results$Year[b] & NEIs$Policy==LongPols[p]
            
            
            NEIs[WhereNei,VarsToFill]<-results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")]
            NEIs$CanProject[WhereNei]<- TRUE
           
          } 
        } # Close Policy loop
      
#         JStocks[a]<- (unique(results$JStocks))
        } # Close ComparisonStocks if
    } # Close compstocks if
  } # Close NeiStats loop

# Make data frame from JStocks, VarBvBmsy, and FvFmsy

NeiDiagnostics<-data.frame(cbind(Year,JStocks,VarBvBmsy,VarFvFmsy))

NEIs<- NEIs[NEIs$CanProject==T,]

Stocks<- unique(NEIs$IdOrig)

for (p in 1:length(Pols))
{
  for(s in 1:length(Stocks))
  {
    
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

  Biomass<- NEIs[NEIs$Policy=='Historic',colnames(NEIs) %in% colnames(BiomassData)]
  
  Biomass$BvBmsy<- log(Biomass$BvBmsy)
  
  return(list(ProjNeis=NEIs,BiomassNeis=Biomass,DiagnosticsNEIs=NeiDiagnostics))
} # close function
