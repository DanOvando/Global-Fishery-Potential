######################################################
##
## Function to, for each NEI stock, find comparable species level  
## stocks and apply the median values of interest to the NEI stock
##
######################################################

NearestNeighborNeis<- function(BiomassData,MsyData,ProjData,BaselineYear)
{
  
  
  data(fishbase)  
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
  
  #Pull out NEI fisheries
  NEIs<-MsyData[MsyData$Dbase!='RAM' & MsyData$RanCatchMSY==F & ((grepl("nei",MsyData$CommName,ignore.case=T)) | (grepl("nei",MsyData$CommName,ignore.case=T) & (is.infinite(MsyData$BvBmsy)==T | MsyData$BvBmsy==999)) | (grepl("spp",MsyData$SciName) & grepl("not identified",MsyData$SpeciesCatName) & MsyData$Dbase=="FAO")),]
  
  FinalYear<- ddply(NEIs,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T))
  
  FinalProjYear<- ddply(ProjData,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T))
  
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
  
  ExtendFAO<- T
  
  ExtendResults <- (mclapply(1:(length(Stocks)), ExtendTimeSeries,mc.cores=NumCPUs,NEIs,BaselineYear,ExtendFAO=ExtendFAO))      
  
  BaselineYear<- OrigBaselineYear
  
  
  
  
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
  
  allstocks<-ddply(SpeciesLevel,c('Year','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
                   FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(g,na.rm=T),MedianK=median(k,na.rm=T),
                   MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
  
  # Find comparison stocks --------------------------
  
  VarsToFill<-c("BvBmsy","FvFmsy", "g", "k","Price","MarginalCost")
  
  #   NeiStats<-unique(NEIs[c("CommName", "SciName","RegionFAO","SpeciesCatName")]) # find unique combinations of nei stocks  
  NeiStats<-unique(NEIs[c("CommName", "SciName","SpeciesCatName")]) # find unique combinations of nei stocks  
  
  NeiStats$TaxonLevel<-NA
  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==T]<-"Genus"  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==F]<-"Non-Genus"
  
  nei_stock<-unique(NeiStats$SciName)
  
  
  if(NumCPUs>=1)
  {
    tempNEIs<-(mclapply(1:length(nei_stock),SnowNEIs2,nei_stock=nei_stock,NEIs=NEIs,SpeciesLevel=SpeciesLevel,NeiStats=NeiStats,Spec_ISSCAAP=Spec_ISSCAAP,
                        VarsToFill=VarsToFill,NumCPUs=NumCPUs))
  }
  
  show('Completed NEI Stats mclapply')
  
  NEIs<-ldply(tempNEIs,data.frame)
  
  show("Completed NEI ldply")
  
  #   
  #   if(NumCPUs>1)
  #   {
  #     tempNEIs<-(mclapply(1:length(nei_stock),SnowNEIs,nei_stock=nei_stock,NEIs=NEIs,SpeciesLevel=SpeciesLevel,NeiStats=NeiStats,allstocks=allstocks,Spec_ISSCAAP=Spec_ISSCAAP,
  #                     VarsToFill=VarsToFill,NumCPUs=NumCPUs))
  #   }
  # 
  # show('Completed NEI Stats mclapply')
  # 
  #   NEIs<-ldply(tempNEIs,data.frame)
  # 
  # show("Completed NEI ldply")
  
  #   JStocks<-NA# fill this vector with the number of Jstocks for each nei group 
  #   VarBvBmsy<-NA # fill this vector with the variance in BvBmsy for each nei group 
  #   VarFvFmsy<-NA # fill this vector with the variance in FvFmsy for each nei group
  #   Year<-NA
  #   for(a in 1:nrow(NeiStats))
  #   {
  #     
  #     rm(compstocks)
  #     
  #     
  #     show(paste( round(100*(a/nrow(NeiStats))), '% Done with NeiStats',sep=''))
  #     
  #     if(NeiStats$TaxonLevel[a]=="Genus") # find scientific names for all genus level nei stocks
  #     {
  #       
  #       Genus<-unlist(str_split(NeiStats$SciName[a],pattern=" "))[1] # pull out genus
  #       
  #       WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # search for species names in that genus
  #       
  #       compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp]) # pull out species names
  #       
  #     } 
  #     
  #     if(NeiStats$TaxonLevel[a]=="Non-Genus" & (NeiStats$SciName[a] %in% Spec_ISSCAAP$Family)) # determine if non-genus stock is a family name
  #     {  
  #       family<-NeiStats$SciName[a] # if so find species within that family
  #       
  #       WhereComp<-Spec_ISSCAAP$Family==family
  #       
  #       compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp])
  #     }
  #     
  #     if(NeiStats$TaxonLevel[a]=="Non-Genus" & (tolower(NeiStats$SciName[a]) %in% tolower(Spec_ISSCAAP$Order))) # determine if non-genus stock is an order name
  #     {  
  #       
  #       order<-toupper(NeiStats$SciName[a]) # order of nei stock (translate to uppercase to match sheet)
  #       
  #       WhereComp<-Spec_ISSCAAP$Order==order
  #       
  #       compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp])
  #     }
  #     
  #     if(exists('compstocks'))
  #     {
  #       
  #       ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks,] # pulling comparable stocks from the globe
  #       #       ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks  &
  #       #                                        grepl((NeiStats$RegionFAO[a]),SpeciesLevel$RegionFAO ) & is.na(SpeciesLevel$RegionFAO)==F,]
  #       #       
  #       if(nrow(ComparisonStocks)>0)
  #       {
  #         
  #         for (p in 1:length(LongPols))
  #         {
  #           results<-ddply(ComparisonStocks[ComparisonStocks$Policy==LongPols[p],],c("Year"),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),
  #                          MedianR=median(g,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)),VarBvBmsy=var(BvBmsy,na.rm=T),VarFvFmsy=var(FvFmsy,na.rm=T))
  #           
  #           Year<-append(Year,results$Year,after=length(Year))
  #           JStocks<-append(JStocks,results$JStocks,after=length(JStocks))
  #           VarBvBmsy<-append(VarBvBmsy,results$VarBvBmsy,after=length(VarBvBmsy))
  #           VarFvFmsy<-append(VarFvFmsy,results$VarFvFmsy,after=length(VarFvFmsy))
  #           
  #           for (b in 1:nrow(results))
  #           {
  #             #             WhereNei<- NEIs$SciName==NeiStats$SciName[a] & grepl((NeiStats$RegionFAO[a]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(NEIs$RegionFAO)==F & NEIs$Policy==LongPols[p]
  #             WhereNei<- NEIs$SciName==NeiStats$SciName[a] & NEIs$Year==results$Year[b] & NEIs$Policy==LongPols[p]            
  #             NEIs[WhereNei,VarsToFill]<-results[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
  #             NEIs$CanProject[WhereNei]<- TRUE
  #             
  #           } 
  #         } # Close Policy loop
  #         #         JStocks[a]<- (unique(results$JStocks))
  #       } # Close ComparisonStocks if
  #     } # Close compstocks if
  #   } # Close NeiStats loop
  #   # Make data frame from JStocks, VarBvBmsy, and FvFmsy
  #   
  #   NeiDiagnostics<-data.frame(cbind(Year,JStocks,VarBvBmsy,VarFvFmsy))
  
  NEIs<- NEIs[NEIs$CanProject==T,]
  
  Stocks<- unique(NEIs$IdOrig)
  
  for (p in 1:length(LongPols))
  {
    for(s in 1:length(Stocks))
    {
      
      Where<- NEIs$IdOrig==Stocks[s] & NEIs$Policy==LongPols[p]
      #       Where<- NEIs$IdOrig==Stocks[s] & NEIs$Policy=='CatchShare'
      
      WhereBase<- NEIs$IdOrig==Stocks[s]  & NEIs$Policy=='Historic' & NEIs$Year==BaselineYear
      
      #       WhereHistoric<- NEIs$IdOrig==Stocks[s]  & NEIs$Policy=='Historic' 
      
      msy<-NEIs$Catch[WhereBase]/(NEIs$BvBmsy[WhereBase]*NEIs$FvFmsy[WhereBase])
      
      NEIs$MSY[Where]<- msy
      
      #       NEIs$MSY[WhereHistoric]<- msy[1]
      
      NEIs$Catch[Where]<- NEIs$MSY[Where]*(NEIs$BvBmsy[Where]*NEIs$FvFmsy[Where])
      
      #       c_num <-  NEIs$Price[Where]*(2-NEIs$BvBmsyOpenAccess[Where])*NEIs$BvBmsyOpenAccess[Where]*NEIs$MSY[Where]*2^beta
      #       
      #       c_den = ((2-NEIs$BvBmsyOpenAccess[Where])*NEIs$g[Where])^beta
      
      BOA<- NEIs$BvBmsyOpenAccess
      
      phi<- NEIs$phi
      
      FOA<- (((phi+1)/phi)*(1-BOA^phi/(phi+1)))
      
      c_num <-  NEIs$Price*FOA*BOA*NEIs$MSY
      
      c_den = (NEIs$g*FOA)^beta
      
      cost = c_num/c_den
      
      cost<- cost[Where]
      
      NEIs$MarginalCost[Where]<- cost
      
      #       NEIs$MarginalCost[WhereHistoric]<- cost[1]
      
      NEIs$Profits[Where]<- NEIs$Price[Where]*NEIs$MSY[Where]*(NEIs$BvBmsy[Where]*NEIs$FvFmsy[Where])-NEIs$MarginalCost[Where]*(NEIs$FvFmsy[Where]*NEIs$g[Where])^beta
      
      #       show(s)
    } # close stock loop
    #  show(p)
  } # close policy loop
  
  Biomass<- NEIs[NEIs$Policy=='Historic',colnames(NEIs) %in% colnames(BiomassData)]
  
  Biomass$BvBmsy<- log(Biomass$BvBmsy)
  
  return(list(ProjNeis=NEIs,BiomassNeis=Biomass))
} # close function
