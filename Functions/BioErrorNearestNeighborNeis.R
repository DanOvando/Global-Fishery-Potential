######################################################
##
## Function to, for each NEI stock, find comparable species level  
## stocks and apply the median values of interest to the NEI stock
##
######################################################

BioErrorNearestNeighborNeis<- function(BiomassData,MsyData,ProjData,Projected,BaselineYear)
{
  require(rfishbase,quiet=T)
  require(stringr,quiet=T)
  data(fishbase)  
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
  
  #Pull out NEI fisheries
  NEIs<-ProjData[ProjData$Dbase!='RAM' &  ProjData$Year==BaselineYear & ((grepl("nei",ProjData$CommName,ignore.case=T)
                                          | grepl("nei",ProjData$CommName,ignore.case=T))
                                             | (grepl("spp",ProjData$SciName) & grepl("not identified",ProjData$SpeciesCatName) 
                                                & ProjData$Dbase=="FAO")),]
  
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
  
  ExtendResults <- (lapply(1:(length(Stocks)), ExtendTimeSeries,NEIs,BaselineYear,ExtendFAO=ExtendFAO))      
  
  BaselineYear<- OrigBaselineYear
  
  
  
  
  NEIs <- bind_rows(ExtendResults)
  
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
  
  SpeciesLevel<-Projected[!(Projected$IdOrig %in% unique(NEIs$IdOrig)) & !(Projected$IdOrig %in% DropItProj),] 
  
  SpeciesLevel$k<- (SpeciesLevel$MSY*((SpeciesLevel$phi+1)^(1/SpeciesLevel$phi)))/SpeciesLevel$g
  
  SpeciesLevel$MarginalCost<- SpeciesLevel$cost
  
  allstocks<-ddply(SpeciesLevel,c('Year','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
                   FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(g,na.rm=T),MedianK=median(k,na.rm=T),
                   MedianPrice=median(Price,na.rm=T),MedianCost=median(cost,na.rm=T),JStocks=length(unique(IdOrig)))
  
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
    
    tempNEIs<-(lapply(1:length(nei_stock),SnowNEIs2,nei_stock=nei_stock,NEIs=NEIs,
                        SpeciesLevel=SpeciesLevel,NeiStats=NeiStats,Spec_ISSCAAP=Spec_ISSCAAP,
                        VarsToFill=VarsToFill,NumCPUs=NumCPUs))
  }
  
  show('Completed NEI Stats mclapply')
  
  NEIs<-bind_rows(tempNEIs)
  
  show("Completed NEI ldply")
  
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
      
      NEIs$Catch[Where]<- NEIs$MSY[Where]*(NEIs$BvBmsy[Where]*NEIs$FvFmsy[Where])

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
      
            show(s)
    } # close stock loop
    #  show(p)
  } # close policy loop
  
  
  return(list(ProjNeis=NEIs,BiomassNeis=Biomass))
} # close function
