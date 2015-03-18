##############################################
##
## Snow NEIs 2.0 using original methods
##
##############################################

SnowNEIs2<-function(s,nei_stock,NEIs,SpeciesLevel,NeiStats,Spec_ISSCAAP,VarsToFill,NumCPUs)
{
  
  write.table((paste(round(100*(s/length(nei_stock)),2),'% done with SnowNEIs2',sep='')), 
              file = 'SnowNEIs Progress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)  
  
  NeiCat<-(nei_stock[s])
  
  # isolate NeiStats data for NEI type
  stats<-NeiStats[NeiStats$SciName==NeiCat,]
  
  # Subset out NEIs to fill with results
  neis<-NEIs[NEIs$SciName==NeiCat,]
  
  # Subset SpeciesLevel from which to draw results
  species<-SpeciesLevel[SpeciesLevel$SpeciesCatName %in% neis$SpeciesCatName,]
  
  # Find row with taxon information in species AFSIS
  WhereNEI<-match(NeiCat,Spec_ISSCAAP$Species_AFSIS) 
  
  JStocks<-NA# fill this vector with the number of Jstocks for each nei group 
  VarBvBmsy<-NA # fill this vector with the variance in BvBmsy for each nei group 
  VarFvFmsy<-NA # fill this vector with the variance in FvFmsy for each nei group
  Year<-NA    
    
    if(unique(stats$TaxonLevel)=="Genus") # find scientific names for all genus level nei stocks
    {
      
      Genus<-unlist(str_split(NeiCat,pattern=" "))[1] # pull out genus
      
      WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # search for species names in that genus
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp]) # pull out species names
      
    } 
    
    if(unique(stats$TaxonLevel)=="Non-Genus" & (NeiCat %in% Spec_ISSCAAP$Family)) # determine if non-genus stock is a family name
    {  
      family<-NeiCat # if so find species within that family
      
      WhereComp<-Spec_ISSCAAP$Family==family
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp])
    }
    
    if(unique(stats$TaxonLevel)=="Non-Genus" & (tolower(NeiCat) %in% tolower(Spec_ISSCAAP$Order))) # determine if non-genus stock is an order name
    {  
      
      order<-toupper(NeiCat) # order of nei stock (translate to uppercase to match sheet)
      
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
          results<-ddply(ComparisonStocks,c("Year",'Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),
                         MedianG=median(g,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)),VarBvBmsy=var(BvBmsy,na.rm=T),VarFvFmsy=var(FvFmsy,na.rm=T))
          
          if(length(unique(results$Policy))>1)
          {
            for (b in 1:nrow(results))
            {
              #             WhereNei<- NEIs$SciName==NeiStats$SciName[a] & grepl((NeiStats$RegionFAO[a]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(NEIs$RegionFAO)==F & NEIs$Policy==LongPols[p]
              WhereNei<- neis$SciName==NeiCat & neis$Year==results$Year[b] & neis$Policy==results$Policy[b]            
              neis[WhereNei,VarsToFill]<-results[b,c("BvBmsy25", "FvFmsy75", "MedianG", "MedianK","MedianPrice", "MedianCost")]
              neis$CanProject[WhereNei]<- TRUE
              
            } 
          }

          if(length(unique(results$Policy))==1)
          {
            for (b in 1:nrow(results))
            {
              #             WhereNei<- NEIs$SciName==NeiStats$SciName[a] & grepl((NeiStats$RegionFAO[a]),NEIs$RegionFAO ) & NEIs$Year==results$Year[b]  & is.na(NEIs$RegionFAO)==F & NEIs$Policy==LongPols[p]
              WhereNei<- neis$SciName==NeiCat & neis$Year==results$Year[b] & neis$Policy==results$Policy[b]            
              neis[WhereNei,VarsToFill]<-results[b,c("BvBmsy25", "FvFmsy75", "MedianG", "MedianK","MedianPrice", "MedianCost")]
            } 
          }
          
          
      } # Close ComparisonStocks if
      
    } # Close compstocks if
  
  return(neis)
  
  } # Close NeiStats loop
  # Make data frame from JStocks, VarBvBmsy, and FvFmsy
  
#   NeiDiagnostics<-data.frame(cbind(Year,JStocks,VarBvBmsy,VarFvFmsy))
  
  