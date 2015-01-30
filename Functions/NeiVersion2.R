#############################################
##
## Nearest Neighbor NEI v2.0
## Find median BvBmsy,FvFmsy, MSY, r, Price, and Marginal Cost
## of taxonomically comparable stocks. Run through projection analysis
##
##############################################

# VarsToFill<-c('BvBmsy','FvFmsy','MSY','r')
# Level<-'All'
# MinStocks<-2

NeiVersion2<-function(MsyData,Level,MinStocks)
{
  ### Read in data for taxonomy matching-------------------------------------
    
  # data(fishbase)  
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
  
  ### Separate out Species level and NEI fisheries from Biomass Data-------------------------------
  
  NEIs<-MsyData[MsyData$IdLevel %in% c('Neis','Unidentified'),]
  
  SpeciesLevel<-MsyData[MsyData$IdLevel=='Species',]
  
  # Drop NEI stocks that end before the baseline year
  
  # FinalYear<- ddply(NEIs,c('IdOrig'),summarize,MaxYear=max(Year,na.rm=T))
  # 
  # DropIt<- FinalYear$IdOrig[FinalYear$MaxYear<(BaselineYear)]
  # 
  # NEIs<- NEIs[(NEIs$IdOrig %in% DropIt)==F,]
  
  ### Prepare NEI data for nearest neighbor analysis --------------------------
  
  # Find unique combinations of nei stocks
  NeiStats<-unique(NEIs[c('Year',"CommName", "SciName","RegionFAO","SpeciesCatName")])   
  # NeiStats<-unique(NEIs[c("CommName", "SciName","SpeciesCatName")]) # find unique combinations of nei stocks  
  
  # Label as genus or non-genus level NEI
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==T]<-"Genus"  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==F]<-"Non-Genus"
  
  # Add columns to fill with data
  NeiStats$JStocks<-NA
  NeiStats$MedianBvBmsy<-NA # calculated as the sum of biomass of comp stocks divided by the sum of Bmsy
  NeiStats$MedianFvFmsy<-NA
  NeiStats$MedianMSY<-NA
  NeiStats$MedianR<-NA
  NeiStats$Level<-NA
  NeiStats$Region<-NA
  
  # Define variables to be filled for each NEI stock
  VarsToFill<-c('Year','IdOrig','BvBmsy','FvFmsy','MSY','r')
  
  # Create empty list to fill
  ResultsNEIs<-list()
  
  ### Loop over years and rows in NEI stats and find median values for 'VarsToFill'--------------------------------------- 
  
  yrs<-unique(NeiStats$Year)
  
  for(a in 1:length(yrs)) # loop over years
  {
    tempSpeciesLevel<-SpeciesLevel[SpeciesLevel$Year==yrs[a],] # subset to year
    
    tempNeiStats<-NeiStats[NeiStats$Year==yrs[a],] # subset to year
    
    show(paste( round(100*(a/length(yrs))), '% Done with NeiStats',sep=''))
    
  
    for(b in 1:nrow(tempNeiStats)) 
    {
      WhereComp<-NA
      compstocks<-NA
      CompStocks<-matrix(NA,nrow=0,ncol=length(VarsToFill))
      results<-NA
      level<-NA
      region<-NA
      
      # Genus if
      if(tempNeiStats$TaxonLevel[b]=='Genus' & ((Level=='Genus') | (Level=='All'))) # find scientific names for all genus level nei stocks
      {
        
        Genus<-unlist(str_split(tempNeiStats$SciName[b],pattern=" "))[1] # pull out genus
        
        WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # search for species names in that genus
        
        compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp]) # pull out species names
        
        if(exists('compstocks'))
        {
          CompStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% compstocks) & SpeciesLevel$RegionFAO==tempNeiStats$RegionFAO[b],c(VarsToFill)]
          
          region<-'FAO Region'
          
          if(nrow(CompStocks)<MinStocks)
          {
            CompStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% compstocks),c(VarsToFill)]
            
            region<-'Global'
          }
          
          level<-'Genus'
        }
      } # close genus if
      
      # Family if
      if((Level=='Family') | ((Level=='All')) & nrow(CompStocks)<MinStocks) # find scientific names for all genus level nei stocks
      {
        
        family<-unique(Spec_ISSCAAP$Family[WhereComp]) # find family name of that genus
        
        compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out species names in that family
        
        if(exists('compstocks'))
        {
          CompStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% compstocks) & SpeciesLevel$RegionFAO==tempNeiStats$RegionFAO[b],c(VarsToFill)]
          
          region<-'FAO Region'
          
          if(nrow(CompStocks)<MinStocks)
          {
            CompStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% compstocks),c(VarsToFill)]
            
            region<-'Global'
          }
          
          level<-'Family'
        }
      } # close Family if   
      
      # Order if
      if((Level=='Order') | ((Level=='All') & nrow(CompStocks)<MinStocks)) # determine if non-genus stock is a family name
      {  
        
        order<-unique(Spec_ISSCAAP$Order[WhereComp]) # find family name of that genus
        
        compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out species names in that family
        
        if(exists('compstocks'))
        {
          CompStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% compstocks) & SpeciesLevel$RegionFAO==tempNeiStats$RegionFAO[b],c(VarsToFill)]
          
          region<-'FAO Region'
          
          if(nrow(CompStocks)<MinStocks)
          {
            CompStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% compstocks),c(VarsToFill)]
            
            region<-'Global'
          }
          
          level<-'Order'
        }
      }# close Order if
      
      if(exists('CompStocks') & nrow(CompStocks)>MinStocks)
      {
        results<-ddply(CompStocks,c("Year"),summarize, JStocks=length(unique(IdOrig)),MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                       MedianMSY=median(MSY,na.rm=T),MedianR=median(r,na.rm=T))
        
        tempNeiStats$JStocks[b]<-results$JStocks[1]
        tempNeiStats$MedianBvBmsy[b]<-results$MedianBvBmsy[1] # calculated as the sum of biomass of comp stocks divided by the sum of Bmsy
        tempNeiStats$MedianFvFmsy[b]<-results$MedianFvFmsy[1]
        tempNeiStats$MedianMSY[b]<-results$MedianMSY[1]
        tempNeiStats$MedianR[b]<-results$MedianR[1]
        tempNeiStats$Level[b]<-level
        tempNeiStats$Region[b]<-region
    
      } # close results ddply
      
    } # close stocks loop
    
    ResultsNEIs[[a]]<-tempNeiStats
  
  } # close years loop
  
  ResultsNEIs<-ldply(ResultsNEIs,data.frame)
  
  ### Assign median values to matching NEI stocks--------------------------------------------
  
  for (c in 1:nrow(ResultsNEIs))
  {
    where<-NEIs$CommName==ResultsNEIs$CommName[c] & NEIs$Year==ResultsNEIs$Year[c] & NEIs$RegionFAO==ResultsNEIs$RegionFAO[c]
    
    NEIs[where,c('BvBmsy','FvFmsy','MSY','r')]<-ResultsNEIs[c,c('MedianBvBmsy','MedianFvFmsy','MedianMSY','MedianR')]
    
    show(paste( round(100*(c/nrow(ResultsNEIs))), '% Done with Nei Assignment',sep=''))
  }
  
  ### Bind species level data and NEI data back together-------------------------------------
  
  OutputNEI<-rbind(SpeciesLevel,NEIs)
  
  return(OutputNEI)
  
}