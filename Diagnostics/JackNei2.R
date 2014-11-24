JacknifeNeis<-function(MsyData,ProjectionData,Policy,BaselineYear,Level,MinStocks)
{
  
  ### Subset Msy and Projection data to only historic years
  RamNeiDiag<-MsyData[MsyData$Dbase=='RAM' & MsyData$Year<=BaselineYear,] 
  
  RamStatus<-unique(RamNeiDiag[c("IdOrig","Year","SciName","BvBmsy","FvFmsy")]) # make new data table to keep original BvBmsy and FvFmsy values
   
  RamSampleIds<-unique(RamNeiDiag[c("SciName","RegionFAO")])
  
  RamSampleIds$Genus<-NA
  
  ProjDataDiag<-ProjectionData[ProjectionData$IdLevel=="Species" & ProjectionData$Year<=BaselineYear,]
  
  ### Prep Ram stocks to resemble NEIs for nearest neighbor jackknife 
  
  # get genus of ram stocks
  for(e in 1:nrow(RamSampleIds))
  { 
    RamSampleIds$Genus[e]<-unlist(str_split(unique(RamSampleIds$SciName[e]),pattern=" "))[1]
    
#     show(e)
  }
  
  ids<-unique(RamStatus$IdOrig)

#   RamNeiDiag$Dbase<-"FAO" # convert Dbase to FAO
#   RamNeiDiag$BvBmsy<-NA # remove true values
#   RamNeiDiag$CommName<-paste(RamNeiDiag$CommName,"nei",sep=" ") # add nei to CommName
#   RamNeiDiag$RanCatchMSY<-FALSE # change to FALSE
#   
  # function to convert SciName into "Genus spp" for NearestNeighborNEI to recognize
  MakeGenusNei<-function(x) 
  {
    genus<-unlist(str_split(x,pattern=" "))[1]
    
    genus<-paste(genus,"spp",sep=" ")
    
    return(genus)
  }
  
  # apply function to data subset
  RamNeiDiag$SciName<-sapply(RamNeiDiag$SciName,MakeGenusNei) 
  
  # Create Ram "NeiStats" dataset to loop through 
  NeiStats<-unique(RamNeiDiag[c('IdOrig','Year',"SciName","SpeciesCatName",'BvBmsy','FvFmsy')])
  
  NeiStats$TaxonLevel<-NA
  NeiStats$Level<-NA # create variable to identify which taxonomic level was selected for each stock
  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==T]<-"Genus"  
  NeiStats$TaxonLevel[grepl("spp",NeiStats$SciName)==F]<-"Non-Genus"
  
  NeiStats$EstimateB<-NA # variable to fill with BvBmsy estimate
  NeiStats$EstimateF<-NA # variable to fill with FvFmsy estimate
  NeiStats$JStocks<-NA # variable to fill with JStocks
  NeiStats$EstLevel<-NA # variable to fill with the taxonomic level used for estimate

  # loop over years and then stocks to estimate status of ram 'nei' stocks  
  yrs<-unique(NeiStats$Year)

  for(a in 1:length(yrs)) # loop over years
  {
    SpeciesLevel<-ProjDataDiag[ProjDataDiag$Year==yrs[a],] # subset to year
    
    tempNeiStats<-NeiStats[NeiStats$Year==yrs[a] & is.na(NeiStats$BvBmsy)==F,] # subset to year
    
    show(paste( round(100*(a/length(yrs))), '% Done with NeiStats',sep=''))
    
    for(b in 1:nrow(tempNeiStats)) # loop over stocks in year[a]
    {
      SpeciesLevel<-SpeciesLevel[SpeciesLevel$IdOrig!=tempNeiStats$IdOrig[b],] # drop stock being estimated
      
      rm(compstocks)
      rm(ComparisonStocks)
      rm(results)
      rm(level)
    
   # Genus if
    if((Level=='Genus') | (Level=='All')) # find scientific names for all genus level nei stocks
    {
      
      Genus<-unlist(str_split(tempNeiStats$SciName[b],pattern=" "))[1] # pull out genus
      
      WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # search for species names in that genus
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereComp]) # pull out species names
      
      if(exists('compstocks'))
      {
      ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks,]
      
      level<-'Genus'
      }
    } # close genus if
    
   # Family if
    if((Level=='Family') | ((Level=='All') & nrow(ComparisonStocks)<MinStocks)) # find scientific names for all genus level nei stocks
    {
      
      Genus<-unlist(str_split(tempNeiStats$SciName[b],pattern=" "))[1] # pull out genus
      
      WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # locate genus
      
      family<-unique(Spec_ISSCAAP$Family[WhereComp]) # find family name of that genus
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out species names in that family
      
    if(exists('compstocks'))
      {
        ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks,]
        
        level<-'Family'
      }
    } # close Family if   
    
   # Order if
    if((Level=='Order') | ((Level=='All') & nrow(ComparisonStocks)<MinStocks)) # determine if non-genus stock is a family name
    {  
      Genus<-unlist(str_split(tempNeiStats$SciName[b],pattern=" "))[1] # pull out genus
      
      WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # locate genus
      
      order<-unique(Spec_ISSCAAP$Order[WhereComp]) # find family name of that genus
      
      compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out species names in that family
      
      if(exists('compstocks'))
      {
        ComparisonStocks<-SpeciesLevel[SpeciesLevel$SciName %in% compstocks,]
        
        level<-'Order'
      }
    }# close Order if
    
    if(exists('ComparisonStocks') & nrow(ComparisonStocks)>MinStocks)
    {
      results<-ddply(ComparisonStocks,c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                     JStocks=length(unique(IdOrig)))
      
      tempNeiStats$EstimateB[b]<-results$MedianBvBmsy[1]
      tempNeiStats$EstimateF[b]<-results$MedianFvFmsy[1]
      tempNeiStats$JStocks[b]<-results$JStocks[1]
      tempNeiStats$EstLevel[b]<-level
    }
   
  } # close stocks loop
  
   if(a==1) { JackNei<-tempNeiStats }
   
   if(a>1) { JackNei<-rbind(JackNei,tempNeiStats) }
   
} # close years loop
  

# Plot observed v. predicted BvBmsy and FvFmsy, calculate proportional error
pdf(file=paste(FigureFolder,'Nei Jackknife Results.pdf',sep=''),width=12,height=10)
 
# plotted together
print(ggplot(JackNei[JackNei$BvBmsy<3,],aes(x=BvBmsy,y=EstimateB)) +
  geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
  coord_cartesian(xlim=c(0,4)) +
  geom_abline(intercept=0,slope=1))

# plotted in facets
print(ggplot(JackNei[is.na(JackNei$EstLevel)==F,],aes(x=BvBmsy,y=EstimateB)) +
  geom_point(aes(size=JStocks,color=EstLevel),alpha=.6) +
  facet_grid(.~EstLevel) +
  coord_cartesian(xlim=c(0,4)) +
  geom_smooth(method='lm') +
  geom_abline(intercept=0,slope=1))

# wrap by species category
print(ggplot(JackNei[JackNei$BvBmsy<3 & is.na(JackNei$EstLevel)==F,],aes(x=BvBmsy,y=EstimateB)) +
  geom_point(aes(size=JStocks,color=EstLevel),alpha=.6) +
  facet_wrap(~SpeciesCatName) +
  coord_cartesian(xlim=c(0,4)) +
  geom_smooth(method='lm') +
geom_abline(intercept=0,slope=1))

dev.off()

# FvFmsy
ggplot(JackNei,aes(x=FvFmsy,y=EstimateF)) +
  geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
  coord_cartesian(xlim=c(0,4)) +
  geom_abline(intercept=0,slope=1)
      
# Proportional error

JackNei$PropErrorB<-(JackNei$EstimateB/JackNei$BvBmsy)-1
      
JackNei$PropErrorF<-(JackNei$EstimateF/JackNei$FvFmsy)-1  
  
ggplot(JackNei[JackNei$PropErrorB<10,],aes(EstLevel,PropErrorB)) +
  geom_boxplot() 

ggplot(JackNei[JackNei$PropErrorF<10,],aes(EstLevel,PropErrorF)) +
  geom_boxplot() 

return(JackNei)

}