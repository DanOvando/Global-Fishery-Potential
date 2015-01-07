#############################################
##
## This function performs a jackknife analysis of the NEI method using RAM stocks with known B/Bmsy values
##
#############################################
JacknifeNeis<-function(MsyData,ProjectionData,Policy,BaselineYear,Level,MinStocks,Replacement)
{
  
#   Level<-'All'
#   MinStocks<-5
#   Replacement<-FALSE
#   BaselineYear<-2012
  

  library(ggplot2)
  library(gridExtra)
  
  Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes 
  
  ### Subset Msy data to ram stocks
  RamOriginal<-MsyData[MsyData$Dbase=='RAM' & MsyData$Year<=BaselineYear,]
  
  # function to convert SciName into "Genus spp" for creating genus level nei groups from ram stocks
  MakeGenusNei<-function(x) 
  {
    genus<-unlist(str_split(x,pattern=" "))[1]
    
    genus<-paste(genus,"spp",sep=" ")
    
    return(genus)
  }
  
  # apply function to data subset
  RamNeiDiag<-RamOriginal
  
  RamNeiDiag$SciName<-sapply(RamNeiDiag$SciName,MakeGenusNei)
  
#   RamNeiDiag<-RamNeiDiag[RamNeiDiag$SciName=='Sebastes spp',] # For running on just Sebastes
  
  source('Diagnostics/StitchFishJackknife.R')

  TestStitch<-StitchFishJackknife(Data=RamNeiDiag,IdVar,Level='SciName',Groups=unique(RamNeiDiag$SciName),GroupSamples=10,Iterations=1)
  
  # Create Ram "NeiStats" dataset to loop through 
  NeiStats<-TestStitch[,c('IdOrig','Year','CommName',"SciName","SpeciesCatName",'BvBmsy',
                            'FvFmsy','Biomass','Catch','Bmsy','NumStocks','IDs')]
 
  NeiStats$TaxonLevel<-Level
  
  NeiStats$EstimateB<-NA # variable to fill with BvBmsy estimate (sum B / sum Bmsy)
  NeiStats$CatchWtMeanB<-NA # catch weighted BvBmsy
  NeiStats$MedianBvBmsy<-NA # median BvBmsy
  NeiStats$MeanBvBmsy<-NA # mean BvBmsy
  NeiStats$JStocks<-NA # variable to fill with JStocks
  NeiStats$EstLevel<-NA # variable to fill with the taxonomic level used for estimate
     
  # loop over years and then stocks to estimate status of ram 'nei' stocks  
  yrs<-unique(NeiStats$Year)
  
  for(a in 1:length(yrs)) # loop over years
  {
    SpeciesLevel<-RamOriginal[RamOriginal$Year==yrs[a],] # subset to year
    
    tempNeiStats<-NeiStats[NeiStats$Year==yrs[a] & is.na(NeiStats$BvBmsy)==F,] # subset to year
    
    show(paste( round(100*(a/length(yrs))), '% Done with NeiStats',sep=''))
    
    for(b in 1:nrow(tempNeiStats)) # loop over stocks in year[a]
    {
      drop<-c(unlist(str_split(tempNeiStats$IDs[b],pattern="_"))) # pull out RAM ids in synthetic nei and drop from species level sample pool
      
      tempSpeciesLevel<-SpeciesLevel
      
      if(Replacement==FALSE)
      {
        tempSpeciesLevel<-SpeciesLevel[!(SpeciesLevel$IdOrig %in% drop),] # DROP STOCK BEING ESTIMATED  
      }
      
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
          ComparisonStocks<-tempSpeciesLevel[tempSpeciesLevel$SciName %in% compstocks,]
          
          level<-'Genus'
        }
      } # close genus if
      
      # Family if
      if((Level=='Family') | ((Level=='All') & nrow(ComparisonStocks)<MinStocks)) # find scientific names for all genus level nei stocks
      {
        
#         Genus<-unlist(str_split(tempNeiStats$SciName[b],pattern=" "))[1] # pull out genus
#         
#         WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # locate genus
        
        family<-unique(Spec_ISSCAAP$Family[WhereComp]) # find family name of that genus
        
        compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out species names in that family
        
        if(exists('compstocks'))
        {
          ComparisonStocks<-tempSpeciesLevel[tempSpeciesLevel$SciName %in% compstocks,]
          
          level<-'Family'
        }
      } # close Family if   
      
      # Order if
      if((Level=='Order') | ((Level=='All') & nrow(ComparisonStocks)<MinStocks)) # determine if non-genus stock is a family name
      {  
#         Genus<-unlist(str_split(tempNeiStats$SciName[b],pattern=" "))[1] # pull out genus
#         
#         WhereComp<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # locate genus
        
        order<-unique(Spec_ISSCAAP$Order[WhereComp]) # find family name of that genus
        
        compstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out species names in that family
        
        if(exists('compstocks'))
        {
          ComparisonStocks<-tempSpeciesLevel[tempSpeciesLevel$SciName %in% compstocks,]
          
          level<-'Order'
        }
      }# close Order if
      
      if(exists('ComparisonStocks') & nrow(ComparisonStocks)>MinStocks)
      {
        results<-ddply(ComparisonStocks,c("Year"),summarize,AggregateBvBmsy=sum(Biomass,na.rm=T)/sum(Bmsy,na.rm=T),
                       JStocks=length(unique(IdOrig)),CatchWtMeanB=sum(Catch*BvBmsy,na.rm=T)/sum(Catch,na.rm=T),
                       MedianBvBmsy=median(BvBmsy,na.rm=T), MeanBvBmsy=mean(BvBmsy,na.rm=T))
        
        tempNeiStats$EstimateB[b]<-results$AggregateBvBmsy[1] # calculated as the sum of biomass of comp stocks divided by the sum of Bmsy
        tempNeiStats$JStocks[b]<-results$JStocks[1]
        tempNeiStats$EstLevel[b]<-level
        tempNeiStats$CatchWtMeanB[b]<-results$CatchWtMeanB[1]
        tempNeiStats$MedianBvBmsy[b]<-results$MedianBvBmsy[1]
        tempNeiStats$MeanBvBmsy[b]<-results$MeanBvBmsy[1]
      }
      
    } # close stocks loop
    
    if(a==1) { JackNei<-tempNeiStats }
    
    if(a>1) { JackNei<-rbind(JackNei,tempNeiStats) }
    
  } # close years loop
  
  
  # Plot observed v. predicted BvBmsy and FvFmsy, calculate proportional error
  pdf(file=paste(FigureFolder,'Nei Jackknife Results.pdf',sep=''),width=12,height=10)
  
  # plotted together
  print(ggplot(JackNei,aes(x=BvBmsy,y=EstimateB)) +
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
          facet_wrap(~NumStocks,scales='free') +
          coord_cartesian(xlim=c(0,4)) +
          geom_smooth(method='lm') +
          geom_abline(intercept=0,slope=1))
  
  dev.off()
  
# plot four BvBmsy metrics in grid and loop over nei levels groups

neilevels<-unique(JackNei$NumStocks)

pdf(file=paste(FigureFolder,'ISSCAAP Nei Jackknife Results.pdf',sep=''),width=12,height=10)

for(a in 1:length(neilevels))
{
  Aplot<-ggplot(JackNei[JackNei$NumStocks==neilevels[a],],aes(x=BvBmsy,y=EstimateB)) +
          geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
          coord_cartesian(xlim=c(0,4)) +
          geom_abline(intercept=0,slope=1) +
          ylab('Sum B/Sum Bmsy')

  Bplot<-ggplot(JackNei[JackNei$NumStocks==neilevels[a],],aes(x=BvBmsy,y=CatchWtMeanB)) +
          geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
          coord_cartesian(xlim=c(0,4)) +
          geom_abline(intercept=0,slope=1) +
          ylab('Catch-Weighted Mean BvBmsy')

  Cplot<-ggplot(JackNei[JackNei$NumStocks==neilevels[a],],aes(x=BvBmsy,y=MedianBvBmsy)) +
          geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
          coord_cartesian(xlim=c(0,4)) +
          geom_abline(intercept=0,slope=1) +
          ylab('Median BvBmsy')
  
  Dplot<-ggplot(JackNei[JackNei$NumStocks==neilevels[a],],aes(x=BvBmsy,y=MeanBvBmsy)) +
          geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
          coord_cartesian(xlim=c(0,4)) +
          geom_abline(intercept=0,slope=1) +
          ylab('Mean BvBmsy')

  print(grid.arrange(arrangeGrob(Aplot,Bplot,Cplot,Dplot,ncol=2),
                     main=textGrob(paste('Jackknife results for a NEI of',a,'stock(s)',sep=' '),
                     gp=gpar(fontsize=20,font=3))))
}

dev.off()
 
  # Same plots for catch weighted mean
  
  print(ggplot(JackNei,aes(x=BvBmsy,y=CatchWtMeanB)) +
          geom_point(aes(color=EstLevel,size=JStocks),alpha=.6) +
          coord_cartesian(xlim=c(0,4)) +
          geom_abline(intercept=0,slope=1))
  
  print(ggplot(JackNei[is.na(JackNei$EstLevel)==F,],aes(x=BvBmsy,y=CatchWtMeanB)) +
          geom_point(aes(size=JStocks,color=EstLevel),alpha=.6) +
          facet_grid(.~EstLevel) +
          coord_cartesian(xlim=c(0,4)) +
          geom_smooth(method='lm') +
          geom_abline(intercept=0,slope=1))
  
  print(ggplot(JackNei[JackNei$BvBmsy<3 & is.na(JackNei$EstLevel)==F,],aes(x=BvBmsy,y=CatchWtMeanB)) +
          geom_point(aes(color=EstLevel),alpha=.6) +
          facet_wrap(~NumStocks) +
          coord_cartesian(xlim=c(0,4)) +
          geom_smooth(method='lm') +
          geom_abline(intercept=0,slope=1))
  
  # wrap by SciName
  print(ggplot(JackNei[JackNei$BvBmsy<3 & is.na(JackNei$EstLevel)==F,],aes(x=BvBmsy,y=EstimateB)) +
          geom_point(aes(color=EstLevel),alpha=.6) +
          facet_wrap(~SciName) +
          coord_cartesian(xlim=c(0,4)) +
          geom_smooth(method='lm') +
          geom_abline(intercept=0,slope=1)) 
  
  print(ggplot(JackNei[JackNei$SciName=='Sebastes spp',],aes(x=BvBmsy,y=CatchWtMeanB)) +
          geom_point(aes(size=JStocks,color=EstLevel),alpha=.6) +
          facet_wrap(~NumStocks) +
          coord_cartesian(xlim=c(0,4)) +
          geom_smooth(method='lm') +
          geom_abline(intercept=0,slope=1))

  
    
  # Proportional error
  
  JackNei$PropErrorB<-(JackNei$EstimateB/JackNei$BvBmsy)-1
  
  JackNei$PropErrorF<-(JackNei$EstimateF/JackNei$FvFmsy)-1  
  
  ggplot(JackNei[JackNei$PropErrorB<10,],aes(EstLevel,PropErrorB)) +
    geom_boxplot() 
  
  ggplot(JackNei[JackNei$PropErrorF<10,],aes(EstLevel,PropErrorF)) +
    geom_boxplot() 
  
  return(JackNei)
  
}