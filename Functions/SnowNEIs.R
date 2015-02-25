
# nei_stock<-unique(NeiStats$SciName)

SnowNEIs<-function(s,nei_stock,NEIs,SpeciesLevel,NeiStats,allstocks,Spec_ISSCAAP,VarsToFill,NumCPUs)
{
  
  write.table((paste(round(100*(s/length(nei_stock)),2),'% done with SnowNEIs',sep='')), 
              file = 'SnowNEIs Progress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)  
  
  
NeiCat<-(nei_stock[s])

# isolate NeiStats data for NEI type
stats<-NeiStats[NeiStats$SciName==NeiCat,]

# Subset out NEIs to fill with results
neis<-NEIs[NEIs$SciName==NeiCat,]

# Subset SpeciesLevel from which to draw results
species<-SpeciesLevel[SpeciesLevel$SpeciesCatName %in% neis$SpeciesCatName,]

# find regional and global values for species category to refrence as default

global<-ddply(species,c('Year','SpeciesCatName','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
                  FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),
                  MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))

# Find row with taxon information in species AFSIS
WhereNEI<-match(NeiCat,Spec_ISSCAAP$Species_AFSIS) 


### Find comp stocks for genus and non-genus level NEI categories---------------------------------

# GENUS
if(stats$TaxonLevel=="Genus") # find scientific names for all genus level nei stocks
{
  
  Genus<-unlist(str_split(NeiCat,pattern=" "))[1] # pull out genus
  
  WhereG<-grepl(Genus,Spec_ISSCAAP$Species_AFSIS) # search for species names in that genus
  
  Gstocks<-unique(Spec_ISSCAAP$Species_AFSIS[WhereG]) # pull out Genus level compstock species names
  
  family<-unique(Spec_ISSCAAP$Family[WhereNEI]) # find family name of the NEI category
  
  Fstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family]) # pull out family compstocks
  
  order<-unique(Spec_ISSCAAP$Order[WhereNEI]) # find family name of that genus
  
  Orderstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Order==order])
  
} 

# NON-GENUS
if(stats$TaxonLevel=="Non-Genus") # determine if non-genus stock is a family name
{  
  family<-unique(Spec_ISSCAAP$Family[WhereNEI]) # find family name of the NEI category # if so find species within that family
  
  Fstocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Family==family])
  
  order<-unique(Spec_ISSCAAP$Order[WhereNEI]) # find family name of that genus
  
  Ostocks<-unique(Spec_ISSCAAP$Species_AFSIS[Spec_ISSCAAP$Order==order])
}

### Summarize results for comp stocks and fill in NEIs------------------------------------------

# find unique regions that that nei exists in with which to determine whether to move to higher level
regs<-unique(neis$RegionFAO)

# GENUS results
if(exists('Gstocks')==T)
{
  Gcomp<-species[(species$SciName %in% Gstocks) & (species$RegionFAO %in% regs),]
  
  Gcomp<-ddply(Gcomp,c('Year','RegionFAO','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
               FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),
               MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))  
}

# FAMILY results
if(exists('Fstocks')==T)
{
  Fcomp<-species[(species$SciName %in% Fstocks) & (species$RegionFAO %in% regs),]
  
  Fcomp<-ddply(Fcomp,c('Year','RegionFAO','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
               FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),
               MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
}

# ORDER results
if(exists('Ostocks')==T)
{
  Ocomp<-species[(species$SciName %in% Ostocks) & (species$RegionFAO %in% regs),]
  
  Ocomp<-ddply(Ocomp,c('Year','RegionFAO','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
               FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),
               MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
}

# ISSCAAP
Catcomp<-ddply(species[species$RegionFAO %in% regs,],c('Year','RegionFAO','Policy'),summarize,BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),
               FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),
               MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))

### Loop over regions and policies and fill in results using hierarchical approach (Gcomp->Fcomp->Ocomp->ISSCAAP)----------------------------------------------------

for(a in 1:length(regs))
{
  WhereReg<-neis$RegionFAO==regs[a]
  
  # Genus
  if((exists('Gcomp')==T))
    {
      if(regs[a] %in% Gcomp$RegionFAO & length(unique(Gcomp$Policy))>1)
      {
        for(b in 1:nrow(Gcomp))
        {
          neis[WhereReg & neis$Year==Gcomp$Year[b] & neis$Policy==Gcomp$Policy[b],VarsToFill]<-Gcomp[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
          
          neis$CanProject[WhereReg & neis$Year==Gcomp$Year[b] & neis$Policy==Gcomp$Policy[b]]<-TRUE
          
          neis$NeiLevel[WhereReg & neis$Year==Gcomp$Year[b] & neis$Policy==Gcomp$Policy[b]]<-'Genus'
          
          done<-TRUE
        }
      }
  }
  
  # FAMILY
 if (exists('Fcomp')==T & exists('done')==F) 
    {
      if((regs[a] %in% Fcomp$RegionFAO) & length(unique(Fcomp$Policy))>1)
      {
        for(b in 1:nrow(Fcomp))
        {
          neis[WhereReg & neis$Year==Fcomp$Year[b] & neis$Policy==Fcomp$Policy[b],VarsToFill]<-Fcomp[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
          
          neis$CanProject[WhereReg & neis$Year==Fcomp$Year[b] & neis$Policy==Fcomp$Policy[b]]<-TRUE
          
          neis$NeiLevel[WhereReg & neis$Year==Fcomp$Year[b] & neis$Policy==Fcomp$Policy[b]]<-'Family'
          
          done<-TRUE
        }
      }
    }
 
# Order  
 if(exists('Ocomp')==T & exists('done')==F) 
  {
    if((regs[a] %in% Ocomp$RegionFAO) & length(unique(Ocomp$Policy))>1)
    {
      for(b in 1:nrow(Ocomp))
      {
        neis[WhereReg & neis$Year==Ocomp$Year[b] & neis$Policy==Ocomp$Policy[b],VarsToFill]<-Ocomp[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
        
        neis$CanProject[WhereReg & neis$Year==Ocomp$Year[b] & neis$Policy==Ocomp$Policy[b]]<-TRUE
        
        neis$NeiLevel[WhereReg & neis$Year==Ocomp$Year[b] & neis$Policy==Ocomp$Policy[b]]<-'Order'
        
        done<-TRUE
      }
    }
  }
    
  # ISSCAAP in that region
if(exists('Catcomp')==T & exists('done')==F)
  {
    if((regs[a] %in% Catcomp$RegionFAO) & length(unique(Catcomp$Policy))>1)
    {
      for(b in 1:nrow(Catcomp))
      {
        neis[WhereReg & neis$Year==Catcomp$Year[b] & neis$Policy==Catcomp$Policy[b],VarsToFill]<-Catcomp[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
        
        neis$CanProject[WhereReg & neis$Year==Catcomp$Year[b] & neis$Policy==Catcomp$Policy[b]]<-TRUE
        
        neis$NeiLevel[WhereReg & neis$Year==Catcomp$Year[b] & neis$Policy==Catcomp$Policy[b]]<-'ISSCAP in Region'
        
        done<-TRUE
      }
    }
  }
    # ISSCAAP globally
  if(exists('done')==F & length(unique(global$Policy))>1)
    {
      for(b in 1:nrow(global))
      {
        neis[neis$Year==global$Year[b] & neis$Policy==global$Policy[b],VarsToFill]<-global[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
        
        neis$CanProject[neis$Year==global$Year[b] & neis$Policy==global$Policy[b]]<-TRUE
        
        neis$NeiLevel[neis$Year==global$Year[b] & neis$Policy==global$Policy[b]]<-'ISSCAAP Global'
        
        done<-TRUE
    }
  }
  # All stocks globally
  if(exists('done')==F)
  {
    for(b in 1:nrow(allstocks))
    {
    neis[neis$Year==allstocks$Year[b] & neis$Policy==allstocks$Policy[b],VarsToFill]<-allstocks[b,c("BvBmsy25", "FvFmsy75", "MedianR", "MedianK","MedianPrice", "MedianCost")]
    
    neis$CanProject[neis$Year==allstocks$Year[b] & neis$Policy==allstocks$Policy[b]]<-TRUE
    
    neis$NeiLevel[neis$Year==allstocks$Year[b] & neis$Policy==allstocks$Policy[b]]<-'Global'
    
    done<-TRUE
  }
}
  rm(done)
}

### Return nei dataframe

idlevel<-unique(neis[,c('IdOrig','CommName','RegionFAO','NeiLevel')])

neis$NeiLevel<-NULL

return(neis)
}

