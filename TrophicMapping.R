
require(rfishbase)

if(exists("FaoNeiLevel")==F)
{
  source('Database_Build.r')
  
  FaoNeiLevel<-fao[grepl("nei",fao$CommName),]
}

# CountryToRun<-"Indonesia"
# CountryNEIs<-subset(FaoNeiLevel,(Country %in% CountryToRun))

NeiStats<-unique(CountryNeis[c("SciName","Year","RegionFAO")]) # 
NeiStats$TaxonLevel<-NA
NeiStats$BvBmsy<-NA # fill with median values from comparison stocks found at end of script
NeiStats$FvFmsy<-NA


NeiSciNames<-unique(NeiStats$SciName)

# TaxonDf<-data.frame(matrix(nrow=length(NeiSciNames),ncol=2))

# loop over NEI fisheries for the country of interest.
# Steps:
# 1) Determine taxonomic level
# 2) Use the CommName, taxonomic level, and which_fish function to identify all scientific names of lower taxonomic level
# 3) Use fish_names to return the scientific names
# 4) Subset Global Status using the above names, RegionFAO, and Year
# 5) Take median B/Bmsy and F/Fmsy of subset

# Step 1
for (j in 1:length(NeiSciNames))
{
  
  where<-NeiStats$SciName==NeiSciNames[j]
  
  if(grepl("spp",as.character(NeiSciNames[j]))==T)
  {NeiStats$TaxonLevel[where]<-"Genus"
  
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Family"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Family"
  
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Class"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Class"
  
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Order"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Order"}
}

# Step 2 (write steps 2:5 as a function for use with Taxon DF and consider using lapply to return median estimates)

NeiStats<-NeiStats[is.na(NeiStats$TaxonLevel)==F,]

for (m in 1:nrow(NeiStats))
{
  if(NeiStats$TaxonLevel[m]=="Genus")
  {
    Genus<-unlist(str_split(NeiStats$SciName[m],pattern=" "))[1]
    
    WhereFish<-which_fish(as.character(Genus),using=NeiStats$TaxonLevel[m]) # use passed data to find matches in fishbase data
  
  } else {WhereFish<-which_fish(as.character(NeiStats$SciName[m]),using=NeiStats$TaxonLevel[m])}
  
  SubsetNames<-fish_names(fish.data[WhereFish], name=c("ScientificName")) # create a vector of matched names
  
  ComparisonStocks<-MsyData[(MsyData$SciName %in% SubsetNames) & MsyData$Year==NeiStats$Year[m] &
                           MsyData$RegionFAO==as.character(NeiStats$RegionFAO[m]),]
  
  if(nrow(ComparisonStocks)>0)
  {
  results<-ddply(ComparisonStocks,c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T))
  
  NeiStats$BvBmsy[m]<-results$MedianBvBmsy[1]
  NeiStats$FvFmsy[m]<-results$MedianFvFmsy[1]
  }
  show(m)
    
} # close NeiStats loop
  

