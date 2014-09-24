######################################################
##
## Function to, for each NEI stock, find comparable species level  
## stocks and apply the median values of interest to the NEI stock
##
######################################################

# 
# Steps:
# 1) Subset nei stocks from Data
# 2) Find column indexes for BvBmsy, FvFmsy, Msy, r, k, Price, and MarginalCost
# In Loop:
# 3) Determine taxonomic level of each nei stock
# 4) Use the CommName, taxonomic level, and which_fish function to identify all species names of lower taxonomic level
# 5) Subset Data to contain comparison stocks using the above names and FAO region of the nei stock
# 6) Use ddply to find medians of comparison stocks in Years defined by "Years" variable
# 7) Use column indexes to take median status values from comparison stocks and apply to NEI stock
# In Loop:
# 8) Calculate MSY for each nei stock using catch in the baseline year and BvBmsy and FvFmsy from above steps
# 9) Project catch for each nei stock using catch in the baseline year and MSY calculated in step 8
# Final Step/Return:
# 10) Return an NEI only dataframe which is then to be bound to the Species-Level ProjectionData from before this function


# Step 1

MatchNEIs<- function(Data,ProjData,BaselineYear)
{

  Data<- MsyData
  
  ProjData<- ProjectionData
  
data(fishbase)  

# Data<-ProjectionData  

  #     SampleIds<- sample(unique(MsyData$IdOrig[MsyData$Dbase=='FAO']),1000,replace=FALSE)
  #     # # # 
  #     ProjectTestData<-  MsyData[! MsyData[,IdVar] %in% SampleIds,]  
  #     ProjectTestData<-RunProjection(ProjectTestData,BaselineYear)

# Step 1
  
NEIs<-Data[Data$Dbase!='RAM' & Data$RanCatchMSY==F & ((grepl("nei",Data$CommName,ignore.case=T)) | (grepl("nei",Data$CommName,ignore.case=T) & (is.infinite(Data$BvBmsy)==T | Data$BvBmsy==999)) | (grepl("spp",Data$SciName) & grepl("not identified",Data$SpeciesCatName) & Data$Dbase=="FAO")),]

SpeciesLevel<-ProjData[!(ProjData$IdOrig %in% unique(NEIs$IdOrig)),] 

# Step 2

VarsToFill<-c("BvBmsy","FvFmsy", "r", "k","Price","MarginalCost")

NeiStats<-unique(NEIs[c("SciName","RegionFAO","SpeciesCatName")]) # find unique combinations of nei stocks  
NeiStats$TaxonLevel<-NA

NeiStats$SciName<-gsub("\\(.*\\)","",NeiStats$SciName) # delete anything within parentheses in SciName
NeiStats$SciName<-gsub(",.*$","",NeiStats$SciName) # delete anything after a comma in SciName

NeiSciNames<-unique(NeiStats$SciName)

# Step 3

for (j in 1:length(NeiSciNames))
{
  
  where<-NeiStats$SciName==NeiSciNames[j]
  
  if(grepl("spp",as.character(NeiSciNames[j]))==T) 
  {NeiStats$TaxonLevel[where]<-"Genus"
   
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Genus"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Genus"
   
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Family"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Family"
   
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Class"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Class"
   
  } else if(length(unique(which_fish(as.character(NeiSciNames[j]),using=c("Order"))))==2) 
  {NeiStats$TaxonLevel[where]<-"Order"}

  show(j)
} # close NeiSciNames loop

# subset out non-fish nei stocks

NonFishNeis<-c( "Miscellaneous marine molluscs"   ,    "Squids, cuttlefishes, octopuses"   ,  "Miscellaneous marine crustaceans" ,  
                "Miscellaneous aquatic invertebrates" ,"Crabs, sea-spiders"   ,  "Sea-urchins and other echinoderms" , 
                "Shrimps, prawns"      ,               "Lobsters, spiny-rock lobsters")

NonFish<-NeiStats[(NeiStats$SpeciesCatName %in% NonFishNeis),]

# Step 4-7 

NeiStats<-NeiStats[is.na(NeiStats$TaxonLevel)==F,]

for (m in 1:nrow(NeiStats))
{
  if(NeiStats$TaxonLevel[m]=="Genus")
  {
    Genus<-unlist(str_split(NeiStats$SciName[m],pattern=" "))[1]
    
    WhereFish<-which_fish(as.character(Genus),using=NeiStats$TaxonLevel[m]) # use passed data to find matches in fishbase data
    
  } else {WhereFish<-which_fish(as.character(NeiStats$SciName[m]),using=NeiStats$TaxonLevel[m])}
  
  SubsetNames<-fish_names(fish.data[WhereFish], name=c("ScientificName")) # create a vector of matched names
  
  ComparisonStocks<-SpeciesLevel[(SpeciesLevel$SciName %in% SubsetNames) & SpeciesLevel$Year>(BaselineYear-1) &
                              SpeciesLevel$RegionFAO==as.character(NeiStats$RegionFAO[m]),]
  
  if(nrow(ComparisonStocks)>0) # consider changing this to instead be a requirement for a number of J comparison stocks
  {
    results<-ddply(ComparisonStocks,c("Year"),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                   MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
    
    # for msy, find last year with real catch data (should be 2009 or "baseline year"), calculate msy based on method dan and i discussed
    # fill BvBmsy,FvFmsy, r, k, Price, and Marginal Cost for all combinations of nei category and fao region in NeiStats loop
    # in new loop, fill in MSY and catch for each fishery using IdLevel variable
    # for median catch, catch in first year will be real catch and may need to loop to calculate future median catch using below method?
    # once msy is calculated for baseline year, calculate catch by MSY * (medianFvFmsy * medianBvMsy for the given year)
    
    
    for (b in 1:nrow(results))
    {
    WhereNei<-NEIs$SciName==NeiStats$SciName[m] & NEIs$RegionFAO==NeiStats$RegionFAO[m] & NEIs$Year==results$Year[b]
    
    NEIs[WhereNei,VarsToFill]<-results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")]
    }
  }
  show(m)  
} # close NeiStats loop

# repeat process of finding comparable stocks for NonFish  nei stocks

for (m in 1:nrow(NonFish))
{
  NonFishCompStocks<-SpeciesLevel[SpeciesLevel$SpeciesCatName==NonFish$SpeciesCatName[m] & SpeciesLevel$RegionFAO==NonFish$RegionFAO[m],]
  
  NonFishResults<-ddply(NonFishCompStocks,c("Year"),summarize, MedianBvBmsy=median(BvBmsy,na.rm=T), MedianFvFmsy=median(FvFmsy,na.rm=T),
                        MedianR=median(r,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T),MedianCost=median(MarginalCost,na.rm=T),JStocks=length(unique(IdOrig)))
  
  for (b in 1:nrow(NonFishResults))
  {
    WhereNei<-NEIs$SciName==NonFish$SciName[m] & NEIs$RegionFAO==NonFish$RegionFAO[m] & NEIs$Year==NonFishResults$Year[b]
    
    NEIs[WhereNei,VarsToFill]<-results[b,c("MedianBvBmsy", "MedianFvFmsy", "MedianR", "MedianK","MedianPrice", "MedianCost")]
    
  } # close results loop
} # close NonFish loop

# loop over NEIs and calculate MSY using catch in the BaselineYear and the median BvBmsy and FvFmsy values from above
# then calculate projected catch using this MSY value and the same BvBmsy and FvFmsy values

for(h in 1:nrow(NEIs))
{
  if(NEIs$Year[h]>BaselineYear-1)
    {
  BaseYear<-NEIs[NEIs$IdOrig==NEIs$IdOrig[h] & NEIs$Year==BaselineYear,]
  
  msy<-BaseYear$Catch/(BaseYear$BvBmsy*BaseYear$FvFmsy)
  
  NEIs$MSY[h]<-msy
  
  if(NEIs$Year[h]>BaselineYear)
  {
    NEIs$Catch[h]<-msy*(NEIs$BvBmsy[h]*NEIs$FvFmsy[h])
  } # close second if
} # close first if
show(h)
} # close loop

} # close function
