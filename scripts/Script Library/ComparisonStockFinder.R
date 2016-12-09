######################################################
##
## Function to create a csv containing a the list of potential 
## comparable stocks for each nei stock in a country
##
######################################################

# 
# Steps:
# 1) Subset country's nei stocks from FAO
# 2) Determine taxonomic level of each nei stock
# 3) Use the CommName, taxonomic level, and which_fish function to identify all species names of lower taxonomic level
# 4) Subset FAO to find comparison stocks using the above names and FAO region of the nei stock  

# fulldata<- read.csv(paste(ResultFolder,'Raw Compiled Database.csv',sep=''))

require(rfishbase)
data(fishbase)

# Step 1
CountryToRun<-c("Indonesia")

CountryNEIs<-subset(fulldata,(fulldata$Country %in% CountryToRun) & grepl("nei",fulldata$CommName) & fulldata$Dbase=="FAO" & !(fulldata$SpeciesCatName %in% SpeciesCategoriesToOmit))

NonFishNeis<-c( "Miscellaneous marine molluscs"   ,    "Squids, cuttlefishes, octopuses"   ,  "Miscellaneous marine crustaceans" ,  
                "Miscellaneous aquatic invertebrates" ,"Crabs, sea-spiders"   ,  "Sea-urchins and other echinoderms" , 
                "Shrimps, prawns"      ,               "Lobsters, spiny-rock lobsters")

NeiStats<-unique(CountryNEIs[c("SciName","RegionFAO","SpeciesCatName")]) # 
NeiStats<-NeiStats[!(NeiStats$SpeciesCatName %in% NonFishNeis),]

NeiCategories<-unique(CountryNEIs[c("CommName","RegionFAO","SpeciesCatName")]) 

write.csv(file=paste(ResultFolder,'NEI Categories.csv',sep=''),NeiCategories)

NeiStats$TaxonLevel<-NA

NeiStats$SciName<-gsub("\\(.*\\)","",NeiStats$SciName) # delete anything within parentheses in SciName
NeiStats$SciName<-gsub(",.*$","",NeiStats$SciName) # delete anything after a comma in SciName

NeiSciNames<-unique(NeiStats$SciName)

# Step 2
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

  }# close loop

# Step 3 
NeiStats<-NeiStats[is.na(NeiStats$TaxonLevel)==F,]

# ComparablesTable<-matrix(NA,ncol=500,nrow=1000) # make large empty matrix to fill with loop

for (m in 1:nrow(NeiStats))
{
  if(NeiStats$TaxonLevel[m]=="Genus")
  {
    Genus<-unlist(str_split(NeiStats$SciName[m],pattern=" "))[1]
    
    WhereFish<-which_fish(as.character(Genus),using=NeiStats$TaxonLevel[m]) # use passed data to find matches in fishbase data
    
  } else {WhereFish<-which_fish(as.character(NeiStats$SciName[m]),using=NeiStats$TaxonLevel[m])}
  
  SubsetNames<-fish_names(fish.data[WhereFish], name=c("ScientificName")) # create a vector of matched Scientific names

  # Step 4
  Comparables<-unique(fulldata$CommName[(fulldata$SciName %in% SubsetNames) & fulldata$RegionFAO==NeiStats$RegionFAO[m]])
  NeiName<-unique(CountryNEIs$CommName[CountryNEIs$SciName==NeiStats$SciName[m]])
  Comparables<-c(paste(NeiName,NeiStats$RegionFAO[m],sep="-"),Comparables)
  
  {
  if(m==1)
  {
    length(Comparables)<-500
    ComparablesFinal<-Comparables
  }
  
  if(m>1)
  {
    length(Comparables)<-500
    ComparablesFinal<-cbind(ComparablesFinal,Comparables)
#     ComparablesTable2<-data.frame(matrix(Comparables,ncol=1,nrow=length(Comparables))) # create new column for final data frame
#     NeiName<-unique(CountryNEIs$CommName[CountryNEIs$SciName==NeiStats$SciName[m]]) # get common name of nei assemblage
#     colnames(ComparablesTable2)<-NeiName # add name of nei assemblage to column header
#     ComparablesTable$Country<-rep(CountryToRun,length(Comparables))
#     ComparablesTableFinal<-merge(ComparablesTable,ComparablesTable2)
  } # close m if
  } # close length if
  show(m)
} # close loop

ComparableStocks<-data.frame(ComparablesFinal)

write.csv(file=paste(ResultFolder,'ComparableFishStocks.csv',sep=''),ComparableStocks)
# Non-Fish NEIs

NonFish<-NeiCategories[(NeiCategories$SpeciesCatName %in% NonFishNeis),]

for (m in 1:nrow(NonFish))
{
  
  SubsetNames2<-unique(fulldata$CommName[fulldata$SpeciesCatName==NonFish$SpeciesCatName[m] & fulldata$RegionFAO==NonFish$RegionFAO[m] & !grepl("nei",fulldata$CommName,ignore.case=T)])  # create a vector of matched Scientific names
  
  
#  if(length(SubsetNames2)>0)                      
#  {
  Comparables<-c(paste(NonFish$CommName[m],NonFish$RegionFAO[m],sep="-"),SubsetNames2)
  
    if(m==1)
    {
      length(Comparables)<-500
      ComparablesFinalNonFish<-Comparables
    }
    
    if(m>1)
    {
      length(Comparables)<-500
      ComparablesFinalNonFish<-cbind(ComparablesFinalNonFish,Comparables)
      #     ComparablesTable2<-data.frame(matrix(Comparables,ncol=1,nrow=length(Comparables))) # create new column for final data frame
      #     NeiName<-unique(CountryNEIs$CommName[CountryNEIs$SciName==NeiStats$SciName[m]]) # get common name of nei assemblage
      #     colnames(ComparablesTable2)<-NeiName # add name of nei assemblage to column header
      #     ComparablesTable$Country<-rep(CountryToRun,length(Comparables))
      #     ComparablesTableFinal<-merge(ComparablesTable,ComparablesTable2)
    } # close m if
   # close length if
show(m)
} 

write.csv(file=paste(ResultFolder,'NEI Non-Fish Comparables.csv',sep=''),ComparablesFinalNonFish)