#############################################################
##
## Catch Share V2.0
##
#############################################################

# This code identifies matches between countries and catch share stocks using EDF's database and raw FAO data. 
# The complication with assigning and treating catch share stocks differently in the analysis is that many catch share stocks are contained within multinational RAM 
# assessments and cannot easily be separated out for the purposes of projections...

# CatchSharePercent<-50
# DataR<-RawData

FindCatchShares<-function(DataR,CatchSharePercent)
{
  ### Prep Data-----------------------------------------------------------------------------------
  
  ## EDF Catch Share Data
  
  # Read in EDF's Catch Share Database
  CSstocks<-read.csv("Data/EDF_CatchShares_SpeciesLevel.csv",stringsAsFactors=F)
  
  # Select only columns of interest
  CSstocks<-CSstocks[,c('SystemName','WorldRegion','Country','ProgramName','SystemID','RegionFAO','Genus','Species')]
  
  # Merge genus and species columns
  CSstocks$SciName<-paste(CSstocks$Genus,CSstocks$Species,sep=' ')
  
  # replace spp. with spp
  CSstocks$SciName<-gsub('spp.','spp',CSstocks$SciName)
  
  # split CSstocks out by unique FAO region
  tempCSstocks<-list()
  
  for(a in 1:nrow(CSstocks))
  {
    temp<-CSstocks[a,]
    
    regions<-unlist(strsplit(temp$RegionFAO,split=",",fixed=T)) # split apart FAO regions
    num<-length(regions) # how many?
    
    if(num==1){tempCSstocks[[a]]<-temp}
    
    if(num>1)
      { 
        temp2<-data.frame(matrix(NA,nrow=num,ncol=length(colnames(temp))))
        colnames(temp2)<-colnames(temp)
        temp2[,]<-temp
        temp2$RegionFAO<-regions 
        
        tempCSstocks[[a]]<-temp2
        
    }# close if statement
  
#     show(a)
  }
  
  CSstocks<-ldply(tempCSstocks)
  
  # create unique id to use for matching to GFR data
  CSstocks$IdCS1<-seq(1:nrow(CSstocks))
  
  CSstocks$IdCS<-paste(CSstocks$IdCS,CSstocks$Country,CSstocks$SciName,sep='_')
  
  CSstocks$IdCS1<-NULL
  
  ## GFR Stock Data
  
  # Get unique FAO stocks for countries and match to EDF list. This will indicate what stocks are catch shares and can be referenced against RAM to see if they are covered
  # by a RAM assessment with multiple countries, all of which may not be catch shares
  GFRstocks<-unique(DataR[,c('IdOrig','Dbase','Country','CommName','SciName','RegionFAO','CatchShare')])
    
  # order by country
  GFRstocks<-GFRstocks[with(GFRstocks,order(Country)),]
  
  # break out FAO, RAM, and RAM multinational subsets 
  fao<-subset(GFRstocks,Dbase=='FAO')
  
  ram<-subset(GFRstocks,Dbase=='RAM')
  
  # break apart RAM FAO regions
  tempRMstocks<-list()
  
  for(a in 1:nrow(ram))
  {
    temp<-ram[a,]
    
    regions<-unlist(strsplit(temp$RegionFAO,split=",",fixed=T)) # split apart FAO regions
    num<-length(regions) # how many?
    
    if(num==1){tempRMstocks[[a]]<-temp}
    
    if(num>1)
    { 
      temp2<-data.frame(matrix(NA,nrow=num,ncol=length(colnames(temp))))
      colnames(temp2)<-colnames(temp)
      temp2[,]<-temp
      temp2$RegionFAO<-regions 
      
      tempRMstocks[[a]]<-temp2
      
    }# close if statement
    
#     show(a)
  }
  
  ram<-ldply(tempRMstocks)
  
  multiRam<-subset(ram,Country=='Multinational')
  
  ## Standardize country names between datasets
  
  # Find non-matching country names
#   unique(CSstocks$Country[!(CSstocks$Country %in% GFRstocks$Country)])
  
  # Adjust non-matching country names to match
  CSstocks$Country[CSstocks$Country=='United States of America']<-'USA'
  CSstocks$Country[CSstocks$Country=='French Southern Territories']<-'French Southern Terr'
  CSstocks$Country[CSstocks$Country=='Vietnam']<-'Viet Nam'
  CSstocks$Country[CSstocks$Country=='Falkland Islands']<-'Falkland Is.'
  
  # Standardize SciNames and look for errors
#   sort(unique(CSstocks$SciName))
  
  CSstocks$SciName[CSstocks$SciName=='Trachurus trachurus capensis']<-'Trachurus capensis'
  CSstocks$SciName[CSstocks$SciName=="Clupea harengus harengus"]<-'Clupea harengus'
  CSstocks$SciName[CSstocks$SciName=="Clupea harengus pallasi"]<-'Clupea pallasi'
  
  # unique(CSstocks$SciName[!(CSstocks$SciName %in% GFRstocks$SciName)])
  
  ### Find country/species/region matches-----------------------------------------------------------------------------------
  
  ## Find country level RAM stocks that match
  matchR<-join(CSstocks,ram,by=c('Country','RegionFAO','SciName'),type='inner')
  
  ## Find all FAO matches and save ids
  matchF<-join(CSstocks,fao,by=c('Country','RegionFAO','SciName'),type='inner')
  
  faoCSids<-unique(matchF$IdOrig)
  
  ## Determine how many catch share fisheries have been matched (out of the 964 combinations of fishery and fao region)
  miss<-CSstocks[!(CSstocks$IdCS %in% matchR$IdCS) & !(CSstocks$IdCS %in% matchF$IdCS),]
  
#   unique(miss$SciName[(miss$SciName %in% GFRstocks$SciName)])
  
  # Pull out 2012 fao data and relabel for Catch Share matches
  fao2<-subset(DataR,Dbase=='FAO' & Year==2012 & is.na(Catch)==F)
  
  fao2<-fao2[,c('IdOrig','Country','CommName','SciName','RegionFAO','Catch','CatchShare')]
  
  fao2$CatchShare<-NA
  
  fao2$CatchShare[fao2$IdOrig %in% faoCSids]<-1
  
  fao2$CatchShare[!(fao2$IdOrig %in% faoCSids)]<-0
  
  ## Match multinational RAM stocks to fao 2012 data and calculate percent of catch in catchshares
  matchM<-join(multiRam,fao2,by=c('RegionFAO','SciName'),type='inner')
  
  colnames(matchM)<-c('IdOrig_RAM','Dbase','Country_RAM','CommName_RAM','SciName','RegionFAO','CatchShare_RAM','IdOrig_FAO','Country_FAO','CommName_FAO','Catch','CatchShare_FAO')
  
  CSpercs<-ddply(matchM,c('IdOrig_RAM','CommName_RAM','CatchShare_FAO'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  CSpercs<-ddply(CSpercs,c('IdOrig_RAM'),mutate,AllCatch=sum(TotalCatch,na.rm=T),Perc=100*(TotalCatch/AllCatch))
  
  CSpercs$CatchShare_Final<-NA
  
  CSpercs$CatchShare_Final[CSpercs$CatchShare_FAO==1 & CSpercs$Perc>CatchSharePercent]<-1
  
  CSpercs$CatchShare_Final[CSpercs$CatchShare_FAO==1 & CSpercs$Perc<CatchSharePercent]<-0
  
  CSpercs$CatchShare_Final[CSpercs$CatchShare_FAO==0]<-0
  
  ### Consolidate list of matched fao, RAM and multinational RAM stocks to list as Catch Shares--------------------------------------------------
  
  ## Add ids from matchF, matchR and CSpercs
  allCSmatch<-c(faoCSids,unique(matchR$IdOrig),CSpercs$IdOrig_RAM[CSpercs$CatchShare_Final==1])
  
  allCSmatch<-unique(allCSmatch)
  
  ## Check and plot amount of catch labeled as catch share based on FAO matches------------------------------------------------------------------
  
  # Global level
  check<-ddply(fao2,c('CatchShare'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  check<-ddply(check,c('CatchShare'),mutate,AllCatch=sum(check$TotalCatch,na.rm=T),Perc=100*TotalCatch/AllCatch)
  
  # pdf(file='Global Catch Share Fractions.pdf',width=8,height=11)
  # 
  # ggplot(check,aes(x=as.factor(CatchShare),y=Perc)) +
  #   geom_bar(stat='identity') +
  #   labs(x='Use of Catch Shares',y='Estimated Fraction of Current Global Harvest',title='Global Use of Catch Shares') +
  #   theme(
  #     axis.text.x=element_text(angle=60,hjust=1),
  #     text=element_text(size=12))
  # 
  # dev.off()
  
  # Country level
  check2<-ddply(fao2,c('Country','CatchShare'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  check2<-ddply(check2,c('Country'),mutate,AllCatch=sum(TotalCatch,na.rm=T))
  
  check2<-ddply(check2,c('Country','CatchShare'),mutate,Perc=100*TotalCatch/AllCatch)
  
  Countries<-c('Argentina','Canada','Chile','China','Denmark','Iceland','India','Indonesia','Japan','Malaysia','Mexico','Morocco','Namibia','Norway','Peru','Philippines',
               'Republic of Korea','Russian Federation','Senegal','South Africa','Spain','Thailand','United Kingdom','USA','Viet Nam','New Zealand')
  
  check2<-subset(check2,Country %in% Countries)
  
  # pdf(file='Catch Share Fractions.pdf',width=8,height=11)
  # 
  # ggplot(check2,aes(x=Country,y=Perc,fill=as.factor(CatchShare))) +
  #   geom_bar(stat='identity') +
  #   labs(fill='Use of Catch Shares',x='Country',y='Estimated Fraction of Current Harvest\n Managed with Catch Shares',title='Breakdown of Current Assessments and Approaches to Management') +
  #   coord_flip() +
  #   theme(
  #     axis.text.x=element_text(angle=60,hjust=1),
  #     text=element_text(size=12))
  # 
  # dev.off()
  
  return(ListOfCatchShares=allCSmatch)

}


