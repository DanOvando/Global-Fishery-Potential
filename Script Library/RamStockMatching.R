###########################################
##
## This function matches RAM stocks to FAO
## stocks for comparing upside results to
## governance scores
##
###########################################

# Data<-RawData
# ProjData<-ProjectionData

TEST<-function(Data)
{
  library(plyr)
  library(dplyr)
  library(stringr)
  
  Data<-tbl_df(Data)
  
  # Subset data for RAM and FAO
  Data<- Data %>%
    filter(Dbase %in% c('RAM','FAO')) %>%
    select(Dbase,IdOrig,Country,RegionFAO,SciName,CommName,Year,Catch)
    
  fao<- Data %>%
    filter(Dbase=='FAO',is.na(Catch)==F,Year==2012)
  
  fao<-unique(fao[c('Dbase','IdOrig','Country','RegionFAO','SciName','CommName')])
    
  ram<- Data %>%
    filter(Dbase=='RAM')
  
  ram<-ram[ram$IdOrig %in% ProjData$IdOrig,]
  
  ram<-unique(ram[c('Dbase','IdOrig','Country','RegionFAO','SciName','CommName')])
  
  ram<-RamSciNameAdjuster(ram,VersionToUse='SciNameToUse')
  
  ram<-ram[with(ram,order(Country)),]
    
  # for ram create duplicate stocks for every fao potential fao region for that stock
  
  ramstocks<-unique(ram$IdOrig)
  
  ram2<-list()
  
  for(a in 1:length(ramstocks))
  {
    temp<- ram %>%
      filter(IdOrig==ramstocks[a])
    
    RegionFAO<-unlist(strsplit(ram$RegionFAO[a],split=",",fixed=T)) # split apart FAO regions
    num<-length(RegionFAO) # how many?
    
    if(num==1){ram2[[a]]<-temp}
    
    if(num>0){
      
      Dbase<-rep(as.character(ram$Dbase[a],num))
      IdOrig<-rep(as.character(ram$IdOrig[a],num)) 
      SciName<-rep(as.character(ram$SciName[a],num)) # duplicate info for all columns 
      Country<-rep(ram$Country[a],num)
      CommName<-rep(as.character(ram$CommName[a],num))
      
      ram2[[a]]<-data_frame(Dbase,IdOrig,Country,RegionFAO,SciName,CommName) # make new data frame with unique rows for each country in the assessment    
      
    }# close if statement
    show(a)
  }
  
  ram2<-ldply(ram2)
  
  ram2<-tbl_df(ram2)
  
  # Match RAM single region stocks to FAO
  match<-inner_join(ram2,fao,by=c('Country','RegionFAO','SciName')) %>%
    rename(IdOrig_RAM=IdOrig.x, IdOrig_FAO=IdOrig.y,Country_RAM=Country,CommName_RAM=CommName.x,CommName_FAO=CommName.y) %>%
    mutate(Country_FAO=Country_RAM)
  
  # Match RAM multinational stocks 
  multi<-ram2[!(ram2$IdOrig %in% match$IdOrig_RAM),]  
  
  multi<- multi %>%
    filter(Country=='Multinational')
  
  match2<-inner_join(multi,fao,by=c('RegionFAO','SciName')) %>%
    rename(IdOrig_RAM=IdOrig.x, IdOrig_FAO=IdOrig.y,Country_RAM=Country.x,CommName_RAM=CommName.x,CommName_FAO=CommName.y,Country_FAO=Country.y) 
  
  ram2$IdOrig_Unlumped<-NA
  
  
  
  for(b in 1:length(ramstocks))
  {
    match<-grepl(ramstocks[b],ramU$IdOrig_RamU)
    
    if(length(unique(match))==2)
    {
      ramU$IdOrig[match]<-ramstocks[b]
    }
    show(b)
  }
  
  # Join country matches and multinational matches
  match3<-rbind(match,match2)
  
  # Find stocks still missing
  missids<-ramstocks[!(ramstocks %in% match3$IdOrig_RAM)]
  
  miss<-ram2[(ram2$IdOrig %in% missids),]
  
  # On Unlumped results
  ramU<-unique(UnlumpedProjectionData[UnlumpedProjectionData$Dbase=='RAM' & UnlumpedProjectionData$Year==2012,c('IdOrig','Country','CommName','SciName','RegionFAO')])
  
  ramU<- ramU %>%
    tbl_df() %>%
    rename(IdOrig_RamU=IdOrig)
  
  ramU$IdOrig<-NA
  
  for(b in 1:length(ramstocks))
  {
    match<-grepl(ramstocks[b],ramU$IdOrig_RamU)
    
    if(length(unique(match))==2)
    {
      ramU$IdOrig[match]<-ramstocks[b]
    }
    show(b)
  }
  
  ramU<-RamSciNameAdjuster(ramU,VersionToUse='SciNameToUse')
  
  ramUmatch<-inner_join(ramU,fao,by=c('Country','RegionFAO','SciName')) %>%
    rename(IdOrig_RAM=IdOrig.x, IdOrig_FAO=IdOrig.y,CommName_RAM=CommName.x,CommName_FAO=CommName.y)
  
  test<-inner_join(ram2,ramU,by=c('Country','RegionFAO','SciName'))
  
  # Write csv's with results
  
  write.csv(file='RAM Country Level Matches.csv',match)
  
  write.csv(file='RAM Multinational Level Matches.csv',match2)
  
  write.csv(file='RAM FAO Matches.csv',match3)
  
  write.csv(file='RAM Stocks Without Exact Matches.csv',miss)
  
  write.csv(file='Matched Unlumped RAM and FAO Stocks.csv',ramUmatch)
  
  length(unique(match$IdOrig_RAM))
  length(unique(match2$IdOrig_RAM))
  length(unique(miss$IdOrig))
  length(unique(ramUmatch$IdOrig_RAM))
  length(unique(ramU$IdOrig))
  
  
}
  