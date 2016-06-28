###############################################
##
## Function to re-distribute benefits from RAM
## multinational stocks
##
###############################################
# 
# Data<-UnlumpedProjectionData
# YearsBack<-4

DivyMultinational<-function(Data,RawData,BaselineYear,YearsBack)
{
  
  ### Subset to multinational stocks and raw FAO data in desired years------------------------------
  
  # years to evaluate FAO data
  RefYrs<-c((BaselineYear-YearsBack):BaselineYear)
  
  # pull out raw FAO data
  f<-RawData[RawData$Dbase=='FAO' & (RawData$Year %in% RefYrs),]
  
  # adjust SciNames of certain RAM stocks to match FAO
  Data<-RamSciNameAdjuster(Data,VersionToUse='SciNameToUse') 
  
  # Subset Unlumped ProjectionData to Multinational stocks
  multi<-Data[Data$Dbase=='RAM' & Data$Country=='Multinational',]
  
  # remove tuna and billfish RAM multinational stocks and leave as is
  Tunas<-multi[multi$SpeciesCatName=='Tunas, bonitos, billfishes',]
  
  Tunas$Country<-'High Seas Tuna and Billfish'
  
  multi<-multi[!(multi$SpeciesCatName=='Tunas, bonitos, billfishes'),]
  
  # Summary table of multinational stocks to reference
  multiRef<-unique(multi[c('IdOrig','CommName','SciName','SpeciesCatName','RegionFAO')])
  
  # unique multinational ids to loop over and redistribute results
  MultiIds<-unique(multiRef$IdOrig)
  
  ### Loop over Multinational stocks----------------------------------------------
  
  # Create empty list to store new multinational stocks
  
  DivyMulti<-list()
  
  # For each stock: 
  # 1) find all countries fishing that species in the same FAO region in raw FAO data
  # 2) calculate total catch of species and the percent contributed by each country in raw FAO data
  # 3) use percentage
  
  for(h in 1:length(MultiIds))
  {
    # SciName of stock
    sci<-multiRef$SciName[multiRef$IdOrig==MultiIds[h]]
    
    # Fao region (unlist in case there are multiple)
    regs<-unlist(str_split(multiRef$RegionFAO[multiRef$IdOrig==MultiIds[h]],pattern=','))
    
    # subset ProjectionData for that Multinational stock 
    tempProj<-Data[Data$IdOrig==MultiIds[h],]
    
    # Subset fao to sci name and regions that match multinational stock
    tempF<-f[f$SciName==sci & (f$RegionFAO %in% regs) & is.na(f$Catch)==F,
             c('IdOrig','Country','Year','SciName','SpeciesCatName','Catch')]
    
    if(nrow(tempF)>0)
    {
      
    cntrys<- tempF %>%
      group_by(Country) %>%
      summarize(TotalCatch=sum(Catch,na.rm=T))
      
#       ddply(tempF,c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T))
    
#     cntrys<-ddply(tempF,c('Country'),summarize,TotalCatch=sum(Catch,na.rm=T))
    
    
        cntrys$PercOfTotal<-cntrys$TotalCatch/sum(cntrys$TotalCatch,na.rm=T)
    
    cntryIds<-unique(cntrys$Country)
    
    # create empty dataframe to fill
    NewMulti<-data.frame(matrix(nrow=0,ncol=ncol(tempProj)))
    
    colnames(NewMulti)<-colnames(tempProj)
    
    # loop over countries 
    for(d in 1:length(cntryIds))
    {
      # calculate projection data results for projected policies
      unlumpMulti<-tempProj
      
      unlumpMulti$Country<-cntryIds[d]
      
      unlumpMulti$IdOrig<-paste(MultiIds[h],cntryIds[d],sep='_')
      
      unlumpMulti$Catch<-unlumpMulti$Catch*cntrys$PercOfTotal[cntrys$Country==cntryIds[d]]
      
      unlumpMulti$Biomass<-unlumpMulti$Biomass*cntrys$PercOfTotal[cntrys$Country==cntryIds[d]]
      
      unlumpMulti$MSY<-unlumpMulti$MSY*cntrys$PercOfTotal[cntrys$Country==cntryIds[d]]
      
      unlumpMulti$k<-unlumpMulti$k*cntrys$PercOfTotal[cntrys$Country==cntryIds[d]]
      
      unlumpMulti$Profits<-unlumpMulti$Profits*cntrys$PercOfTotal[cntrys$Country==cntryIds[d]]
      
      unlumpMulti$DiscProfits<-unlumpMulti$DiscProfits*cntrys$PercOfTotal[cntrys$Country==cntryIds[d]]
      
      NewMulti<-bind_rows(NewMulti,unlumpMulti)
      
      } # close country loop
    
    DivyMulti[[h]]<-NewMulti
    
#     show(h)
    
    } # close if statement for if fao stocks exist
    
    # if can't find data for RAM stock in FAO (unknown species), add unchanged Projection data back for that stock 
    if(nrow(tempF)==0)
    {
      DivyMulti[[h]]<-tempProj
      
#       print('No matching FAO country stocks')
#       
#       show(h)
    }
    
  } # close Multinational stock loop
  
  # flatten list
  DivyMulti<- bind_rows(DivyMulti)
  
  # add tuna stocks back in
  DivyMultiFinal<-bind_rows(Tunas,DivyMulti)
  
  # Remove multinational from original UnlumpedProjectionData and add in new DivyMulti
  FinalData<-Data[!(Data$Dbase=='RAM' & Data$Country=='Multinational'),]

  FinalData<-bind_rows(FinalData,DivyMultiFinal)
  
  return(FinalData)
}
  
  