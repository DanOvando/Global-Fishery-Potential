
CountryCatch<-function(Country,MinYr,MaxYr){

  ######################
  ## Function to calculate several catch statistics from FAO data for a given country
  ######################
  
  # Country = Country to run analysis on
  # MinYr/MaxYr = bounds on how number of catch years
  
  CountryData<-fao[grepl(Country,fao$Country),]
  show(paste(length(unique(CountryData$IdOrig)),"FAO Fisheries"))
  show(paste(length(unique(CountryData$SciName)),"Scientific Names"))
  
  CountryData$Type[grepl("nei",CountryData$CommName)]<-"NEI"
  CountryData$Type[!grepl("nei",CountryData$CommName)]<-"Non NEI"
  
  CatchPercentages<- CountryData %>% 
    group_by(Type,Year) %>% 
    summarize(TotalCatch=sum(Catch,na.rm=T))
  
  TotalCatch<- CountryData %>% 
    group_by(Year) %>% 
    summarize(TotalCatch=sum(Catch,na.rm=T))
  
  for(i in 1:nrow(CatchPercentages))
  {
    CatchPercentages$PercentCatch[i]<-CatchPercentages$TotalCatch[i]/TotalCatch[TotalCatch$Year==CatchPercentages$Year[i],2]
  }
  
  show(paste(CatchPercentages$PercentCatch[CatchPercentages$Year==2011 & CatchPercentages$Type=="NEI"]*100,"% of 2011 Catch from nei fisheries"))
  
  CountryPlot<-ggplot(CatchPercentages,aes(x=Year,y=PercentCatch*100, colour=Type))+geom_line()
  print(CountryPlot)
  
  CountrySummary<- CountryData %>% 
    group_by(IdOrig,CommName) %>% 
    summarize(TotalCatch=sum(Catch,na.rm=T),CatchYears=sum(is.na(Catch)==F))
  
  show(paste(length(CountrySummary$CatchYears[CountrySummary$CatchYears>MinYr & CountrySummary$CatchYears<MaxYr ]),"fisheries with between 7-9 years of catch data")) 
  
  ids<-unique(CountrySummary$IdOrig[CountrySummary$CatchYears>MinYr & CountrySummary$CatchYears<MaxYr ]) # Ids of new fisheries
  
  Results<-list(CountrySummary,CatchPercentages)
  
  if(length(ids)==0) {return(Results)} else{
    
    Country7to9<-subset(CountryData,(IdOrig %in% ids))
    
    Country7to9<-Country7to9[is.na(Country7to9$Catch)==F,] # subset only the years with catch
    
    CountrySummary7to9<- Country7to9 %>% 
      group_by(IdOrig,CommName) %>% 
      summarize(TotalCatch=sum(Catch,na.rm=T),MeanYear=mean(Year,na.rm=T))
    # Mean Year shows how recently the catch began
    
    # compare total catch of nei fisheries to non nei fisheries in new data
    CountrySummary7to9$Type[grepl("nei",CountrySummary7to9$CommName)]<-"NEI"
    CountrySummary7to9$Type[!grepl("nei",CountrySummary7to9$CommName)]<-"Non NEI"
    
    Catch7to9<- CountrySummary7to9 %>% 
      group_by(Type) %>% 
      summarize(TotalCatch=sum(TotalCatch,na.rm=T),PercentTotal=(sum(TotalCatch,na.rm=T)/sum(CountrySummary7to9$TotalCatch,na.rm=T)*100),Fisheries=length(unique(IdOrig)))
    # number and percent catch of nei and non nei fisheries in recent data
    
    Country2011<-CountryData[CountryData$Year==2011,]
    Country2011$New<-"Old Stock"
    Country2011$New[(Country2011$IdOrig %in% ids)]<-"New Stock"
    Country2011$Type[grepl("nei",Country2011$CommName)]<-"NEI"
    Country2011$Type[!grepl("nei",Country2011$CommName)]<-"Non NEI"
    
    RecentPercent<- Country2011 %>% 
      group_by(New) %>% 
      summarize(TotalCatch=sum(Catch,na.rm=T),PercentOfTotal=sum(Catch,na.rm=T)/sum(Country2011$Catch,na.rm=T))
    show(paste(RecentPercent$PercentOfTotal[1]*100,"% of 2011 Catch from short short or recent fisheries"))
    
    Results$CountryData<-CountryData
    Results$CountrySummary7to9<-CountrySummary7to9
    Results$Country2011<-Country2011
    Results$RecentPercent<-RecentPercent
    return(Results)
  } # close else
} # close function
##############