CheckLumping<- function(ProjectionData,UnlumpedProjectionData,BatchFolder)
{
  
  DiagnosticFolder<- paste(BatchFolder,'Diagnostics/Unlumping/',sep='')
  
  dir.create(DiagnosticFolder,recursive=T)
  
  FinalP<- subset(ProjectionData,Year==max(Year))
  
  FinalUP<- subset(UnlumpedProjectionData,Year==max(Year))
  
  FinalP$Lumping<- 'Lumped'
  
  FinalUP$Lumping<- 'Unlumped'
  
  TUP<- rbind(FinalP,FinalUP)
  
  Summary<- ddply(TUP,c('Lumping','Policy'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                  TotalProfits=sum(Profits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianB=median(BvBmsy,na.rm=T),
                  MedianF=median(FvFmsy,na.rm=T))
  
  pdf(paste(DiagnosticFolder,'Unlumping Effects.pdf',sep=''))
  
  print(ggplot(data=Summary,aes(x=Lumping,y=TotalCatch))+geom_bar(stat='identity',position='dodge')+facet_wrap(~Policy))
  
  print(ggplot(data=Summary,aes(x=Lumping,y=TotalMSY))+geom_bar(stat='identity',position='dodge')+facet_wrap(~Policy))
  
  print(ggplot(data=Summary,aes(x=Lumping,y=TotalProfits))+geom_bar(stat='identity',position='dodge')+facet_wrap(~Policy))
  
  print(ggplot(data=Summary,aes(x=Lumping,y=TotalBiomass))+geom_bar(stat='identity',position='dodge')+facet_wrap(~Policy))
  
  print(ggplot(data=Summary,aes(x=Lumping,y=MedianB))+geom_bar(stat='identity',position='dodge')+facet_wrap(~Policy))
  
  print(ggplot(data=Summary,aes(x=Lumping,y=MedianF))+geom_bar(stat='identity',position='dodge')+facet_wrap(~Policy))
  
  dev.off()
  
  CountrySummary<- ddply(subset(TUP,Policy=='Catch Share Three'),c('Lumping','Country'),summarize,TotalCatch=sum(Catch,na.rm=T),TotalMSY=sum(MSY,na.rm=T),
                         TotalProfits=sum(Profits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianB=median(BvBmsy,na.rm=T),
                         MedianF=median(FvFmsy,na.rm=T))
  
  CountryDiff<- ddply(subset(TUP,Policy=='Catch Share Three'),c('Country'),summarize,LumpedCatch=sum(Catch[Lumping=='Lumped'],na.rm=T)
                      ,UnlumpedCatch=sum(Catch[Lumping=='Unlumped'],na.rm=T))
  
  CountryDiff$LumpChange=100*((CountryDiff$UnlumpedCatch-CountryDiff$LumpedCatch)/CountryDiff$LumpedCatch)
  
  
  CountryDiff$LumpChange[CountryDiff$LumpChange>200]<- 200
  
  CountryDiff$Country<- as.factor(CountryDiff$Country)
  
  CountryDiff$Country <- reorder(CountryDiff$Country, CountryDiff$LumpChange)
  
  
  pdf(paste(DiagnosticFolder,'Country Changes from Unlumping.pdf',sep=''),width=12,height=6)
  print(ggplot(data=CountryDiff,aes(x=Country,y=LumpChange))+geom_bar(stat='identity',position='dodge')+
          theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9,size=4)))
  dev.off()
  
}



