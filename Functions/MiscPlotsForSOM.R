
PlotsForSOM<-function(RawData,FullData,UnlumpedProjectionData,UnlumpedUpsidesAllStocks)
{
  
  dbase<- RawData %>% 
    filter(Year>=1950 & Year<2013) %>% 
    group_by(Year,Dbase) %>% 
    summarize(TotalCatch=sum(Catch,na.rm=T))
  
  fdbase<-ddply(FullData[FullData$Year>=1950 & FullData$Year<2013,],c('Year','Dbase'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  dbase$Dbase<-factor(dbase$Dbase,levels=c('FAO','SOFIA','RAM'))
  
  # Total Catch Raw Database
  
  pdf(file=paste(FigureFolder,'Raw Database Total Catch.pdf',sep=''))
  
  print(ggplot(dbase,aes(x=Year,y=TotalCatch,color=Dbase)) +
    geom_ribbon(aes(ymin=0,ymax=TotalCatch,fill=Dbase),alpha=0.4) +
    geom_line(lwd=2) +
    guides(color=F) +
    theme(text=element_text(size=18)) +
    labs(x='Year',y='Total Catch (MT)',fill='Database'))
  
  dev.off()
  
  # Id Level of Catch FAO data
 
  isscaap<-ddply(RawData[RawData$Dbase=='FAO',],c('Year','SpeciesCatName'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  print(ggplot(isscaap,aes(x=Year,y=TotalCatch,color=SpeciesCatName)) +
          geom_ribbon(aes(ymin=0,ymax=TotalCatch,fill=SpeciesCatName),alpha=0.4) +
          geom_line(lwd=2) +
          guides(color=F) +
          theme(text=element_text(size=18)) +
          labs(x='Year',y='Total Catch (MT)',fill='Database'))
  
  ggplot(dbase,aes(x=Year,y=TotalCatch,color=Dbase)) +
    geom_ribbon(aes(ymin=0,ymax=TotalCatch,fill=Dbase),alpha=0.4) +
    geom_line(lwd=2) +
    theme(text=element_text(size=18)) +
    labs(x='Year',y='Total Catch (MT)')
  
  # Barplots of Upside by ISSCAAP category for Catch Share Three and Fmsy Three
  
  FisheryUpsides<-UnlumpedUpsideAllStocks$FisheryUpside
  
  spcats<-unique(UnlumpedProjectionData[,c('IdOrig','SpeciesCatName')])
  
  FisheryUpsides<-join(FisheryUpsides,spcats,by=c('IdOrig'),type='left',match='first')
  
  pie<-ddply(FisheryUpsides[FisheryUpsides$Year==2050,],c('Policy','SpeciesCatName'),summarize,NPVUpside=sum(AbsChangeFromSQNPV,na.rm=T),
             BiomassUpside=sum(AbsChangeFromSQTotalBiomass,na.rm=T),HarvestUpside=sum(AbsChangeFromSQTotalCatch,na.rm=T))
  
  pie<-melt(pie,measure.vars=c('NPVUpside','BiomassUpside','HarvestUpside'))
  
  pdf(file=paste(FigureFolder,'ISSCAAP Upside Barplots.pdf'),height=8.5,width=17)

  print(ggplot(data=pie[pie$Policy %in% c('Catch Share Three','Fmsy Three'),], aes(x=factor(Policy),y=value,fill = factor(SpeciesCatName))) +
    geom_bar(stat='identity',width=1) +
    facet_wrap(~variable,scales='free') +
    labs(x='Policy',y='Absolute Upside Relative to BAU',title='Absolute Upsides Relative to BAU by Species Category',fill='Species Category') +
    theme(text=element_text(size=16),legend.text=element_text(size=10),panel.margin=unit(1,'cm')))
  
  dev.off()
  
  return()
}