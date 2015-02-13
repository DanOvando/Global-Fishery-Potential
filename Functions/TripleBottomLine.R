
# AllStockData<-CountryUpsidesAllSQ
# OverfishData<-CountryUpsidesOverfishAllSQ
# Policy<-'Opt'
# Limit<-300
# StatusQuoPolicy<-'StatusQuoOpenAccess'

TripleBottomLine<-function(AllStockData,OverfishData,Policy,Limit,StatusQuoPolicy,FigureFolder)
{
  ## Rename AllStockData and OverfishData to allow for passing lumped and unlumped upside data to the function

  UpsidesAllStocks<-AllStockData
  
  UpsidesOverfish<-OverfishData
  
  ## Subset upside data to not include country groups-------------------------------
  
  AllData<-UpsidesAllStocks[UpsidesAllStocks$Policy==Policy & UpsidesAllStocks$Denominator==StatusQuoPolicy & !(UpsidesAllStocks$Country %in% c('Global','EU','Asia','Multinational','Lumped')),]
  
  OverfishedData<-UpsidesOverfish[UpsidesOverfish$Policy==Policy & UpsidesOverfish$Denominator==StatusQuoPolicy & !(UpsidesOverfish$Country %in% c('Global','EU','Asia','Multinational','Lumped')),]
  
  ## Cap extremely high/low results--------------------------------------------------------
  
  # Percents
  AllData$PercChangeFromSQTotalCatch[AllData$PercChangeFromSQTotalCatch>200]<-200
  AllData$PercChangeFromSQTotalCatch[AllData$PercChangeFromSQTotalCatch<(-Limit)]<-(-Limit)
  
  AllData$PercChangeFromSQTotalBiomass[AllData$PercChangeFromSQTotalBiomass>Limit]<-Limit
  AllData$PercChangeFromSQTotalBiomass[AllData$PercChangeFromSQTotalBiomass<(-Limit)]<-(-Limit)
  
  AllData$PercChangeFromSQNPV[AllData$PercChangeFromSQNPV>Limit]<-Limit
  AllData$PercChangeFromSQNPV[AllData$PercChangeFromSQNPV<(-Limit)]<-(-Limit)
  
  
  OverfishedData$PercChangeFromSQTotalCatch[OverfishedData$PercChangeFromSQTotalCatch>Limit]<-Limit
  OverfishedData$PercChangeFromSQTotalCatch[OverfishedData$PercChangeFromSQTotalCatch<(-Limit)]<-(-Limit)
  
  OverfishedData$PercChangeFromSQTotalBiomass[OverfishedData$PercChangeFromSQTotalBiomass>Limit]<-Limit
  OverfishedData$PercChangeFromSQTotalBiomass[OverfishedData$PercChangeFromSQTotalBiomass<(-Limit)]<-(-Limit)
  
  OverfishedData$PercChangeFromSQNPV[OverfishedData$PercChangeFromSQNPV>Limit]<-Limit
  OverfishedData$PercChangeFromSQNPV[OverfishedData$PercChangeFromSQNPV<(-Limit)]<-(-Limit)
  
  # Absolutes
  AllData$AbsChangeFromSQTotalCatch[AllData$AbsChangeFromSQTotalCatch>2000000]<-2000000
  AllData$AbsChangeFromSQTotalBiomass[AllData$AbsChangeFromSQTotalBiomass>20000000]<-20000000
  
  OverfishedData$AbsChangeFromSQTotalCatch[OverfishedData$AbsChangeFromSQTotalCatch>Limit]<-2000000
  OverfishedData$AbsChangeFromSQTotalBiomass[OverfishedData$AbsChangeFromSQTotalBiomass>Limit]<-20000000
  
  
  ## Combine % upsides and absolute upsides into separate dataframes and melt for plotting-------------------------------
  
  # Percentages
  
  AllDataPercs<-AllData[,c('Country','Subset','PercChangeFromSQTotalCatch','PercChangeFromSQTotalBiomass','PercChangeFromSQNPV',
                           'PercChangeTotalCatch','PercChangeTotalBiomass','PercChangeTotalProfits')]
  
  OverfishPercs<-OverfishedData[,c('Country','Subset','PercChangeFromSQTotalCatch','PercChangeFromSQTotalBiomass','PercChangeFromSQNPV',
                                 'PercChangeTotalCatch','PercChangeTotalBiomass','PercChangeTotalProfits')]
  
  Percs<-rbind(AllDataPercs,OverfishPercs)
  
  # Absolutes
  
  AllDataAbs<-AllData[,c('Country','Subset','AbsChangeFromSQTotalCatch','AbsChangeFromSQTotalBiomass','AbsChangeFromSQNPV',
                         'AbsChangeTotalCatch','AbsChangeTotalBiomass','AbsChangeTotalProfits')]
  
  OverfishAbs<-OverfishedData[,c('Country','Subset','AbsChangeFromSQTotalCatch','AbsChangeFromSQTotalBiomass','AbsChangeFromSQNPV',
                               'AbsChangeTotalCatch','AbsChangeTotalBiomass','AbsChangeTotalProfits')]
  
  Abs<-rbind(AllDataAbs,OverfishAbs)
  
  ### Plot results relative to Status Quo in 4 panel figure ----------------------------------
  
  # Percent
panelA<-ggplot(Percs,aes(x=PercChangeFromSQTotalBiomass,PercChangeFromSQTotalCatch,size=PercChangeFromSQNPV)) +
    geom_point(aes(color=Subset),alpha=0.6) +
    guides(color=F) +
    scale_color_manual(values=c('blue','red')) +
    geom_abline(intercept=0,slope=0) +
    geom_vline(xintercept=0) +
    facet_wrap(~Subset) +
    theme(text=element_text(size=18)) +
    labs( x = 'Percent Change from Status Quo Biomass',
          y ="Percent Change from Status Quo Catch",size='Percent Change from\n Status Quo NPV')
  
  
  # Absolutes
panelB<-ggplot(Abs,aes(x=AbsChangeFromSQTotalBiomass,y=AbsChangeFromSQTotalCatch,size=AbsChangeFromSQNPV)) +
  geom_point(aes(color=Subset),alpha=0.6) +
  scale_size(range=c(2,8)) +
  guides(color=F) +
  scale_color_manual(values=c('blue','red')) +
  geom_abline(intercept=0,slope=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~Subset) +
  theme(text=element_text(size=18)) +
  labs( x = 'Absolute Change from Status Quo Biomass',
        y ="Absolute Change from Status Quo Catch",size='Absolute Change from\n Status Quo NPV')


  # Arrange plots in grid and PDF

  pdf(file=paste(FigureFolder,'Triple Bottom Line for ',Policy,' Relative to ',StatusQuoPolicy,'.pdf',sep=''),height=10,width=16,pointsize=6)

  print(grid.arrange(arrangeGrob(panelA,panelB,ncol=1)))  

  dev.off()
  
  ### Plot results relative to Current in 4 panel figure ----------------------------------

# Percent
panelC<-ggplot(Percs,aes(x=PercChangeTotalBiomass,PercChangeTotalCatch,size=PercChangeTotalProfits)) +
  geom_point(aes(color=Subset),alpha=0.6) +
  guides(color=F) +
  scale_color_manual(values=c('blue','red')) +
  geom_abline(intercept=0,slope=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~Subset) +
  theme(text=element_text(size=18)) +
  labs( x = 'Percent Change from Current Biomass',
        y ="Percent Change from Current Catch",size='Percent Change from\n Current Profits')


# Absolutes
panelD<-ggplot(Abs,aes(x=AbsChangeTotalBiomass,y=AbsChangeTotalCatch,size=AbsChangeTotalProfits)) +
  geom_point(aes(color=Subset),alpha=0.6) +
  scale_size(range=c(2,8)) +
  guides(color=F) +
  scale_color_manual(values=c('blue','red')) +
  geom_abline(intercept=0,slope=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~Subset) +
  theme(text=element_text(size=18)) +
  labs( x = 'Absolute Change from Current Biomass',
        y ="Absolute Change from Current Catch",size='Absolute Change from\n Current NPV')

  # Arrange plots in grid and PDF
  
  pdf(file=paste(FigureFolder,'Triple Bottom Line for ',Policy,' Relative to Current','.pdf',sep=''),height=10,width=16,pointsize=6)
  
  print(grid.arrange(arrangeGrob(panelC,panelD,ncol=1)))  
  
  dev.off()

  # Plot percentage and absolute results for All stocks

panelE<-ggplot(AllData,aes(x=AbsChangeFromSQTotalBiomass,y=AbsChangeFromSQTotalCatch,size=AbsChangeFromSQNPV)) +
  geom_point(aes(color=Subset),alpha=0.6) +
  scale_size(range=c(2,8)) +
  guides(color=F) +
  scale_color_manual(values=c('blue','red')) +
  geom_abline(intercept=0,slope=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~Subset) +
  theme(text=element_text(size=18)) +
  labs( x = 'Absolute Change from Status Quo Biomass',
        y ="Absolute Change from Status Quo Catch",size='Absolute Change from\n Status Quo NPV')

panelF<-ggplot(AllData,aes(x=PercChangeFromSQTotalBiomass,PercChangeFromSQTotalCatch,size=PercChangeFromSQNPV)) +
  geom_point(aes(color=Subset),alpha=0.6) +
  guides(color=F) +
  scale_color_manual(values=c('blue','red')) +
  geom_abline(intercept=0,slope=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~Subset) +
  theme(text=element_text(size=18)) +
  labs( x = 'Percent Change from Status Quo Biomass',
        y ="Percent Change from Status Quo Catch",size='Percent Change from\n Status Quo NPV')

# Arrange plots in grid and PDF

# pdf(file=paste(FigureFolder,'Triple Bottom Line for ',Policy,' Relative to Open Access','.pdf',sep=''),height=10,width=10,pointsize=6)
# 
# print(grid.arrange(arrangeGrob(panelF,panelE,ncol=1,as.table=T)))  
# 
# dev.off()
# 
# 
# pdf(file=paste(FigureFolder,'Triple Bottom Line Percents for ',Policy,' Relative to Open Access','.pdf',sep=''),height=8,width=10,pointsize=6)
# print(panelE)  
# dev.off()
# 
# pdf(file=paste(FigureFolder,'Triple Bottom Line Absolutes for ',Policy,' Relative to Open Access','.pdf',sep=''),height=8,width=10,pointsize=6)
# print(panelF)  
# dev.off()

# Calculate Absolute upsides of overfish recovery as a percentage of all stocks

  return()
}