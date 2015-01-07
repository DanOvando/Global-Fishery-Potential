##############################################
##
## Code to plot boxplots of status by ISSCAAP
## group through time and distributions of 
## status for RAM and unassessed
##
##############################################

# Pass BiomassData
# Loop over categories and run through Analyze Fisheries
# Save timeseries of median estimates
# Bind together and plot historical timeseries

# Data<-BiomassData

StatusPlots<-function(FullData,BiomassData,BaselineYear,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
{
  
  source('Functions/AnalyzeFisheries.R')
  
  cats<-unique(BiomassData$SpeciesCatName)
  
  for (a in 1:length(cats))
  {
    show(a)
    
    temp<-BiomassData[BiomassData$SpeciesCatName==cats[a],]
    
    tempStatus<-AnalyzeFisheries(BiomassData[BiomassData$SpeciesCatName==cats[a],],paste(cats[a],' Status',sep=''),'Year',
                                 1990:BaselineYear,RealModelSdevs,NeiModelSdevs,TransbiasBin,TransbiasIterations)
    
    status<-ddply(tempStatus$Data,c('SpeciesCatName','Year'),summarize,MedianStatus=median(BvBmsy,na.rm=T))
    
    if(a==1)
    {
      Status<-status
      AllStockStatus<-tempStatus$Data[,c('Year','SpeciesCatName','BvBmsy','Dbase')]
    }
    if(a>1)
    {
      Status<-rbind(Status,status)
      AllStockStatus<-rbind(AllStockStatus,tempStatus$Data[,c('Year','SpeciesCatName','BvBmsy','Dbase')])
    }
    
  } # close loop
  
  # plot median estimates for RAM and Unassessed through time
  Status$Dbase<-'Unassessed'
  
  ramStatus<-ddply(FullData[FullData$Dbase=='RAM' & FullData$Year>=1990,],c('SpeciesCatName','Year','Dbase'),
                            summarize,MedianStatus=median(BvBmsy,na.rm=T))
  
  ramcats<-unique(ramStatus$SpeciesCatName)
  
  ramStatus<-rbind(ramStatus,Status)
  
  ramStatus<-ramStatus[ramStatus$SpeciesCatName %in% ramcats,]
  
  pdf(file=paste(FigureFolder,'Median Status Comparison RAM and Unassessed.pdf',sep=''),width=16,height=10) 
  print(ggplot(ramStatus,aes(Year,MedianStatus,color=Dbase)) +
    geom_line() +
    facet_wrap(~SpeciesCatName,scales='free') + 
    geom_abline(intercept=1,slope=0) + 
    labs(x='Year',y='Median Status',title='Median Status Estimates from RAM and Costello et al.'))
  dev.off()
  
  # plot distribution of ram estimates and unassessed 
  AllStockStatus$Dbase[AllStockStatus$Dbase=='FAO' | AllStockStatus$Dbase=='SOFIA']<-'Unassessed'
  
  AllStockStatus<-AllStockStatus[AllStockStatus$SpeciesCatName %in% ramcats,]
  
  pdf(file=paste(FigureFolder,'Status Distribution Comparison RAM and Unassessed.pdf',sep=''),width=16,height=10) 
  print(ggplot(AllStockStatus,aes(BvBmsy,fill=Dbase)) +
    geom_density(alpha=0.6) +
    facet_wrap(~SpeciesCatName,scales='free') +
    geom_vline(xintercept=1) +
    labs(x='B/Bmsy',y='Density',title='Distribution of B/Bmsy for RAM and Species Level Unassessed Stocks'))
  dev.off()
  
  # boxplots of unassessed status over time
  pdf(file=paste(FigureFolder,'Status of Unassessed by ISSCAAP.pdf',sep=''),width=16,height=10) 
  print(ggplot(AllStockStatus[AllStockStatus$Dbase=='Unassessed',],aes(as.factor(Year),BvBmsy)) +
    geom_boxplot(outlier.shape=NA) +
    facet_wrap(~SpeciesCatName,scale='free_y',ncol=4) + 
    theme_set(theme_gray(base_size = 10)) +
    coord_cartesian(ylim=c(0,2)) +
    theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
    geom_abline(intercept=1,slope=0) +
    labs(x='Year (1990-2012)',y='B/Bmsy',title='Unassessed Status by ISSCAAP Category'))
  dev.off()
  
  return(StatusISSCAAP=Status)
}