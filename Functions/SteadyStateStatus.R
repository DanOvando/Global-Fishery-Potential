###########################################
##
## Function plots steady state B/Bmsy by policies
##
###########################################

SteadyStateStatus<-function(Data,Subset)
{
#   Data<-ProjectionData
#   Subset<-c('Complete Projection Data.pdf')
  
  # Plot histogram of BvBmsy values at end of projection
  HistData<-Data[Data$Year==max(Data$Year),c('IdOrig','Policy','BvBmsy','SpeciesCatName','RegionFAO')]
  
  pdf(file=paste(FigureFolder,Subset,sep=''),width=16,height=10) 
  print(ggplot(HistData,aes(BvBmsy)) +
          geom_histogram(binwidth=0.05) +
          facet_wrap(~Policy,scales='free') +
          theme(text=element_text(size=20)) +
          labs(x='B/Bmsy',y='Frequency',title='Projected Steady State B/Bmsy'))
  dev.off() 
  
  return()
}