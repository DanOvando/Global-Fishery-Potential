PriceBarPlot<-function(Data,SpeciesCategoriesToOmit,FigureFolder)
{
  Prices<- read.csv('Data/Species Category Prices.csv',stringsAsFactors=F) # Current model price data from EDF
  
  Prices<-Prices[!(Prices$SpeciesCatName %in% SpeciesCategoriesToOmit) 
                 & is.na(Prices$Price)==F,]
  
  Prices<-Prices[with(Prices,order(-Price)),]
  
  levels<-Prices$SpeciesCatName
  
  Prices$SpeciesCatName<-factor(Prices$SpeciesCatName,levels=c(levels))
  
  pdf(file=paste(FigureFolder,"Price Barplot.pdf",sep=''),width=16,height=10)  
  
  print(ggplot(Prices,aes(x=SpeciesCatName,y=Price)) +
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
    theme(text=element_text(size=20)) +
    labs(y='Price ($/MT)',x="ISSCAAP Category"))
  
  dev.off()
  
  return()
}