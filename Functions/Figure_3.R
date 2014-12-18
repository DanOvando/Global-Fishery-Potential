##########################################
### Global Upside Model Paper
### Figure 3: Upside Comparisons of P3 to P4
### for top 20 fishing nations
##########################################

# Data<-CumulativesFinal
# Countries<-c('Japan','Morocco','Republic of Korea','USA','Russian Federation','Global','Spain','Peru','Iceland','Thailand','Mexico','India',
# 'China','Philippines','Taiwan Province of China','Indonesia','Norway','Malaysia','Multinational')

Figure3<-function(CumulativesFinal,Countries)
{
  Data<-CumulativesFinal[CumulativesFinal$Country %in% Countries,]
  
  Data<-Data[,c('Country','Policy','NPV')]
  
  Data<-reshape(Data, idvar='Country',timevar='Policy',direction='wide')
  
  # calculate difference in percentages between CatchShare and Opt policies
  Data$NPV.P3toP4<-Data$NPV.CatchShare-Data$NPV.Opt
  
  # find order of largest to smalles increase from SQ for CatchShares
  Data<-Data[with(Data,order(-NPV.CatchShare)),]
  
  levels<-Data$Country
  
  # Isolate variables for stacked bar plot (Country,NPV of Opt, and diff in NPV btw Opt and CatchShare)
  Plot<-Data[,c('Country','NPV.Opt','NPV.P3toP4','NPV.CatchShare')]
  
  Plot<-melt(Plot, id.vars='Country', measure.vars=c('NPV.Opt','NPV.P3toP4','NPV.CatchShare'))
  
  Plot$Country<-factor(Plot$Country,levels=c(levels))
  
  # create stacked barplot
  
  pdf(file=paste(FigureFolder,"Figure 3.pdf",sep=''),width=16,height=10)  
  
  panelA<-ggplot(Plot[(Plot$Country!='Chile' & Plot$Country!='Multinational')  & Plot$variable!='NPV.CatchShare',],aes(x=Country,y=value,fill=variable)) +
    geom_bar(stat='identity') +
    guides(fill=F) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(text=element_text(size=20)) +
    labs(y='Percentage Increase in NPV',fill="Policy Decision")
  
  panelB<-ggplot(Plot[(Plot$Country=='Multinational' | Plot$Country=='Global') & Plot$variable!='NPV.CatchShare',],aes(x=Country,y=value,fill=variable)) +
    geom_bar(stat='identity') +
    scale_fill_discrete(labels=c('From SQ to Optimal','From Optimal\nto Catch Shares'),guide=guide_legend(reverse=T)) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(text=element_text(size=20)) +
    labs(y=NULL,x=NULL,fill="Policy Decision")
  
  print(grid.arrange(arrangeGrob(panelA,panelB,ncol=2,nrow=1, widths=c(12,4),heights=c(10,10))))  
  
  dev.off()
  
  return(FigureThreeData=Plot)
}