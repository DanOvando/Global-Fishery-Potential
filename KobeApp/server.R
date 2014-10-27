# Global Fishery Recovery Shiny App
#
# Shiny Server Script

library(plyr)
library(lattice)
library(MASS)

shinyServer(function(input, output) {
  

  output$kobe_plot<- renderPlot({
    
    if(input$Country=="Global")
      {
      Data<-Data[Data$Year==input$Year & is.na(Data$FvFmsy)==F & is.na(Data$BvBmsy)==F,]
      
    } else ( Data<-Data[(Data$Country %in% input$Country) & Data$Year==input$Year & is.na(Data$FvFmsy)==F & is.na(Data$BvBmsy)==F, ])
      
#     Data<-Data[(Data$Country %in% input$Country) & Data$Year==input$Year & is.na(Data$FvFmsy)==F & is.na(Data$BvBmsy)==F, ]
      
    
   if(input$Neis==FALSE){Data<-Data[Data$IdLevel!="Neis",]}
    
    Data<-Data[Data$BvBmsy!=999 |is.infinite(Data$BvBmsy)!=TRUE,]
    
    if (any(Data$BvBmsy<0,na.rm=T)){Data$BvBmsy<- exp(Data$BvBmsy)}
    
    Data$FvFmsy[Data$FvFmsy>1.9]<- 1.9
    
    Data$BvBmsy[Data$BvBmsy>1.9]<- 1.9
    
#     shinyKobe(Data,input$Year)  
    
    DensityData<- (kde2d(Data$BvBmsy,Data$FvFmsy,n=500))
    
#     FTrend<- ddply(Data,c('Year','Dbase'),summarize,FTrend=mean(FvFmsy,na.rm=T))

    if(input$DotColor=="Dbase"){DotColor=Data$Dbase
    } else if (input$DotColor=="Country"){DotColor=Data$Country
    } else if (input$DotColor=="IdLevel"){DotColor=Data$IdLevel}
    

    layout(t(matrix(c(1,2))), widths = c(4,1), heights = c(1,1), respect = FALSE)
    par(mai=c(1,1,1,0))
    image(DensityData$x,DensityData$y,DensityData$z,col=KobeColors(50),xlab='B/Bmsy',ylab='F/Fmsy')
    points(Data$BvBmsy,Data$FvFmsy,pch=16,
    col=as.factor(DotColor),                          
    cex=0.75+(Data$Catch)/max(Data$Catch,na.rm=T))
    abline(h=1,v=1,lty=2)
    plot.new()
    par(mai=c(0,0,0,0))
    legend('center',legend=unique(DotColor),pch=16,col=(as.factor(unique(DotColor))),cex=1.5,bty='n')

    
  })
  
})
  
  