################################################
##
## Server file for upside model Kobe plot app
##
## By: Tyler Clavelle
## Date: 02/24/2016
##
################################################

## Load packages and data ----
library(dplyr)
library(lattice)
library(MASS)

# read kobe data list object
load("data/KobeAppData.rdata")

# calculate top 30 countries in terms of catch in our dataset and use those for the choices

# Vector of region ids and names

zone<-c('Arctic Sea','Northwest Atlantic','Northeast Atlantic','West Central Atlantic','Eastern Central Atlantic','Mediterranean and Black Sea',
        'Southwest Atlantic','Southeast Atlantic','Western Indian Ocean','Eastern Indian Ocean','Northwest Pacific','Northeast Pacific','Western Central Pacific','Eastern Central Pacific',
        'Southwest Pacific','Southeast Pacific','Atlantic Antarctic','Indian Ocean Antarctic','Pacific Antarctic')

codes<-c('18','21','27','31','34','37','41','47','51','57','61','67','71','77','81','87','48','58','88') # codes

names<-data.frame(zone,codes,stringsAsFactors=F) # df of names and corresponding codes for every FAO major region

# list of dot color choices to use
dotcolor<-list()
dotcolor[['Global']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName')
dotcolor[['Country']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName',"By Country"='Country')
dotcolor[['ISSCAAP']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName')
dotcolor[['Region']]<-c("By Database"="Dbase","By ISSCAAP"='SpeciesCatName')

# assign colors to use for kobe plot
KobeColors<- colorRampPalette(c("powderblue", "white",'#FFFF99'))

RGBcols<-c('#A0A0A0','#FC6969','#0B610B')

## Initialize Shiny server function ----
shinyServer(function(input, output) {
  
  output$kobe_plot<- renderPlot({
    
    # subset data based on desired kobe plot level 
    if(input$Level=="Global")
    {
      Data<-data$lumped %>%
        filter(is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      Data$Dbase[Data$Dbase!='RAM']<-'Unassessed'
    } 
    if(input$Level=="By Country")
    {
      Data<-data$unlumped %>%
        filter(Country %in% input$Country & is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      Data$Dbase[Data$Dbase!='RAM']<-'Unassessed'
    }
    
    if(input$Level=="By ISSCAAP")
    {
      Data<-data$lumped %>%
        filter(SpeciesCatName %in% input$isscaap & is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      Data$Dbase[Data$Dbase!='RAM']<-'Unassessed'
    }
    
    if(input$Level=="By FAO Region")
    {
      regs<-names$codes[names$zone %in% input$Region]
      
      Data<-data$lumped %>%
        filter(grepl(regs,RegionFAO) & is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      Data$Dbase[Data$Dbase!='RAM']<-'Unassessed'
    }
    
    # toggle assessment level
    if(input$Dbase=='Both'){Data<-Data}
    if(input$Dbase=='RAM'){Data<-Data[Data$Dbase=='RAM',]}
    if(input$Dbase=='Unassessed'){Data<-Data[Data$Dbase=='Unassessed',]}
    
    # toggle NEIs
    if(input$Neis==FALSE){Data<-Data[Data$IdLevel!="Neis",]}
    
    # toggle stock size
    Data<- Data %>%
      filter(Catch>=input$Size)
    
    # QAQC of BvBmsy values
    Data<-Data[Data$BvBmsy!=999 |is.infinite(Data$BvBmsy)!=TRUE,]
    
    # calculate median values prior to capping values for the plot
    KobeMedians<- Data %>%
      mutate(bxcatch=BvBmsy*Catch,fxcatch=FvFmsy*Catch) %>%
      group_by(Policy) %>%
      summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),
                WtMeanB=sum(bxcatch,na.rm=T)/sum(Catch,na.rm=T),WtMeanF=sum(fxcatch,na.rm=T)/sum(Catch,na.rm=T),
                WtGeomMeanB=exp(sum(Catch * log(BvBmsy),na.rm=T)/sum(Catch,na.rm=T)),
                WtGeomMeanF=exp(sum(Catch * log(FvFmsy + 1e-3),na.rm=T)/sum(Catch,na.rm=T)))
    
    # cap FvFmsy and BvBmsy values to the kobe plot axis limits
    Data$FvFmsy[Data$FvFmsy>4]<- 4
    
    Data$BvBmsy[Data$BvBmsy>2.5]<- 2.5
    
    # calculate kernal density
    DensityData<- (kde2d(Data$BvBmsy,Data$FvFmsy,n=100,lims=c(0,2.5,0,4)))
    
    # change dot color based on whether global plot or individual countries are plotted
    if(input$Level=="Global"){DotColor=Data$Dbase
    } else if (input$Level=="By Country"){DotColor=Data$Country
    } else if (input$Level=="By FAO Region"){DotColor=Data$Dbase
    } else if (input$Level=="By ISSCAAP"){DotColor=Data$SpeciesCatName}

    if (input$ColorID==TRUE){DotColor=Data$IdLevel}
    
    # if(DotColor==Data$Dbase){DotColor<-paste(c('#A0A0A0','#FC6969'),99,sep = '')}

    # plot Kobe plot

    layout(t(matrix(c(1,2))), widths = c(4,1), heights = c(1,1), respect = FALSE)
    par(mai=c(1,1,1,0))
    image(DensityData$x,DensityData$y,DensityData$z,col=KobeColors(50),xlab='B/Bmsy',ylab='F/Fmsy')
    points(Data$BvBmsy,Data$FvFmsy,pch=16,
           col=as.factor(DotColor),                          
           cex=1.95*log((Data$Catch))/max(log(Data$Catch),na.rm=T))
    points(KobeMedians$MedianB,KobeMedians$MedianF,pch=17,col='#0B610B', cex=1.5)
    points(KobeMedians$WtGeomMeanB,KobeMedians$WtGeomMeanF,pch=15,col='#0B610B', cex=1.5)
    abline(h=1,v=1,lty=2)
    plot.new()
    par(mar=c(0,0,0,0))
    legend('left',legend=unique(DotColor),pch=16,col=(as.factor(unique(DotColor))),cex=0.75,bty='n')
 
    })
  
  # plotB<-renderPlot(kobe_plot)
  # 
  # output$downloadPDF<-downloadHandler(
  #   filename = function() {
  #     paste('kobe-', Sys.Date(), '.pdf', sep = '')
  #   },
  #   content  = function(file){
  #     pdf(plotB,file)
  #   })
  
})

