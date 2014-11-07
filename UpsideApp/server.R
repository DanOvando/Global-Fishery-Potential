# Global Fishery Recovery Shiny App
#
# Shiny Server Script

library(plyr)
library(lattice)
library(ggplot2)
library(ggvis)
library(BH)

shinyServer(function(input, output) {
  
#   TempData<-reactive({
#     Data[(Data$Country %in% input$Country),]
#   })
  
  output$upside_plot<- renderPlot({
    
    # Subset data based on input
    
    Data<-Data[(Data$Country %in% input$Country) & Data$Policy==input$Policy,]
    
    FinalYr<-FinalYr[(FinalYr$Country %in% input$Country) & FinalYr$Policy==input$Policy,]
    
    # Build dataframe to plot based on axis inputs and country
    c<-Data$Country
    
    if(input$dotsize=="Food") { s<-Data$Food}
    if(input$dotsize=="NPV"){s<-Data$NPV}
    if(input$dotsize=="Fish"){s<-Data$Fish}
    
    if(input$yaxis=="NPV") { y<-Data$NPV}
    if(input$yaxis=="PercChangeTotalProfits"){y<-FinalYr$PercChangeTotalProfits}
    if(input$yaxis=="PercChangeFromSQMedianProfits"){y<-FinalYr$PercChangeFromSQMedianProfits}
    if(input$yaxis=="PercChangeMedianProfits"){y<-FinalYr$PercChangeMedianProfits}
    
    if(input$xaxis=='PercChangeFromSQTotalBiomass'){x<-FinalYr$PercChangeFromSQTotalBiomass} 
    if(input$xaxis=='PercChangeTotalBiomass'){x<-FinalYr$PercChangeTotalBiomass}
    if(input$xaxis=='PercChangeFromSQMedianBiomass'){x<-FinalYr$PercChangeFromSQMedianBiomass}
    if(input$xaxis=='PercChangeMedianBiomass'){x<-FinalYr$PercChangeMedianBiomass}
    if(input$xaxis=='PercChangeTotalCatch'){x<-FinalYr$PercChangeTotalCatch}
    if(input$xaxis=="Food") { x<-Data$Food}
    
    PlotData<-data.frame(c,x,y,s)
    colnames(PlotData)<-c("Country","xVar","yVar","Size")
#     colnames(PlotData)<-c("Country",input$xaxis,input$yaxis,"Food")
    
    # Plot upside results 
  
# ggviz interactive plot
# PlotData %>%
#   ggvis(x=~xVar,
#         y=~yVar,
#         fill=~Country) %>%
#   layer_points() %>%
#   add_axis("x", title = input$xaxis) %>%
#   add_axis("y", title = input$yaxis) %>%
#   add_legend("fill", title = "Country") %>%
#   add_tooltip(all_values, "hover")
  


# ggplot plot test

  qplot(xVar,yVar,data=PlotData,color=Country, size=Size,
        main=c("Percentage Upsides"),
              
       ylab= (if(input$yaxis=="NPV") { ylab<-"Percent Change from SQ NPV"
                                       } else if (input$yaxis=="PercChangeTotalProfits") { ylab<-"Percent Change from Current Total Profits"
                                      } else if(input$yaxis=="PercChangeFromSQMedianProfits") { ylab<-"Percent Change from SQ Median Profits"
                                      }else if(input$yaxis=="PercChangeMedianProfits") { ylab<-"Percent Change from Current Median Profits"}),
  
      xlab= (if(input$xaxis=="PercChangeFromSQTotalBiomass") { xlab<-"Percent Change in Total Biomass from SQ"
                                      } else if (input$xaxis=="PercChangeTotalBiomass") { xlab<-"Percent Change from Current Biomass"
                                      } else if (input$xaxis=="PercChangeFromSQMedianBiomass") { xlab<-"Percent Change from SQ Median Biomass"
                                      } else if (input$xaxis=="PercChangeMedianBiomass") { xlab<-"Percent Change from Current Median Biomass"
                                      } else if (input$xaxis=="Food") { xlab<-"Percent Change from Status Quo Food"})) + scale_size_continuous(range=c(4,10))
  })
})