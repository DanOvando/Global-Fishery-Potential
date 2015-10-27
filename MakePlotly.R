library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(googleVis)

ResultFolder <- 'Results/6.0 global demand common phi/Data/'

data<-read.csv(paste(ResultFolder,'UnLumped Projection DataAll Stocks Country Upsides.csv',sep=''), stringsAsFactors = F)
data2<-read.csv(paste(ResultFolder,'UnLumped Projection DataOverfish Only Country Upsides.csv',sep=''), stringsAsFactors = F)

data<- subset(data,Country!='High Seas Tuna and Billfish' & Policy == 'Catch Share Three')

data2<- subset(data2,Country!='High Seas Tuna and Billfish' & Policy == 'Catch Share Three')


head(data)
head(data2)


data2 %>%
  ggvis(x = xvar, y = yvar) %>%
  layer_points(size := 50, size.hover := 200,
               fillOpacity := 0.2, fillOpacity.hover := 0.5,
               stroke = ~has_oscar, key := ~ID) %>%
  add_tooltip(movie_tooltip, "hover") %>%
  add_axis("x", title = xvar_name) %>%
  add_axis("y", title = yvar_name) %>%
  add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
  scale_nominal("stroke", domain = c("Yes", "No"),
                range = c("orange", "#aaa")) %>%
  set_options(width = 500, height = 500)
})


Bubble <- gvisBubbleChart(data2, idvar="Country", 
                          xvar="PercChangeTotalBiomass", yvar="PercChangeTotalCatch",
                          colorvar="PercChangeTotalProfits", sizevar="TotalMSY"
                          width = 800, height = 2000)
plot(Bubble)


country.change <- ggplot(subset(data2, Policy == 'Catch Share Three'), aes(pmin(PercChangeTotalBiomass,200)/100,pmin(200,PercChangeTotalCatch)/100, fill = pmax(0,pmin(400,PercChangeTotalProfits))/100, size = TotalMSY)) +
geom_point(shape = 21, alpha = 0.6) + 
#   geom_vline(aes(xintercept = 0), linetype = 'longdash') + 
#   geom_hline(aes(xintercept = 0), linetype = 'longdash') + 
  theme_minimal() + 
  xlab('% Change in Biomass') + 
  ylab('% Change in Catch') + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(labels = percent) + 
  scale_fill_gradient2(name = '% Change in Profits',low = 'white',high = 'green',mid = 'yellow',midpoint = 2,labels = percent) + 
  scale_size_continuous(range = c(2,14), name = 'MSY')
   
country.change <- ggplot(subset(data2, Policy == 'Catch Share Three'), aes(pmin(PercChangeTotalBiomass,200)/100,pmin(200,PercChangeTotalCatch)/100, fill = pmax(0,pmin(400,PercChangeTotalProfits))/100, size = TotalMSY)) +
  geom_point(shape = 21, alpha = 0.6)

(gg <- ggplotly(country.change))
