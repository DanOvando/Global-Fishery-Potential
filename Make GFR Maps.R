
## read in map data:
library(maps)
library(mapproj)
library(rgdal)
library(dplyr)
library(tidyr)
library(devtools)
library(RColorBrewer)
library(ggplot2)
require(stringr)
library(broom)

# devtools::install_github('hadley/ggplot2')
runname <- '6.0 global demand common phi'

load(file = paste('Results',runname,'Data','ProjectionData Data.rdata', sep = '/'))

rawdata <- read.csv(file = paste('Results',runname,'Data','Raw Database.csv', sep = '/'), stringsAsFactors = F)

  realcatch <- rawdata %>%
    subset(Year == 2012 & Dbase == 'FAO') %>%
  group_by(Country) %>%
  summarize(total.fao.catch = sum(Catch, na.rm = T))

#   

proj <- CRS("+proj=moll")

ocean <-  raster(file.path('Data/ocean.tif'))

p <- readOGR(dsn='Data/World_EEZ_v8_20140228_LR/', layer= 'World_EEZ_v8_2014')

p@data$Country <- as.character(p@data$Country)

# p = spTransform(p,crs(ocean))

# p <- spTransform(p, CRS("+proj=moll +R=10567000 +lon_0=0 +x_0=0 +y_0=0 +units=m +towgs84=0,0,0,0,0,0,0 +no_defs"))

# p <- spTransform(p, CRS("+proj=moll +R=10567000 +lon_0=0 +x_0=0 +y_0=0 +units=m +towgs84=0,0,0,0,0,0,0 +no_defs"))

p@data

proj4string(p)

country.stats <- UnlumpedProjectionData %>%
  subset(Year == 2012) %>%
  group_by(Country) %>%
  summarize(total.catch = sum(Catch, na.rm = T), median.bvbmsy = median(BvBmsy, na.rm = T)) %>%
  left_join(realcatch, by = 'Country') %>%
  ungroup() %>%
  mutate(perc.of.fao.catch = 100*pmin(1,total.catch/total.fao.catch))


# Missing countries

sort(country.stats$Country[!country.stats$Country %in% p$Country])

sort(p$Country[!p$Country %in% country.stats$Country])

p$Country<- gsub("Alaska", "United States",p$Country)

p$Country<- gsub("Hawaii", "United States",p$Country)

p$Country<- gsub("Ecuador Islands", "Ecuador",p$Country)

country.stats$Country<- gsub("USA", "United States",country.stats$Country)

country.stats$Country<- gsub("Russian Federation", "Russia",country.stats$Country)

country.stats$Country<- gsub("Viet Nam", "Vietnam",country.stats$Country)

country.stats$Country<- gsub("Republic of Korea", "South Korea",country.stats$Country)

matched.country <- left_join(p@data,country.stats, by = 'Country')

matched.country$id <- 0:(nrow(matched.country) - 1)

p@data$total.catch <- matched.country$total.catch

eezdata <- tidy(p) %>% #turn shapefile into a dataframe that defines the polygons
  mutate( id = as.integer(id)) %>%
  left_join(matched.country, by  = 'id')

col.brks  <- seq(0, 1, length.out = 6)
leg_on <- T

# mp1 <- fortify(map(fill=TRUE, plot=FALSE))
# mp2 <- mp1
# mp2$long <- mp2$long + 360
# mp2$group <- mp2$group + max(mp2$group) + 1
# mp <- rbind(mp1, mp2)


casey.theme <- theme(axis.ticks = element_blank(), axis.line = element_blank(),axis.text = element_blank(),
      text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
      plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
      legend.position = ifelse(leg_on, 'right', 'none'), 
        axis.title = element_blank())

df_plot <- (ggplot(data = eezdata, aes(x = long, y = lat, group = group, fill = perc.of.fao.catch/100)) +  
 scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), space = 'Lab', na.value = 'gray80',
                       breaks = col.brks, labels = percent, limits = c(0, 1), name = '% of FAO Catch') + 
  geom_polygon(color = 'black', size = .1) +
   casey.theme 
   )

global.plot <- df_plot +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  borders('world', color = 'gray69', fill = 'gray80', size = .25) # create a layer of borders
  
ggsave('gfr_map.png', width = 10, height = 6, plot = global.plot)


bvbmsy.plot <- (ggplot(data = eezdata, aes(x = long, y = lat, group = group, fill = median.bvbmsy)) +  
              scale_fill_gradient2(low = 'red',mid = 'yellow', high = 'green',midpoint = 1) + 
              geom_polygon(color = 'black', size = .1) +
              casey.theme 
)

global.plot <- bvbmsy.plot +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  borders('world', color = 'gray69', fill = 'gray80', size = .25) # create a layer of borders

ggsave('gfr_bvbmsy_map.png', width = 10, height = 6, plot = global.plot)

  
