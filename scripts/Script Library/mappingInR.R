#http://www.datasciencetoolkit.org/street2coordinates/
#http://blog.revolutionanalytics.com/2009/10/geographic-maps-in-r.html
#http://www.stanford.edu/~cengel/cgi-bin/anthrospace/download-global-administrative-areas-as-rdata-files
#http://cran.r-project.org/web/views/Spatial.html


########################################
#           Mapping in R
#           Allan Hicks
#           11/28/2012
########################################


getwd()   #let's see where the working directory is to put files in
#or change the working directory
setwd("/Users/danovando/Desktop")

#packages
library(maps)       #basic mapping functions and some data
library(mapdata)    #some additional hires data
library(maptools)   #useful tools such as reading shapefiles
library(mapproj)


#Map of the world
par(mfrow=c(2,2))
map()
map('usa')  # national boundaries
map('state') #state boundaries
map('state', region = c('washington', 'oregon', 'california'))  # map of three states
map.axes()

par(mfrow=c(1,1))
#you can use axes limits to draw a subarea of a dataset
map('state',xlim=c(-125,-116),ylim=c(33,49))  
map.axes()


#maintains projection even after resizing window
#expand the window wider and taller with the mouse and notice how it doesn't change the size of the plot
#but, replot the same thing and notice how it fills in the figure area
map('state', xlim=c(-125, -116), ylim=c(33, 49))  
map.axes()

#keep the window wide and plot two different datasets side by side
par(mfrow=c(1,2))
map('state', xlim=c(-125, -121.9), ylim=c(46, 48.5))
title(main="state")  #note that main does not work in map function, so use title as a low-level plotting function
map.axes()
map('worldHires', xlim=c(-125, -121.9), ylim=c(46, 48.5))
title(main="worldHires")  
map.axes()

#now add state and rivers
map('state',add=T,col="red")
map("rivers",add=T,col="blue")  #look at Columbia near bottom
#notice how the different datasets have slightly different points



#keep the window wide and plot two different datasets side by side to see resolution
par(mfrow=c(1,2))
map('state', xlim=c(-123.5, -121.9), ylim=c(47, 48.8),resolution=5)
title(main="state")  #note that main does not work in map function, so use title as a low-level plotting function
map.axes()
map('state', xlim=c(-123.5, -121.9), ylim=c(47, 48.8),resolution=0,add=T,col="red")

map('worldHires', xlim=c(-125, -121.9), ylim=c(46, 48.5),resolution=5)
title(main="worldHires")  
map.axes()
map('worldHires', xlim=c(-125, -121.9), ylim=c(46, 48.5),resolution=0,add=T,col="red")


graphics.off()
#to add points and lines, you can use typical plotting functions
#first, create a fake dataset of say catches
set.seed(30)
catches <- data.frame(lat=runif(10,46,48),lon=rnorm(10,-124.7,0.3),catch=rnorm(10,100,30))
tows <- data.frame(lat.start=catches$lat,lon.start=catches$lon,
        lat.end=catches$lat+0.05,lon.end=catches$lon-0.05)

map('worldHires', xlim=c(-125.3, -122.9), ylim=c(45.9, 48.5))
map.axes()
points(catches$lon,catches$lat,pch=16)

segments(tows$lon.start,tows$lat.start,tows$lon.end,tows$lat.end)

#or arrows showing direction
arrows(tows$lon.start,tows$lat.start,tows$lon.end,tows$lat.end,length=0.05,col="green")

map.scale(relwidth=0.1,ratio=F,cex=0.4,metric=F)




#####################################
#projections
#first look at the default projection
#The default is to use a rectangular projection with the aspect ratio chosen so that longitude and latitude scales are equivalent at the center of the picture. 
map('worldHires', xlim=c(-126.3, -122.9), ylim=c(34.9, 48.5))
map.axes()
map.scale(-126.2,35.4,relwidth=0.25,ratio=F,cex=0.4)
map.scale(-126.2,48.5,relwidth=0.25,ratio=F,cex=0.4)
abline(v=-125,col=gray(0.8))

par(mfrow=c(2,2),mar=c(0,0,5,0))
m <- map("state",main="default projection")
title(main="Default")
text(state.center, state.abb,cex=0.5)
map("state",proj='mercator')
title(main="Mercator projection")
text(mapproject(state.center), state.abb,cex=0.5)
map("state",proj='bonne', param=45)
title(main="Bonne projection")
text(mapproject(state.center), state.abb,cex=0.5)
map("state",proj="orthographic")
title(main="Orthographic")
text(mapproject(state.center), state.abb,cex=0.5)

quartz()
map(,proj='mercator')


tmp <- mapproject(seq(-125.5,-120,length=10),seq(33.4,45,length=10))
tmp <- mapproject(seq(-125.5,-120,length=10),rep(40,10))
points(tmp$x,tmp$y,pch=20,col="red")
tmp <- mapproject(rep(-120,length=20),seq(33,50,length=20))
points(tmp$x,tmp$y,pch=20,col="blue")


#why do we have to use mapproject
graphics.off()
map("state",proj='mercator')
title(main="Mercator projection")
map.axes()
#notice that the map axes are not in lat lon
#can use axis
map("state",proj='mercator')
title(main="Mercator projection")
box()
axis(1,at=mapproject(c(-120,-110),c(30,30))$x,labels=c("-120","-110"))




######################################
#reading in shapefiles (uses maptools)
#water shapefile is from US Census (http://www.census.gov/geo/www/tiger/)
water <- readShapePoly("mapData\\tl_2008_53033_areawater.shp")
map("state",region="Washington")
plot(water,col="blue",add=T)

graphics.off()
map("state",region="Washington",xlim=c(-122.4,-122.1), ylim=c(47.5, 47.8))
plot(water,col="blue",add=T)
map.axes()

windows()
plot(water,col="blue", xlim=c(-122.4,-122.1), ylim=c(47.5, 47.8))
axis(1)
axis(2)



############################################
#bathymetry data from GeoDAS or xyz format soundings
#xyz files, works best if you can get gridded data
#also see PBSmapping manual for free sources of bathymetric data

#these functions can take a long time, which is why I save the results to read in later
#plus the xyz file is very large. Let me know if you would like to see the file, or you can create your own
 #xyz <- read.table("mapData\\westCoast.xyz",header=F,col.names=c("x","y","z"))
 #xyz$z <- -1*xyz$z
 #apply(xyz,2,range)
#reads in x,y,z data and make it useable by contours or contourLines functions
 #require(PBSmapping)
 #wcBathy <- makeTopography(xyz,digits=1)  #use smaller digits if data are sparse to make smoother lines
 ### see ?makeTopography for other examples
 #save(wcBathy,file="mapData\\wcBathy.rda")

load("mapData\\wcBathy.rda")

graphics.off()
map('state', xlim=c(-127.3, -116.5), ylim=c(33, 49))
map.axes()
contour(wcBathy,levels=c(55,183),add=T)

map("state",region=c("Washington","Oregon"),xlim=c(-128, -121), ylim=c(45, 49),lwd=3)
map.axes()
contour(wcBathy,levels=50,add=T,drawlabels=F,col=gray(0.7),lty=1,lwd=1)
contour(wcBathy,levels=100,add=T,drawlabels=F,col=gray(0.5),lty=1,lwd=1)
contour(wcBathy,levels=200,add=T,drawlabels=F,col=gray(0.3),lty=1,lwd=1)
legend("bottomleft",c("50m","100m","200m"),col=c(gray(0.7),gray(0.5),gray(0.3)),lty=1,lwd=1)

#that data I downloaded was sparse, and you can see some serious errors
#you may wnat to work on smaller areas at a time, or get shapefiles from a GIS specialist











#######################################################################################
# PBS Mapping\
# In R 2.15.X it is not available in the install packages list
# Download zip file from: http://cran.open-source-solution.org/web/packages/PBSmapping/index.html
# and install package from zip file
#######################################################################################
graphics.off()

library(PBSmapping) #powerful mapping functions developed by Pacific Biological Station
data(nepacLL)
data(nepacLLhigh)
plotMap(nepacLL, xlim=c(-129, -122.6), ylim=c(48, 51.1))

plotMap(nepacLL, xlim=c(-129, -122.6), ylim=c(48, 51.1),col="green",bg="lightblue")

windows(width=9,height=7)  #you have to experiment with dimensions to get axes labels, etc.
par(mfrow=c(1,2))
plotMap(nepacLL, xlim=c(-125, -121.9), ylim=c(46, 48.9),main="Low res (nepacLL)")
plotMap(nepacLLhigh, xlim=c(-125, -121.9), ylim=c(46, 48.9),main="High res (nepacLLhigh)")

#compare worldHires in map package
map("worldHires", xlim=c(-125, -121.9), ylim=c(46, 48.9),add=T,col="red")

#make sure to redraw map after resizing window!



#reading in shapefiles (uses maptools)
#water shapefile is from US Census (http://www.census.gov/geo/www/tiger/)
water <- importShapefile("mapData\\tl_2008_53033_areawater.shp")
plotMap (nepacLLhigh, xlim=c(-125, -118.9), ylim=c(45, 48.5))
addPolys(water,col="blue")
#after importing shapefiles, you may have to set the attribute to LL. 
#For example, attributes(data)$projection <- "LL"

plotMap (nepacLLhigh,xlim=c(-122.4,-122.1), ylim=c(47.5, 47.8),col="green",bg="yellow")
addPolys(water,col=rgb(0,0,1,alpha=0.4))





############################################
#bathymetry data from GeoDAS or xyz format soundings
#xyz files, works best if you can get gridded data
#also see PBSmapping manual for free sources of bathymetric data

#these functions can take a long time, which is why I save the results to read in later
#plus the xyz file is very large. Let me know if you would like to see the file, or you can create your own
 #xyz <- read.table("mapData\\westCoast.xyz",header=F,col.names=c("x","y","z"))
 #xyz$z <- -1*xyz$z
 #apply(xyz,2,range)
#reads in x,y,z data and make it useable by contours or contourLines functions
 #wcBathy <- makeTopography(xyz,digits=1)  #use smaller digits if data are sparse to make smoother lines
 #save(wcBathy,file="mapData\\wcBathy.rda")

load("mapData\\wcBathy.rda")
isob <- c(55,183);
xyzCL <- contourLines(wcBathy,levels=isob)
xyzCP <- convCP(xyzCL)   #convert to poly set
wcPoly <- xyzCP$PolySet
attr(wcPoly,"projection") <- "LL"

plotMap(nepacLLhigh, xlim=c(-132, -116), ylim=c(31, 51),bg="lightblue",col="darkgreen")
addLines(wcPoly,col="darkblue",lty=1)

plotMap(nepacLLhigh, xlim=c(-128, -121), ylim=c(45, 49),bg="lightblue",col="darkgreen")
xyzCL <- contourLines(wcBathy,levels=c(50,100,200))
xyzCP <- convCP(xyzCL)   #convert to poly set
wcPoly <- xyzCP$PolySet
attr(wcPoly,"projection") <- "LL"
addLines(wcPoly,col=c("darkblue","blue","darkcyan"),lty=1)

#that data I downloaded was sparse, and you can see some serious errors
#you may wnat to work on smaller areas at a time, or get shapefiles from a GIS specialist



graphics.off()
#add things to the map, such as the catches from above
#we can use the PBSmapping function bubble, or the base function symbols
#the most recent PBSmapping package (I am using 2.63.37) requires EventData in addBubbles
#EventData is a data.frame with columns labeled EID, X, Y at a minimum, and a column Z for addBubbles
plotMap(nepacLLhigh, xlim=c(-128, -121), ylim=c(45, 49),bg="lightblue",col="darkgreen")
addLines(wcPoly,col=c("darkblue","red"),lty=1)
addBubbles(data.frame(EID=1:length(catches$lon),X=catches$lon,Y=catches$lat,Z=catches$catch),z.max=200,legend.title="Catch",max.size=0.5,
    symbol.fg=rgb(0,0,0,0.6), symbol.bg=rgb(0,0,0,0.3),legend.cex=0.5)

plotMap(nepacLLhigh, xlim=c(-128, -121), ylim=c(45, 49),bg="lightblue",col="darkgreen")
addLines(wcPoly,col=c("darkblue","red"),lty=1)
symbols(catches$lon,catches$lat,circles=sqrt(catches$catch),add=T,inches=0.2,
    fg=rgb(0,0,0,0.6), bg=rgb(0,0,0,0.3))
#and then create your own legend using the 'legend' function

plotMap(nepacLLhigh, xlim=c(-128, -121), ylim=c(45, 49),bg="lightblue",col="darkgreen")
addLines(wcPoly,col=c("darkblue","red"),lty=1)
arrows(tows$lon.start,tows$lat.start,tows$lon.end,tows$lat.end,length=0.03,col="orange",lwd=2)
map.scale(-127.4,45.5,relwidth=0.1,ratio=F,cex=0.4)
map.scale(-127.4,48.5,relwidth=0.1,ratio=F,cex=0.4)
abline(v=-126.5)

#draw a compass on the map
northarrow <- function(loc,size,bearing=0,cols,cex=1,...) {
    #From Tanimura, Kuroiwa, Mizota 2007. J. Statistical Software. V 19
    # checking arguments
    if(missing(loc)) stop("loc is missing")
    if(missing(size)) stop("size is missing")
    # default colors are white and black
    if(missing(cols)) cols <- rep(c("white","black"),8)
    # calculating coordinates of polygons
    radii <- rep(size/c(1,4,2,4),4)
    x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
    y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
    # drawing polygons
    for (i in 1:15) {
        x1 <- c(x[i],x[i+1],loc[1])
        y1 <- c(y[i],y[i+1],loc[2])
        polygon(x1,y1,col=cols[i])
    }
    # drawing the last polygon
    polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
    # drawing letters
    b <- c("E","N","W","S")
    for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
                        (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],cex=cex)
}

northarrow(c(-127,48),size=0.5)



################################################################################
###Additional info
#You can get GSHHS coastline data from http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
#You can get bathymetric data from http://topex.ucsd.edu/cgi-bin/get_data.cgi

#The PBSmapping user guide has a lot of really good information
#Shoudl be installed with package. Look for location when loading package in R




#Some extras for west coasters
source("mapData/WestCoastMap.R")

par(mfrow=c(1,2))
plotMap(westCoastLL,xlim=c(-128.5,-115.9),ylim=c(31.95,49),col=gray(0.7),tck=-0.02,main="PSMFC areas")
addLines(WCstatesInlandPBS)
addPolys(PSMFCareasPBS,col=gray(0.9),lwd=2)
addLabels(PSMFCareaNames)

plotMap(westCoastLL,xlim=c(-128.5,-115.9),ylim=c(31.95,49),col=gray(0.7),tck=-0.02,main="INPFC areas")
addLines(WCstatesInlandPBS)
addPolys(INPFCareasPBS,col=gray(0.9),lwd=2)
addLabels(INPFCareaNames)



#Some more advanced topics


#plotting when no lines produces an error
map('worldHires', xlim=c(-125.3, -122.9), ylim=c(45.9, 48.5))
polygon(c(-124.1,-124.1,-123.9,-123.9),c(48.4,48.5,48.5,48.4),col="red")
polygon(c(-125.1,-125.1,-124.9,-124.9),c(46.4,46.5,46.5,46.4),col="blue")

graphics.off()
map('worldHires', xlim=c(-124.1, -123.9), ylim=c(48.4, 48.5))
windows()
map('worldHires', xlim=c(-125.1, -124.9), ylim=c(46.4, 46.5))

graphics.off()
#create a map object if this occurs
#see below

######################################
#data structure for plotting with maps
x <- map("state",region="Washington")
#have a look at x
x0 <- map("state",region="Washington",resolution=0)
length(x$x)
length(x0$x)


#Create a map object 
#lines can be specified as x and y with breaks notated as NA
examp.map <- list(
    x=c(-124.6,-124.6,-124.7,-124.7,-124.6,NA,-124.8,-124.8,-124.85,-124.85,-124.8),
    y=c(47.2,47.3,47.3,47.2,47.2,NA,46.9,47.0,47.4,47.0,46.9),
    range=c(-125.3,-122.9,45.9,48.5),
    names=c("area1","area2")
)
class(examp.map) <- "map"

map("state",region="Washington",xlim=c(-125.1, -122.9), ylim=c(45.4, 48.5))
map.axes()
map(examp.map,add=T,fill=T,col=gray(0.8))

map(examp.map)
map.axes()

#make tows a map object
tows.map <- list(
    x=as.vector(t(data.frame(tows$lon.start,tows$lon.end,NA))),
    y=as.vector(t(data.frame(tows$lat.start,tows$lat.end,NA))),
    range=c(-125.3,-122.9,45.9,48.5),
    names=paste("tow",1:nrow(tows),sep="")
)
class(tows.map) <- "map"

map("state",region=c("Washington","Oregon"),xlim=c(-125.1, -122.9), ylim=c(45.4, 48.5))
map(tows.map,add=T)
map(tows.map,region="tow1",col="red",add=T)
map.axes()




#A look at the PBSmapping format
head(wcPoly,15)
apply(wcPoly,2,range)
unique(wcPoly$PID)
str(wcPoly)
boxes <- as.PolySet(data.frame(PID=c(rep(1,5),rep(2,5)),POS=c(1:5,1:5),
X=c(-124.6,-124.6,-124.7,-124.7,-124.6,
    -124.8,-124.8,-124.85,-124.85,-124.8),
Y=c(47.2,47.3,47.3,47.2,47.2,
    46.9,47.0,47.4,47.0,46.9)
),projection="LL")
plotMap(boxes)

plotMap(nepacLLhigh, xlim=c(-128, -121), ylim=c(45, 49),bg="lightblue",col="darkgreen")
addPolys(boxes)



################################
# rworldmap package
#################################
library(rworldmap)

theCountries<-c("DEU", "COD", "BFA", "ZAF")
malDF<-data.frame(country=theCountries, Participants=c(1, 1, 1, 1)) ##list countries to highlight and speciffy corresponding categories
malMap<-joinCountryData2Map(malDF, joinCode="ISO3", nameJoinColumn="country")
mapCountryData(malMap, nameColumnToPlot="Participants", catMethod="categorical", oceanCol="white", missingCountryCol=gray(.8))

myC1 <- malMap[malMap$ISO3 %in% c("BFA","COD","ZAF"),c("NAME","LON","LAT","ISO3")]
text(myC1$LON,myC1$LAT-10,myC1$ISO3)
myC2 <- malMap[malMap$ISO3 %in% c("DEU"),c("NAME","LON","LAT","ISO3")]
text(myC2$LON-40,myC2$LAT,myC2$ISO3,cex=0.7)
arrows(myC2$LON-30,myC2$LAT,myC2$LON,myC2$LAT,length=0.1)


#####################################
#mapplotting package
####################################
data(landings)
data(coast)
xlim <- c(-15,0)
ylim <- c(50,56)
xyz <- make.xyz(landings$Lon,landings$Lat,landings$LiveWeight,landings$Species)
col <- rainbow(5)
basemap(xlim, ylim, main = 'Species composition of gadoid landings')
draw.shape(coast, col='cornsilk')
draw.barplot2D(xyz$x, xyz$y, xyz$z, width = 0.8, height = 0.4, col=col)
legend('topright', colnames(xyz$z), fill=col, bg='lightblue', inset=0.02)


data(landings)
data(coast)
xlim <- c(-12,-5)
ylim <- c(50,56)
agg <- aggregate(list(z=landings$LiveWeight),list(x=landings$Lon,y=landings$Lat),sum)
basemap(xlim, ylim, main = 'Gadoid landings')
draw.shape(coast, col='cornsilk')
draw.bubble(agg$x, agg$y, agg$z, maxradius=0.5, pch=21, bg='#00FF0050')
legend.z <- round(max(agg$z)/1000,0)
legend.bubble('topright', z=legend.z, maxradius=0.5, inset=0.02, bg='lightblue', txt.cex=0.8,
pch=21, pt.bg='#00FF0050')


#our example from above
plotMap(nepacLLhigh, xlim=c(-128, -121), ylim=c(45, 49),bg="lightblue",col="darkgreen")
addLines(wcPoly,col=c("darkblue","red"),lty=1)
draw.bubble(catches$lon,catches$lat,catches$catch,maxradius=0.2,col=rgb(0,0,0,0.6),bg=rgb(0,0,0,0.3),pch=21)
legend.z <- round(max(catches$catch),0)
legend.bubble('bottomleft', z=legend.z, maxradius=0.2, inset=0.02, bg='lightblue', txt.cex=0.8,
pch=21, pt.bg=rgb(0,0,0,0.3))
