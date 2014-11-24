#####################
#
# Economic Summaries & Diagnostics
#
# Purpose: Compare model inputs (prices) and outputs (revenues, profits) to available real world data
#
#####################

### DATA ###

PriceDiagnostics<-function(ProjectionData,SpeciesCategoriesToOmit,BaselineYear)
{
  
NMFS<-read.csv("Data/NMFS_PRICE_DATA_0814.csv",stringsAsFactors=F) # NMFS Price Data

PriceData<- read.csv('Data/Species Category Prices.csv') # Current model price data from EDF

Spec_ISSCAAP=read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # species names and corresponding ISSCAAP codes

GroupNames_ISSCAAP<-read.csv("Data/ISSCAAP Codes.csv",stringsAsFactors=F)

SpeciesCategoriesToOmit<- c('Corals','Frogs and other amphibians','Eared seals, hair seals, walruses',
                            'Turtles','Pearls, mother-of-pearl, shells','Crocodiles and alligators','Miscellaneous aquatic plants'
                            ,'Freshwater crustaceans','Sperm-whales, pilot-whales','Green seaweeds','Red seaweeds',
                            'Brown seaweeds','Sea-squirts and other tunicates','Blue-whales, fin-whales',
                            'Miscellaneous aquatic mammals','Sponges','Krill, planktonic crustaceans','Miscellaneous freshwater fishes',
                            'River eels','Freshwater molluscs')

### Price Comparison Calculations

# Calculate average NMFS prices by ISSCAAP Category

colnames(NMFS)<-c('Year','Species','AvgUsPrice','AvgUsPrice05Dols') # rename columns

NMFS<-NMFS[NMFS$Year==2011,] # extract data for desired year - Closest to EDF data (SAUP data from 2006 in 2005 Dollars)

NMFS$Group<-NA # add group variable to summarize prices by
NMFS$SpeciesCat<-NA

for(a in 1:nrow(NMFS))
{
  NMFS$Group[a]<-unlist(str_split(NMFS$Species[a],pattern=","))[1] # create group variable using common name

  if(sum(grepl(NMFS$Group[a],Spec_ISSCAAP$CommName,ignore.case=T)>0))
  {
  NMFS$SpeciesCat[a]<-Spec_ISSCAAP$SpeciesCat_ISSCAAP_code[grepl(NMFS$Group[a],Spec_ISSCAAP$CommName,ignore.case=T)]
  }
}

NMFS$SpeciesCat[NMFS$Group=="CLAMS OR BIVALVES"]<-56 # add categories to groups that failed loop
NMFS$SpeciesCat[NMFS$Group=="SCUPS OR PORGIES"]<-33

NMFS<-NMFS[is.na(NMFS$AvgUsPrice05Dols)==F,]

# calculate mean price for each category

PriceNMFS<-ddply(NMFS,c("SpeciesCat"),summarize,AvgPrice=mean(AvgUsPrice,na.rm=T),AvgPrice05Dols=mean(AvgUsPrice05Dols,na.rm=T),Stocks=length(unique(Species)))

# drop groups without a name
PriceNMFS<-PriceNMFS[is.na(PriceNMFS$SpeciesCat)==F,]

# add in group names

PriceNMFS$ISSCAAP<-NA

for (b in 1:nrow(PriceNMFS))
{
  PriceNMFS$ISSCAAP[b]<-GroupNames_ISSCAAP$Definition[GroupNames_ISSCAAP$ISSCAAP.code==PriceNMFS$SpeciesCat[b]]
}

NMFS<-NMFS[is.na(NMFS$SpeciesCat)==F,]
NMFS$ISSCAAP<-NA

codes<-unique(NMFS$SpeciesCat)

for (b in 1:length(codes))
{
  NMFS$ISSCAAP[NMFS$SpeciesCat==codes[b]]<-GroupNames_ISSCAAP$Definition[GroupNames_ISSCAAP$ISSCAAP.code==codes[b]]
}

# calculate proportional error for each NMFS entry 
# (model price/NMFS price) - 1

NMFS$PropError<-NA

for(f in 1:nrow(NMFS))
{
  where<-PriceData$SpeciesCatName==NMFS$ISSCAAP[f]
  
  if(any(where)==T)
  {
  NMFS$PropError[f]<-(PriceData$Price[where]/NMFS$AvgUsPrice[f])-1
  }
}

# add in number of fisheries in Projection Data for each ISSCAAP group
FisheriesPerGroup<-ddply(ProjectionData[ProjectionData$IdLevel=='Species' & ProjectionData$Year==BaselineYear,],
                         c('SpeciesCatName'),summarize,Fisheries=length(unique(IdOrig,na.rm=T)))

for(d in 1:nrow(FisheriesPerGroup))
{
  NMFS$FisheriesInData[NMFS$ISSCAAP==FisheriesPerGroup$SpeciesCatName[d]]<-FisheriesPerGroup$Fisheries[d]
}

# boxplot of all proportional errors

NmfsBoxPlot<-ddply(NMFS[!(NMFS$ISSCAAP %in% SpeciesCategoriesToOmit),],('ISSCAAP'),summarize,
                   min=min(PropError,na.rm=T),
                   q1=quantile(PropError,0.25,na.rm=T),
                   med=median(PropError,na.rm=T),
                   q3=quantile(PropError,0.75,na.rm=T),
                   max=max(PropError,na.rm=T),
                   Fisheries=mean(FisheriesInData,na.rm=T)
                   )
pdf(file=paste(FigureFolder,"Price Data Prop Error.pdf",sep=''),width=15,height=10)

print(ggplot(NmfsBoxPlot,aes(x=factor(ISSCAAP))) +
  geom_boxplot(aes(lower = q1, upper = q3, middle = med, ymin = min, ymax = max), stat = "identity") +
  coord_flip(ylim=c(-2,5)) +
  geom_text(aes(y = min,label = Fisheries,color=Fisheries),hjust = 3) +
  geom_hline(yintercept=0) +
  labs(title="Proportional Error of Price Data",x='ISSCAAP Group',y='Proportional Error'))

dev.off()
  

# add in our currently used price data

PriceNMFS$PriceEDF<-NA

for (c in 1:nrow(PriceNMFS))
{
  PriceNMFS$PriceEDF[c]<-PriceData$Price[PriceData$SpeciesCatName==PriceNMFS$ISSCAAP[c]]
}

# Calculate percent difference in prices

PriceNMFS$PriceDiff<-round(log(PriceNMFS$PriceEDF/PriceNMFS$AvgPrice,10)/log(10,10))

PriceNMFS$PriceDiff05Dols<-round(log(PriceNMFS$PriceEDF/PriceNMFS$AvgPrice05Dols,10)/log(10,10))

# write.csv(PriceNMFS,file="EDF_NMFS_PriceComparison_102114.csv")


### Boxplot of prices by category. subset to only plot categories for which we have price data

PriceNMFS<-PriceNMFS[!(PriceNMFS$ISSCAAP %in% SpeciesCategoriesToOmit),] # subset to only include categories of interest
NMFS<-NMFS[!(NMFS$ISSCAAP %in% SpeciesCategoriesToOmit),] 

pdf(file=paste(FigureFolder,"NMFS Model Price Comparison.pdf",sep=''),width=15,height=10)
print(ggplot(NMFS, aes(as.factor(ISSCAAP),AvgUsPrice)) + # primary plot variables
  geom_boxplot() + # call to make boxplot
  geom_point(data=PriceNMFS,aes(x=as.factor(ISSCAAP),y=PriceEDF,color='red',shape=as.factor(PriceDiff)),size=4) + # add points for EDF price
  coord_flip(ylim=c(0,25000)) + # make boxplot horizontal
  theme(text=element_text(size=20)) +
  labs(title="NMFS and EDF Price Comparison", x = "ISSCAAP Group", y = "Price in $/MT",color="EDF Price",shape="Order of Magnitude \nComparison"))
dev.off()

### FAO EXPORT VALUE AND QUANTITY DATA
# First dataset is value by commodity category. 
# Create new dataset that has production by commodity in order to match value to quantity
# Notes: 
# 1) value is in $1000 USD

### FAO Export Value csv
FaoValue<-read.csv('Data/fao_value_111214.csv',stringsAsFactors=F, na.strings=c("...","-","0 0"))
FaoValue[,6:41]<-apply(FaoValue[,6:41],2,function(y) as.numeric(gsub(" F","",y))) # remove " F" from certain data points and convert catch record to numeric

# Name columns
colnames(FaoValue)[1:5]<-c('Country','Commodity','SpeciesCatName','SpeciesCat','Trade')

# convert to long format and subset to most recent year
FaoValue<-reshape(FaoValue,varying=6:41,direction="long", v.names="Value", timevar="Year",times=1976:2011,)
FaoValue<-FaoValue[FaoValue$Year==2011 & is.na(FaoValue$Value)==F,1:7]

# multiply value by 1000 to go from units of 1000 USD to USD
FaoValue$Value<-FaoValue$Value*1000

### FAO Export Quantity csv
FaoQuantity<-read.csv('Data/fao_quantity_111214.csv',stringsAsFactors=F, na.strings=c("...","-","0 0"))
FaoQuantity[,6:41]<-apply(FaoQuantity[,6:41],2,function(y) as.numeric(gsub(" F","",y))) # remove " F" from certain data points and convert catch record to numeric

# Name columns
colnames(FaoQuantity)[1:5]<-c('Country','Commodity','SpeciesCatName','SpeciesCat','Trade')

# convert to long format and subset out unwanted columns
FaoQuantity<-reshape(FaoQuantity,varying=6:41,direction="long", v.names="Value", timevar="Year",times=1976:2011,)
FaoQuantity<-FaoQuantity[FaoQuantity$Year==2011 & is.na(FaoQuantity$Value)==F,1:7]

### Match values to quantities
FaoValue$Quantity<-NA

for(a in 1:nrow(FaoValue))
{
  where<-FaoQuantity$Country==FaoValue$Country[a] & FaoQuantity$Commodity==FaoValue$Commodity[a]
  
  if(any(where)==T) {FaoValue$Quantity[a]<-FaoQuantity$Value[where]} 
}

# Calculate export value per ton
FaoValue$ValuePerTon<-FaoValue$Value/FaoValue$Quantity

# Aggregate by ISSCAAP category
FaoGlobalValues<-ddply(FaoValue[is.na(FaoValue$ValuePerTon)==F,],c('SpeciesCatName'),summarize,MeanValue=mean(ValuePerTon,na.rm=T))

# Add FAO export value prices to PriceNMFS and PriceData

PriceData$FaoPrice<-NA
PriceData$NMFSPrice<-NA

for(b in 1:nrow(PriceData))
{
  price<-FaoGlobalValues$SpeciesCatName==PriceData$SpeciesCatName[b]
  
  price2<-PriceNMFS$ISSCAAP==PriceData$SpeciesCatName[b]
  
  if(any(price)==T) {PriceData$FaoPrice[b]<-FaoGlobalValues$MeanValue[price]}
  if(any(price2)==T) {PriceData$NMFSPrice[b]<-PriceNMFS$AvgPrice[price2]}
}

# plot three price sources

PricePlot<-melt(PriceData,na.rm=T,id.var='SpeciesCatName')

PricePlot<-PricePlot[!(PricePlot$SpeciesCatName %in% SpeciesCategoriesToOmit),]

# all together
ggplot(PricePlot[PricePlot$SpeciesCatName!='Sturgeons, paddlefishes',],aes(SpeciesCatName,value,fill=variable)) +
  geom_bar(stat='identity',position='dodge') +
  coord_flip()

# faceted plot
ggplot(PricePlot[PricePlot$SpeciesCatName!='Sturgeons, paddlefishes',],aes(SpeciesCatName,value,fill=variable)) +
  geom_bar(stat='identity',position='dodge',axis.text.x=theme()) +
  facet_wrap(~SpeciesCatName,scales='free')
 
return(list(PriceComparison=PriceData ))
}
  