#####################
#
# Economic Summaries & Diagnostics
#
# Purpose: Compare model inputs (prices) and outputs (revenues, profits) to available real world data
#
#####################

### DATA ###

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

pdf(file="NMFS Model Price Comparison.pdf",width=15,height=10)
ggplot(NMFS, aes(as.factor(ISSCAAP),AvgUsPrice05Dols,)) + # primary plot variables
  geom_boxplot() + # call to make boxplot
  geom_point(data=PriceNMFS,aes(x=as.factor(ISSCAAP),y=PriceEDF,color='red',shape=as.factor(PriceDiff)),size=4) + # add points for EDF price
  coord_flip() + # make boxplot horizontal
  theme(text=element_text(size=20)) +
  labs(title="NMFS and EDF Price Comparison", x = "ISSCAAP Group", y = "Price in $/MT",color="EDF Price",shape="Order of Magnitude \nComparison")
dev.off()



