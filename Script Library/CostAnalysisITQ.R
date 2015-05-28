#########################################
##
## New Zealand Catch Share Cost Analysis
##
#########################################


### Read in and process ITQ data---------------------------------------------

StocksNZ<-read.csv('Data/NZ_ITQ_Stocks.csv',stringsAsFactors=F)

PriceITQ<-read.csv('Data/NZ_prices.csv',stringsAsFactors=F)

PriceITQ$Species<-NA

# calculate cost to revenue (equal to 1 - lease/revenue, where revenue is the grnwt price per kg and lease is converted to kg)

PriceITQ$CostOverRevenue<-1-(((PriceITQ$avg_lease_per_tonne)/1000)/PriceITQ$grnwt_price)

### Read in matched New Zealand RAM data and catch share price data---------------------------

# Read in previously matched RAM and ITQ stocks list

RamITQ<-read.csv('Data/New Zealand RAM Stocks To Match.csv',stringsAsFactors=F)

# Subset ProjectionData to include info for matched stocks in matched years and add in cost over revenue, lease price and grnwt price to matched stocks

DataNZ<-ProjectionData[(ProjectionData$IdOrig %in% c(RamITQ$IdOrig)) & (ProjectionData$Year %in% unique(PriceITQ$year)),
                       c('IdOrig','CommName','Year','BvBmsy','FvFmsy','r','MSY','Price')]

DataNZ$AvgLeasePricePerMT<-NA
DataNZ$PricePerKg<-NA
DataNZ$CostOverRevenue<-NA
DataNZ$NewZStockId<-NA

# Add stock Id to DataNZ

nzstocks<-unique(RamITQ$IdOrig)

for(a in 1:length(nzstocks))
{
  DataNZ$NewZStockId[DataNZ$IdOrig==nzstocks[a]]<-RamITQ$Stock_Code[RamITQ$IdOrig==nzstocks[a]]
}

# Add relevant data on ITQ prices to DataNZ

nzstocks<-unique(DataNZ$NewZStockId)

for(b in 1:length(nzstocks))
{    
  temp<-PriceITQ[grepl(nzstocks[b],PriceITQ$fishstock,fixed=T),]
  
  yrs<-unique(temp$year)
  
  for(c in 1:length(yrs))
  {
    whereD<-DataNZ$Year==yrs[c] & DataNZ$NewZStockId==nzstocks[b]
    
    whereI<-match(yrs[c],temp$year)
    
    DataNZ$AvgLeasePricePerMT[whereD]<-temp$avg_lease_per_tonne[whereI]
    DataNZ$PricePerKg[whereD]<-temp$grnwt_price[whereI]
    DataNZ$CostOverRevenue[whereD]<-temp$CostOverRevenue[whereI]
  }
  show(b)
}

# subset remove NA's

DataNZ<-DataNZ[is.na(DataNZ$CostOverRevenue)==F,]

### calculate 'c' and boa parameters for each data point

DataNZ$cParam<-(DataNZ$CostOverRevenue*DataNZ$Price*DataNZ$MSY*DataNZ$FvFmsy*DataNZ$BvBmsy)/((DataNZ$FvFmsy*DataNZ$r)/2)^beta

DataNZ$bParam<-
