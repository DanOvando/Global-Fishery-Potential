#############################################
##
## Trace Stocks dropped in the analysis
##
#############################################

# Create dataframe with all unique stock ids in RawData
StockList<-unique(RawData[,c('IdOrig','Country','CommName','RegionFAO')])

Tracker<-data.frame(StockList,stringsAsFactors=F)

StockList<-join(DroppedStocks,StockList,by=c('IdOrig'),type='left')

StockList<-StockList[,c('IdOrig','Country','CommName','RegionFAO',colnames(StockList)[2:11])]

StockList<-StockList[with(StockList, order(Country)),]

# Add 2012 catch to stocks for reference
catch2012<-RawData[RawData$Year==2012,c('IdOrig','Catch')]

StockList<-join(StockList,catch2012,by=c('IdOrig'),type='left')

colnames(StockList)[colnames(StockList)==c('Catch')]<-'Catch2012'

write.csv(file='Stocks Dropped Due To Min Data Requirements.csv',StockList)


