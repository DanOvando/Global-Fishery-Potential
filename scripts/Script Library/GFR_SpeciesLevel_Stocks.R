StocksGFR<-function(FullData,Year)
{
  stocks<-FullData[FullData$IdLevel=='Species' & FullData$Year==Year,c('IdOrig','Country','RegionFAO','CommName',
                                                                       'SciName','SpeciesCat','SpeciesCatName','Catch')]
  
  stocks<-stocks[order(stocks$Country),]
  
  stocks<-stocks[stocks$Country!='',]

  write.csv(file='GFR_StockList_SpeciesLevel_121914.csv',stocks)
  
}