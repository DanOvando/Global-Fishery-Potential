# Data<-ProjectionData
# Year<-2006

function(Data,Year)
{
  # calculate total catch for each country in fao
  CountryCatch<-ddply(fao[fao$Year==Year,],c("Country"),summarize,TotalCatch=sum(Catch,na.rm=T))
  
  # Subset Data to BaselineYear
  Data<-Data[Data$Year==Year,]
  
  # Add in revenue column
  Data$Revenue<-Data$Price * Data$MSY * Data$FvFmsy * Data$BvBmsy
  
  # calculate total catch and total revenue for each country as projected by the model
  CountryRev<-ddply(Data,c('Country'),summarize,TotalRevenue=sum(Revenue,na.rm=T),TotalCatch=sum(Catch,na.rm=T))
  
  # read in SAUP country revenue data
  
  SAUP<-read.csv("Data/SAUP_Country_RevenueData.csv",stringsAsFactors=F)
  colnames(SAUP)<-c("ISO",'Country','Value06in05Dols','CatchMT','OECDvalue','NMFS09Value')
  
  # add column of total catch to CountryRev to compare catch in model to total catch as a check against revenue 
  CountryRev$SaupCatch<-NA
  CountryRev$SaupRevenue<-NA
  
  for(a in 1:nrow(CountryRev))
  {
    match<-match(CountryRev$Country[a],SAUP$Country)
    
    if(is.na(match)==F)
    {
      CountryRev$SaupCatch[a]<-SAUP$CatchMT[match]
    
      CountryRev$SaupRevenue[a]<-SAUP$Value06in05Dols[match]
    }
  }
  
  # calculate percent of country's fullcatch that is being projected
  CountryRev$PercentOfTotalCatch<-CountryRev$TotalCatch/CountryRev$SaupCatch*100
  
  # calculate percent of country's total revenue that is currently estimated by the model
  CountryRev$PercentOfTotalRevenue<-CountryRev$TotalRevenue/CountryRev$SaupRevenue*100
  
  # calculate order of magnitude difference
  CountryRev$OrdMagCatchDiff<-round(log(CountryRev$TotalCatch/CountryRev$SaupCatch,10)/log(10,10))
  
  CountryRev$OrdMagRevDiff<-round(log(CountryRev$TotalRevenue/CountryRev$SaupRevenue,10)/log(10,10))
  
  # barplot of percent of total revenue
  ggplot(CountryRev,aes(x=Country,y=PercentOfTotalRevenue)) + geom_bar(stat='identity',) + coord_cartesian(ylim=c(0,200))
  
  # Boxplot and distribution of percent of total revenue and total catch
  
  plotdata<-CountryRev[is.na(CountryRev$PercentOfTotalCatch)==F & is.na(CountryRev$PercentOfTotalRevenue)==F,] # subset out NA values
  
  plotdata<-melt(plotdata,id.vars="Country",measure.vars=c('PercentOfTotalCatch','PercentOfTotalRevenue')) # melt data for ggplot
      

pdf(file='Model Econ Output Compared to SAUP Boxplot.pdf')  
 ggplot(plotdata, aes(variable, value)) + 
    geom_boxplot(aes(fill = variable)) +
    theme(legend.position = "none") +
    theme(text=element_text(size=15)) +
    labs(title="Model Revenue & Catch Compared with SAUP/FAO", x = "Percent of SAUP Value",fill="Metric") +
    coord_cartesian(ylim=c(0,200))
dev.off()  

pdf(file='Model Econ Output Compared to SAUP Histogram.pdf',width=10,height=7)  
 ggplot(plotdata,aes(value,fill=variable)) + # plot data
    geom_histogram(binwidth=5,alpha=.4) + # histogram specs
    coord_cartesian(xlim=c(0,400)) + # limit x axis to 400% 
    theme(text=element_text(size=15)) +
    labs(title="Model Revenue & Catch Compared with SAUP/FAO", x = "Percent")
  dev.off()

}