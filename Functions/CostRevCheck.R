###############################
#
# Function to Compute Cost Revenue  
# Diagnostics of Upside Model
#
##############################

CostRevCheck<-function(Data,RawData,BaselineYear)
{
  # subset Projection data to examine results in baseline year
  eTest<-ProjectionData[is.na(ProjectionData$BvBmsy)==F & is.na(ProjectionData$FvFmsy)==F & ProjectionData$Year==BaselineYear,]
  
  # calculate revenue
  eTest$Revenue<-eTest$Price * eTest$MSY * eTest$FvFmsy * eTest$BvBmsy
  
  # calculate cost, cost per ton and cost:revenue ratio
  eTest$Cost<-eTest$MarginalCost * (eTest$FvFmsy * eTest$r / 2)^beta
  
  eTest$CostPerTon<-eTest$Cost/eTest$Catch
  
  eTest$CostRevRatio<-eTest$Cost/eTest$Revenue
  
  eTest<-eTest[is.na(eTest$SpeciesCatName)==F,]
    
  # Summarize revenue by country
  CountryRevenues<-ddply(eTest,c("Country"),summarize,TotalRevenue=sum(Revenue,na.rm=T), TotalCatch=sum(Catch,na.rm=T),TotalStocks=length(unique(IdOrig)))
  
  fao<-RawData[RawData$Dbase=='FAO',]
  
  faoCatch<-ddply(fao[fao$Year==BaselineYear,],c('Country','Year'),summarize,TotalCatch=sum(Catch,na.rm=T))
  
#   show(sum(CountryRevenues$TotalCatch)) # total catch in ProjectionData
#   show(sum(CountryRevenues$TotalCatch)/sum(faoCatch$TotalCatch)) # percent of total global catch in baseline year
#   
  # calculate percent of total catch that is projected for each country
  CountryRevenues$PercTotal<-NA
  
  for(a in 1:nrow(CountryRevenues))
  {
    where<-faoCatch$Country==CountryRevenues$Country[a]
    
    if(any(where)==TRUE)
    {
      CountryRevenues$PercTotal[a]<-(CountryRevenues$TotalCatch[a]/faoCatch$TotalCatch[where])*100
    }
  }
  
  # plot histograms of cost:revenue ratio by species category
  pdf(file=paste(FigureFolder,"Histograms Cost Revenue Ratio by Species Category.pdf",sep=''),width=16, height=10)
  print(ggplot(eTest[eTest$CostRevRatio<4,],aes(CostRevRatio)) +
    geom_histogram(binwidth=.1) +
    aes(y=..density..) +
    facet_wrap(~SpeciesCatName) +
    theme(text=element_text(size=15)) +  # for removing gray background: ,panel.background=element_blank()) +
    labs(title="Cost to Revenue Ratio by ISSCAAP Group", x = "Cost:Revenue Ratio", y = "Density"))
  dev.off()
  
  # Boxplots by species category and region
  
  regs<-c('37','51','27','21','77','34','81','48','47','31','87','57','41','71','67','61','88','58','18')
    
  pdf(file=paste(FigureFolder,"Cost to Revenue Ratio by Species Category and Region.pdf",sep=''),width=12, height=10)
  
  for(b in 1:length(regs))
  {
    eTestTemp<-eTest[grepl(regs[b],eTest$RegionFAO),]
    
    print(ggplot(eTestTemp,aes(SpeciesCatName,CostRevRatio)) + 
            geom_boxplot(varwidth=T) +
            coord_flip(ylim=c(0,2)) + # make boxplot horizontal
            theme(text=element_text(size=20),legend.position='none') +
            labs(title=paste(regs[b],"Cost to Revenue Ratio by Species Category",sep=' '), 
                 x = "ISSCAAP Group", y = "Cost to Revenue Ratio",fill="ISSCAAP Group"))
  }
  
  dev.off()
  
  # Summary stats of cost:revenue ratio across all data
  OverallCostRevSummary<-data.frame(matrix(summary(eTest$CostRevRatio),nrow=1,ncol=length(summary(eTest$CostRevRatio))))
  colnames(OverallCostRevSummary)<-c('Min','1st Quart','Median','Mean','3rd Quart','Max','NAs')
  
  # Table of summary stats for cost:revenue ratios per ISSCAAP category
  SpCats<-unique(eTest$SpeciesCatName)
  
  SummaryTable<-data.frame(matrix(nrow=length(SpCats),ncol=9))
  colnames(SummaryTable)<-c('ISSCAAP Group','Stocks', 'Min','1st Quart','Median','Mean','3rd Quart','Max')

  SummaryTable2<-data.frame(matrix(nrow=length(SpCats),ncol=9))
  colnames(SummaryTable2)<-c('ISSCAAP Group','Stocks', 'Min','1st Quart','Median','Mean','3rd Quart','Max')

  for(b in 1:length(SpCats))
  {
    temp<-eTest$CostRevRatio[eTest$SpeciesCatName==SpCats[b]]
    
    x<-summary(temp)
    
    SummaryTable[b,1]<-SpCats[b] # name of category
    SummaryTable[b,2]<-length(temp) # number of stocks
    SummaryTable[b,3:9]<-x[1:7] # stats
    
    temp2<-eTest$CostPerTon[eTest$SpeciesCatName==SpCats[b]]
    
    x<-summary(temp2)
    
    SummaryTable2[b,1]<-SpCats[b] # name of category
    SummaryTable2[b,2]<-length(temp) # number of stocks
    SummaryTable2[b,3:9]<-x[1:7] # stats
    
  }
  
  write.csv(file=paste(ResultFolder,'Catch_Revenue_Ratios_ISSCAAP.csv',sep=''),SummaryTable)
  write.csv(file=paste(ResultFolder,'CostPerTon_Ratios_ISSCAAP.csv',sep=''),SummaryTable2)

  write.csv(file=paste(ResultFolder,'Catch_Revenue_Ratio_AllData.csv',sep=''),OverallCostRevSummary)
  
  # subset fisheries with outlier cost:revenue ratios to look for causes
  OutlierRatios<-eTest[(eTest$CostRevRatio>2 | eTest$CostRevRatio<0.2) & is.na(eTest$IdOrig)==F,
                       c('IdOrig','SpeciesCatName', 'CommName','Year','Catch','Fmort','BvBmsy','FvFmsy','CostRevRatio')]
  OutlierRatios<-OutlierRatios[is.na(OutlierRatios$IdOrig)==F,]
  
  # subset eTest to include only economically relevent columns
  eTest<-eTest[,c('IdOrig','SpeciesCatName', 'CommName','Year','Catch','Fmort','BvBmsy','FvFmsy','Price',
                  'Revenue','Cost','CostRevRatio','MSY','r','MarginalCost')]


  
  return(list(FisheryValues=eTest,CountryRevenues=CountryRevenues,CostRevenueAllData=OverallCostRevSummary,ISSCAAPCostRevenues=SummaryTable,CostRevenueOutliers=OutlierRatios))
  
}