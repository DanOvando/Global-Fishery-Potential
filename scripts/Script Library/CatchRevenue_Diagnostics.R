###################################
##
## Economic Fact-Checking
##
##################################

eTest<-ProjectionData[ProjectionData$IdLevel=="Species" & is.na(ProjectionData$BvBmsy)==F & is.na(ProjectionData$FvFmsy)==F & ProjectionData$Year==BaselineYear,]

# calculate revenue
eTest$Revenue<-eTest$Price * eTest$MSY * eTest$FvFmsy * eTest$BvBmsy

# calculate cost
eTest$Cost<-eTest$MarginalCost * (eTest$FvFmsy * eTest$r / 2)^beta

eTest$CostRevRatio<-eTest$Cost/eTest$Revenue

# eTest<-eTest[eTest$CostRevRatio<4,] # if want to cap ratio

# Summarize revenue by country

CountryRevenues<-ddply(eTest,c("Country"),summarize,TotalRevenue=sum(Revenue,na.rm=T), TotalCatch=sum(Catch,na.rm=T),TotalStocks=length(unique(IdOrig)))

faoCatch<-ddply(fao[fao$Year==BaselineYear,],c('Year','Country'),summarize,TotalCatch=sum(Catch,na.rm=T))

sum(CountryRevenues$TotalCatch) # total catch in ProjectionData
sum(CountryRevenues$TotalCatch)/faoCatch$TotalCatch[1] # percent of total global catch in baseline year

# plot revenue against profits and cost

ggplot(eTest,aes(Revenue,Profits)) + geom_point(alpha=.4) + geom_abline(intercept=0,slope=1) # profit
  # no points above 1to1 line 

ggplot(eTest,aes(Revenue,Cost)) + geom_point(alpha=.4) + geom_abline(intercept=0,slope=1) # cost

ggplot(eTest,aes(Profits)) + geom_histogram()

ggplot(eTest[eTest$CostRevRatio<5,],aes(CostRevRatio)) + geom_histogram(binwidth=0.1)

# subset fisheries with outlier cost:revenue ratios to look for causes
OutlierRatios<-eTest[eTest$CostRevRatio>2 & is.na(eTest$IdOrig)==F,]
OutlierRatios<-OutlierRatios[is.na(OutlierRatios$IdOrig)==F,]

# Boxplot of Cost/Revenue Ratio by SpeciesCat

pdf(file="Cost to Revenue Ratio by Species Category.pdf",width=12, height=10)
ggplot(eTest,aes(SpeciesCatName,CostRevRatio)) + 
  geom_boxplot(aes(fill=SpeciesCatName),legend.position='none') +
  coord_flip() + # make boxplot horizontal
  theme(text=element_text(size=20),legend.position='none') +
  labs(title="Cost to Revenue Ratio by Species Category", x = "ISSCAAP Group", y = "Cost to Revenue Ratio",fill="ISSCAAP Group")
dev.off()

# Boxplots by species category and region

regs<-c('37','51','27','21','77','34','81','48','47','31','87','57','41','71','67','61','88','58','18')

pdf(file="Cost to Revenue Ratio by Species Category and Region.pdf",width=12, height=10)

for(b in 1:length(regs))
{
  eTestTemp<-eTest[grepl(regs[b],eTest$RegionFAO),]
  
  print(ggplot(eTestTemp,aes(SpeciesCatName,CostRevRatio)) + 
    geom_violin(aes(fill=SpeciesCatName),legend.position='none') +
    coord_flip() + # make boxplot horizontal
    theme(text=element_text(size=20),legend.position='none') +
    labs(title=paste(regs[b],"Cost to Revenue Ratio by Species Category",sep=' '), x = "ISSCAAP Group", y = "Cost to Revenue Ratio",fill="ISSCAAP Group"))
}

dev.off()

##

ggplot(eTest,aes(SpeciesCatName,CostRevRatio)) + 
  geom_boxplot(aes(fill=SpeciesCatName),outlier.shape=NA) +
  scale_y_continuous(limits=c(0,5)) +
  coord_flip() + # make boxplot horizontal
  theme(text=element_text(size=15)) +
  labs(title="Cost to Revenue Ratio by Species Category", x = "ISSCAAP Group", y = "Cost to Revenue Ratio",fill="ISSCAAP Group")


# summary table of boxplot stats
SpCats<-unique(eTest$SpeciesCatName)

SummaryTable<-data.frame(matrix(nrow=length(SpCats),ncol=9))
colnames(SummaryTable)<-c('ISSCAAP Group','Stocks', 'Min','1st Quart','Median','Mean','3rd Quart','Max')

for(b in 1:length(SpCats))
{
  temp<-eTest$CostRevRatio[eTest$SpeciesCatName==SpCats[b]]
  
  x<-summary(temp)
  
  SummaryTable[b,1]<-SpCats[b] # name of category
  SummaryTable[b,2]<-length(temp) # number of stocks
  SummaryTable[b,3:9]<-x[1:7] # stats
}

write.csv(SummaryTable,file='Catch_Revenue_Ratios_ISSCAAP.csv')

# Histograms and scatterplots of ratio by Species Category
pdf(file='Histograms_Scatterplots_CostRevenue_ISSCAAP.pdf')
for(a in 1:length(SpCats))
{
  a1<-ggplot(eTest[eTest$SpeciesCatName==SpCats[a] & eTest$CostRevRatio<5,],aes(CostRevRatio)) + 
    geom_histogram(binwidth=0.1) +
    labs(title=paste(SpCats[a],"Cost to Revenue Ratio",sep=' '))
 print(a1)
  
  b1<-ggplot(eTest[eTest$SpeciesCatName==SpCats[a],],aes(Revenue,Cost)) + 
    geom_point(alpha=.4) + 
    geom_abline(intercept=0,slope=1) +
#     coord_cartesian(xlim=c(0,1000000))
    labs(title=paste(SpCats[a],"Cost against Revenue",sep=' '))
 print(b1)
 
}
dev.off()



# Histograms of Historic Cost/Revenue Ratio by Policy

polys<-unique(eTest$Policy)

pdf()

for(a in 1:length(polys))
{

  temp<-eTest[eTest$Policy==polys[a],]
  
  layout(matrix(c(1,2,1,2,3,4,3,4), 4, 2, byrow = TRUE))
  
plot1<-  hist(temp$CostRevRatio,
       main=paste("Hist of Cost/Rev Ratio",polys[a], sep=" "),
       xlab="Cost/Revenue Ratio")
print(plot1)

plot2<- hist(temp$CostRevRatio[temp$CostRevRatio<2],
       main=paste("Truncated Hist of Cost/Rev Ratio", polys[a],sep=" "),
       xlab="Cost/Revenue Ratio")
  
print(plot2)
  # hist(temp$CostRevRatio[temp$CostRevRatio<4 & temp$Dbase=="RAM"])
  # 
  # hist(temp$CostRevRatio[temp$CostRevRatio<4 & temp$Dbase=="FAO"])
  
plot3<- hist(log(temp$CostRevRatio),
       main=paste("Hist of Log Cost/Rev Ratio",polys[a],sep=" "),
       xlab="Log of Cost/Revenue Ratio")
print(plot3)  
  # plot ratio against BvBmsy
  
plot4<-  plot(temp$CostRevRatio[temp$Dbase=="FAO" & temp$CostRevRatio<4] ~ temp$BvBmsy[temp$Dbase=="FAO"  & temp$CostRevRatio<4],
         main=paste("Cost/Rev Ratio against B/Bmsy for FAO",polys[a],sep=" "),
        ylab="Cost/Revenue Ratio",
       xlab="B/Bmsy")
  print(plot4)
#   xyplot(CostRevRatio ~ BvBmsy, data=temp[temp$CostRevRatio<3 & temp$Dbase=="FAO",],
#          main="Historic Cost/Revenue Ratio against B/Bmsy for FAO Fisheries")
}
  dev.off()

# Boxplot
pdf()
layout(matrix(c(1,1,1,1,2,2,2,2), 2, 4, byrow = TRUE))

plot1<-boxplot(CostRevRatio~Policy,data=eTest,
        ylab="Cost/Revenue Ratio",
        xlab="Policy")
print(plot1)

plot2<-boxplot(CostRevRatio~Policy,data=eTest[eTest$CostRevRatio<5,],
        ylab="Cost/Revenue Ratio",
        xlab="Policy")
print(plot2)
dev.off()
