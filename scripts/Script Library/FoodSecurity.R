
library(dplyr)
library(ggplot2)

foodsec<-read.csv('Data/FoodSecurityAnalysis.csv',stringsAsFactors=F)

cntryres<-read.csv(paste(ResultFolder,'Country Results All Stocks.csv',sep=''),stringsAsFactors=F)

cntryres<-cntryres %>%
  filter(IdOrig=='All Stocks')

# calculate upsides
cntryres$FoodUpsideTodayCS3<-cntryres$Catch_CS3-cntryres$Catch_Today

cntryres$FoodUpsideBAUCS3<-cntryres$Catch_CS3-cntryres$Catch_BAU

cntryres$FoodUpsideTodayCS<-cntryres$Catch_CS-cntryres$Catch_Today

cntryres$FoodUpsideBAUCS<-cntryres$Catch_CS-cntryres$Catch_BAUPessimistic

# subset data
cntryres<-cntryres[,c('Country','FoodUpsideTodayCS3','FoodUpsideBAUCS3','FoodUpsideTodayCS','FoodUpsideBAUCS')]

colnames(cntryres)<-c('Region','FoodUpsideTodayCS3','FoodUpsideBAUCS3','FoodUpsideTodayCS','FoodUpsideBAUCS')

df<-inner_join(cntryres,foodsec)

ggplot(df,aes(x=log(Total_kcal),y=log(FoodUpsideBAUCS3))) +
  geom_point(size=3)

# Plot food security indicators

pdf(paste(ResultFolder,'Food_Security_Total.pdf',sep=''))

ggplot(df,aes(x=log(Total_kcal),y=log(FoodUpsideBAUCS))) +
  geom_point(size=5) +
  labs(x='Log Total Food Deficit of Country (kcal)',y='Log Total Food Upside from RBFM Relative to BAU (MT)')

dev.off()

pdf(paste(ResultFolder,'Food_Security_PerCapita.pdf',sep=''))

ggplot(df,aes(x=food_gap_kcal_percapita_yr,y=log(FoodUpsideBAUCS))) +
  geom_point(size=5) +
  labs(x='Log Annual Per-Capita Food Deficit of Country (kcal)',y='Log Total Food Upside from RBFM Relative to BAU (MT)')

dev.off()