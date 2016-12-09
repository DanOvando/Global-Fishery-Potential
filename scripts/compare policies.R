real <- subset(OriginalProjectionData, Policy == "CatchShare" & Year == 2013 & IdLevel == 'Neis' )

fake <- subset(OriginalProjectionData,  Policy == "CatchShare" & Year == 2050 & IdLevel == 'Neis' )

quartz()
par(mfrow = c(2,2))
plot(fake$Catch,real$Catch)
abline(a = 0,b=1)
plot(fake$Price,real$Price)
abline(a = 0,b=1)
plot(fake$Profits,real$Profits)
abline(a = 0,b=1)
plot(fake$MarginalCost,real$MarginalCost)
abline(a = 0,b=1)


oa <- subset(ProjectionData,Policy == 'Business As Usual Pessimistic' & Dbase!= 'RAM' & CatchShare ==0)

oa <- oa %>%
  group_by(Year) %>%
summarize(meanb = mean(BvBmsy, na.rm = T),meanf = mean(FvFmsy, na.rm = T))

ggplot(oa,aes(meanb,meanb)) + geom_line()