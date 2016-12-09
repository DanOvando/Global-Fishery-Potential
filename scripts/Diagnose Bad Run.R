pd53 <- ProjectionData

upd53 <- UnlumpedProjectionData

pd54 <- ProjectionData

upd54 <- UnlumpedProjectionData


historic_pd53 <- subset(pd53,Policy == 'Historic' & Year == 2012) %>%
  arrange(IdOrig) %>%
  ungroup()

historic_pd54 <- subset(pd54,Policy == 'Historic' & Year == 2012) %>%
  arrange(IdOrig) %>%
  ungroup()

historic_pd53 <-  historic_pd53[(historic_pd53$IdOrig %in% historic_pd54$IdOrig),]

historic_pd54 <-  historic_pd54[(historic_pd54$IdOrig %in% historic_pd53$IdOrig),]




quartz()
par(mfrow = c(2,2))
plot(historic_pd54$MSY,historic_pd53$MSY)
abline(a=0,b=1)
plot(historic_pd54$Price,historic_pd53$Price)
abline(a=0,b=1)
plot(historic_pd54$MarginalCost,historic_pd53$MarginalCost)
abline(a=0,b=1)
plot(historic_pd54$Catch,historic_pd53$Catch)
abline(a=0,b=1)


quartz()
par(mfrow = c(2,2))
plot(historic_pd54$MSY[historic_pd54$CatchShare == 1],historic_pd53$MSY[historic_pd54$CatchShare == 1])
abline(a=0,b=1)
plot(historic_pd54$Price[historic_pd54$CatchShare == 1],historic_pd53$Price[historic_pd54$CatchShare == 1])
abline(a=0,b=1)
plot(log(historic_pd54$MarginalCost[historic_pd54$CatchShare == 1]),log(historic_pd53$MarginalCost[historic_pd54$CatchShare == 1]))
abline(a=0,b=1)
plot(historic_pd54$Catch[historic_pd54$CatchShare == 1],historic_pd53$Catch[historic_pd54$CatchShare == 1])
abline(a=0,b=1)
