AB=a[is.na(a$Profits)==F,]

AB$WTF<- AB$Price*AB$BvBmsy*AB$FvFmsy*AB$MSY-AB$MarginalCost*(AB$FvFmsy*AB$g)^1.3


quartz()
AB$CatchShareRam<- AB$CatchShare==1 & AB$Dbase=='RAM'

ggplot(AB,aes(x=Profits,y=WTF,color=CatchShareRam))+geom_point(alpha=0.5)

ggplot(AB,aes(x=Profits,fill=Policy))+geom_histogram(alpha=0.5)