

real <- subset(ProjectionData,IdLevel == 'Species' & Policy == 'CatchShare')

fake <- subset(MonteMat,IdLevel == 'Species' & Policy == 'CatchShare')

real$database <- 'real'

fake$database <- 'fake'

quartz()
plot(real$BvBmsy,real$FvFmsy)


(ggplot(subset(fake,Year > 2012 & IdOrig =="9177-FAO-34-31"  ),aes(BvBmsy, FvFmsy,group = IdOrig,color = Dbase))
+ geom_point() + geom_smooth())

real <- trendit(subset(ProjectionData, IdLevel == 'Species'))

fake <- trendit(subset(MonteMat, IdLevel == 'Species'))

fake$dataset <- 'fake'

real$dataset <- 'real'

fok <- rbind(fake, real)

quartz()

arrr <- (ggplot(subset(fok,Policy %in% c('CatchShare')),
                aes(Year,total_profits,color = dataset,color = Policy,size = total_catch)) + geom_point())


