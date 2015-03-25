CatchAndBioSummary<- function(Data,FigureFolder)
{

AllYearSummary<- ddply(subset(Data,Policy=='Historic' | Policy=='Catch Share Three'),c('Country','Year'),
                    summarize,TotalCatch=sum(Catch,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),
                    PercRam=sum(Dbase=='RAM')/length(Dbase))

YearSummary<- subset(AllYearSummary,Year==2012)

YearSummary$Country<- as.factor(YearSummary$Country)

## Reorder fullname based on the the sum of the other columns
YearSummary$Country <- reorder(YearSummary$Country, 1/YearSummary$TotalBiomass)

YearSummary$BioRank<- order(YearSummary$TotalBiomass,decreasing=T)

YearSummary$CatchRank<- order(YearSummary$TotalCatch,decreasing=T)

## Examine the new factor order
levels(YearSummary$Country)
attributes(YearSummary$Country)

pdf(paste(FigureFolder,'2012 Biomass Rankings.pdf',sep=''))
print(ggplot((YearSummary[YearSummary$BioRank,][1:20,]),aes(x=Country,y=TotalBiomass,fill=PercRam))+geom_bar(stat='identity')
+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
dev.off()

YearSummary$Country <- reorder(YearSummary$Country, 1/YearSummary$TotalCatch)


pdf(paste(FigureFolder,'2012 Catch Rankings.pdf',sep=''))
print(ggplot((YearSummary[YearSummary$CatchRank,][1:20,]),aes(x=Country,y=TotalCatch,fill=PercRam))+geom_bar(stat='identity')
 +theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
dev.off()

YearSummary<- subset(AllYearSummary,Year==2050)

YearSummary$Country<- as.factor(YearSummary$Country)

## Reorder fullname based on the the sum of the other columns
YearSummary$Country <- reorder(YearSummary$Country, 1/YearSummary$TotalBiomass)

YearSummary$BioRank<- order(YearSummary$TotalBiomass,decreasing=T)

YearSummary$CatchRank<- order(YearSummary$TotalCatch,decreasing=T)

## Examine the new factor order
levels(YearSummary$Country)
attributes(YearSummary$Country)

pdf(paste(FigureFolder,'2050 Biomass Rankings.pdf',sep=''))
print(ggplot((YearSummary[YearSummary$BioRank,][1:20,]),aes(x=Country,y=TotalBiomass,fill=PercRam))+geom_bar(stat='identity')
      +theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
dev.off()

YearSummary$Country <- reorder(YearSummary$Country, 1/YearSummary$TotalCatch)


pdf(paste(FigureFolder,'2050 Catch Rankings.pdf',sep=''))
print(ggplot((YearSummary[YearSummary$CatchRank,][1:20,]),aes(x=Country,y=TotalCatch,fill=PercRam))+geom_bar(stat='identity')
 +theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
dev.off()

#### Species Categories

SpCat<- ddply(subset(ProjectionData,Year==2012)
             ,c('SpeciesCatName'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),
             MeanRamBvBmsy=mean(BvBmsy[Dbase=='RAM'],na.rm=T))

SpCat$SpeciesCatName<- as.factor(SpCat$SpeciesCatName)

## Reorder fullname based on the the sum of the other columns
SpCat$SpeciesCatName<- reorder(SpCat$SpeciesCatName, SpCat$MedianBvBmsy)

pdf(paste(FigureFolder,'2012 BvBmsy by Species Category.pdf',sep=''))
print(ggplot(SpCat,aes(x=SpeciesCatName,y=MedianBvBmsy,fill=MeanRamBvBmsy))+geom_bar(stat='identity')
 +theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
dev.off()

}


