
OverlapPlot<-function(FinalData)
{
   
FilteredCatch<-ddply(FinalData[FinalData$Year<=2011,],c('Year'),summarize,ModelCatch=sum(Catch,na.rm=T))

FaoCatch<-ddply(fao[fao$Year>=min(FilteredCatch$Year),],c('Year'),summarize,GlobalCatch=sum(Catch,na.rm=T))

OverlapTest<-cbind(FilteredCatch,FaoCatch$GlobalCatch)

# pdf()
print(xyplot(ModelCatch+FaoCatch$GlobalCatch~Year,data=OverlapTest,type='b',
             auto.key=list(space='inside'),
             main='Global Catch Comparison Between FAO Dataset and Filtered Dataset',
             ylab='Total Catch in Dataset'))
# dev.off()

return()

}