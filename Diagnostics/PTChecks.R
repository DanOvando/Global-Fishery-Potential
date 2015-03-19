quartz()
ggplot(data=(ProjectionData),aes(x=Biomass/k,y=FvFmsy,group=IdOrig))+geom_line()+facet_wrap(~Policy,scales='free')

ddply(subset(ProjectionData,Year==max(Year)),c('Policy'),summarize,totalcatch=sum(Catch,na.rm=T))

quartz()
(ggplot(data=(ProjectionData),aes(x=Year,y=Biomass/k,group=IdOrig))+geom_line()+facet_wrap(~Policy,scales='free')
