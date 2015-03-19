CountryChange=ddply(subset(ProjectionData,Policy=='CatchShare'),c('Country'),
                    summarize,DeltaProfits=sum(Profits[Year==max(Year)])/sum(Profits[Year==2013])-1,
                    DeltaBio=sum(Biomass[Year==max(Year)],na.rm=T)/sum(Biomass[Year==2013],na.rm=T)-1,MSY=sum(MSY,na.rm=T))


quartz()
ggplot(data=CountryChange,aes(x=DeltaBio,y=DeltaProfits))+geom_point()

biomass x
delta profits y
size = msy
