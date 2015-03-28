arg1=subset(ProjectionData, Year>=2012)

arg2=subset(UnlumpedProjectionData,Year>=2012)

change=ddply(arg2,c('Policy','Year'),summarize,Profits= sum(Profits,na.rm=T),DiscProfits=sum(DiscProfits,na.rm=T))
             
quartz()
ggplot(data=change,aes(x=Year,y=Profits,color=Policy))+geom_point()

arg=ddply((UnlumpedProjectionData),c('Country'),
      summarize,CSProfits=sum(Biomass[Policy=='CatchShare' & Year==max(Year)],na.rm=T),
      SQProfits=sum(Biomass[Policy=='Business As Usual' & Year==max(Year)],na.rm=T),
      CurrentProfits=sum(Biomass[Policy=='Business As Usual' & Year==2013],na.rm=T))

arg$Futurewtf<- ((arg$CSProfits-arg$SQProfits)/arg$SQProfits)*100*sign(arg$SQProfits) 

arg$Currentwtf<- (arg$CSProfits-arg$CurrentProfits)/arg$CurrentProfits*100*sign(arg$CurrentProfits) 


quartz()
ggplot(data=arg,aes(x=Country,y=Futurewtf))+geom_bar(stat='identity')

quartz()
ggplot(data=arg,aes(x=Country,y=Currentwtf))+geom_bar(stat='identity')

arg=ddply(subset(UnlumpedProjectionData),c('Country'),
          summarize,DeltaProfits=sum(Profits[Policy=='CatchShare' & Year==max(Year)],na.rm=T)/sum(Profits[Policy=='Business As Usual' & Year==2013],na.rm=T))

quartz()
ggplot(data=arg,aes(x=Country,y=DeltaProfits))+geom_bar(stat='identity')

5303862060tig