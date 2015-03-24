# fuck<- subset(ProjectionData,
# # 
# # # arg=ddply((fuck),c('Country'),summarize,
# # #           DeltaBio=((sum(Biomass[Poli(cy=='CatchShare' & Year==2050],na.rm=T) - sum(Biomass[Policy=='CatchShare' & Year==2012],na.rm=T)) 
# # #                     /sum(Biomass[Policy=='Business As Usual Optimistic' & Year==2050],na.rm=T))-1)
# # 
# arg=ddply((fuck),c('Year','Policy'),summarize,TotalBio=sum(Biomass,na.rm=T),MeanTotalBio=mean(Biomass,na.rm=T),
#           PresentValue=sum(DiscProfits,na.rm=T),MeanBio=mean(BvBmsy,na.rm=T),MeanBioRatio=mean(Biomass/Bmsy,na.rm=T),TotalBmsy=sum(Bmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T))
# 
# arg=subset(arg,Year>=2012)
# 
# quartz()
# ggplot(data=arg,aes(x=Year,y=(MeanBioRatio),color=Policy))+geom_point(alpha=0.6)
# # 
# # # ggplot(data=fuck,aes(y=Biomass/k))+geom_density()
# # 
# # fuck<- ProjectionData
# # 
# # # arg=ddply((fuck),c('Country'),summarize,
# # #           DeltaBio=((sum(Biomass[Policy=='CatchShare' & Year==2050],na.rm=T) - sum(Biomass[Policy=='CatchShare' & Year==2012],na.rm=T)) 
# # #                     /sum(Biomass[Policy=='Business As Usual Optimistic' & Year==2050],na.rm=T))-1)
# # 
# # arg=ddply((fuck),c('Country','Year','Policy'),summarize,TotalBio=sum(Biomass,na.rm=T),
# #           PresentValue=sum(DiscProfits,na.rm=T),MeanBio=mean(BvBmsy,na.rm=T),TotalBmsy=sum(Bmsy,na.rm=T),TotalMSY=sum(MSY,na.rm=T))
# # 
# # arg=subset(arg,Year>=2012)
