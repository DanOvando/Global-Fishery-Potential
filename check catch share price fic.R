realproj <- subset(DataPlus,Policy == 'Historic' & IdLevel == 'Species' & Year == 2012 & CatchShare ==1)
# realproj <- subset(projdata,Policy == 'Historic' & IdLevel == 'Species' & Year == 2012)


# realproj <- subset(ProjectionData,Policy == 'Historic' & IdLevel == 'Species' & Year == 2012)
# 
fakeproj <- subset(DataPlus,Year == 2050 & Policy == 'StatusQuoOpenAccess' & IdLevel == 'Species' & CatchShare ==1)

# ggplot(realproj,aes(BvBmsy,FvFmsy)) + geom_point()
# 
# quartz()
par(mfrow = c(1,2))
plot(realproj$Price,fakeproj$Price)
abline(a=0,b=1)
plot(realproj$MarginalCost,fakeproj$MarginalCost)
abline(a = 0,b = 1)