# GFRMonteCarlo<- function(CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
# {
#   

load('Results/2_18_15 Complete Run/Data/Global Fishery Recovery Results.rdata')
library(parallel)
library(plyr)
library(ggplot2)
library(reshape2)

# library(proftools)
# library(shiny)
source('Diagnostics/SnowMonteCarlo.R')

Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])

Stocks<- Stocks[Stocks %in% CatchMSYPossibleParams$IdOrig ]

# Rprof(tmp <- tempfile(),line.profiling=T)
NumCPUs<- 3

MonteMat<- SnowMonteCarlo(100,Stocks=Stocks,ProjectionData=ProjectionData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                    PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=0.5)
# Rprof()
# summaryRprof(tmp)
# unlink(tmp)
# quartz()
# ggplot(data=subset(ProjectionData,Policy=='StatusQuoOpenAccess'),aes(x=BvBmsy,y=FvFmsy,group=IdOrig))
# +geom_line()
  # MonteMat<- ldply(MonteMat)
save(MonteMat,file=paste(ResultFolder,'MonteCarlo_Results.Rdata',sep=''))
# load('Results/2_18_15 Complete Run/Data/MonteCarlo_Results.Rdata')


MonteCarlo<- ddply(MonteMat,c('Iteration','Year','Policy'),summarize,MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
                   Catch=sum(Catch,na.rm=T),BvBmsy=median(BvBmsy,na.rm=T),FvFmsy=median(FvFmsy,na.rm=T))

MonteCarlo<- subset(MonteCarlo,is.infinite(Catch)==F & is.na(BvBmsy)==F)

pdf(file='Diagnostics/MonteCarlo_MSY.pdf')
(ggplot(data=subset(MonteCarlo,Policy=='CatchShare' & Year==BaselineYear),aes(MSY),alpha=0.8)+geom_density(fill='steelblue2'))
dev.off()


pdf(file='Diagnostics/MonteCarlo_Profits.pdf')
ggplot(data=MonteCarlo,aes(Profits,fill=factor(Year)))+geom_density(alpha=0.7)+facet_wrap(~Policy)
dev.off()

pdf(file='Diagnostics/MonteCarlo_Catch.pdf')
ggplot(data=MonteCarlo,aes((Catch),fill=factor(Year)))+geom_density(alpha=0.7)+facet_wrap(~Policy)
dev.off()

pdf(file='Diagnostics/MonteCarlo_BvBmsy.pdf')
ggplot(data=(MonteCarlo),aes(BvBmsy,fill=factor(Year)))+geom_density(alpha=0.7)+facet_wrap(~Policy,scale='free')
dev.off()

pdf(file='Diagnostics/MonteCarlo_FvFmsy.pdf')
ggplot(data=MonteCarlo,aes(FvFmsy,fill=factor(Year)))+geom_density(alpha=0.7)+facet_wrap(~Policy,scale='free')
dev.off()

pdf(file='Diagnostics/MonteCarlo_BvBmsyViolin.pdf')
ggplot(data=(MonteCarlo),aes(y=jitter(BvBmsy),x=factor(Year),fill=factor(Year)))+geom_violin(alpha=0.7)+facet_wrap(~Policy,scale='free')
dev.off()

pdf(file='Diagnostics/MonteCarlo_FvFmsyViolin.pdf')
ggplot(data=(MonteCarlo),aes(y=jitter(FvFmsy),x=factor(Year),fill=factor(Year)))+geom_violin(alpha=0.7)+facet_wrap(~Policy,scale='free')
dev.off()


