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
show(ResultFolder)

save(Stocks,NumCPUs,file=paste(ResultFolder,'MonteCarlo_Test.Rdata',sep=''))

# Rprof(tmp <- tempfile(),line.profiling=T)
NumCPUs<- 1

MonteMat<- SnowMonteCarlo(50,Stocks=Stocks[1:50],ProjectionData=ProjectionData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                    PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=0.2)
# Rprof()
# summaryRprof(tmp)
# unlink(tmp)

# MonteMat<- ldply(MonteMat)

MonteMat<- MonteMat[is.infinite(MonteMat$FvFmsy)==F,]

MonteCarlo<- ddply(MonteMat,c('Iteration','Year','Policy'),summarize,MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
                   Catch=sum(Yields,na.rm=T))

save(MonteCarlo,MonteMat,file=paste(ResultFolder,'MonteCarlo_Results.Rdata',sep=''))

quartz()
ggplot(data=subset(MonteCarlo,Year==2013 & Policy=='Opt'),aes(MSY))+geom_density(fill='steelblue2')

quartz()
ggplot(data=subset(MonteCarlo,Year==2012 | Year==2047),aes(Catch,fill=factor(Year)))+geom_density(alpha=0.2)+facet_wrap(~Policy)




# }