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


# Rprof(tmp <- tempfile(),line.profiling=T)
NumCPUs<- 2

MonteMat<- SnowMonteCarlo(100,Stocks=Stocks[1:100],ProjectionData=ProjectionData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                    PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=0.1)
# Rprof()
# summaryRprof(tmp)
# unlink(tmp)
# quartz()
# ggplot(data=subset(ProjectionData,Policy=='StatusQuoOpenAccess'),aes(x=BvBmsy,y=FvFmsy,group=IdOrig))
# +geom_line()
  # MonteMat<- ldply(MonteMat)
save(MonteMat,file=paste(ResultFolder,'MonteCarlo_Results.Rdata',sep=''))
# load('Results/2_18_15 Complete Run/Data/MonteCarlo_Results.Rdata')
# 


# MonteCarlo<- ddply(MonteMat,c('Iteration','Year','Policy'),summarize,MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
#                    Catch=sum(Yields,na.rm=T))
# 
# 
# quartz()
# ggplot(data=subset(MonteCarlo,Year==2013 & Policy=='Opt'),aes(MSY))+geom_density(fill='steelblue2')
# 
# quartz()
# ggplot(data=subset(MonteCarlo,Year==2012 | Year==2047),aes(Catch,fill=factor(Year)))+geom_density(alpha=0.2)+facet_wrap(~Policy)
# 



# }