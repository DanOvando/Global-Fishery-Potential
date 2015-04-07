# GFRMonteCarlo<- function(CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
# {
#   
rm(list=ls())
load('Results/2.5/Data/Global Fishery Recovery Results.rdata')
library(parallel)
library(plyr)
library(ggplot2)
library(reshape2)

# library(proftools)
# library(shiny)
source('Diagnostics/SnowMonteCarlo.R')

load(paste(ResultFolder,'ProjectionData Data.rdata',sep=''))

Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])

Stocks<- Stocks[Stocks %in% CatchMSYPossibleParams$IdOrig ]

# Rprof(tmp <- tempfile(),line.profiling=T)
NumCPUs<- 3


MonteMat<- SnowMonteCarlo(500,Stocks=Stocks,ProjectionData=ProjectionData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                          PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=0.5)
# Rprof() 
# summaryRprof(tmp)
# unlink(tmp)
# quartz()
# ggplot(data=subset(ProjectionData,Policy=='StatusQuoOpenAccess'),aes(x=BvBmsy,y=FvFmsy,group=IdOrig))
# +geom_line()
# MonteMat<- ldply(MonteMat)
save(MonteMat,file=paste(ResultFolder,'MonteCarlo_Results.Rdata',sep=''))
# load('Results/Mycothpids Ahoy 2_25_15/Data/MonteCarlo_Results.Rdata')


MonteCarlo<- ddply(subset(MonteMat),c('Iteration','Year','Policy'),summarize,MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
                   Catch=sum(Catch,na.rm=T),BvBmsy=median(BvBmsy,na.rm=T),FvFmsy=median(FvFmsy,na.rm=T),MedianBOA=median(BOA,na.rm=T))

MonteCarlo<- subset(MonteCarlo,is.infinite(Catch)==F & is.na(BvBmsy)==F & Policy!='Food' & Policy != 'StatusQuoFForever')

FigureFolder<- paste(BatchFolder,'Diagnostics/Monte Carlo/',sep='')

dir.create(FigureFolder,recursive=T)

pdf(file=paste(FigureFolder,'MonteCarlo_MSY.pdf',sep=''))
MCMSY<- (ggplot(data=subset(MonteCarlo,Policy=='CatchShare' & Year==BaselineYear),aes(MSY),alpha=0.8)+geom_density(fill='steelblue2'))
print(MCMSY)
dev.off()


# pdf(file=paste(FigureFolder,'MC_Profits.pdf',sep=''))
# MCProfits<- (ggplot(data=MonteCarlo,aes(Profits,fill=factor(Year)))+geom_density(alpha=0.7)+facet_wrap(~Policy)
# )
# print(MCProfits)
# dev.off()

pdf(file=paste(FigureFolder,'MC_Profits.pdf',sep=''))
MCProfits<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Profits,fill=Policy))+
               geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+
               geom_vline(aes(xintercept=0,alpha=0.8),linetype='longdash',size=1)+facet_wrap(~Policy,scale='free')+ylab("Scaled Density")
#                coord_cartesian(xlim=c(-3e11,2e11))
             
)
print(MCProfits)
dev.off()

pdf(file=paste(FigureFolder,'MC_Catch.pdf',sep=''))
MCCatch<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Catch,fill=Policy))+
               geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+
               facet_wrap(~Policy)+ylab("Scaled Density")
             #                coord_cartesian(xlim=c(-3e11,2e11))
             
)
print(MCCatch)
dev.off()


# pdf(file=paste(FigureFolder,'MC_Catch.pdf',sep=''))
# MCCatch<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Catch,fill=Policy))+
#              geom_density(alpha=0.5)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
# )
# print(MCCatch)
# dev.off()


pdf(file=paste(FigureFolder,'MC_BvBmsy.pdf',sep=''))
MCBvB<-(ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(BvBmsy,fill=(Policy)))+
          geom_density(alpha=0.7)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+facet_wrap(~Policy,scale='free'))
print(MCBvB)
dev.off()

pdf(file=paste(FigureFolder,'MC_BvBmsy_OA.pdf',sep=''))
MCBvB_OA<-(ggplot(data=subset(MonteCarlo,Year==max(Year) & Policy=='CatchShare'),aes(MedianBOA,fill=(Policy)))+
          geom_density(alpha=0.7)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
print(MCBvB_OA)
dev.off()

pdf(file=paste(FigureFolder,'MC_FvFmsy.pdf',sep=''))
MCFvF<-(ggplot(data=subset(MonteCarlo,Year==max(Year) ),aes(jitter(FvFmsy,factor=.1),fill=(Policy)))+
          geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
#         + coord_cartesian(ylim=c(0,25))+xlab('FvFmsy'))
)
        
        print(MCFvF)
        dev.off()
        


        
        save(MCProfits,MCCatch,MCBvB,MCFvF,MCMSY,file=paste(FigureFolder,'MonteCarlo Plots.rdata',sep=''))
        
        
        