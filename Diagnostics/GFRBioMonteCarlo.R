# GFRMonteCarlo<- function(CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
# {
#   
rm(list=ls())
load('Results/Catch Share Fix Full Run No Discount/Data/Global Fishery Recovery Results.rdata')
library(parallel)
library(plyr)
library(ggplot2)
library(reshape2)

# library(proftools)
# library(shiny)
source('Diagnostics/BioMonteCarlo.R')
source('Functions/BioErrorNearestNeighborNeis.R')

load(paste(ResultFolder,'ProjectionData Data.rdata',sep=''))

Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])

Stocks<- Stocks[Stocks %in% CatchMSYPossibleParams$IdOrig ]

NumCPUs<- 1


MonteMat<- BioMonteCarlo(5,Stocks=Stocks,ProjectionData=ProjectionData,BiomassData=BiomassData,
                         MsyData=MsyData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                          PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=0.5)

BioMonte<- ddply(subset(MonteMat,Policy %in% c('Business As Usual','Business As Usual Pessimistic'
                                               ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three'))
                        ,c('Iteration','Policy'),summarize,FinalProfits=sum(Profits[Year==2050],na.rm=T)
                 ,FinalBiomass=sum(Biomass[Year==2050],na.rm=T))

FigureFolder<- paste(BatchFolder,'Diagnostics/Monte Carlo/',sep='')

dir.create(FigureFolder,recursive=T)

pdf(file=paste(FigureFolder,'BvBmsy Monte Carlo.pdf',sep=''))

print(ggplot(data=BioMonte,aes(x=FinalBiomass,y=FinalProfits,color=Policy))+geom_point(size=4,alpha=0.7))
dev.off()

save(MonteMat,file=paste(ResultFolder,'MonteCarlo_Results.Rdata',sep=''))
# load('Results/3.0/Data/MonteCarlo_Results.Rdata')


MonteCarlo<- ddply(subset(MonteMat,Policy=='Catch Share Three' | Policy=='Fmsy Three' |  Policy=='Business As Usual' | Policy=='Business As Usual Pessimistic'),c('Iteration','Year','Policy'),summarize,MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
                   Catch=sum(Catch,na.rm=T),BvBmsy=median(BvBmsy,na.rm=T),FvFmsy=median(FvFmsy,na.rm=T),MedianBOA=median(BOA,na.rm=T))

MonteCarlo$Policy[MonteCarlo$Policy=='Fmsy Three']<- 'Fmsy'

MonteCarlo$Policy[MonteCarlo$Policy=='Catch Share Three']<- 'RBFM'

MonteCarlo$Policy[MonteCarlo$Policy=='Business As Usual']<- 'BAU (S1)'

MonteCarlo$Policy[MonteCarlo$Policy=='Business As Usual Pessimistic']<- 'BAU (S2)'


MonteCarlo<- subset(MonteCarlo,is.infinite(Catch)==F & is.na(BvBmsy)==F)

FigureFolder<- paste(BatchFolder,'Diagnostics/Monte Carlo/',sep='')

dir.create(FigureFolder,recursive=T)

pdf(file=paste(FigureFolder,'MonteCarlo_MSY.pdf',sep=''))
MCMSY<- (ggplot(data=subset(MonteCarlo,Policy=='RBFM' & Year==BaselineYear),aes(MSY),alpha=0.8)+geom_density(fill='steelblue2'))
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
               geom_vline(aes(xintercept=0,alpha=0.8),color='red',linetype='longdash',size=1)+facet_wrap(~Policy)+ylab("Scaled Density")
             +scale_fill_discrete(name = "Policy Alternative")
             #                coord_cartesian(xlim=c(-3e11,2e11))
             
)
print(MCProfits)
dev.off()

pdf(file=paste(FigureFolder,'MC_Catch.pdf',sep=''))
MCCatch<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Catch,fill=Policy))+
             geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+
             facet_wrap(~Policy)+ylab("Scaled Density")+scale_fill_discrete(name = "Policy Alternative")
           #                coord_cartesian(xlim=c(-3e11,2e11))
           
)
print(MCCatch)
dev.off()

# 
# pdf(file=paste(FigureFolder,'MC_Catch.pdf',sep=''))
# MCCatch<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Catch,fill=Policy))+
#              geom_density(alpha=0.5)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
# )
# print(MCCatch)
# dev.off()


pdf(file=paste(FigureFolder,'MC_BvBmsy.pdf',sep=''))
MCBvB<-(ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(BvBmsy,fill=(Policy)))+
          geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
        +geom_vline(aes(xintercept=1),color='red',linetype='longdash')+xlim(c(0,2.5))+
          facet_wrap(~Policy)+scale_fill_discrete(name = "Policy Alternative"))
print(MCBvB)
dev.off()

pdf(file=paste(FigureFolder,'MC_BvBmsy_OA.pdf',sep=''))
MCBvB_OA<-(ggplot(data=subset(MonteCarlo,Year==max(Year) & Policy=='RBFM'),aes(MedianBOA,fill=(Policy)))+
             geom_density(alpha=0.7)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
print(MCBvB_OA)
dev.off()

pdf(file=paste(FigureFolder,'MC_FvFmsy.pdf',sep=''))
MCFvF<-(ggplot(data=subset(MonteCarlo,Year==max(Year) ),aes(jitter(FvFmsy,factor=.1),fill=(Policy)))+
          geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
        +xlab('F/Fmsy')+facet_wrap(~Policy)+scale_fill_discrete(name = "Policy Alternative")+xlim(c(0,2))
        +geom_vline(aes(xintercept=1),color='red',linetype='longdash')
        #         + coord_cartesian(ylim=c(0,25))+xlab('FvFmsy'))
)

print(MCFvF)
dev.off()




save(MCProfits,MCCatch,MCBvB,MCFvF,MCMSY,file=paste(FigureFolder,'MonteCarlo Plots.rdata',sep=''))


