# GFRMonteCarlo<- function(CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
# {
#   
rm(list=ls())
load('Results/4.2/Data/Global Fishery Recovery Results.rdata')
library(parallel)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)

# library(proftools)
# library(shiny)
source('Diagnostics/BioMonteCarlo.R')
source('Functions/BioErrorNearestNeighborNeis.R')

load(paste(ResultFolder,'ProjectionData Data.rdata',sep=''))

Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])

Stocks<- Stocks[Stocks %in% PolicyStorage$IdOrig ]

NumCPUs<- 1

load(paste(ResultFolder,'MsyData.Rdata',sep=''))

PolicyStorage$IdOrig<- as.character(PolicyStorage$IdOrig)

PolicyStorage<- PolicyStorage[order(PolicyStorage$IdOrig),]

# Rprof()
MonteMat<- (BioMonteCarlo(50,Stocks=Stocks,ProjectionData=ProjectionData,BiomassData=BiomassData,
                         MsyData=MsyData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                         PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=0))

#    Rprof(NULL)
#     RProfData<- readProfileData('Rprof.out')
#     flatProfile(RProfData,byTotal=TRUE)

save(file=paste(ResultFolder,'Bio Monte Carlo.Rdata',sep=''),MonteMat)

# load(file=paste(ResultFolder,'Bio Monte Carlo.Rdata',sep=''))



LastProj<- subset(ProjectionData,Policy=='CatchShare') %>%
  select(IdOrig,Year,MSY,BvBmsy,FvFmsy,Catch,IdLevel,Biomass) %>%
  mutate(Iteration=0,Name=paste(IdOrig,Year,sep='-'))

LastMonte<- subset(MonteMat,Policy=='CatchShare') %>%
  select(IdOrig,Year,MSY,Catch,BvBmsy,FvFmsy,IdLevel,Biomass,Iteration) %>%
  mutate(Name=paste(IdOrig,Year,sep='-'))


Check<- ddply(LastMonte,c('Name'),summarize,MeanFvFmsy=mean(FvFmsy),MeanBvBmsy=mean(BvBmsy),MeanMSY=mean(MSY),MeanBiomass=mean(Biomass),MeanCatch=mean(Catch))

Comp<- join(LastProj,Check,by='Name')

CmpCatch<- ggplot(data=subset(Comp),aes(Catch,MeanCatch,color=IdLevel))+geom_point()

Wrong<- which((Comp$Biomass/Comp$MeanBiomass-1)>1)

BioMonte<- ddply(subset(MonteMat,Policy %in% c('Business As Usual','Business As Usual Pessimistic'
                                               ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three'))
                 ,c('Iteration','Policy'),summarize,FinalProfits=sum(Profits[Year==2050],na.rm=T)
                 ,FinalBiomass=sum(Biomass[Year==2050],na.rm=T),FinalFisheries=length(unique(IdOrig)))



ProjMonte<- ddply(subset(ProjectionData,Policy %in% c('Business As Usual','Business As Usual Pessimistic'
                                               ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three'))
                 ,c('Policy'),summarize,FinalProfits=sum(Profits[Year==2050],na.rm=T)
                 ,FinalBiomass=sum(Biomass[Year==2050],na.rm=T),FinalFisheries=length(unique(IdOrig)))


FigureFolder<- paste(BatchFolder,'Diagnostics/Monte Carlo 2/',sep='')

dir.create(FigureFolder,recursive=T)

pdf(file=paste(FigureFolder,'BvBmsy Monte Carlo.pdf',sep=''),width=7,height=5)

BioMonte$Policy[BioMonte$Policy=='Business As Usual']<- 'BAU (CC)'

BioMonte$Policy[BioMonte$Policy=='Business As Usual Pessimistic']<- 'BAU'

BioMonte$Policy[BioMonte$Policy=='Catch Share Three']<- 'RBFM (CC)'

BioMonte$Policy[BioMonte$Policy=='CatchShare']<- 'RBFM'

BioMonte$Policy[BioMonte$Policy=='Fmsy Three']<- 'Fmsy (CC)'

BioMontePlot<- (ggplot(data=BioMonte,aes(x=FinalBiomass,y=FinalProfits,color=Policy))+geom_point(size=4,alpha=0.7)+
  ylab('2050 Profits ($)')+xlab('2050 Biomass (MT)'))

print(BioMontePlot)
dev.off()


BioMonte<- BioMonte %>% 
  group_by(Iteration) %>%
  mutate(BioOrder=rank(-FinalBiomass),ProfitOrder=rank(-FinalProfits))

BioMonte<- BioMonte %>%
  group_by(Policy) %>%
  mutate(MeanBioRank=mean(BioOrder),MeanProfitRank=mean(ProfitOrder))

BioMonte$Policy<- reorder(BioMonte$Policy,BioMonte$MeanProfitRank)

BioMonte$ProfitOrder<- as.factor(BioMonte$ProfitOrder)

BioMonte$ProfitOrder<- reorder(BioMonte$ProfitOrder,-as.numeric(BioMonte$ProfitOrder))


BioMonte$BioOrder<- as.factor(BioMonte$BioOrder)

BioMonte$BioOrder<- reorder(BioMonte$BioOrder,-as.numeric(BioMonte$BioOrder))
Font<- 'Helvetica'

FontColor<- 'Black'

BarTheme<- theme(text=element_text(size=16,family=Font,color=FontColor),
                     axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9,color=FontColor),
                     axis.text.y=element_text(color=FontColor))


ProfitRanking<- (ggplot(BioMonte,aes(Policy,fill=factor(ProfitOrder)))+geom_bar()+BarTheme
 +scale_fill_brewer(palette='RdYlGn')+guides(fill=guide_legend(reverse=T,title='Profit Ranking'))+ylab('Iterations'))

BioRanking<- (ggplot(BioMonte,aes(Policy,fill=factor(BioOrder)))+geom_bar()+BarTheme
                 +ylab('Iterations')+scale_fill_brewer(palette='RdYlGn')+guides(fill=guide_legend(reverse=T,title='Biomass Ranking')))


save(BioMontePlot,ProfitRanking,BioRanking,file=paste(FigureFolder,'BioMontePlots.Rdata',sep=''))

save(MonteMat,file=paste(ResultFolder,'BioMonteCarlo_Results.Rdata',sep=''))


