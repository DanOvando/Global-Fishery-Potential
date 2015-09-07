expanded_monte_carlo <- function(runfolder,CPUs,mciterations = 250,real_elastic_demand = T, real_sp_group_demand = F, elasticity = -0.9)
{
  elastic_demand <- real_elastic_demand
  
  sp_group_demand <- real_sp_group_demand
  
  show(elastic_demand)
  funcs <- as.vector(lsf.str())
  
  rm(list = funcs)
  
  sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
  
  NumCPUs <- CPUs
  
    load(paste('Results/',runfolder,'/Data/ProjectionData Data.rdata', sep = ''))
  
  PolicyStorage <- read.csv(paste('Results/',runfolder,'/Data/PolicyStorage.csv', sep = ''),stringsAsFactors = F)
  
  RealProjectionData <- ProjectionData
  
  ProjectionData<- OriginalProjectionData #Fisheries that have B/Bmsy, MSY, and we've run the projections
  
  NoBmsy<- is.na(ProjectionData$Bmsy)
  
  ProjectionData$k[NoBmsy]<- ((ProjectionData$MSY/ProjectionData$g)*(1/ProjectionData$BtoKRatio))[NoBmsy]
  
  ProjectionData$Bmsy[NoBmsy]<- (ProjectionData$MSY/ProjectionData$g)[NoBmsy]
  
  ProjectionData$Biomass[is.na(ProjectionData$Biomass) | ProjectionData$Biomass==0]<- (ProjectionData$BvBmsy*ProjectionData$Bmsy)[is.na(ProjectionData$Biomass) | ProjectionData$Biomass==0]
  
  ProjectionData<- ProjectionData %>%
    group_by(IdOrig,Policy) %>%
    mutate(NPV=cumsum(DiscProfits))
  rm(OriginalProjectionData,OriginalBiomassData,OriginalFullData,OriginalMsyData,MsyData,UnlumpedProjectionData)
  #   load(paste('Results/',runfolder,'/Data/MsyData.rdata', sep = ''))
  
  FigureFolder<- paste('Results/',runfolder,'/Diagnostics/expanded monte carlo/',sep='')
  
  ResultFolder<- paste('Results/',runfolder,'/Diagnostics/expanded monte carlo/',sep='')
  
  dir.create(FigureFolder,recursive=T)
  
  Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])
  
  Stocks<- Stocks[Stocks %in% PolicyStorage$IdOrig ]
  
  PolicyStorage$IdOrig<- as.character(PolicyStorage$IdOrig)
  
  PolicyStorage<- PolicyStorage[order(PolicyStorage$IdOrig),]
  
  # Rprof()
  
  MonteMat<- (run_expanded_montecarlo(mciterations,Stocks=Stocks,projdata=ProjectionData,
                                      PolicyStorage=PolicyStorage,ErrorSize=0, NumCPUs = CPUs,
                                      ResultFolder = ResultFolder, elastic_demand = elastic_demand, sp_group_demand = sp_group_demand,
                                      elasticity = elasticity))
  
  #    Rprof(NULL)
  #     RProfData<- readProfileData('Rprof.out')
  #     flatProfile(RProfData,byTotal=TRUE)
  
  #   save(file=paste(FigureFolder,'Expanded Monte Carlo.Rdata',sep=''),MonteMat)
  
  # load(file=paste(ResultFolder,'Bio Monte Carlo.Rdata',sep=''))
  
  
  LastProj<- subset(ProjectionData,Policy=='CatchShare') %>%
    dplyr::select(IdOrig,Year,MSY,BvBmsy,FvFmsy,Catch,IdLevel,Biomass) %>%
    mutate(Iteration=0,Name=paste(IdOrig,Year,sep='-'))
  
  LastMonte<- subset(MonteMat,Policy=='CatchShare') %>%
    dplyr::select(IdOrig,Year,MSY,Catch,BvBmsy,FvFmsy,IdLevel,Biomass,Iteration) %>%
    mutate(Name=paste(IdOrig,Year,sep='-'))
  
  
  Check<- LastMonte %>%
    group_by(Name) %>%
    summarize(MeanFvFmsy=mean(FvFmsy),MeanBvBmsy=mean(BvBmsy),MeanMSY=mean(MSY),MeanBiomass=mean(Biomass),MeanCatch=mean(Catch))
  
  
  Comp<- join(LastProj,Check,by='Name')
  
  CmpCatch<- ggplot(data=subset(Comp),aes(Catch,MeanCatch,color=IdLevel))+geom_point()
  
  Wrong<- which((Comp$Biomass/Comp$MeanBiomass-1)>1)
  #   
  #   BioMonte<- ddply(subset(MonteMat,Policy %in% c('Business As Usual','Business As Usual Pessimistic'
  #                                                  ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three'))
  #                    ,c('Iteration','Policy'),summarize,FinalProfits=sum(Profits[Year==2050],na.rm=T)
  #                    ,FinalBiomass=sum(Biomass[Year==2050],na.rm=T),FinalFisheries=length(unique(IdOrig)))
  #   
  #   
  #   
  MonteMat$Year[MonteMat$Policy == 'Historic' & MonteMat$Year == 2012] <- 2050
  
  BioMonte<- subset(MonteMat, Year == 2050 & Policy %in% c('Business As Usual','Business As Usual Pessimistic'
                                                           ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three','Historic')) %>%
    group_by(Iteration,Policy) %>% 
    summarize(FinalProfits=sum(Profits,na.rm=T)
              ,FinalBiomass=sum(Biomass,na.rm=T),FinalCatch = sum(Catch, na.rm = T),
              FinalFisheries=length(unique(IdOrig))) %>%
    ungroup() #%>%
#     dplyr::select(-Iteration)
  
  BioMonte$Policy[BioMonte$Policy=='Business As Usual']<- 'BAU (CC)'
  
  BioMonte$Policy[BioMonte$Policy=='Business As Usual Pessimistic']<- 'BAU'
  
  BioMonte$Policy[BioMonte$Policy=='Catch Share Three']<- 'RBFM (CC)'
  
  BioMonte$Policy[BioMonte$Policy=='CatchShare']<- 'RBFM'
  
  BioMonte$Policy[BioMonte$Policy=='Fmsy Three']<- 'Fmsy (CC)'
  
  BioMonte$Policy[BioMonte$Policy=='Historic']<- 'Today'
  
  
  RealProjectionData$Year[RealProjectionData$Policy == 'Historic' & RealProjectionData$Year == 2012] <- 2050
  
  
  ProjMonte<- subset(RealProjectionData,Year == 2050 & Policy %in% c('Business As Usual','Business As Usual Pessimistic'
                                                                 ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three','Historic')) %>%
    group_by(Policy) %>% 
    summarize(FinalProfits=sum(Profits,na.rm=T)
              ,FinalBiomass=sum(Biomass,na.rm=T),FinalCatch = sum(Catch, na.rm = T),
              FinalFisheries=length(unique(IdOrig)))
  
  ProjMonte$Policy[ProjMonte$Policy=='Business As Usual']<- 'BAU (CC)'
  
  ProjMonte$Policy[ProjMonte$Policy=='Business As Usual Pessimistic']<- 'BAU'
  
  ProjMonte$Policy[ProjMonte$Policy=='Catch Share Three']<- 'RBFM (CC)'
  
  ProjMonte$Policy[ProjMonte$Policy=='CatchShare']<- 'RBFM'
  
  ProjMonte$Policy[ProjMonte$Policy=='Fmsy Three']<- 'Fmsy (CC)'
  
  ProjMonte$Policy[ProjMonte$Policy=='Historic']<- 'Today'
  
    #   FigureFolder<- paste(BatchFolder,'Diagnostics/Monte Carlo 2/',sep='')
  #   
  #   dir.create(FigureFolder,recursive=T)
  
  pdf(file=paste(FigureFolder,'BvBmsy Monte Carlo.pdf',sep=''),width=7,height=5)
  
  
  BioMonte$monte <- 'MonteCarlo'
  
  ProjMonte$monte <- 'Real'
  
#   CompMonte <- rbind(BioMonte,ProjMonte)
  

  BioMontePlot<- (ggplot(data=BioMonte,aes(x=FinalBiomass,y=FinalProfits,color=Policy,size = FinalCatch))+geom_point(alpha=0.7)+
                    ylab('2050 Profits ($)')+xlab('2050 Biomass (MT)'))
  
#   
#   BioCompPlot<- (ggplot(data=CompMonte,aes(x=FinalBiomass,y=FinalProfits,color=Policy,size = FinalCatch))+geom_point(alpha=0.7)+
#                     ylab('2050 Profits ($)')+xlab('2050 Biomass (MT)') + facet_wrap(~monte))
#   browser()

  
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
  
  dropit <- ls()[!(ls() %in% c('BioMonte','BaselineYear','FigureFolder','BioMontePlot'))]
  
  rm(list = dropit)
  
  Font<- 'Helvetica'
  
  FontColor<- 'Black'
  
  BarTheme<- theme(text=element_text(size=16,family=Font,color=FontColor),
                   axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9,color=FontColor),
                   axis.text.y=element_text(color=FontColor))
  
  
  ProfitRanking<- (ggplot(BioMonte,aes(Policy,fill=factor(ProfitOrder)))+geom_bar()+BarTheme
                   +scale_fill_brewer(palette='RdYlGn')+guides(fill=guide_legend(reverse=T,title='Profit Ranking'))+ylab('Iterations'))
  
  BioRanking<- (ggplot(BioMonte,aes(Policy,fill=factor(BioOrder)))+geom_bar()+BarTheme
                +ylab('Iterations')+scale_fill_brewer(palette='RdYlGn')+guides(fill=guide_legend(reverse=T,title='Biomass Ranking')))
  
  
  expanded_montecarlo_plots <- list(BioMontePlot = BioMontePlot ,ProfitRanking = BioMontePlot,BioRanking = BioMontePlot)
  
  save(list = c('BioMontePlot','ProfitRanking','BioRanking'),file=paste(FigureFolder,'BioMontePlots.Rdata',sep=''))
  
  #   save(MonteMat,file=paste(ResultFolder,'BioMonteCarlo_Results.Rdata',sep=''))
  
  #   return(list(MonteMat = MonteMat, BioMonte = BioMonte))
  return(expanded_montecarlo_plots)
  
}

