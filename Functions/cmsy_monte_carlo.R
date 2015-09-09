cmsy_monte_carlo <- function(runfolder,CPUs,mciterations = 250,BaselineYear = 2012, real_elastic_demand = T, real_sp_group_demand = F, elasticity)
{
  #   load(paste('Results/',runfolder,'/Data/Global Fishery Recovery Results.rdata', sep = ''))
  
  elastic_demand <- real_elastic_demand
  
  sp_group_demand <- real_sp_group_demand
  load(paste('Results/',runfolder,'/Data/Global Fishery Recovery Results.rdata', sep = ''))
  
  funcs <- as.vector(lsf.str())
  show(mciterations)
  rm(list = funcs)
  
  sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
  
  NumCPUs <- CPUs
  
  load(paste('Results/',runfolder,'/Data/ProjectionData Data.rdata', sep = ''))
  
  FigureFolder<- paste('Results/',runfolder,'/Diagnostics/cmsy monte carlo/',sep='')
  
  dir.create(FigureFolder,recursive=T)
  
  Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])
  
  Stocks<- Stocks[Stocks %in% CatchMSYPossibleParams$IdOrig ]
  
  PolicyStorage <- read.csv(paste('Results/',runfolder,'/Data/PolicyStorage.csv', sep = ''),stringsAsFactors = F)
  
  MonteMat<- run_cmsy_montecarlo(Iterations = mciterations,Stocks=Stocks,projdata =ProjectionData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                                 PolicyStorage=PolicyStorage,ErrorSize=0.25,NumCPUs = NumCPUs,
                                 elastic_demand = elastic_demand,sp_group_demand = sp_group_demand, elasticity = elasticity)
  
  #   save(MonteMat,file=paste(FigureFolder,'MonteCarlo_Results.Rdata',sep=''))
  MonteCarlo<- subset(MonteMat,Policy=='Catch Share Three' | Policy=='Fmsy Three' |  Policy=='Business As Usual' | Policy=='Business As Usual Pessimistic') %>%
    group_by(Iteration,Year,Policy) %>%
    summarize(MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
              Catch=sum(Catch,na.rm=T),BvBmsy=median(BvBmsy,na.rm=T),FvFmsy=median(FvFmsy,na.rm=T),MedianBOA=median(BOA,na.rm=T))
  
  MonteCarlo$Policy[MonteCarlo$Policy=='Fmsy Three']<- 'Fmsy'
  
  MonteCarlo$Policy[MonteCarlo$Policy=='Catch Share Three']<- 'RBFM'
  
  MonteCarlo$Policy[MonteCarlo$Policy=='Business As Usual']<- 'BAU (S1)'
  
  MonteCarlo$Policy[MonteCarlo$Policy=='Business As Usual Pessimistic']<- 'BAU (S2)'
  
  MonteCarlo<- subset(MonteCarlo,is.infinite(Catch)==F & is.na(BvBmsy)==F)
  
  #   FigureFolder<- paste(BatchFolder,'Diagnostics/Monte Carlo/',sep='')
  #   
  #   dir.create(FigureFolder,recursive=T)
  
  #   pdf(file=paste(FigureFolder,'MonteCarlo_MSY.pdf',sep=''))
  #   show(object.size(MonteCarlo, units = 'GB'))
  
  dropit <- ls()[!(ls() %in% c('MonteCarlo','BaselineYear','FigureFolder'))]
  
  rm(list = dropit)
  
  MCMSY<- (ggplot(data=subset(MonteCarlo,Policy=='RBFM' & Year== max(Year) ),
                  aes(MSY),alpha=0.8)+geom_density(fill='steelblue2'))
  
  ggsave(file=paste(FigureFolder,'MCMSY.pdf',sep=''), plot = MCMSY,height = 6,width = 8)
  
  #   print(MCMSY)
  #   dev.off()
  #   ggsave(file=paste(FigureFolder,'MonteCarlo_MSY.pdf',sep=''), plot = MCMSY)
  
  #   save(MCMSY, file = 'wtf.Rdata')
  
  # pdf(file=paste(FigureFolder,'MC_Profits.pdf',sep=''))
  # MCProfits<- (ggplot(data=MonteCarlo,aes(Profits,fill=factor(Year)))+geom_density(alpha=0.7)+facet_wrap(~Policy)
  # )
  # print(MCProfits)
  # dev.off()
  
  MCProfits<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Profits,fill=Policy))+
                 geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+
                 geom_vline(aes(xintercept=0,alpha=0.8),color='red',linetype='longdash',size=1)+facet_wrap(~Policy)+ylab("Scaled Density")
               +scale_fill_discrete(name = "Policy Alternative")
               #                coord_cartesian(xlim=c(-3e11,2e11))
               
  )
  ggsave(file=paste(FigureFolder,'MC_Profits.pdf',sep=''), plot = MCProfits,height = 6,width = 8)
  
  #   print(MCProfits)
  #   dev.off()
  
  MCCatch<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Catch,fill=Policy))+
               geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))+
               facet_wrap(~Policy)+ylab("Scaled Density")+scale_fill_discrete(name = "Policy Alternative")
             #                coord_cartesian(xlim=c(-3e11,2e11))
             
  )
  ggsave(file=paste(FigureFolder,'MC_Catch.pdf',sep=''), plot = MCCatch,height = 6,width = 8)
  
  #   print(MCCatch)
  #   dev.off()
  
  # 
  # pdf(file=paste(FigureFolder,'MC_Catch.pdf',sep=''))
  # MCCatch<- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(Catch,fill=Policy))+
  #              geom_density(alpha=0.5)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
  # )
  # print(MCCatch)
  # dev.off()
  
  
  #   pdf(file=paste(FigureFolder,'MC_BvBmsy.pdf',sep=''))
  MCBvB <- (ggplot(data=subset(MonteCarlo,Year==max(Year)),aes(BvBmsy,fill=(Policy)))+
              geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
            +geom_vline(aes(xintercept=1),color='red',linetype='longdash')+xlim(c(0,2.5))+
              facet_wrap(~Policy)+scale_fill_discrete(name = "Policy Alternative"))
  #   print(MCBvB)
  #   dev.off()
  ggsave(file=paste(FigureFolder,'MC_BvBmsy.pdf',sep=''), plot = MCBvB,height = 6,width = 8)
  
  
  #   pdf(file=paste(FigureFolder,'MC_BvBmsy_OA.pdf',sep=''))
  MCBvB_OA<-(ggplot(data=subset(MonteCarlo,Year==max(Year) & Policy=='RBFM'),aes(MedianBOA,fill=(Policy)))+
               geom_density(alpha=0.7)+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))
  #   print(MCBvB_OA)
  #   dev.off()
  ggsave(file=paste(FigureFolder,'MC_BvBmsy_OA.pdf',sep=''), plot = MCBvB_OA,height = 6,width = 8)
  
  
  #   pdf(file=paste(FigureFolder,'MC_FvFmsy.pdf',sep=''))
  MCFvF<-(ggplot(data=subset(MonteCarlo,Year==max(Year) ),aes(jitter(FvFmsy,factor=.1),fill=(Policy)))+
            geom_density(alpha=0.7,aes(y=..scaled..))+theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9))
          +xlab('F/Fmsy')+facet_wrap(~Policy)+scale_fill_discrete(name = "Policy Alternative")+xlim(c(0,2))
          +geom_vline(aes(xintercept=1),color='red',linetype='longdash')
          #         + coord_cartesian(ylim=c(0,25))+xlab('FvFmsy'))
  )
  ggsave(file=paste(FigureFolder,'MC_FvFmsy.pdf',sep=''),plot = MCFvF,height = 6,width = 8)
  #   print(MCFvF)
  #   dev.off()
  #   browser()
  #   b <- list(a = MCProfits)
  #   save(b, file = 'workplease.Rdata')
  
  save(list = c('MCProfits','MCCatch','MCBvB','MCFvF','MCMSY'),file=paste(FigureFolder,'MonteCarlo Plots.rdata',sep=''))
  
  cmsy_montecarlo_plot <- list (MCProfits = MCProfits ,MCCatch = MCCatch,MCBvB = MCBvB,MCFvF = MCFvF, MCMSY = MCMSY)
  
  return(cmsy_montecarlo_plot)
  
}

