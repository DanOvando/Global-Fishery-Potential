GFRMonteCarlo<- function(CatchMSYPossibleParams,PolicyStorage,ErrorVars,ErrorSize)
{
  

  Stocks<- unique(ProjectionData$IdOrig[is.na(ProjectionData$IdOrig)==F & ProjectionData$Year==BaselineYear])
      
  Stocks<- Stocks[Stocks %in% CatchMSYPossibleParams$IdOrig ]
  
  MonteMat<- mclapply(1:length(Stocks),SnowMonteCarlo,mc.cores=NumCPUs,Stocks=Stocks,ProjectionData=ProjectionData,CatchMSYPossibleParams=CatchMSYPossibleParams,
                        PolicyStorage=PolicyStorage,ErrorVars=ErrorVars,ErrorSize=ErrorSize,Iterations=200)
  
  MonteMat<- ldply(MonteMat)
  
  MonteMat<- MonteMat[is.infinite(MonteMat$FvFmsy)==F,]
  
  MonteCarlo<- ddply(MonteMat,c('Iteration','Year','Policy'),summarize,MSY=sum(MSY,na.rm=T),Profits=sum(Profits,na.rm=T),
                     Catch=sum(Yields,na.rm=T))
    
  
  quartz()
  ggplot(data=subset(MonteCarlo,Year==2013 & Policy=='Opt'),aes(MSY))+geom_density(fill='steelblue2')

  quartz()
  ggplot(data=subset(MonteCarlo,Year==2012 | Year==2047),aes(Catch,fill=factor(Year)))+geom_density(alpha=0.2)+facet_wrap(~Policy)
  
  

  
}