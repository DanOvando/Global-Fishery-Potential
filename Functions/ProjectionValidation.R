###########################################
##
## Function to produce input data csv and
## output results csv of ProjectionData to
## be independently verified by Chris
##
###########################################

ProjectionValidation<-function(ProjectionData,BaselineYear)
{
  input<-ProjectionData[ProjectionData$Year==BaselineYear & ProjectionData$IdLevel=='Species',c('IdOrig','Country','CatchShare','Year','BvBmsy','FvFmsy','MSY','r','Price','MarginalCost')]

  output<-ProjectionData[ProjectionData$Year==max(ProjectionData$Year,na.rm=T) & ProjectionData$IdOrig %in% input$IdOrig,
                         c('IdOrig','Country','CatchShare','Year','Policy','BvBmsy','FvFmsy','MSY','r','MarginalCost','Profits','Biomass','Catch')]

  npv<-ddply(ProjectionData[ProjectionData$IdOrig %in% output$IdOrig & ProjectionData$Policy!='Historic',],c('IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T))
  
  for(a in 1:nrow(npv))
  {
    where<-output$IdOrig==npv$IdOrig[a] & output$Policy==npv$Policy[a]
    
    output$NPV[where]<-npv$FinalNPV[a]
  }

  output2<-output[,c('IdOrig','Country','CatchShare','Policy','Year','Catch','Biomass','BvBmsy','NPV','FvFmsy')]
  
  write.csv(input,file=paste(ResultFolder,'Parameters to Test DynOpt and Projections.csv',sep=''))
  
  write.csv(output2,file=paste(ResultFolder,'Results of Projections to Validate.csv',sep=''))
  
  return(list(InputToTest=input,OutputToVerify=output2))
}


