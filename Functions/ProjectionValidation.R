###########################################
##
## Function to produce input data csv and
## output results csv of ProjectionData to
## be independently verified by Chris
##
###########################################

# Replace FinalNPV with Profits_2050

ProjectionValidation<-function(ProjectionData,BaselineYear)
{
#   input<-ProjectionData[ProjectionData$Year==BaselineYear & ProjectionData$IdLevel=='Species',c('IdOrig','Country','CatchShare','Year','BvBmsy','FvFmsy','MSY','','Price','MarginalCost')]
#   input<-ProjectionData[ProjectionData$Year==BaselineYear & ProjectionData$IdLevel=='Species',c('IdOrig','Country','CatchShare','Year','BvBmsy','FvFmsy','MSY','g','phi','Price','MarginalCost')]
  
  input<-ProjectionData[ProjectionData$Year==BaselineYear & (ProjectionData$Catch>0 & is.na(ProjectionData$Catch)==F),c('IdOrig','Country','CatchShare','Year','BvBmsy','FvFmsy','MSY','g','phi','Price','MarginalCost','Profits','BvBmsyOpenAccess','Catch')]
  
  
  output<-ProjectionData[ProjectionData$Year==max(ProjectionData$Year,na.rm=T) & ProjectionData$IdOrig %in% input$IdOrig,
                         c('IdOrig','Country','CatchShare','Year','Policy','BvBmsy','FvFmsy','MSY','g','phi','MarginalCost','Profits','Biomass','Catch','Price')]

  npv<- ProjectionData[ProjectionData$IdOrig %in% output$IdOrig & ProjectionData$Policy!='Historic',] %>%
    group_by(IdOrig,Policy) %>%
    summarize(FinalNPV=sum(DiscProfits,na.rm=T))
    
#   npv<-ddply(ProjectionData[ProjectionData$IdOrig %in% output$IdOrig & ProjectionData$Policy!='Historic',],c('IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T))

  # npv$name<- paste(npv$IdOrig,npv$Policy,sep='-')
  # 
  # output$name<- paste(output$IdOrig,output$Policy,sep='-')
  
  output<- left_join(output,npv,by=c('IdOrig','Policy'))

  output<- output[order(output$IdOrig),]
  
  output2<- output[,c('IdOrig','Country','CatchShare','Policy','Year','Catch','Biomass','BvBmsy','Profits','FvFmsy','Price')]
  
  write.csv(input,file=paste(ResultFolder,'Parameters to Test DynOpt and Projections.csv',sep=''))
  
  write.csv(output2,file=paste(ResultFolder,'Results of Projections to Validate.csv',sep=''))
  
  return(list(InputToTest=input,OutputToVerify=output2))
}


