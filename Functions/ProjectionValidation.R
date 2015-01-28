###########################################
##
## Function to produce input data csv and
## output results csv of ProjectionData to
## be independently verified by Chris
##
###########################################

ProjectionValidation<-function(ProjectionData,BaselineYear)
{
  input<-ProjectionData[ProjectionData$Year==BaselineYear,c('IdOrig','Country','CatchShare','Year','BvBmsy','FvFmsy','MSY','r','Price','MarginalCost')]

  output<-ProjectionData[ProjectionData$Year==max(ProjectionData$Year,na.rm=T),c('IdOrig','Country','CatchShare','Year','Policy','BvBmsy','FvFmsy','MSY','r','MarginalCost','Profits','Biomass','Catch')]

  npv<-ddply(ProjectionData,c('IdOrig','Policy'),summarize,FinalNPV=sum(DiscProfits,na.rm=T))
  
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

# CatchShares<-read.csv('Data/GFR_CS_matches.csv',stringsAsFactors=F)
# 
# colnames(CatchShares)<-'IdOrig'
# 
# input<-ProjectionData[ProjectionData$Year==BaselineYear,c('IdOrig','Country','Year','BvBmsy','FvFmsy','MSY','r','Price','MarginalCost')]
# 
# input$CatchShare<-rep(0,nrow(input))
# input$CatchShare[input$IdOrig %in% CatchShares$IdOrig]<-1
# 
# input<-input[,c('IdOrig','Country','CatchShare','Year','BvBmsy','FvFmsy','MSY','r','Price','MarginalCost')]
# 
# output2$CatchShare<-rep(0,nrow(output2))
# output2$CatchShare[output2$IdOrig %in% CatchShares$IdOrig]<-1
# 
# output2<-output2[,c('IdOrig','Country','CatchShare','Policy','Year','Catch','Biomass','BvBmsy','NPV','FvFmsy')]
