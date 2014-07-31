######################################
#RunRegressions--------------------------------------------------
#This code runs each of the desired regressions on supplied regression formatted data
######################################

RunRegressions<- function(Data,RegList,FigureName)
{
  
   
  RegNames<- names(RegList)
  
  Models<- list()
  
  for (m in 1:length(RegNames))
  {
  
  ModelVars<- eval(parse(text=paste('RegList$',RegNames[m],sep='')))
    
  WhereVars<- colnames(Data) %in% ModelVars
  
  TempReg<- lm(LogBvBmsy ~. -1, data=Data[,WhereVars])

     pdf(file=paste(FigureFolder,FigureName,' ',RegNames[m],' Observed vs Predicted.pdf',sep=''))
    plot(TempReg$fitted.values+TempReg$residuals,TempReg$fitted.values,xlab='Observed Log B/Bmsy',
         ylab='Predicted Log B/Bmsy',main=paste('Model ',RegNames[m],sep=''))
    abline(0,1)
    dev.off()
  
  pdf(file=paste(FigureFolder,RegNames[m],' Leverage Plots.pdf',sep=''))
  leveragePlots(TempReg,ask=F)
  dev.off()
  
  show(summary(TempReg))
  
  eval(parse(text=paste('Models$',RegNames[m],'=TempReg',sep='')))
  
  }
  
  return(Models)
} #Close function