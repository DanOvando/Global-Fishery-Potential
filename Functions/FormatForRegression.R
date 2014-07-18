FormatForRegression<- function(Data,DependentVariable,CatchLags,LifeHistoryVars,IsLog,IdVar)
{
  
  # Format Data For Regression  -------- This code reformats data frames into
  # the structure for a panel regression #
  
  # Create Regression Data Frame --------------------------------------------
  
#    Data<- FaoData
  
  LifeHistoryVars<- sort(LifeHistoryVars)
  
  DependentName<- DependentVariable
  
  DependentName<- if (IsLog==T){paste('Log',DependentVariable,sep='')}
  
  RegNames<- c(IdVar,DependentName,'ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'TimeToMaxCatch','InitialScaledCatchSlope'
               ,'MeanScaledCatch','CatchToRollingMax','SpeciesCatName',LifeHistoryVars)
  
  RegFrame<- as.data.frame(matrix(NA,nrow=dim(Data)[1],ncol=length(RegNames)))
  
  colnames(RegFrame)<- RegNames
  
  RegFrame[,IdVar]<- Data[,IdVar]
  
  DependentTemp<-  Data[,DependentVariable]
  
  if (IsLog==T){DependentTemp<- log(DependentTemp)}
  
  DependentTemp[is.infinite(DependentTemp)]<- NA
  
  RegFrame[,DependentName]<-DependentTemp
  
  
  
  
  # Populate Life History ---------------------------------------------------
  
  
  WhereLifeHistory<- colnames(Data) %in% LifeHistoryVars
  
  WhereToGo<- colnames(RegFrame) %in% LifeHistoryVars
  
  DataLifeNameOrder<- order(colnames(Data[0,WhereLifeHistory]))
  
  ## Populate life history variables
  
  LifeData<- Data[,WhereLifeHistory]
  
  LifeData<- LifeData[,DataLifeNameOrder]
  
  RegFrame[,WhereToGo]<- LifeData
  
  RegFrame$SpeciesCatName<- Data$SpeciesCatName
  
  
  
  # Loop Over Fisheries -----------------------------------------------------
  
  Fisheries<- unique(RegFrame[,IdVar])
  
  SlopeWindow<- 1:6
  
  for (f in 1:length(Fisheries)) 
  {
    
#     if (is.integer(f/50)){   }
     show(paste(round(100*(f/length(Fisheries))),"% Done with Regression Formating",sep=''))  
# show(Fisheries[f])
    
    Where<- Data[,IdVar]==Fisheries[f]
    
    TempFrame<- Data[Where,]
    
    MaxCatch<- max(TempFrame$Catch,na.rm=T)
    
    TempCatch<- TempFrame$Catch
    
    ScaledCatch<-  TempCatch/MaxCatch
    
    RegFrame[Where,'ScaledCatch']<- ScaledCatch #Create scaled catch
    
    RegFrame[Where,'MeanScaledCatch']<- mean(ScaledCatch ,na.rm=T)#Create scaled catch
    
    RegFrame[Where,'TimeToMaxCatch']<- which(TempCatch==MaxCatch)[1] #Create time till max catch
    
    
    InitialSlope<- NA
    
    FirstCatch<- which(is.na(ScaledCatch)==F)[1]
    
    if (sum(is.na(ScaledCatch[1:6]))<5)
    {
      InitialSlope<- lm(formula=ScaledCatch[FirstCatch:(FirstCatch+5)] ~  SlopeWindow,na.action='na.omit')$coefficients[2]
    }
    RegFrame[Where,'InitialScaledCatchSlope']<- InitialSlope #Create initial slope of scaled catch
    
    BlankCatch<- matrix(NA,nrow=length(ScaledCatch),ncol=1)
    
    MaxFrame<- BlankCatch
    
    for (c in FirstCatch:length(BlankCatch))
    {
      MaxFrame[c]<- max(TempCatch[FirstCatch:c],na.rm=T) 
    }
    
    MaxFrame[is.infinite(MaxFrame)]<-  NA
    
    RegFrame[Where,'CatchToRollingMax']<- TempCatch/MaxFrame #Create rolling scaled catch
    
    ## Populate lagged catches ##
    
    for (l in 1:CatchLags)
    {
      
      TempLag<- BlankCatch
      
      LagIndex<- pmax(0,(1:length(BlankCatch))-l)
      
      TempLag[(1+l):length(BlankCatch)]<- ScaledCatch[LagIndex]
      
      WhereCol<- colnames(RegFrame)==paste('ScaledCatch',l,'Back',sep='')
      
      RegFrame[Where,WhereCol]<- TempLag # Create lagged scaled catches
      
    }
    
  }#Close fisheries loop
    
  return(RegFrame)
  
}


