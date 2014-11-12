###
############### Function to run upside calculations on individual fisheries in a Country
###

FisheriesToRun<-function(Data,Country)

{  
  TempProjectionData<-Data

  Country<-Country

  ids<-unique(TempProjectionData$IdOrig)

for (a in 1:length(ids))

  {

  tempProjectionData<-TempProjectionData[TempProjectionData$IdOrig==ids[a],]

    #Calculate baseline metrics values 
    
    Baseline<- ddply(subset(tempProjectionData,Year==BaselineYear & Policy=='Historic'),c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                     TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                     MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),NumberOfStocks=length(unique(IdOrig))
                     ,DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    BaselineMarker<- as.data.frame(matrix(NA,nrow=length(unique(tempProjectionData$Policy))-1,ncol=dim(Baseline)[2]))
    
    colnames(BaselineMarker)<- colnames(Baseline)
    
    BaselineMarker[,c(1,3:dim(Baseline)[2])]<- Baseline[,c(1,3:dim(Baseline)[2])]
    
    BaselineMarker$Policy<- Policies[Policies!='Historic']
    
    #Analyze time trends in metrics
    TimeTrend<- ddply(tempProjectionData,c('Year','Policy'),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                      TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),
                      NumberOfStocks=length(unique(IdOrig)),DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
    
    TimeTrend<- rbind(TimeTrend,BaselineMarker)
    
    YearOrder<- order(TimeTrend$Year)
    
    TimeTrend<- TimeTrend[YearOrder,]
    
    # Calculate Upside Metrics -----------------------------------------------------
    
    
    TimeTrend$PercChangeTotalProfits<-100*(TimeTrend$TotalProfits/Baseline$TotalProfits-1) #Percent change in total profits from current
    
    TimeTrend$PercChangeTotalCatch<-100*(TimeTrend$TotalCatch/Baseline$TotalCatch-1) #Percent change in total catch from current
    
    TimeTrend$PercChangeTotalBiomass<-100*(TimeTrend$TotalBiomass/Baseline$TotalBiomass-1) #Percent change in total biomass from current
    
    TimeTrend$PercChangeMedianProfits<-100*(TimeTrend$MedianProfits/Baseline$MedianProfits-1) #Percent change in median profits from current
    
    TimeTrend$PercChangeMedianCatch<-100*(TimeTrend$MedianCatch/Baseline$MedianCatch-1) #Percent change in median catch from current
    
    TimeTrend$PercChangeMedianBiomass<-100*(TimeTrend$MedianBvBmsy/Baseline$MedianBvBmsy-1) #Percent change in median B/Bmsy from current
    
    TimeTrend$AbsChangeTotalProfits<-TimeTrend$TotalProfits-Baseline$TotalProfits #absolute change in total profits from current
    
    TimeTrend$AbsChangeTotalCatch<-TimeTrend$TotalCatch-Baseline$TotalCatch #Percent change in total catch from current
    
    TimeTrend$AbsChangeTotalBiomass<-TimeTrend$TotalBiomass-Baseline$TotalBiomass #Percent change in total biomass from current
    
    TimeTrend$AbsChangeMedianProfits<-TimeTrend$MedianProfits-Baseline$MedianProfits #Absolute change in median profits from current
    
    TimeTrend$AbsChangeMedianCatch<-TimeTrend$MedianCatch-Baseline$MedianCatch #Percent change in median catch from current
    
    TimeTrend$AbsChangeMedianBiomass<-TimeTrend$MedianBvBmsy-Baseline$MedianBvBmsy #Percent change in median B/Bmsy from current
    
    TimeTrend$Country<-Country
  
    TimeTrend$IdOrig<-ids[a]
  
    TimeTrend$CommName<-unique(tempProjectionData$CommName[tempProjectionData$IdOrig==ids[a]])
  
    if(a==1){TimeTrendFinal<-TimeTrend}
    if(a>1){TimeTrendFinal<-rbind(TimeTrendFinal,TimeTrend)}
  
    # Calculate cumulative changes in metrics ------ 
    
    Cumulatives<- ddply(TimeTrend[TimeTrend$Year>BaselineYear,],c('Policy'),summarize,NPV=sum(DiscProfits,na.rm=T),Food=sum(TotalCatch,na.rm=T),Fish=sum(TotalBiomass,na.rm=T),
                        MedianProfits=sum(MedianProfits,na.rm=T),MedianCatch=sum(MedianCatch,na.rm=T),MedianBiomass=sum(MedianBvBmsy,na.rm=T))
    
    RawCumulatives<-Cumulatives # store cumulatives before calculating % and absolute changes
  
    CumeNumbers<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbersAbs<- as.matrix(Cumulatives[,2:dim(Cumulatives)[2]])
    
    CumeNumbers<- 100*(t(t(CumeNumbers)/CumeNumbers[which(Cumulatives$Policy=='SQ'),])-1) # Calculate % Change cumulative metrics
    
    CumeNumbersAbs<- t(t(CumeNumbersAbs)-CumeNumbersAbs[which(Cumulatives$Policy=='SQ'),]) # Calculate absolute change in cumulative metrics  
    
    Cumulatives[,2:dim(Cumulatives)[2]]<- CumeNumbers   
    
    Cumulatives[,8:13]<-CumeNumbersAbs
    
    colnames(Cumulatives)[8:13]<-paste("Abs",colnames(CumeNumbersAbs),sep="")
    
    Cumulatives[,colnames(Cumulatives)=="AbsMedianBiomass"]<-Cumulatives$AbsMedianBiomass/(ProjectionTime)
    
    Cumulatives<- Cumulatives[Cumulatives$Policy!='SQ' & Cumulatives$Policy!='Historic',]
    
    Cumulatives$Country<-Country
    
    Cumulatives$IdOrig<-ids[a]
  
    Cumulatives$CommName<-unique(tempProjectionData$CommName[tempProjectionData$IdOrig==ids[a]])

    RawCumulatives$Country<-Country
  
    RawCumulatives$IdOrig<-ids[a]
  
    RawCumulatives$CommName<-unique(tempProjectionData$CommName[tempProjectionData$IdOrig==ids[a]])
  
    if(a==1){
      CumulativesFinal<-Cumulatives
      RawCumulativesFinal<-RawCumulatives
    } 
  
    if(a>1){
      CumulativesFinal<-rbind(CumulativesFinal,Cumulatives)
      RawCumulativesFinal<-rbind(RawCumulativesFinal,RawCumulatives)
    }
    
    # Calculate  metrics in final year -----------------------------------------------------
    
    FinalYear<- TimeTrend[TimeTrend$Year==max(TimeTrend$Year),]  
    
    FinalYear$AbsChangeFromSQTotalProfits<- FinalYear$TotalProfits-FinalYear$TotalProfits[FinalYear$Policy=='SQ'] # Absolute change in total profits relative to BAU
    
    FinalYear$AbsChangeFromSQTotalCatch<- FinalYear$TotalCatch-FinalYear$TotalCatch[FinalYear$Policy=='SQ'] # Absolute change in total catch relative to BAU
    
    FinalYear$AbsChangeFromSQTotalBiomass<- FinalYear$TotalBiomass-FinalYear$TotalBiomass[FinalYear$Policy=='SQ'] # Absolute change in total biomass relative to BAU
    
    FinalYear$PercChangeFromSQTotalProfits<- 100*(FinalYear$TotalProfits/FinalYear$TotalProfits[FinalYear$Policy=='SQ']-1) # Percent change in total profits relative to BAU
    
    FinalYear$PercChangeFromSQTotalCatch<- 100*(FinalYear$TotalCatch/FinalYear$TotalCatch[FinalYear$Policy=='SQ']-1) # Percent change in total catch relative to BAU
    
    FinalYear$PercChangeFromSQTotalBiomass<- 100*(FinalYear$TotalBiomass/FinalYear$TotalBiomass[FinalYear$Policy=='SQ']-1) # Percent change in total biomass relative to BAU
    
    FinalYear$PercChangeFromSQMedianProfits<- 100*(FinalYear$MedianProfits/FinalYear$MedianProfits[FinalYear$Policy=='SQ']-1) # Percent change in median profits relative to BAU
    
    FinalYear$PercChangeFromSQMedianCatch<- 100*(FinalYear$MedianCatch/FinalYear$MedianCatch[FinalYear$Policy=='SQ']-1) # Percent change in median catch relative to BAU
    
    FinalYear$PercChangeFromSQMedianBiomass<- 100*(FinalYear$MedianBvBmsy/FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ']-1) # Percent change in median B/Bmsy relative to BAU
    
    FinalYear$AbsChangeFromSQMedianProfits<- FinalYear$MedianProfits-FinalYear$MedianProfits[FinalYear$Policy=='SQ']  # Absolute change in median profits relative to BAU
    
    FinalYear$AbsChangeFromSQMedianCatch<- FinalYear$MedianCatch-FinalYear$MedianCatch[FinalYear$Policy=='SQ'] # Absolute change in median catch relative to BAU
    
    FinalYear$AbsChangeFromSQMedianBiomass<- FinalYear$MedianBvBmsy-FinalYear$MedianBvBmsy[FinalYear$Policy=='SQ'] # Absolute change in median B/Bmsy relative to BAU
    
    FinalYear$Country<-Country
  
    FinalYear$IdOrig<-ids[a]
    
    if(a==1){FinalYearFinal<-FinalYear} 
    if(a>1){FinalYearFinal<-rbind(FinalYearFinal,FinalYear)}
  
}

return(list(TimeTrend=TimeTrendFinal,Cumulatives=CumulativesFinal,FinalYear=FinalYearFinal,RawCumulatives=RawCumulativesFinal))

}

    