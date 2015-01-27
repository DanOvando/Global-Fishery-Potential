################################################
##
## Upside Calculation Function
## This function calculates the upside results of
## the GFR analysis. Can calculate for any desired
## aggregation of stocks (e.g., Countries, ISSCAAP)
##
################################################


# DenominatorPolicy<-'StatusQuoFForever'
# GroupingVars<-c('Country','Year','Policy') # or c('IdOrig','Year','Policy') for Fishery level upside
# Subset<-'All Stocks'

UpsideCalculator<-function(Data,BaselineYear,DenominatorPolicy,GroupingVars,Subset)
{
  ### Define data set to analyze-------------------------------------------------------------
  
  # Only analyze overfished or overfishing stocks if desired
  if (Subset=='Overfish') 
  {
    OverfishIds<-Data$IdOrig[Data$Year==2012 & (Data$BvBmsy<1 | Data$FvFmsy>1)]
    
    Data<-Data[Data$IdOrig %in% OverfishIds,]
  }
    
   # Analyze Projections -----------------------------------------------------
        
      # Calculate baseline metrics values 
      
      Baseline<- ddply(subset(Data,Year==BaselineYear & Policy=='Historic'),c(GroupingVars),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                       TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),
                       MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),NumberOfStocks=length(unique(IdOrig))
                       ,DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
            
      # Analyze time trends in metrics
      
      TimeTrend<- ddply(Data,c(GroupingVars),summarize,MedianBvBmsy=median(BvBmsy,na.rm=T),MedianFvFmsy=median(FvFmsy,na.rm=T),
                        TotalCatch=sum(Catch,na.rm=T),TotalProfits=sum(Profits,na.rm=T),MedianCatch=median(Catch,na.rm=T),MedianProfits=median(Profits,na.rm=T),
                        NumberOfStocks=length(unique(IdOrig)),DiscProfits=sum(DiscProfits,na.rm=T),TotalBiomass=sum(Biomass,na.rm=T),MedianBiomass=median(Biomass,na.rm=T))
        
      # Add baseline values to TimeTrend. Values are all the same regardless of policy
  
      ids<-unique(Baseline$Country)
  
      for(a in 1:length(ids))
      {
        where<-TimeTrend$Country==ids[a]
        
        whereBase<-Baseline$Country==ids[a]
        
        TimeTrend$BaselineTotalProfits[where]<-Baseline$TotalProfits[whereBase]
        
        TimeTrend$BaselineTotalCatch[where]<-Baseline$TotalCatch[whereBase]
        
        TimeTrend$BaselineTotalBiomass[where]<-Baseline$TotalBiomass[whereBase]
      }
  
      # Calculate Upside Metrics 
      
      # TimeTrend
      TimeTrend$PercChangeTotalProfits<-100*(TimeTrend$TotalProfits/TimeTrend$BaselineTotalProfits-1) #Percent change in total profits from current
      
      TimeTrend$PercChangeTotalCatch<-100*(TimeTrend$TotalCatch/TimeTrend$BaselineTotalCatch-1) #Percent change in total catch from current
      
      TimeTrend$PercChangeTotalBiomass<-100*(TimeTrend$TotalBiomass/TimeTrend$BaselineTotalBiomass-1) #Percent change in total biomass from current
         
      TimeTrend$AbsChangeTotalProfits<-TimeTrend$TotalProfits-TimeTrend$BaselineTotalProfits #absolute change in total profits from current
      
      TimeTrend$AbsChangeTotalCatch<-TimeTrend$TotalCatch-TimeTrend$BaselineTotalCatch #Percent change in total catch from current
      
      TimeTrend$AbsChangeTotalBiomass<-TimeTrend$TotalBiomass-TimeTrend$BaselineTotalBiomass #Percent change in total biomass from current
             
      # Calculate  metrics in final year 
      
      # Isolate final year values
      
      FinalYear<- TimeTrend[TimeTrend$Year==max(TimeTrend$Year),]       
      
      FinalYear$AbsChangeFromSQTotalProfits<- FinalYear$TotalProfits-FinalYear$TotalProfits[FinalYear$Policy==DenominatorPolicy] # Absolute change in total profits relative to BAU
      
      FinalYear$AbsChangeFromSQTotalCatch<- FinalYear$TotalCatch-FinalYear$TotalCatch[FinalYear$Policy==DenominatorPolicy] # Absolute change in total catch relative to BAU
      
      FinalYear$AbsChangeFromSQTotalBiomass<- FinalYear$TotalBiomass-FinalYear$TotalBiomass[FinalYear$Policy==DenominatorPolicy] # Absolute change in total biomass relative to BAU
      
      FinalYear$PercChangeFromSQTotalProfits<- 100*(FinalYear$TotalProfits/FinalYear$TotalProfits[FinalYear$Policy==DenominatorPolicy]-1) # Percent change in total profits relative to BAU
      
      FinalYear$PercChangeFromSQTotalCatch<- 100*(FinalYear$TotalCatch/FinalYear$TotalCatch[FinalYear$Policy==DenominatorPolicy]-1) # Percent change in total catch relative to BAU
      
      FinalYear$PercChangeFromSQTotalBiomass<- 100*(FinalYear$TotalBiomass/FinalYear$TotalBiomass[FinalYear$Policy==DenominatorPolicy]-1) # Percent change in total biomass relative to BAU

      FinalYear$Subset<-Subset

      FinalYear$Denominator<-DenominatorPolicy
      
      # Calculate cumulative changes in metrics 
      
      Cumulatives<- ddply(TimeTrend[TimeTrend$Year>BaselineYear,],c('Country','Policy'),summarize,NPV=sum(DiscProfits,na.rm=T),Food=sum(TotalCatch,na.rm=T))
            
      for(d in 1:nrow(Cumulatives))
      {
        Cumulatives$PercChangeFromSQNPV[d]<-100*((Cumulatives$NPV[d]/Cumulatives$NPV[Cumulatives$Country==Cumulatives$Country[d] & Cumulatives$Policy==DenominatorPolicy])-1)
        
        Cumulatives$PercChangeFromSQFood[d]<-100*((Cumulatives$Food[d]/Cumulatives$Food[Cumulatives$Country==Cumulatives$Country[d] & Cumulatives$Policy==DenominatorPolicy])-1)
        
        Cumulatives$AbsChangeFromSQNPV[d]<-Cumulatives$NPV[d]-Cumulatives$NPV[Cumulatives$Country==Cumulatives$Country[d] & Cumulatives$Policy==DenominatorPolicy]
        
        Cumulatives$AbsChangeFromSQFood[d]<-Cumulatives$Food[d]-Cumulatives$Food[Cumulatives$Country==Cumulatives$Country[d] & Cumulatives$Policy==DenominatorPolicy]
      }

      Cumulatives$Subset<-Subset

      Cumulatives$Denominator<-DenominatorPolicy
      
      # Merge Final Year and Cumulatives and remove results of 

      UpsideResults<-merge(FinalYear,Cumulatives)
  
    return(UpsideResults)
      
    }
