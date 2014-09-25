##
## Function to assign resilience value for each fishery based on life history parameters 
## 
## Resilience assigned according to Fishbase guidlines found in the "Determining Life History Params" Manual 

FindResilience<-function(Data)
{
  ResData<-unique(Data[c("IdOrig","VonBertK","AgeMat")])
   
  ResData$k<-NA
  ResData$tm<-NA
  ResData$Res<-NA
 
  ResData<-ResData[is.na(ResData$VonBertK)==F | is.na(ResData$AgeMat)==F,]
  
  ResData$k[ResData$VonBertK>0.3]<-"High"
  ResData$k[ResData$VonBertK>=0.16 & ResData$VonBertK<=0.3]<-"Medium"
  ResData$k[ResData$VonBertK>=0.05 & ResData$VonBertK<0.16]<-"Low"
  ResData$k[ResData$VonBertK<0.05]<-"Very low"
  
  ResData$tm[ResData$AgeMat<1]<-"High"
  ResData$tm[ResData$AgeMat>=2 & ResData$AgeMat<=4]<-"Medium"
  ResData$tm[ResData$AgeMat>=5 & ResData$AgeMat<=10]<-"Low"
  ResData$tm[ResData$AgeMat>10]<-"Low"
      
  ResData$Res[grepl("Very low", ResData$k) | grepl("Very low", ResData$tm)]<-"Very low"
  ResData$Res[(grepl("Low", ResData$k) | grepl("Low", ResData$tm)) & is.na(ResData$Res)==T]<-"Low"  
  ResData$Res[(grepl("Medium", ResData$k) | grepl("Medium", ResData$tm)) & is.na(ResData$Res)==T]<-"Medium"
  ResData$Res[(grepl("High", ResData$k) | grepl("High", ResData$tm)) & is.na(ResData$Res)==T]<-"High"
  
  for(a in 1:nrow(ResData))
  {
    WhereRes<-Data$IdOrig==ResData$IdOrig[a]
    
    Data$Res[WhereRes]<-ResData$Res[a]
    
    show(paste((a/nrow(ResData)*100),"% Done with Resilience",sep=""))
  }
  
  return(FullData=Data)
  
}
  