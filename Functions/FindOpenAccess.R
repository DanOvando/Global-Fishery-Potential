##################################
## Find Open Access Equilibrium for each species category
##################################

# Data<-MsyData
# BOAtol<-0.1

FindOpenAccess<-function(MsyData,BaselineYear,BOAtol)
{
  
  Data<-MsyData
  
  SpeciesCats<- unique(Data$SpeciesCatName)
  
  OpenAccess<-data.frame(matrix(NA,nrow=length(SpeciesCats),ncol=2))
  colnames(OpenAccess)<-c('SpeciesCatName','BvBmsyOpenAccess')
  
  for (s in 1:length(SpeciesCats))
  {
    CategoryStocks<-Data[Data$SpeciesCatName==SpeciesCats[s] & Data$Year==BaselineYear &  Data$Dbase!='RAM',]
    
    KobeSpace<-ddply(CategoryStocks,c('IdOrig'),summarize,KobeSpot=abs((2-FvFmsy-BvBmsy))) 
    # Any stock on the equilibrium line has a value of 2 for the sum of BvBmsy and FvFmsy
    # This ddply calculates the distance from the equilibrium line for each stock
    # subset this dataset to only include stocks within a desired tolerance, make option on Master
    
    IdsBOA<-KobeSpace$IdOrig[KobeSpace$KobeSpot<=BOAtol] # identify stocks that are within tolerance of equilibrium
    
    OpenAccess$SpeciesCatName[s]<-SpeciesCats[s] # store species category
    OpenAccess$BvBmsyOpenAccess[s]<-summary(CategoryStocks$BvBmsy[CategoryStocks$IdOrig %in% IdsBOA])[2] # store BvBmsy of 25 percentile 
  }
  
  OpenAccess<-OpenAccess[is.na(OpenAccess$BvBmsyOpenAccess)==F,]
  
  OpenAccess$BvBmsyOpenAccess[(OpenAccess$BvBmsyOpenAccess)>=1.9]<- 1.9
  
  
  return(BvBmsyOpenAccess=OpenAccess)
}
