
################################################
##
## This function does a final check for RAM and FAO
## overlap before calculating upside results
##
################################################

CheckDuplicates<-function(Data)
{
  
  Checkfile<-subset(Data, Year==2012,select=c(IdOrig,Country, SciName,CommName,RegionFAO,Dbase,Catch))
  for ( i in 1:dim(Checkfile)[1] )
  {
    y<-subset(Checkfile, Checkfile$Country==Checkfile$Country[i] & Checkfile$SciName==Checkfile$SciName[i] & Checkfile$RegionFAO==Checkfile$RegionFAO[i],select=c(IdOrig,Country, SciName,CommName,RegionFAO,Dbase,Catch))
    size<-dim(y)[1]
    if (i==1){x<-y}
    if (size>1){x<-rbind(x,y)}
#     show(Checkfile$Country[i])
  }
  z<-x[-1,] # deleting first row
  zs<-subset(z, Dbase=="FAO",select=c(IdOrig,Country, SciName,CommName,RegionFAO,Dbase,Catch))
  zu<-unique(zs)
  
  ids<-zu$IdOrig
  
  Data<-Data[!(Data$IdOrig %in% ids),]
  
  Data<-Data[!(is.na(Data$IdOrig)),]
  
  return(Data)
}

# TEST<-CheckDuplicates(UnlumpedProjectionData[UnlumpedProjectionData$Country %in% c('Argentina','Japan') & UnlumpedProjectionData$CanProject==T,])
# 
# TEST<-unique(TEST[TEST$Year==2012,c('IdOrig','Country','CommName','SciName','RegionFAO')])
# 
# TEST<-TEST[with(TEST,order(SciName)),]
# 
# Data<-UnlumpedProjectionData[UnlumpedProjectionData$Country=='Japan',]
