######################################
#Run Projections --------------------------------------------------
# This code projects fished populations forward under different policy scenarios 
######################################

RunProjection<- function(Data)
{
  
 Data<- MsyData
 
  Data$MarginalCost<- NA
  
  
# Loop over Each Stock ----------------------------------------------------

Policies<- c('Opt','CatchShare','SQ','Fmsy','CloseDown')

Stocks<- unique(Data[,IdVar])

TempStockMatrix<- as.data.frame(matrix(NA,nrow=0,ncol=dim(Data)[2]+3))

colnames(TempStockMatrix)<- c(colnames(Data),'Policy','Profits','FvFmsy')


for (s in 1:length(Stocks))
{
  show(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''))
  
  Where<- Data[,IdVar]==Stocks[s]
  
  StockData<- Data[Where,]
  
  RecentStockData<-  StockData[dim(StockData)[1],]
  
  Price<- RecentStockData$Price
  MSY<- RecentStockData$MSY
  BOA<- RecentStockData$BvBmsyOpenAccess
  r<- RecentStockData$r
  
  FStatusQuo<- ((RecentStockData$Catch)/MSY)/RecentStockData$BvBmsy
  
  FStatusQuo[is.na(FStatusQuo)]<- 0
  
  Where<- Data[,IdVar]==Stocks[s]
  
  c_num <-  Price*(2-BOA)*BOA*MSY*2^beta
  
  c_den = ((2-BOA)*r)^beta
  
  cost = c_num/c_den
  
  Data$MarginalCost[Where]<- cost 
    
 MsyProfits = Price*MSY - cost*(r/2)^beta
  
 OptPolicy<-  RunDynamicOpt2(MSY,r,Price,cost,beta,Discount,bvec,tol)$Policy

 CatchSharePolicy<-  RunDynamicOpt2(MSY,r,CatchSharePrice*Price,CatchShareCost*cost,beta,Discount,bvec,tol)$Policy
 
 SQPolicy<- FStatusQuo*matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])

 FmsyPolicy<- matrix(1,nrow=dim(OptPolicy)[1],ncol=dim(OptPolicy)[2])

 CloseDownPolicy<- bvec
 
 CloseDownPolicy[bvec<1]<- 0 

 CloseDownPolicy[bvec>=1]<- 1 
 
 for (p in 1:length(Policies))
 {
   
 eval(parse(text=paste('Policy<-',Policies[p],'Policy',sep=''))) 
   
 Projection<- Sim_Forward(Policy,bvec,RecentStockData$BvBmsy,ProjectionTime,Price,MSY,cost,r,beta,delta)
 
 PolicyMatrix<- as.data.frame(matrix(NA,nrow=ProjectionTime,ncol=dim(TempStockMatrix)[2]))
 
 PolicyMatrix[,1:dim(RecentStockData)[2]]<- RecentStockData
 
 colnames(PolicyMatrix)<- c(colnames(RecentStockData),'Policy','Profits','FvFmsy')
 
 PolicyMatrix$Catch<- Projection$Yields

 PolicyMatrix$BvBmsy<- Projection$BvBmsy

 PolicyMatrix$FvFmsy<- Projection$FvFmsy

 PolicyMatrix$Year<- RecentStockData$Year+(1:ProjectionTime)

 PolicyMatrix$Profits<- Projection$Profits

 PolicyMatrix$Policy<- Policies[p]

 TempStockMatrix<- rbind(TempStockMatrix,PolicyMatrix)
 
 } # close policies loop
 
 
} #Close stocks loop


TempStockMatrix[,grepl('Back',colnames(TempStockMatrix))]<- NA

TempStockMatrix$TempStockMatrix<- NA

TempStockMatrix$ScaledCatch<- NA

Data$Policy<- NA

Data$Profits<- NA

Data$FvFmsy<- NA

Data$Policy[is.na(Data$Policy)]<- 'Historic'

HistoricData<- Data$Policy=='Historic'

Data$FvFmsy[HistoricData]<- (Data$Catch[HistoricData]/Data$MSY[HistoricData])/Data$BvBmsy[HistoricData]

c_num<- Data$Price[HistoricData]* Data$MSY[HistoricData]*  (2-Data$BvBmsyOpenAccess[HistoricData]) * Data$BvBmsyOpenAccess[HistoricData]

c_den<- ((2-Data$BvBmsyOpenAccess[HistoricData])*Data$r[HistoricData]/2)^beta

Costs<- c_num/c_den

Data$MarginalCost[HistoricData]<- Costs

Data$Profits[HistoricData]= Data$Price[HistoricData]*Data$MSY[HistoricData]*Data$FvFmsy[HistoricData]*Data$BvBmsy[HistoricData] -Data$MarginalCost[HistoricData]*(Data$FvFmsy[HistoricData]*Data$r[HistoricData]/2)^beta


DataPlus<- rbind(Data,TempStockMatrix)


return(DataPlus)
# Stack results -----------------------------------------------------------

} #Close Function
