######################################
#Run Projections --------------------------------------------------
# This code projects fished populations forward under different policy scenarios 
######################################

RunProjection<- function(Data,beta...)
{
  
  Data<- MsyData
 
# Loop over Each Stock ----------------------------------------------------

Stocks<- unique(Data[,IdVar])

for (s in 1:length(Stocks))
{
 
  Price<- RecentStockData$Price
  MSY<- RecentStockData$MSY
  BOA<- RecentStockData$BvBmsyOpenAccess
  r<- RecentStockData$r
  
  
  Where<- Data[,IdVar]==Stocks[s]
  
  show(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''))
  
  StockData<- Data[Where,]
  
  RecentStockData<-  StockData[dim(StockData)[1],]
  
  c_num <-  Price*MSY*(2-BOA)*BOA
  
  c_den = ((2-BOA)*r/2)^beta
  
  cost = c_num/c_den
  
  Data$MarginalCost[Where]<- cost 
    
 MsyProfits = Price*MSY - cost*(r/2)^beta
  
 NormalControl<-  RunDynamicOpt2(MSY,r,Price,cost,Beta,Discount,bvec,tol)$Policy

 CatchShareControl<-  RunDynamicOpt2(MSY,r,CatchSharePrice*Price,CatchShareCost*cost,Beta,Discount,bvec,tol)$Policy
 
 Projection<- Sim_Forward(NormalControl,bvec,BOA,ProjectionTime,Price,MSY,cost,r,Beta,delta)

 Projection<- Sim_Forward(CatchShareControl,bvec,BOA,ProjectionTime,Price,MSY,cost,r,Beta,delta)
 
 
} #Close stocks loop

# Determine Policy Functions ----------------------------------------------


# Project forward under each policy ---------------------------------------


# Stack results -----------------------------------------------------------


