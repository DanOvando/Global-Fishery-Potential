
# Policies<-c('Catch Share Three','Fmsy Three')
# discRt<-0.05
# TimeHor<-38

GlobalResultsFigOne<-function(UpsideAllStocks,UpsideOverfishOnly,Policies,discRt,TimeHor)
{
  AllGlobal<-UpsideAllStocks$GlobalUpside[UpsideAllStocks$GlobalUpside$Policy %in% Policies,]
  
  OverFGlobal<-UpsideOverfishOnly$GlobalUpside[UpsideOverfishOnly$GlobalUpside$Policy %in% Policies,]
  
  #==trevor denominator
  trevPercChangeBioCur  <-100*OverFGlobal$AbsChangeTotalBiomass/AllGlobal$TotalBaselineBiomass
  trevPercChangeCatchCur	<-100*OverFGlobal$AbsChangeTotalCatch/AllGlobal$TotalBaselineCatch
  trevPercChangeBioSQ	<-100*OverFGlobal$AbsChangeFromSQTotalBiomass/AllGlobal$TotalBiomassSQ
  trevPercChangeCatchSQ	<-100*OverFGlobal$AbsChangeFromSQTotalCatch/AllGlobal$TotalCatchSQ
  trevPercChangeNPV		<-(100*(OverFGlobal$TotalNPV-OverFGlobal$TotalNPVSQ)/AllGlobal$TotalNPVSQ)*sign(AllGlobal$TotalNPVSQ)
  
  trevPercChangeNPV[AllGlobal$TotalNPVSQ<=0 & (OverFGlobal$TotalNPV-OverFGlobal$TotalNPVSQ)>0]<- 999
  
  trevPercChangeNPV[AllGlobal$TotalNPVSQ<=0 & (OverFGlobal$TotalNPV-OverFGlobal$TotalNPVSQ)<=0]<- -999
  
  # 1A
  FigOneA<-OverFGlobal[,c('Policy','PercChangeTotalBiomass','PercChangeTotalCatch')]
  
  overfishAnn  	<-OverFGlobal$TotalNPV*discRt/(1-(1+discRt)^-TimeHor)
  
  colQuant    	<-100*(overfishAnn-OverFGlobal$TotalBaselineProfits)/OverFGlobal$TotalBaselineProfits*sign(OverFGlobal$TotalBaselineProfits)
  
  colQuant[OverFGlobal$TotalBaselineProfits<=0 & (overfishAnn-OverFGlobal$TotalBaselineProfits)>0]<- 999
  
  colQuant[OverFGlobal$TotalBaselineProfits<=0 & (overfishAnn-OverFGlobal$TotalBaselineProfits)<=0]<- -999
  
  FigOneA$PercChangeTotalProfits<-colQuant
  
  colnames(FigOneA)<-c('Policy','Biomass','Catch','Profit')
  
  FigOneA$Panel<-'Panel A'
  
  # 1B
  FigOneB<-OverFGlobal[,c('Policy','PercChangeFromSQTotalBiomass','PercChangeFromSQTotalCatch','PercChangeFromSQNPV')]
  
  colnames(FigOneB)<-c('Policy','Biomass','Catch','Profit')
  
  FigOneB$Panel<-'Panel B'
  
  # 1C
 FigOneC<-data.frame(matrix(c(Policies,trevPercChangeBioCur,trevPercChangeCatchCur),nrow=2,ncol=3,byrow=F))
 
 colQuant3  		<-100*(overfishAnn-OverFGlobal$TotalBaselineProfits)/AllGlobal$TotalBaselineProfits*sign(AllGlobal$TotalBaselineProfits)
 
 colQuant3[AllGlobal$TotalBaselineProfits<=0 & (overfishAnn-OverFGlobal$TotalBaselineProfits)>0]<- 999
 
 colQuant3[AllGlobal$TotalBaselineProfits<=0 & (overfishAnn-OverFGlobal$TotalBaselineProfits)<=0]<- -999
 
 FigOneC$PercChangeNPV<-colQuant3

 colnames(FigOneC)<-c('Policy','Biomass','Catch','Profit')
 
 FigOneC$Panel<-'Panel C'
 # 1D
 FigOneD<-data.frame(matrix(c(Policies,trevPercChangeBioSQ,trevPercChangeCatchSQ,trevPercChangeNPV),nrow=2,ncol=4,byrow=F))

 colnames(FigOneD)<-c('Policy','Biomass','Catch','Profit')
 
 FigOneD$Panel<-'Panel D'
 
 # Bind Table
 
 FinalTable<-rbind(FigOneA,FigOneB,FigOneC,FigOneD)
 
 write.csv(FinalTable,file=paste(ResultFolder,'Figure One Global Data Tyler Calcs.csv',sep=''))
 
 return(GlobalFigOneResults=FinalTable)
 
}