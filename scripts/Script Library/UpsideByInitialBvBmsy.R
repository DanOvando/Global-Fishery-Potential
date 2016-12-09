##################################################################---------------------------------------------------------------------
#
# Upside By Initial BvBmsy
#
# Tyler Clavelle
# 8/19/2015
#
# Code description: This code evaluates the contribution to upside broken out by initial B/Bmsy
#
##################################################################---------------------------------------------------------------------

Data<-UnlumpedUpsideAllStocks$FisheryUpside

UpsideByInitialBvBmsy<-function(Data)
{

  # Subset to relevant policies
  df<-Data %>%
    tbl_df() %>%
    filter(Policy %in% c('CatchShare','Catch Share Three','Fmsy','Fmsy Three')) %>%
    select(IdOrig,Country,Policy,Year,CommName,IdLevel,BvBmsy,BaselineBvBmsy,FvFmsy,MSY,AbsChangeTotalCatch,AbsChangeTotalProfits,AbsChangeTotalBiomass)
    
  # Rename policies to RBFM and indicate scenario
  df$Scenario<-NA
  
  df$Scenario[df$Policy %in% c('Catch Share Three','Fmsy Three')]<-'Cons Concern'
  
  df$Scenario[df$Policy %in% c('CatchShare','Fmsy')]<-'All Stocks'
  
  df$Policy[df$Policy %in% c('CatchShare','Catch Share Three')]<-'RBFM'
  
  df$Policy[df$Policy %in% c('Fmsy','Fmsy Three')]<-'Fmsy'
  
  # bin results by baseline BvBmsy
  df$InitialB<-NA
  
  max(df$BaselineBvBmsy)
  
  bins<-seq(from=0.2,to=2.6,by=0.2)
  
  for(b in 1:length(bins))
  {
    low<-bins[b]-0.2
    high<-bins[b]
    
    df$InitialB[df$BaselineBvBmsy>low & df$BaselineBvBmsy<=high]<-as.character(paste(low,high,sep='-'))
  }
  
  abs<-df %>%
    tbl_df() %>%
    group_by(Policy,Scenario,InitialB) %>%
    summarize(TotalProfitUpside=sum(AbsChangeTotalProfits,na.rm=T),TotalBiomassUpside=sum(AbsChangeTotalBiomass,na.rm=T),TotalCatchUpside=sum(AbsChangeTotalCatch,na.rm=T)) %>%
    gather("Metric",'Value',4:6)

  # Plot results
  pdf(file=paste(FigureFolder,'UpsideByInitialBvBmsy.pdf',sep=''),width=10,height=8)
  
  ggplot(subset(abs,Policy=='RBFM'),aes(x=InitialB,y=Value,fill=Metric)) +
    geom_bar(stat='identity') +
    facet_grid(Metric~Scenario,scales='free') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  dev.off()

      

return()

}
  