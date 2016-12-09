load('Results/6.0 global demand common phi/Data/ProjectionData Data.Rdata')

library(ggplot2)


top10<-UnlumpedProjectionData %>%
  tbl_df() %>%
  filter(Policy=='Historic' & Year==2012) %>%
  select(IdOrig,Country,CommName,Year,Catch,BvBmsy,FvFmsy,Price,MSY,Biomass,Profits) %>%
  group_by(Country) %>%
  arrange(desc(Catch)) %>%
  mutate(Rank=dense_rank(desc(Catch))) %>%
  filter(Rank<11)

top10ids<-unique(top10$IdOrig)

top10CatchShare<-UnlumpedProjectionData %>%
  tbl_df() %>%
  filter(Year==2050 & Policy == 'CatchShare' & IdOrig %in% top10ids) %>%
  select(IdOrig,Country,CommName,Year,Catch,BvBmsy,FvFmsy,Price,Biomass,Profits) %>%
  rename(FutureYear = Year,CatchShare_BvBmsy = BvBmsy, CatchShare_FvFmsy = FvFmsy,CatchShare_Price = Price,CatchShare_Catch=Catch,CatchShare_Biomass=Biomass,Catch_ShareProfits=Profits)

chris<-left_join(top10,top10CatchShare[,c('IdOrig','FutureYear','CatchShare_Catch','CatchShare_Biomass','CatchShareProfits')], by = 'IdOrig')

top10bau<-UnlumpedProjectionData %>%
  tbl_df() %>%
  filter(Policy=='Business As Usual' & Year==2050 & IdOrig %in% top10ids) %>%
  select(IdOrig,Country,CommName,Year,Catch,BvBmsy,FvFmsy,Price,MSY,Biomass,Profits) %>%
  rename(BAUPess_BvBmsy = BvBmsy, BAUPess_FvFmsy = FvFmsy, BAUPess_Price = Price,BAUPess_Catch=Catch,BAUPess_Biomass=Biomass,BAUPess_Profits=Profits)

final<-left_join(chris,top10bau[,c('IdOrig','BAUPess_Catch','BAUPess_Biomass','BAUPess_Profits')], by = 'IdOrig')

# Define perc upside function
PercChange<- function(A,B)
{
  PC<- ((A-B)/(B))*100*sign(B)
  
  PC[B<=0 & (A-B)>0]<- 999
  
  PC[B<=0 & (A-B)<=0]<- -999
  
  return(PC)
}

# Calculate abs upsides relative to baselin
final$Abs_Change_Catch<-final$CatchShare_Catch-final$BAUPess_Catch
final$Abs_Change_Biomass<-final$CatchShare_Biomass-final$BAUPess_Biomass
final$Abs_Change_Profits<-final$CatchShare_Profits-final$BAUPess_Profits

# Calculate perc upsides relative to baseline
final$Perc_Change_Catch<-PercChange(final$CatchShare_Catch,final$BAUPess_Catch)
final$Perc_Change_Biomass<-PercChange(final$CatchShare_Biomass,final$BAUPess_Biomass)
final$Perc_Change_Profits<-PercChange(final$CatchShare_Profits,final$BAUPess_Profits)

# Calculate abs upsides relative to Today
final$Abs_Change_Catch_From_Today <- final$CatchShare_Catch-final$Catch
final$Abs_Change_Biomass_From_Today <- final$CatchShare_Biomass-final$Biomass
final$Abs_Change_Profits_From_Today <- final$CatchShare_Profits-final$Profits

# Calculate perc upsides relative to today
final$Perc_Change_Catch_From_Today <- PercChange(final$CatchShare_Catch,final$Catch)
final$Perc_Change_Biomass_From_Today <- PercChange(final$CatchShare_Biomass,final$Biomass)
final$Perc_Change_Profits_From_Today <- PercChange(final$CatchShare_Profits,final$Profits)

# select columns
final<-final %>%
  mutate(FxB_2012=FvFmsy*BvBmsy)

write.csv(final,file='Upside by Country Top 10 Stocks.csv')
