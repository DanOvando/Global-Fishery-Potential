
top10<-UnlumpedProjectionData %>%
  tbl_df() %>%
  filter(Policy=='Catch Share Three' & Year==2050) %>%
  select(IdOrig,Country,CommName,Year,Catch,BvBmsy,FvFmsy,Price,MSY,Biomass,Profits) %>%
  group_by(Country) %>%
  arrange(desc(MSY)) %>%
  mutate(Rank=dense_rank(desc(MSY))) %>%
  filter(Rank<11)

top10ids<-unique(top10$IdOrig)

top10base<-UnlumpedProjectionData %>%
  tbl_df() %>%
  filter(Year==2012 & IdOrig %in% top10ids) %>%
  select(IdOrig,Country,CommName,Year,Catch,BvBmsy,FvFmsy,Price,MSY,Biomass,Profits) %>%
  rename(BaselineCatch=Catch,BaselineBiomass=Biomass,BaselineProfits=Profits)

chris<-left_join(top10,top10base[,c('IdOrig','BaselineCatch','BaselineBiomass','BaselineProfits')])

top10bau<-UnlumpedProjectionData %>%
  tbl_df() %>%
  filter(Policy=='Business As Usual' & Year==2050 & IdOrig %in% top10ids) %>%
  select(IdOrig,Country,CommName,Year,Catch,BvBmsy,FvFmsy,Price,MSY,Biomass,Profits) %>%
  rename(CatchBAU=Catch,BiomassBAU=Biomass,ProfitsBAU=Profits)

final<-left_join(chris,top10bau[,c('IdOrig','CatchBAU','BiomassBAU','ProfitsBAU')])

# Define perc upside function
PercChange<- function(A,B)
{
  PC<- ((A-B)/(B))*100*sign(B)
  
  PC[B<=0 & (A-B)>0]<- 999
  
  PC[B<=0 & (A-B)<=0]<- -999
  
  return(PC)
}

# Calculate abs upsides relative to baselin
final$AbsChangeCatch<-final$Catch-final$BaselineCatch
final$AbsChangeBiomass<-final$Biomass-final$BaselineBiomass
final$AbsChangeProfits<-final$Profits-final$BaselineProfits

# Calculate perc upsides relative to baseline
final$PercChangeCatch<-PercChange(final$Catch,final$BaselineCatch)
final$PercChangeBiomass<-PercChange(final$Biomass,final$BaselineBiomass)
final$PercChangeProfits<-PercChange(final$Profits,final$BaselineProfits)

# Calculate abs upsides relative to bau
final$AbsChangeCatchBAU<-final$Catch-final$CatchBAU
final$AbsChangeBiomassBAU<-final$Biomass-final$BiomassBAU
final$AbsChangeProfits<-final$Profits-final$ProfitsBAU

# Calculate perc upsides relative to bau
final$PercChangeCatchBAU<-PercChange(final$Catch,final$CatchBAU)
final$PercChangeBiomassBAU<-PercChange(final$Biomass,final$BiomassBAU)
final$PercChangeProfitsBAU<-PercChange(final$Profits,final$ProfitsBAU)

# select columns
final<-final %>%
  mutate(FxB=FvFmsy*BvBmsy)

write.csv(final,file='Upside by Country Top 10 Stocks.csv')
