
upside<-read.csv(file=paste(ResultFolder,'Country_Scenario_Results_Final.csv',sep=''),stringsAsFactors = F)

cntrys<-c('Argentina','Bangladesh','Brazil','Canada','Chile','China','France','Iceland','India','Indonesia',
          'Japan','Malaysia','Mexico','Morocco','Myanmar','New Zealand','Nigeria','Norway','Peru','Philippines',
          'Russian Federation','South Africa','Republic of Korea','Spain','Sweden','Thailand','United Kingdom','USA','Viet Nam')

mike<-upside %>%
  tbl_df() %>%
  filter(Country %in% cntrys & Year==2050 & Policy=='RBFM') %>%
  dplyr::select(Country,Policy,Scenario,Year,TotalMSY,TotalCatch,TotalBiomass,TotalProfits,BaselineCatch,
         BaselineBiomass,BaselineProfits,AbsChangeCatch,AbsChangeBiomass,AbsChangeProfits)

chris<-upside %>%
  tbl_df() %>%
  filter(Country %in% cntrys & Year==2050 & Policy=='RBFM') %>%
  dplyr::select(Country,Policy,Scenario,Year,TotalMSY,TotalCatch,TotalBiomass,TotalProfits,TotalCatchBAU,
                TotalBiomassBAU,TotalProfitsBAU,AbsChangeCatchBAU,AbsChangeBiomassBAU,AbsChangeProfitsBAU)

write.csv(mike,file='Upside_Data_for_Governance_103015.csv')

write.csv(chris,file=paste(ResultFolder,'Upside_Data_BAU_Comparison_110215.csv',sep=''))
