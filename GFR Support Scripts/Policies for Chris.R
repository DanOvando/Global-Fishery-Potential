for_chris <- ProjectionData %>%
  filter(Year == 2012)  %>% 
  select(IdOrig, g, k, phi, MSY, MarginalCost, Price)  %>%
  rename(Cost = MarginalCost)

policies <- read.csv('Results/PNAS Submission - 6.01 global demand common phi/Data/PolicyStorage.csv', stringsAsFactors = F)


for_chris <- policies %>%
  left_join(for_chris, by = 'IdOrig')


write.csv(for_chris,file = 'Policies and Params for Chris.csv')