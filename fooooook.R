fuck <- ProjectionData %>%
  ungroup() %>%
  group_by(Year,Policy) %>%
  summarize(num_stocks = length(unique(IdOrig)), global_catch = sum(Catch, na.rm = T )) %>%
  subset(Policy == 'Historic' & Year == 2012)

fucker <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year,Dbase,Policy) %>%
  summarize(num_stocks = length(unique(IdOrig)), global_catch = sum(Catch, na.rm = T )) %>%
  subset(Policy == 'Historic')