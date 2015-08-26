arg <- UnlumpedProjectionData %>% 
  ungroup() %>% 
  subset(Year == 2050) %>% 
  group_by(Country) %>%
  summarize(cs_profit_diff = sum(Profits[Policy == 'Catch Share Three'], na.rm = T) - sum(Profits[Policy == 'Business As Usual Pessimistic'], na.rm = T),
            cs_catch_diff = sum(Catch[Policy == 'Catch Share Three'], na.rm = T) - sum(Catch[Policy == 'Business As Usual Pessimistic'], na.rm = T) )

badc <- subset(arg,cs_profit_diff <= 0)