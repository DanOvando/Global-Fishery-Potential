damn <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  mutate(global_catch = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country,Year, Policy) %>%
  summarize(total_profits = sum(Profits, na.rm = T),
            worldwide_catch = mean(global_catch, na.rm = T),
            delta_profits = sum(Profits, na.rm = T)[Policy == 'Catch Share Three'] - sum(Profits, na.rm = T)[Policy =='Business As Usual'] ) %>%
#   spread(Policy,total_profits) %>%
  subset(Year >= 2012 & (Policy %in% c('CatchShare', 'Catch Share Three',"Business As Usual Pessimistic",
                                       "Business As Usual")))

damn_country <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  mutate(global_catch = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country,Year) %>%
  summarize(cs3_profits = sum(Profits, na.rm = T)[Policy == 'Catch Share Three'],
            bau_profits = sum(Profits, na.rm = T)[Policy =='Business As Usual']) %>%
#   arrange(desc(delta_profits)) %>%
  subset(Year == 2050)



profit_trend <- (ggplot(subset(damn, Country == 'USA'),aes(Year,total_profits, size = worldwide_catch, fill = Policy))
+ geom_line(color = 'black', alpha = 0.2) + geom_point(shape = 21) )

ggsave('USA profits with demand curve.pdf', plot = profit_trend)