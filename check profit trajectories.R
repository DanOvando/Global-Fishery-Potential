rm(list = ls())
load('Results/5.1 global demand spcat phi/Data/ProjectionData Data.rdata')

damn <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  mutate(global_catch = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country,Year, Policy) %>%
  summarize(total_profits = sum(Profits, na.rm = T),
            worldwide_catch = mean(global_catch, na.rm = T)) %>%
  #   spread(Policy,total_profits) %>%
  subset(Year >= 2012 & (Policy %in% c('CatchShare', 'Catch Share Three',"Business As Usual Pessimistic",
                                       "Business As Usual")))

profit_trend <- (ggplot(subset(damn, Country == 'USA'),aes(Year,total_profits, size = worldwide_catch, fill = Policy))
                 + geom_line(color = 'black', alpha = 0.2) + geom_point(shape = 21) )

ggsave('USA profits with demand curve.pdf', plot = profit_trend)

damn <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  mutate(global_catch = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  summarize(total_profits = sum(Profits, na.rm = T),
            worldwide_catch = mean(global_catch, na.rm = T)) %>%
  #   spread(Policy,total_profits) %>%
  subset(Year >= 2012 & (Policy %in% c('CatchShare', 'Catch Share Three',"Business As Usual Pessimistic",
                                       "Business As Usual")))

profit_trend <- (ggplot(damn,aes(Year,total_profits, size = worldwide_catch, fill = Policy))
                 + geom_line(color = 'black', alpha = 0.2) + geom_point(shape = 21) )

ggsave('Global profits with demand curve.pdf', plot = profit_trend)


# No Demand ---------------------------------------------------------------

rm(list = ls())
load('Results/5.1 constant demand spcat phi/Data/ProjectionData Data.rdata')

damn <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  mutate(global_catch = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country,Year, Policy) %>%
  summarize(total_profits = sum(Profits, na.rm = T),
            worldwide_catch = mean(global_catch, na.rm = T)) %>%
  #   spread(Policy,total_profits) %>%
  subset(Year >= 2012 & (Policy %in% c('CatchShare', 'Catch Share Three',"Business As Usual Pessimistic",
                                       "Business As Usual")))

profit_trend <- (ggplot(subset(damn, Country == 'USA'),aes(Year,total_profits, size = worldwide_catch, fill = Policy))
                 + geom_line(color = 'black', alpha = 0.2) + geom_point(shape = 21) )

ggsave('USA profits without demand curve.pdf', plot = profit_trend)

damn <- UnlumpedProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  mutate(global_catch = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  summarize(total_profits = sum(Profits, na.rm = T),
            worldwide_catch = mean(global_catch, na.rm = T)) %>%
  #   spread(Policy,total_profits) %>%
  subset(Year >= 2012 & (Policy %in% c('CatchShare', 'Catch Share Three',"Business As Usual Pessimistic",
                                       "Business As Usual")))

profit_trend <- (ggplot(damn,aes(Year,total_profits, size = worldwide_catch, fill = Policy))
                 + geom_line(color = 'black', alpha = 0.2) + geom_point(shape = 21) )

ggsave('Global profits without demand curve.pdf', plot = profit_trend)
