fuck <- ProjectionData %>%
  group_by(Year,Policy,SpeciesCatName) %>%
  mutate(total_catch = sum(Catch, na.rm = T),total_profits = sum(Profits, na.rm = T),mean_price = mean(Price,na.rm = T))

phew <- (ggplot(subset(fuck,  Policy == 'CatchShare' & IdLevel == 'Species'), aes(log(total_catch), Price,group = IdOrig)) 
+ geom_line(alpha = 0.1) + geom_point(aes(color = Year),alpha = 0.3) + facet_wrap(~SpeciesCatName, scales = 'free') +xlab('Global Species Category Catch') + 
  theme(text = element_text(size = 6)))

fucker <- ProjectionData2 %>%
  group_by(Year,Policy) %>%
  summarize(total_catch = sum(Catch, na.rm = T),total_profits = sum(Profits, na.rm = T),mean_price = mean(Price,na.rm = T)) %>%
  subset(Policy == "Fmsy" & Year == 2050)

a <- ProjectionData %>% group_by(Year,Policy) %>% summarize(tc = sum(Catch, na.rm = T), 
                                                            tb = sum(Biomass, na.rm = T),mp = mean(Price, na.rm = T),ns = length(unique(IdOrig)))



quartz()
ggplot(a,aes(Year,tc)) + geom_point() + facet_wrap(~Policy)





ggsave('downward demand check.pdf',plot = phew)
