damnit <- function(arg,filteryear = 2012)
{

arg %>%
  group_by(Year,Policy) %>%
  summarize(total_catch = sum(Catch, na.rm = T), 
            num_stocks = length(unique(IdOrig)), total_profits = sum(Profits, na.rm = T), mb = mean(BvBmsy, na.rm = T)
            ,mf = mean(FvFmsy, na.rm = T), total_biomass = sum(Biomass, na.rm = T)) %>%
    subset(Year == filteryear)
}