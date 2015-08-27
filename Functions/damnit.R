damnit <- function(arg,filteryear = 2012)
{

arg %>%
  group_by(IdLevel,Year) %>%
  summarize(total_catch = sum(Catch, na.rm = T), 
            num_stocks = length(unique(IdOrig)), total_profits = sum(Profits, na.rm = T), mb = mean(BvBmsy, na.rm = T)
            ,mf = mean(FvFmsy, na.rm = T)) %>%
    subset(Year == filteryear)
}