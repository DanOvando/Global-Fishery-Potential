
trendit <- function(dat)
{

trendy <- dat %>%
  group_by(Policy,Year) %>%
  summarize(median_b = median(BvBmsy, na.rm = T),median_f  = median(FvFmsy, na.rm = T),
            total_catch = sum(Catch, na.rm = T), total_biomass = sum(Biomass, na.rm = T), 
            total_profits = sum(Profits, na.rm = T),total_price = sum(Price, na.rm = T),
            total_costs = sum(MarginalCost, na.rm = T), missing_stocks = sum(is.na(BvBmsy)))

return(trendy)
}