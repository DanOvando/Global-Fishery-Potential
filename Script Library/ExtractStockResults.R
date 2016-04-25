########################################################################################
#
# Script containing function to extract fishery timeseries for countries of interest
#
########################################################################################

# library(dplyr)
# library(tidyr)

StockResults<-function(data=UnlumpedProjectionData, countries, policies, 
                       metrics=c('Catch','Biomass','Profits','Revenue','BvBmsy','FvFmsy')) {
  
  policies<-c('Historic','CatchShare','Catch Share Three','Fmsy','Fmsy Three','Business As Usual','Business As Usual Pessimistic')
  
  out_data<-data %>%
    filter(Country %in% countries & Policy %in% policies) %>%
    select(IdOrig,Country,CommName,SciName,Policy,Year,Catch,Biomass,Price,Profits,BvBmsy,FvFmsy) %>%
    mutate(
      Revenue   = Catch*Price,
      Scenario  = NA)
  
  # Rename policies to match paper
  out_data$Scenario[out_data$Policy %in% c('Catch Share Three','Fmsy Three','Business As Usual')]<-'Conservation Concern'
  out_data$Scenario[out_data$Policy %in% c('CatchShare','Fmsy','Business As Usual Pessimistic')]<-'All Stocks'
  out_data$Scenario[out_data$Policy %in% c('Historic')]<-'Historic'
  
  out_data$Policy[out_data$Policy %in% c('CatchShare','Catch Share Three')]<-'RBFM'
  out_data$Policy[out_data$Policy %in% c('Fmsy','Fmsy Three')]<-'Fmsy'
  
  write.csv(out_data, file = '../Misc Data/Fishery Trajectories.csv')
  
  return(out_data)
}

