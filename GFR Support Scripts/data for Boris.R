library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)

load('Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata')

global_kobe <- ProjectionData %>%
  ungroup() %>%
  filter(Year == 2012) %>%
  select(Year,Dbase, BvBmsy, FvFmsy, Catch) %>%
  mutate(RAM = Dbase == 'RAM') %>%
  select(-Dbase)

write_csv(global_kobe, path = 'GFR Support Scripts/global_kobe_data.csv')


figure_4 <- ProjectionData %>%
  ungroup() %>%
  group_by(Year, Policy) %>%
  summarise(perc_healthy = mean(BvBmsy > 0.8, na.rm = T), 
            total_profits = sum(Profits, na.rm = T),
            total_catch = sum(Catch, na.rm = T)) %>%
  filter(Year >=1980) %>%
  filter(Policy %in% c('Historic', "Catch Share Three", "Fmsy Three","Business As Usual Pessimistic",
                       "Business As Usual" ))

figure_4$Policy[figure_4$Policy == "Catch Share Three"] <- 'RBFM'

figure_4$Policy[figure_4$Policy == "Fmsy Three"] <- 'Fmsy'

figure_4$Policy[figure_4$Policy == 'Business As Usual'] <- 'BAU (conservation concern)'

figure_4$Policy[figure_4$Policy == "Business As Usual Pessimistic"] <- 'BAU (all stocks)'




check <- figure_4 %>%
  ggplot(aes(Year, perc_healthy, size = total_catch, color = total_profits, shape = Policy )) + 
    geom_point() + 
  ylim(c(0,1))




figure_4 <- figure_4 %>%
  ungroup() %>%
  mutate(perc_healthy = 100*perc_healthy) %>%
  rename('% Above 0.8 BvBmsy' = perc_healthy)

write_csv(figure_4, path = 'GFR Support Scripts/figure_4_data.csv')
