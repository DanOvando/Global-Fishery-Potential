
# A bunch of scripts for analyzing current status from GFR ----------------
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(knitr)
library(ggvis)
library(googleVis)

load('Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata')

vis_summary = ProjectionData %>%
  ungroup() %>% 
  mutate(msy_bin = cut(log10(MSY),5)) %>% 
  group_by(Year,Policy) %>% 
  # group_by(Year,Policy,Dbase,msy_bin) %>% 
    summarise( median_b = median(BvBmsy), median_f = median(FvFmsy),
               total_catch = sum(Catch, na.rm = T), total_profits = sum(Profits, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(id = paste(Policy,'-id', sep = '')) %>% 
  # mutate(id = paste(Policy, Dbase, msy_bin, sep = '-')) %>% 
  as_data_frame() 


# op <- options(gvis.plot.tag='chart')

m = gvisMotionChart(
  vis_summary, 
  chartid = 'test',
  idvar='id',
  timevar='Year', 
  xvar='median_b', 
  yvar='median_f', 
  colorvar='Policy',
  sizevar='total_catch',
  options = list(width = 1000,
  height = 600))
plot(m)

print(m, file = 'test gvis.html')