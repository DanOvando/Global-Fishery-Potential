# A bunch of scripts for analyzing current status from GFR ----------------
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(knitr)

load('Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata')

fao_key <- read_csv(file = 'Data/fao_regions_key.csv')


fao_key <- fao_key %>%
  rename(RegionFAO = fao_region_num)

global_summary <- ProjectionData %>%
  select(IdOrig, Country, RegionFAO, SciName, CommName, SpeciesCatName, SpeciesCat, Dbase,
         Policy, Year, Catch, Profits, Biomass, BvBmsy, FvFmsy, MSY) %>%
  mutate(fao_region_long = 'Global')

regional_summary <- global_summary %>%
  select(-fao_region_long)

regional_summary$fao_region_num = NA

kobe_region_nums <- c('67','27','71')

# huh <- regional_summary[grepl('67', regional_summary$RegionFAO),]

region_func <- function(reg,dat){
  
  dat$fao_region_num[grepl(reg,dat$RegionFAO)] <- reg
  
  out <- dat[grepl(reg,dat$RegionFAO),]
  
  return(out)
}

temp <- bind_rows(lapply(kobe_region_nums, region_func, dat = regional_summary))

temp$fao_region_num = as.numeric(temp$fao_region_num)


regional_summary <- temp %>%
  left_join(fao_key, by = c('fao_region_num' = 'RegionFAO'))

kobe_regions <- c('Global',"67-Pacific, Northeast","27-Atlantic, Northeast","71-Pacific, Western Central")

kobe_data <- global_summary %>%
  ungroup() %>%
  select(-fao_region_long) %>%
  mutate(fao_region_long = 'Global') %>%
  bind_rows(filter(regional_summary, Year == 2012 & fao_region_long %in% kobe_regions )) %>%
  filter(Year == 2012 & fao_region_long %in% kobe_regions)


kobe_data$fao_region_long <- factor( kobe_data$fao_region_long,
                                     levels = unique(kobe_data$fao_region_long))

kobe_summary <- kobe_data %>%
  ungroup() %>%
  group_by(fao_region_long) %>%
  summarise(
    median_f = median(FvFmsy, na.rm = T),
    median_b = median(BvBmsy, na.rm = T),
    msy_weighted_geom_mean_f = exp(sum(MSY * log(FvFmsy + 1e-3), na.rm = T)/sum(MSY, na.rm = T)),
    msy_weighted_geom_mean_b = exp(sum(MSY * log(BvBmsy), na.rm = T)/sum(MSY, na.rm = T)),
    Dbase = NA)

kobe_summary$fao_region_long <- factor( kobe_summary$fao_region_long,
                                        levels = unique(kobe_summary$fao_region_long))


kobes <- kobe_data %>%
  filter(Year == 2012) %>%
  ggplot(aes(BvBmsy, pmin(4,FvFmsy))) + 
  facet_wrap(~fao_region_long,as.table = T) + 
  stat_density_2d(aes(fill = ..density..), geom = 'tile', n = 100, alpha = 0.8, contour = F) + 
  scale_fill_gradient2(guide = F,low = 'skyblue1', mid = 'white', high = 'khaki1', midpoint = 0.2) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash') + 
  geom_vline(aes(xintercept = 1), linetype = 'longdash') + 
  geom_point(aes(color = factor(Dbase == 'RAM')), alpha = 0.3) + 
    scale_color_manual(guide = F, values = c('grey','red')) +
  geom_point(data = kobe_summary, aes(median_b, median_f), shape = 17, size = 4) + 
  geom_point(data = kobe_summary, aes(x = msy_weighted_geom_mean_b, y = msy_weighted_geom_mean_f),
             shape = 15, size = 4) + 
  xlab('B/Bmsy') + 
  ylab('F/Fmsy') + 
  theme_classic()

ggsave(file = 'Blogpost Kobe.pdf', kobes, height = 8,width = 8)

arg <- filter(kobe_data, fao_region_num == 67)

barg = read.csv(file = 'wtf.csv', stringsAsFactors = F)

huh <- barg$IdOrig[!barg$IdOrig %in% arg$IdOrig]

buh <- filter(barg, IdOrig %in% huh)

fuh <- filter(kobe_data, IdOrig %in% huh)