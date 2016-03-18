library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)

load('Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata')

fao_key <- read_csv(file = 'Data/fao_regions_key.csv')

use_varwidth <- T

dat <- ProjectionData %>%
  ungroup() %>%
  filter(Year == 2012) %>%
  mutate(log10MSY = log10(MSY),log10MSY_bin = ntile(log10MSY,6),
         log10MSY_bins = cut(log10MSY,6, dig.lab = 1)) %>%
  mutate(fao_region_num = as.numeric(RegionFAO)) %>%
  filter(is.na(fao_region_num) ==F) %>%
  left_join(fao_key, by = 'fao_region_num' ) %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  mutate(mean_log10MSY = mean(log10MSY, na.rm= T)) %>%
  ungroup()


label_dat <- dat %>%
  group_by(log10MSY_bins) %>%
  summarise(numbers = length(BvBmsy),median_f = pmin(2,median(FvFmsy, na.rm = T)),
                                                     perc_ram = mean(Dbase == 'RAM'),
            ypos = quantile(BvBmsy, 0.8))

global_ray_plot <- dat %>%
  group_by(log10MSY_bins) %>%
  mutate(median_f = pmin(2,median(FvFmsy, na.rm = T))) %>%
  ggplot(aes(log10MSY_bins, BvBmsy, fill = median_f)) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2) + 
  geom_boxplot(varwidth = use_varwidth) + 
  xlab('Log10 MSY') + 
  scale_fill_gradient2(low = 'white', high = 'red', mid = 'green', midpoint = 1,
                       limits = c(0,2), breaks = seq(0,2, by = 0.5), 
                       labels = c(seq(0,1.5, by = 0.5), '>2'), name = "Median F/Fmsy") +
  ylab('B/Bmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  geom_label(data = label_dat, aes(x = log10MSY_bins, y = ypos, label = paste('N = ',numbers, sep = '')))
  
ggsave(filename ='GFR Support Scripts/Global Status by MSY - F fill.pdf', global_ray_plot,
       width = 8,height = 6)

regional_ray_plot <- dat %>%
  ungroup() %>%
  # filter(fao_region_num == 21) %>%
  group_by(fao_region_long, mean_log10MSY) %>%
  mutate(regional_median_f = pmin(2,median(FvFmsy, na.rm = T))) %>%
  ggplot(aes(factor(round(mean_log10MSY,0)), BvBmsy, fill = regional_median_f)) +
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 0.75) + 
  geom_boxplot(varwidth = use_varwidth) + 
  facet_wrap(~fao_region_long, scales = 'fixed') + 
  xlab('Log10 MSY') + 
  scale_fill_gradient2(low = 'white', high = 'red', mid = 'green', midpoint = 1,
                       limits = c(0,2.1), breaks = seq(0,2, by = 0.5), 
                       labels = c(seq(0,1.5, by = 0.5), '>2'), name = "Median F/Fmsy") + 
  ylab('B/Bmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95"),
        # axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 7))
  
ggsave(filename ='GFR Support Scripts/Regional Status by MSY - F fill.pdf', regional_ray_plot,
       width = 10,height = 6)

global_ray_ram_plot <- dat %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  mutate(perc_ram = mean(Dbase == 'RAM'), na.rm = T) %>%
  ggplot(aes(log10MSY_bins, BvBmsy, fill = perc_ram)) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2) + 
  geom_boxplot(varwidth = use_varwidth) + 
  xlab('Log10 MSY') + 
  scale_fill_gradient(low = 'white', high = 'blue', labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  ylab('B/Bmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  geom_label(data = label_dat, aes(x = log10MSY_bins, y = ypos, label = paste('N = ',numbers, sep = '')))


ggsave(filename ='GFR Support Scripts/Global Status by MSY - RAM fill.pdf', global_ray_ram_plot,
       width = 8,height = 6)

regional_ray_ram_plot <- dat %>%
  ungroup() %>%
  group_by(fao_region_long, log10MSY_bins) %>%
  mutate(regional_median_f = pmin(2,median(FvFmsy, na.rm = T)), perc_ram = mean(Dbase == 'RAM')) %>%
  # rename('FAO Region' = RegionFAO) %>%
  ggplot(aes(factor(round(mean_log10MSY,0)), BvBmsy, fill = perc_ram)) +
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 0.75) + 
  geom_boxplot(varwidth = use_varwidth) + 
  facet_wrap(~fao_region_long, scales = 'fixed') + 
  xlab('Log10 MSY') + 
  scale_fill_gradient(low = 'white', high = 'blue', labels = percent, name = '% Stock Assessed') + 
  ylab('B/Bmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95"),
        # axis.text.x = element_text(size = 6),
        strip.text = element_text(size = 7))

ggsave(filename ='GFR Support Scripts/Regional Status by MSY - RAM fill.pdf', regional_ray_ram_plot,
       width = 10,height = 6)

