rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)

load('Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata')

fao_key <- read_csv(file = 'Data/fao_regions_key.csv')

use_varwidth <- F

good_color <- 'darkslategray4'

bad_color <- 'orange'

mid_color <- 'white'

ram_color <- 'steelblue2'

cut_breaks <- 10^(seq(-2,7, by = 1))

dat <- ProjectionData %>%
  ungroup() %>%
  filter(Year == 2012 ) %>%
  mutate(log10MSY = log10(MSY),log10MSY_bin = ntile(log10MSY,6),
         log10MSY_bins = cut(MSY,breaks = cut_breaks, dig.lab = 1)) %>%
  mutate(fao_region_num = as.numeric(RegionFAO)) %>%
  filter(is.na(fao_region_num) ==F) %>%
  left_join(fao_key, by = 'fao_region_num' ) %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  mutate(mean_log10MSY = 10^mean(log10MSY, na.rm= T)) %>%
  ungroup()

label_dat <- dat %>%
  group_by(log10MSY_bins) %>%
  summarise(numbers = length(BvBmsy),median_f = pmin(2,median(FvFmsy, na.rm = T)),median_b = pmin(2,median(BvBmsy, na.rm = T)),
            perc_ram = mean(Dbase == 'RAM'),
            ypos = quantile(BvBmsy, 0.9), ypos_b = quantile(pmin(2.5,FvFmsy), 0.9),  total_msy = sum(MSY, na.rm = T)) %>%
  ungroup() %>%
  mutate(p_total_msy = total_msy / sum(total_msy),lower_bin = as.numeric(gsub('\\(','',sub(",.*",'',log10MSY_bins)))) %>%
  filter(lower_bin >= 100)

regional_label_dat <- dat %>%
  group_by(fao_region_long,log10MSY_bins) %>%
  summarise(numbers = length(BvBmsy),median_f = pmin(2,median(FvFmsy, na.rm = T)),median_b = pmin(2,median(BvBmsy, na.rm = T)),
            perc_ram = mean(Dbase == 'RAM'),
            ypos = quantile(BvBmsy, 0.9), ypos_b = quantile(pmin(4,FvFmsy), 0.9),  total_msy = sum(MSY, na.rm = T)) %>%
  mutate(p_total_msy = total_msy / sum(total_msy)) %>%
  ungroup() %>%
  mutate(lower_bin = as.numeric(gsub('\\(','',sub(",.*",'',log10MSY_bins)))) %>%
  filter(lower_bin >= 100) %>%
  arrange(desc(p_total_msy)) #%>%
# filter(fao_region_long == "67-Pacific, Northeast")

dat_global <- dat %>%
  filter(MSY >= 100) %>%
  group_by(log10MSY_bins) %>%
  mutate(median_f = pmin(2,median(FvFmsy, na.rm = T)), total_msy = sum(MSY, na.rm = T),
         perc_ram = mean(Dbase == 'RAM'),median_b = pmin(2,median(BvBmsy, na.rm = T)),
         lower_bin = as.numeric(gsub('\\(','',sub(",.*",'',log10MSY_bins))))

dat_regional <- dat %>%
  filter(MSY >= 100) %>%
  group_by(fao_region_long,log10MSY_bins) %>%
  mutate(median_f = pmin(2,median(FvFmsy, na.rm = T)), total_msy = sum(MSY, na.rm = T),
         perc_ram = mean(Dbase == 'RAM'),median_b = pmin(2,median(BvBmsy, na.rm = T)),
         regional_median_f = pmin(2,median(FvFmsy, na.rm = T)),total_msy = sum(MSY, na.rm = T),
         lower_bin = as.numeric(gsub('\\(','',sub(",.*",'',log10MSY_bins)))) # %>%
# filter(fao_region_long == "67-Pacific, Northeast")


pos_bins <- unique(dat_global$log10MSY_bins)

cut_breaks <- cut_breaks[cut_breaks >= 100]


# Make simple plots -------------------------------------------------------

library(gridExtra)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="top"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  arrangeGrob(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

msy_plot <- dat_global %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  summarise(total_msy = sum(MSY, na.rm = T), percent_ram = unique(perc_ram)) %>%
  ungroup() %>%
  mutate(perc_msy = total_msy / sum(total_msy, na.rm = T) ) %>%
  ggplot(aes(log10MSY_bins, perc_msy, fill = percent_ram)) + 
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  scale_y_continuous(labels = percent)+ 
  ylab('% of Global MSY') + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())


b_plot <- dat_global %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  summarise(median_b = median(BvBmsy, na.rm = T), lower95 = quantile(BvBmsy, 0.025, na.rm = T),
            upper95 = quantile(BvBmsy, 0.975, na.rm = T), lower_quartile = quantile(BvBmsy, 0.25), 
            upper_quartile = quantile(BvBmsy, 0.75), percent_ram = unique(perc_ram)) %>%
  ggplot(aes(fill = percent_ram)) + 
  geom_errorbar(aes(log10MSY_bins, ymax = upper95, ymin = lower95)) + 
  geom_point(aes(log10MSY_bins, median_b), size = 4, shape = 21) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2) + 
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  ylab('B/Bmsy') + 
  scale_y_continuous(breaks = seq(0,2, by = 0.5), labels = c(seq(0,1.5, by = 0.5), '>2')) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

f_plot <- dat_global %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  summarise(median_f = median(FvFmsy, na.rm = T), lower95 = quantile(FvFmsy, 0.025, na.rm = T),
            upper95 = pmin(2,quantile(FvFmsy, 0.975, na.rm = T)), lower_quartile = quantile(FvFmsy, 0.25), 
            upper_quartile = quantile(FvFmsy, 0.75), percent_ram = unique(perc_ram)) %>%
  ggplot(aes(fill = percent_ram)) + 
  geom_errorbar(aes(log10MSY_bins, ymax = upper95, ymin = lower95)) + 
  geom_point(aes(log10MSY_bins, median_f), size = 4, shape = 21) + 
  
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2) + 
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) +
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  ylab('F/Fmsy') + 
  scale_y_continuous(breaks = seq(0,2, by = 0.5), labels = c(seq(0,1.5, by = 0.5), '>2')) + 
  xlab('MSY (tons)')

trevor_plot <-  grid_arrange_shared_legend(msy_plot, b_plot, f_plot)


ggsave(filename ='GFR Support Scripts/Trevor Plot.pdf', trevor_plot,
       width = 8,height = 6)


# Make specific plots -----------------------------------------------------

global_ray_plot <-  dat_global %>%
  ggplot(aes(log10MSY_bins, BvBmsy, fill = median_f)) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2)
for (i in 1:length(pos_bins)){
  global_ray_plot <- global_ray_plot + geom_boxplot(data = filter(dat_global, log10MSY_bins == pos_bins[i]),
                                                    width = label_dat$p_total_msy[label_dat$log10MSY_bins == pos_bins[i]]) 
}
global_ray_plot <- global_ray_plot + 
  xlab('MSY (tons)') + 
  scale_fill_gradient2(low = good_color, high = bad_color, mid = mid_color, midpoint = 1,
                       limits = c(0,2), breaks = seq(0,2, by = 0.5), 
                       labels = c(seq(0,1.5, by = 0.5), '>2'), name = "Median F/Fmsy") +
  ylab('B/Bmsy') + 
  theme_light() + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  geom_label(data = label_dat, aes(x = log10MSY_bins, y = ypos, label = paste('N = ',numbers, sep = '')))


ggsave(filename ='GFR Support Scripts/Global Status by MSY - F fill.pdf', global_ray_plot,
       width = 8,height = 6)

regional_ray_plot <- dat_regional %>%
  ggplot(aes(factor(lower_bin), BvBmsy, fill = regional_median_f, weight = total_msy)) +
  facet_wrap(~fao_region_long, scales = 'fixed') +
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 0.75)
for (i in 1:dim(regional_label_dat)[1]){
  regional_ray_plot <- regional_ray_plot +
    geom_boxplot(data = filter(dat_regional,  log10MSY_bins == regional_label_dat$log10MSY_bins[i]
                               & fao_region_long == regional_label_dat$fao_region_long[i]),
                 width = regional_label_dat$p_total_msy[i])
}
regional_ray_plot <- regional_ray_plot +
  xlab('MSY (tons)') +
  scale_fill_gradient2(low = good_color, high = bad_color, mid = mid_color, midpoint = 1,
                       limits = c(0,2), breaks = seq(0,2, by = 0.5),
                       labels = c(seq(0,1.5, by = 0.5), '>2'), name = "Median F/Fmsy") +
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) +
  ylab('B/Bmsy') +
  theme_light() +
  theme(legend.background = element_rect(fill = "grey95"),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 7))

ggsave(filename ='GFR Support Scripts/Regional Status by MSY - F fill.pdf', regional_ray_plot,
       width = 10,height = 6)

global_ray_ram_plot <- dat_global %>%
  ggplot(aes(log10MSY_bins, BvBmsy, fill = perc_ram)) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2) 
for (i in 1:length(pos_bins)){
  global_ray_ram_plot <- global_ray_ram_plot + geom_boxplot(data = filter(dat_global, log10MSY_bins == pos_bins[i]),
                                                            width = label_dat$p_total_msy[label_dat$log10MSY_bins == pos_bins[i]]) 
}
global_ray_ram_plot <- global_ray_ram_plot + 
  xlab('MSY (tons)') + 
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) + 
  ylab('B/Bmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  geom_label(data = label_dat, aes(x = log10MSY_bins, y = ypos, label = paste('N = ',numbers, sep = '')))


ggsave(filename ='GFR Support Scripts/Global Status by MSY - RAM fill.pdf', global_ray_ram_plot,
       width = 8,height = 6)

regional_ray_ram_plot <- dat_regional %>%
  ggplot(aes(factor(lower_bin), BvBmsy, fill = perc_ram)) +
  facet_wrap(~fao_region_long, scales = 'fixed') +
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 0.75)
for (i in 1:dim(regional_label_dat)[1]){
  regional_ray_ram_plot <- regional_ray_ram_plot +
    geom_boxplot(data = filter(dat_regional,  log10MSY_bins == regional_label_dat$log10MSY_bins[i]
                               & fao_region_long == regional_label_dat$fao_region_long[i]),
                 width = regional_label_dat$p_total_msy[i])
}
regional_ray_ram_plot <- regional_ray_ram_plot +
  xlab('MSY (tons)') +
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) +
  ylab('B/Bmsy') +
  theme_light() +
  theme(legend.background = element_rect(fill = "grey95"),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 7))

ggsave(filename ='GFR Support Scripts/Regional Status by MSY - RAM fill.pdf', regional_ray_ram_plot,
       width = 10,height = 6)


# FvF versions ------------------------------------------------------------

global_fvf_ray_plot <- dat_global %>%
  ggplot(aes(log10MSY_bins, pmin(4,FvFmsy), fill = median_b, weight = total_msy)) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2)
for (i in 1:length(pos_bins)){
  global_fvf_ray_plot <- global_fvf_ray_plot + geom_boxplot(data = filter(dat_global, log10MSY_bins == pos_bins[i]),
                                                            width = label_dat$p_total_msy[label_dat$log10MSY_bins == pos_bins[i]]) 
}
global_fvf_ray_plot <- global_fvf_ray_plot + 
  xlab('MSY (tons)') + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) + 
  scale_fill_gradient2(low = bad_color, high = good_color, mid = mid_color, midpoint = 1,
                       limits = c(0,2), breaks = seq(0,2, by = 0.5), 
                       labels = c(seq(0,1.5, by = 0.5), '>2'), name = "Median B/Bmsy") +
  ylab('F/Fmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  scale_y_continuous(breaks = seq(0,4, by = 0.5), labels = c(seq(0,3.5, by = 0.5), '>4')) + 
  geom_label(data = label_dat, aes(x = log10MSY_bins, y = ypos_b, label = paste('N = ',numbers, sep = '')))

ggsave(filename ='GFR Support Scripts/Global F Status by MSY - B fill.pdf', global_fvf_ray_plot,
       width = 8,height = 6)

regional_fvf_ray_plot <- dat %>%
  ungroup() %>%
  # filter(fao_region_num == 21) %>%
  group_by(fao_region_long, mean_log10MSY) %>%
  mutate(regional_median_b = pmin(2,median(BvBmsy, na.rm = T)),total_msy = sum(MSY, na.rm = T)) %>%
  ggplot(aes(factor(round(mean_log10MSY,0)), pmin(4,FvFmsy), fill = regional_median_b, weight = total_msy)) +
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 0.75) + 
  geom_boxplot(varwidth = use_varwidth) + 
  facet_wrap(~fao_region_long, scales = 'fixed') + 
  xlab('MSY (tons)') + 
  scale_fill_gradient2(low = bad_color, high = good_color, mid = mid_color, midpoint = 1,
                       limits = c(0,2), breaks = seq(0,2, by = 0.5), 
                       labels = c(seq(0,1.5, by = 0.5), '>2'), name = "Median B/Bmsy")  + 
  ylab('F/Fmsy') + 
  theme_light() + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) +
  theme(legend.background = element_rect(fill = "grey95"),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 7)) + 
  scale_y_continuous(breaks = seq(0,4, by = 0.5), labels = c(seq(0,3.5, by = 0.5), '>4')) 

ggsave(filename ='GFR Support Scripts/Regional FvF Status by MSY - B fill.pdf', regional_fvf_ray_plot,
       width = 10,height = 6)

global_fvf_ray_ram_plot <- dat_global %>%
  ggplot(aes(log10MSY_bins, pmin(2.5,FvFmsy), fill = perc_ram, weight = total_msy)) + 
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.2)
for (i in 1:length(pos_bins)){
  global_fvf_ray_ram_plot <- global_fvf_ray_ram_plot + geom_boxplot(data = filter(dat_global, log10MSY_bins == pos_bins[i]),
                                                                    width = label_dat$p_total_msy[label_dat$log10MSY_bins == pos_bins[i]]) 
}
global_fvf_ray_ram_plot <- global_fvf_ray_ram_plot + 
  xlab('MSY (tons)') + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) + 
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  ylab('F/Fmsy') + 
  theme_light() + 
  theme(legend.background = element_rect(fill = "grey95")) + 
  geom_label(data = label_dat, aes(x = log10MSY_bins, y = ypos_b, label = paste('N = ',numbers, sep = ''))) +
  scale_y_continuous(breaks = seq(0,2.5, by = 0.5), labels = c(seq(0,2, by = 0.5), '>2.5')) 


ggsave(filename ='GFR Support Scripts/Global FvF Status by MSY - RAM fill.pdf', global_fvf_ray_ram_plot,
       width = 8,height = 6)

regional_fvf_ray_ram_plot <- dat_regional %>%
  ggplot(aes(factor(lower_bin), pmin(4,FvFmsy), fill = perc_ram)) +
  facet_wrap(~fao_region_long, scales = 'fixed') +
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 0.75)
for (i in 1:dim(regional_label_dat)[1]){
  regional_fvf_ray_ram_plot <- regional_fvf_ray_ram_plot +
    geom_boxplot(data = filter(dat_regional,  log10MSY_bins == regional_label_dat$log10MSY_bins[i]
                               & fao_region_long == regional_label_dat$fao_region_long[i]),
                 width = regional_label_dat$p_total_msy[i])
}
regional_fvf_ray_ram_plot <- regional_fvf_ray_ram_plot +
  xlab('MSY (tons)') +
  scale_fill_gradient(low = mid_color, high = ram_color, labels = percent, name = '% Stock Assessed',
                      limits = c(0,1)) + 
  scale_x_discrete(labels = cut_breaks[1:(length(cut_breaks) - 1)]) +
  ylab('F/Fmsy') +
  theme_light() +
  theme(legend.background = element_rect(fill = "grey95"),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(size = 7)) + 
  scale_y_continuous(breaks = seq(0,4, by = 0.5), labels = c(seq(0,3.5, by = 0.5), '>4')) 

ggsave(filename ='GFR Support Scripts/Regional FvF Status by MSY - RAM fill.pdf', regional_fvf_ray_ram_plot,
       width = 10,height = 6)


# Combine things ----------------------------------------------------------

status_plot <- grid_arrange_shared_legend(global_ray_ram_plot, global_fvf_ray_ram_plot)

ggsave(filename ='GFR Support Scripts/Combined Status Plot.pdf', status_plot,
       width = 8,height = 8)
