# A bunch of scripts for analyzing current status from GFR ----------------
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(knitr)
library(grid)
library(gridExtra)

load('Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.Rdata')

ProjectionData %>%
  filter(Year == 2012) %>%
  group_by(Dbase) %>%
  summarise(tc = sum(Catch, na.rm = T)) %>%
  ungroup() %>%
  mutate(perc_catch = tc/sum(tc, na.rm = T))

global <- ProjectionData %>%
  filter(Year >=2000, Policy %in% c('Business As Usual','Business As Usual Pessimistic','CatchShare',
                                                        'Catch Share Three', 'Historic')) %>%
  group_by(Year, Policy) %>%
  summarise(total_profits = sum(Profits, na.rm = T),
            total_revenue = sum(Profits, na.rm = T),
            total_catch = sum(Catch, na.rm = T),
            mean_price = mean(Price, na.rm = T)) %>%
  ggplot(aes(Year,mean_price, fill = Policy, size = total_catch)) + 
  geom_point(shape = 21, alpha = 0.6)

ProjectionData %>%
  filter(Policy == 'CatchShare' & Year >2012 ) %>%
  group_by(Year) %>%
  summarise(total_catch = sum(Catch, na.rm = T), mean_price = mean(Price, na.rm = T)) %>%
  ggplot(aes(total_catch, mean_price)) + 
  geom_point()


canada <- ProjectionData %>%
  ungroup() %>%
  filter(Year >=2000,Country == 'Canada', Policy %in% c('Business As Usual','Business As Usual Pessimistic','CatchShare',
                                            'Catch Share Three', 'Historic')) %>%
  group_by(Year, Policy) %>%
  summarise(total_profits = sum(Profits, na.rm = T),
            total_revenue = sum(Profits, na.rm = T),
            total_catch = sum(Catch, na.rm = T),
            mean_price = mean(Price, na.rm = T)) %>%
  ggplot(aes(Year,total_profits, fill = Policy)) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, alpha = 0.6, size = 2) + 
  ylab('Profits ($)')

canada <- ProjectionData %>%
  ungroup() %>%
  filter(Year >=2000,Country == 'Canada', Policy %in% c('Business As Usual','Business As Usual Pessimistic','CatchShare',
                                                        'Catch Share Three', 'Historic')) %>%
  group_by(Year, Policy) %>%
  summarise(total_profits = sum(Profits, na.rm = T),
            total_revenue = sum(Profits, na.rm = T),
            total_catch = sum(Catch, na.rm = T),
            mean_price = mean(Price, na.rm = T),perc_ram = 
              mean(Dbase == 'RAM')) %>%
  ggplot(aes(Year,mean_price, fill = Policy)) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, alpha = 0.6, size = 2) + 
  ylab('Mean Price ($/MT)')


# huh <- as.data.frame(CatchMSYPossibleParams) %>%
#   # subset(IdOrig %in% unique(arg$IdOrig)) %>%
#   group_by(IdOrig) %>%
#   summarise(samps = length(g)) %>%
#   arrange((samps))

# old_dat <- ProjectionData
# new_dat <- ProjectionData
# 
# huh <- new_dat %>%
#   subset(!(IdOrig %in% unique(old_dat$IdOrig)))

fao_key <- read_csv(file = 'Data/fao_regions_key.csv')

use_varwidth <- F

good_color <- 'darkslategray4'

bad_color <- 'orange'

mid_color <- 'white'

ram_color <- 'steelblue2'

cut_breaks <- 10^(seq(-2,7, by = 1))


all_dat <- ProjectionData %>%
  ungroup() %>%
  # filter(Year == 2012 ) %>%
  mutate(log10MSY = log10(MSY),log10MSY_bin = ntile(log10MSY,6),
         log10MSY_bins = cut(MSY,breaks = cut_breaks, dig.lab = 1)) %>%
  mutate(fao_region_num = as.numeric(RegionFAO)) %>%
  left_join(fao_key, by = 'fao_region_num' ) %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  mutate(mean_log10MSY = 10^mean(log10MSY, na.rm= T)) %>%
  ungroup()

bio_plot <- all_dat %>%
  ungroup() %>%
  filter(Year <= 2012 & Year >= 1955) %>%
  group_by(Year) %>%
  summarise(total_biomass = sum(Biomass, na.rm = T)/1e6, stocks = length(unique(IdOrig))) %>% 
  ggplot(aes(Year,total_biomass, size = stocks, fill = stocks, group = stocks)) + 
  geom_point(shape = 21) + 
  scale_fill_continuous(name = 'Number of Stocks',low = 'white',high = 'steelblue4') + 
  scale_size_continuous(guide = F) + 
  ylab('Total Biomass/1e6')


ggsave(filename ='GFR Support Scripts/Biomass Trend.pdf', bio_plot,
       width = 8,height = 6)

dat <- ProjectionData %>%
  ungroup() %>%
  filter(Year == 2012 ) %>%
  mutate(log10MSY = log10(MSY),log10MSY_bin = ntile(log10MSY,6),
         log10MSY_bins = cut(MSY,breaks = cut_breaks, dig.lab = 1)) %>%
  mutate(fao_region_num = as.numeric(RegionFAO)) %>%
  left_join(fao_key, by = 'fao_region_num' ) %>%
  ungroup() %>%
  group_by(log10MSY_bins) %>%
  mutate(mean_log10MSY = 10^mean(log10MSY, na.rm= T)) %>%
  ungroup()

min_fun <- function(x){
  # return(y = pmax(1e-3,x))
  return(y = x + 1e-3) #pmax(1e-3,x))
  
}

 global = dat %>%
  ungroup() %>%
   # filter(RegionFAO == '71') %>%
  mutate(FvFmsy = min_fun(FvFmsy),
         BvBmsy = min_fun(BvBmsy)) %>%
  mutate(f_v_catch = FvFmsy * Catch, b_v_catch = BvBmsy * Catch,
         f_v_msy = FvFmsy * MSY, b_v_msy = BvBmsy * MSY,
         f_v_bmsy = FvFmsy * Bmsy, b_v_bmsy = BvBmsy * Bmsy) %>%
  summarise(catch_weighted_arth_mean_f = sum(f_v_catch, na.rm = T)/sum(Catch, na.rm = T),
            catch_weighted_geom_mean_f = exp(sum(Catch * log(FvFmsy), na.rm = T)/sum(Catch, na.rm = T)),
            catch_weighted_arth_mean_b = sum(b_v_catch, na.rm = T)/sum(Catch, na.rm = T),
            catch_weighted_geom_mean_b = exp(sum(Catch * log(BvBmsy), na.rm = T)/sum(Catch, na.rm = T)),
            msy_weighted_arth_mean_f = sum(f_v_msy, na.rm = T)/sum(MSY, na.rm = T),
            msy_weighted_geom_mean_f = exp(sum(MSY * log(FvFmsy), na.rm = T)/sum(MSY, na.rm = T)),
            msy_weighted_arth_mean_b = sum(b_v_msy, na.rm = T)/sum(MSY, na.rm = T),
            msy_weighted_geom_mean_b = exp(sum(MSY * log(BvBmsy), na.rm = T)/sum(MSY, na.rm = T)),
            bmsy_weighted_arth_mean_f = sum(f_v_bmsy, na.rm = T)/sum(Bmsy, na.rm = T),
            bmsy_weighted_geom_mean_f = exp(sum(Bmsy * log(FvFmsy), na.rm = T)/sum(Bmsy, na.rm = T)),
            bmsy_weighted_arth_mean_b = sum(b_v_bmsy, na.rm = T)/sum(Bmsy, na.rm = T),
            bmsy_weighted_geom_mean_b = exp(sum(Bmsy * log(BvBmsy), na.rm = T)/sum(Bmsy, na.rm = T)))

 kable(global, digits = 2)
 
 huh <- dat %>%
   filter(Year == 2012) %>%
  ggplot(aes(BvBmsy, manual_b)) + 
   geom_point(shape = 21, aes(fill = Dbase)) + 
   geom_abline(slope = 1, intercept = 0) + 
   xlab('B/Bmsy') + 
   ylab('Biomass/Biomass at MSY')
 
 fao_key <- fao_key %>%
   rename(RegionFAO = fao_region_num)
   
 global_summary <- ProjectionData %>%
   select(IdOrig, Country, RegionFAO, SciName, CommName, SpeciesCatName, SpeciesCat, Dbase,
          Policy, Year, Catch, Profits, Biomass, BvBmsy, FvFmsy, MSY) %>%
   mutate(RegionFAO = as.numeric(RegionFAO)) %>%
  left_join(fao_key, by = 'RegionFAO')

 
 save(global_summary, file = 'Historic and Projected Data for Ray.Rdata')
 
 regional = dat %>%
   ungroup() %>%
   mutate(RegionFAO = as.numeric(RegionFAO)) %>%
   group_by(RegionFAO) %>%
   # filter(RegionFAO == '71') %>%
   mutate(FvFmsy = min_fun(FvFmsy),
          BvBmsy = min_fun(BvBmsy)) %>%
   mutate(f_v_catch = FvFmsy * Catch, b_v_catch = BvBmsy * Catch) %>%
   summarise(catch_weighted_arth_mean_f = sum(f_v_catch, na.rm = T)/sum(Catch, na.rm = T),
             catch_weighted_geom_mean_f = exp(sum(Catch * log(FvFmsy), na.rm = T)/sum(Catch, na.rm = T)),
             catch_weighted_arth_mean_b = sum(b_v_catch, na.rm = T)/sum(Catch, na.rm = T),
             catch_weighted_geom_mean_b = exp(sum(Catch * log(BvBmsy), na.rm = T)/sum(Catch, na.rm = T))) %>%
  left_join(fao_key, by = 'RegionFAO')
 
 write.csv(file = 'Regional Kobe Status.csv',regional)
 
 write.csv(file = 'Global Kobe Status.csv',round(global,2))
 

# exp(sum(dat$Catch * log(dat$FvFmsy + 1e-4), na.rm = T) / sum(dat$Catch, na.rm = T))


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
  filter(is.na(fao_region_num) ==F) %>%
  group_by(log10MSY_bins) %>%
  mutate(median_f = pmin(2,median(FvFmsy, na.rm = T)), total_msy = sum(MSY, na.rm = T),
         perc_ram = mean(Dbase == 'RAM'),median_b = pmin(2,median(BvBmsy, na.rm = T)),
         lower_bin = as.numeric(gsub('\\(','',sub(",.*",'',log10MSY_bins))))

dat_regional <- dat %>%
  filter(MSY >= 100) %>%
  filter(is.na(fao_region_num) ==F) %>%
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
