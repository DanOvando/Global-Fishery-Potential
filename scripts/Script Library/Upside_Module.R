#############################################---------------------------------------------
## LOAD PACKAGES AND DATA
############################################# --------------------------------------------

# Packages

rm(list = ls())
sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

library(zoo)
library(readr)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Data - Make sure data file is in same directory (folder) as this R script or adjust the pathname accordingly
sample_data = read.csv(file = 'Data/test data for upside module.csv')

load('Results/PNAS Submission - 6.01 global demand common phi/Data/PrmRegressions.Rdata')
 
load('Data/fishbase_data.Rdata')

isscaap <- read.csv('Data/ISSCAAP Codes.csv', stringsAsFactors = F)

regs = RealModels[names(RealModels) != 'M7'] #get rid of NEI model

devtools::use_data(regs,fishbase_lifehistory,isscaap, pkg = '/Users/danovando/Code\ Library/GUM', internal = T)

dat = testdf

dat <- dat %>% dplyr::select(IdOrig,SciName,CommName,Year,Catch,BvBmsy) %>%
  mutate(IdOrig = as.character(IdOrig))


temp_predicted <- list()

# Obtain predicted log B/Bmsy from each model
for (i in 1 :length(regs) ){
  temp_predicted[[i]] = apply_prm(dat = dat, reg = regs[[i]]) %>%
    mutate(model = names(regs[i]), model_number = as.numeric(gsub('M','',model)))
}

data <- bind_rows(temp_predicted) %>%
  mutate(model_worked = is.na(LogBvBmsy) == F) %>%
  filter(model_worked == T) %>% #drop models that didn't work
ungroup() %>%
  group_by(IdOrig) %>%
  filter(model_number == min(model_number)) %>%
  mutate(BvBmsy = exp(LogBvBmsy)) %>%
  rename(year = Year, catch = Catch)#keep the best model that ran for each fishery

data <- FindResilience(data) %>%
  rename(res = Res)

stocks <- unique(data$IdOrig)

sub <- sample(stocks, 10, replace = F)

data <- filter(data, IdOrig %in% sub)

stocks <- unique(data$IdOrig)

apply_fun <- function(i,data,stocks){
  out = run_post_prm_pt_cmsy(dat = filter(data, IdOrig == stocks[i]))$CatchMSY
}

results <- lapply(1:length(stocks), apply_fun, data = data, stocks = stocks) %>%
  bind_rows()

