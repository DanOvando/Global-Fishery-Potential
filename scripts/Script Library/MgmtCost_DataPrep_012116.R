###########################################
##
## Script to calculate fisheries value by Country
## for Costs of Fishery Management paper
##
## Provide calculations using model data and
## just catch/price data from FAO 
##
##########################################

## Load packages and data ----

# packages
library(dplyr)
library(tidyr)

# set model run data to use and load data
modelrun<-'6.01 global demand common phi'

load(paste('/Users/Tyler/Documents/Sustainable\ Fisheries\ Group/SFG\ 2014/Global\ Fishery\ Potential/Results/',
           modelrun,'/Data/ProjectionData Data.rdata',sep = ''))

# read in raw FAO landings data
fao<-read.csv('/Users/Tyler/Documents/Sustainable\ Fisheries\ Group/Database\ Files/FAO/FAO Landings Raw Data.csv',stringsAsFactors = F)

## Data summary ----

# Calculate total catch in 2012 using raw FAO landings data
c<-fao %>%
  filter(Year==2012) %>%
  group_by(Country,Year) %>%
  summarize(harvest_fao_2012=sum(Catch,na.rm=T)) %>%
  ungroup() %>%
  arrange(desc(harvest_fao_2012)) %>%
  filter(harvest_fao_2012>0)

# Subset upsides data to 2012, 2050, and necessary variables. Use this as base dataset for calculating summaries  
up<-UnlumpedProjectionData %>%
  filter(Year %in% c(2012,2050)) %>%
  select(IdOrig,Country,Dbase,CommName,Policy,Year,Catch,Biomass,Profits,Price,CatchShare)

## Calculate total catch in 2012 for upsides by country, RAM, and catch share fisheries. Calculate percentages ----

# Catch and revenue totals by country
upcatch<-up %>%
  filter(Year==2012) %>%
  mutate(revenue=Catch*Price) %>%
  group_by(Country) %>%
  summarize(harvest_2012=sum(Catch,na.rm=T),biomass_2012=sum(Biomass,na.rm=T), profits_2012=sum(Profits,na.rm=T),revenue_2012=sum(revenue,na.rm=T)) %>%
  ungroup()

# Catch total by country for RAM stocks that are not labeled as catch shares
upram<-up %>%
  filter(Year==2012 & Dbase=='RAM' & CatchShare==0) %>%
  group_by(Country,Dbase) %>%
  summarize(harvest_ram=sum(Catch,na.rm=T)) %>%
  ungroup()

# Catch by catch shares
upcs<-up %>%
  filter(Year==2012) %>%
  group_by(Country,CatchShare) %>%
  summarize(harvest_cs=sum(Catch,na.rm=T)) %>%
  ungroup() %>%
  filter(CatchShare==1)

# join catch summaries to fao catch summary. Calculate what % the model data represents of FAO 2012 data, what % is RAM non-catch shares, and what % is CS
df<-left_join(c,upcatch) %>%
  left_join(upram) %>%
  left_join(upcs) %>%
  select(-Dbase,-CatchShare) %>%
  mutate(H_perc_fao2012=harvest_2012/harvest_fao_2012, H_perc_RAM2012=harvest_ram/harvest_2012, H_perc_CS2012=harvest_cs/harvest_2012)

# replace NAs with zeros
df$harvest_cs[is.na(df$harvest_2012)==F & is.na(df$harvest_cs)]<-0
df$harvest_ram[is.na(df$harvest_2012)==F &is.na(df$harvest_ram)]<-0
df$H_perc_CS2012[is.na(df$harvest_2012)==F &is.na(df$H_perc_CS2012)]<-0
df$H_perc_RAM2012[is.na(df$harvest_2012)==F &is.na(df$H_perc_RAM2012)]<-0
df$H_perc_fao2012[is.na(df$harvest_2012)==F &is.na(df$H_perc_fao2012)]<-0

## Summarize country results by policy (CatchShare, Fmsy, Opt, and BAU for both scenarios) ----

# All variables
up2050<-up %>%
  filter(Year==2050 & Policy %in% c('CatchShare','Catch Share Three','Fmsy','Fmsy Three',
                                    'Opt','Business As Usual','Business As Usual Pessimistic','StatusQuoOpenAccess')) %>%
  ungroup() %>%
  mutate(revenue=Catch*Price) %>%
  select(Country,Policy,Catch,Profits,Biomass,revenue) %>%
  group_by(Country,Policy) %>%
  summarize(harvest_policy=sum(Catch,na.rm=T), biomass_policy=sum(Biomass,na.rm=T),revenue_policy=sum(revenue,na.rm=T),profits_policy=sum(Profits,na.rm=T)) %>%
  ungroup() 
  
# Harvest  
up2050catch<- up2050 %>%
  select(Country,Policy,harvest_policy) %>%
  spread(Policy, harvest_policy) %>%
  rename(Harvest_BAU=`Business As Usual`,Harvest_BAUP=`Business As Usual Pessimistic`,Harvest_OA=StatusQuoOpenAccess,
         Harvest_CS=CatchShare,Harvest_CS3=`Catch Share Three`,Harvest_Fmsy=Fmsy,Harvest_Fmsy3=`Fmsy Three`,Harvest_Opt=Opt)

# Biomass  
up2050biomass<- up2050 %>%
  select(Country,Policy,biomass_policy) %>%
  spread(Policy, biomass_policy) %>%
  rename(Biomass_BAU=`Business As Usual`,Biomass_BAUP=`Business As Usual Pessimistic`,Biomass_OA=StatusQuoOpenAccess,
         Biomass_CS=CatchShare,Biomass_CS3=`Catch Share Three`,Biomass_Fmsy=Fmsy,Biomass_Fmsy3=`Fmsy Three`,Biomass_Opt=Opt)

# Revenue 
up2050revenue<- up2050 %>%
  select(Country,Policy,revenue_policy) %>%
  spread(Policy, revenue_policy) %>%
  rename(Revenue_BAU=`Business As Usual`,Revenue_BAUP=`Business As Usual Pessimistic`,Revenue_OA=StatusQuoOpenAccess,
         Revenue_CS=CatchShare,Revenue_CS3=`Catch Share Three`,Revenue_Fmsy=Fmsy,Revenue_Fmsy3=`Fmsy Three`,Revenue_Opt=Opt)

# Profits 
up2050profits<- up2050 %>%
  select(Country,Policy,profits_policy) %>%
  spread(Policy, profits_policy) %>%
  rename(Profits_BAU=`Business As Usual`,Profits_BAUP=`Business As Usual Pessimistic`,Profits_OA=StatusQuoOpenAccess,
         Profits_CS=CatchShare,Profits_CS3=`Catch Share Three`,Profits_Fmsy=Fmsy,Profits_Fmsy3=`Fmsy Three`,Profits_Opt=Opt)

# Join 2050 output dataframes
df<-left_join(df,up2050catch)

df<-left_join(df,up2050biomass)

df<-left_join(df,up2050revenue)

df<-left_join(df,up2050profits)

## Write csv file
write.csv(df,file = 'Results/Cost_of_mgmt_data_020416.csv')
