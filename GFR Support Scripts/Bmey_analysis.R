################################################################################################
##
##
## BMEY Kobe Plot
##
##
################################################################################################


### Load packages, data, functions, and set options -------------------------------------------------------
library(dplyr)
library(tidyr)
library(pracma)
library(pbapply)
library(gridExtra)

# Load Projection Data
load(file = 'Results/PNAS Submission - 6.01 global demand common phi/Data/ProjectionData Data.rdata')

# Read in ASFIS list to use for matching NEIs
asfis<-read.csv(file = 'Data/TaxonCodes.csv', stringsAsFactors = F)

## Source or define functions
source(file = 'GFR Support Scripts/NEI_lookup.R')
source(file = 'GFR Support Scripts/findMEY.R')
source(file = 'Functions/ggKobe.R')

DiffF=function(b,bgrid,f0,f1)
{
  #To be zeroed out.  Want to find b* such that f0(b*) = f1(b*) 
  fval0 = spline(bgrid,f0,xout=b,method="natural")
  fval1 = spline(bgrid,f1,xout=b,method="natural")
  difference = fval0$y - fval1$y
  return(difference)
}

## Set options
calcMEY<-T # run Chris's MEY calculating code?
makeNEIlookup<-T # create new NEI lookup table?

## Load MEY data and NEI lookup table if desired
if(calcMEY==F) { mey_data<-read.csv(file = 'GFR Support Scripts/MEY_results.csv', stringsAsFactors = F) }
if(makeNEIlookup==F) { nei_lookup<-read.csv(file = 'GFR Support Scripts/NEI_lookup.csv', stringsAsFactors = F)}

### Subset Projection Data and run MEY and NEI lookup code if needed --------------------------------------------

# Subset ProjectionData
if(calcMEY==T) {
mey_inputs <- ProjectionData %>%
  filter(Year == 2012)  %>% 
  select(IdOrig, CommName, SciName, SpeciesCatName, SpeciesCat, BvBmsy, FvFmsy, g, k, phi, MSY, MarginalCost, Price)  %>%
  rename(Cost = MarginalCost)

policies <- read.csv('Results/PNAS Submission - 6.01 global demand common phi/Data/PolicyStorage.csv', stringsAsFactors = F)

mey_inputs <- policies %>%
  left_join(mey_inputs, by = 'IdOrig')

# Run Chris's MEY code
mey_results<-findMEY(df = mey_inputs) 
}

# Make NEI lookup table
if(makeNEIlookup==T) { nei_lookup<-NEI_lookup(df = ProjectionData, asfis = asfis) }

### Calculate Bmey and Fmey for NEIs -------------------------------------------------------------------------

# Calculate MEY variables
comp_results<-bind_rows(pblapply(nei_lookup,function(x) {  
  sps<-x$SciName
  phi<-0.188
  
  out<-filter(mey_results, SciName %in% sps) %>%
    summarize(  
      current_b     = quantile(current_b, c(0.25), na.rm = T), # should match actual values assigned to NEIs
      current_f     = quantile(current_f, c(0.75), na.rm =T), # should match actual values assigned to NEIs
      b_mey         = quantile(b_mey, c(0.25), na.rm=T), # Bmey 
      f_mey         = ((phi+1)/phi)*(1 - b_mey^phi/(phi+1)), # Fmey
      current_b_mey = quantile(current_b_mey, c(0.25), na.rm = T), # B/Bmey ** Use this value **
      current_f_mey = quantile(current_f_mey, c(0.75), na.rm = T)) # F/Fmey ** Use this value **
 })) %>%  
  mutate(CommName=names(nei_lookup))  

# Join with nei data
nei_results <- ProjectionData %>%
  tbl_df() %>%
  filter(IdLevel == 'Neis' & Year == 2012) %>%
  select(IdOrig,
         Dbase,
         SciName,
         CommName,
         SpeciesCatName,
         SpeciesCat,
         MSY,
         BvBmsy,
         FvFmsy,
         Policy) %>%
  left_join(comp_results, by = 'CommName') 

### Join Bmey results for species and NEI stocks -------------------------------------------------------------------------

kobe_dat <- ProjectionData %>%
  filter(IdOrig %in% mey_results$IdOrig & Year==2012) %>%
  select(IdOrig, Dbase, SciName, CommName, MSY, SpeciesCat, SpeciesCatName, BvBmsy, FvFmsy) %>%
  unique() %>%
  left_join(mey_results,by = c('IdOrig', 'SciName','CommName','SpeciesCat','SpeciesCatName')) %>%
  bind_rows(nei_results) 

write.csv(file = '../Misc Analyses/MEY data all stocks.csv', kobe_dat) 

### Kobe Plots ----------------------------------------------------------------------------------------------------------

kobe_mey <- ggKobe(kobe_dat, xvar = 'current_b_mey', yvar = 'current_f_mey' ) +
  labs(x = 'B/Bmey', y = 'F/Fmey')

ggsave('MEY Kobe no Median.pdf', kobe_mey, height = 8, width = 9)

 kobe_msy <- ggKobe(kobe_dat, xvar = 'current_b', yvar = 'current_f' ) +
  labs(x = 'B/Bmsy', y = 'F/Fmsy')

ggsave('MSY Kobe.pdf', kobe_msy)

kobes <- arrangeGrob(kobe_msy, kobe_mey)

grid.draw(kobes)

ggsave('Kobe Comparison.pdf', kobes)

