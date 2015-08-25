run_gfr_diagnostics <- function(runfolder, NumCPUs = 1, IUULevel = 1.25, cmsy_iterations = 1000,
                                do_iuu = F, do_ind_jack = F, do_reg_jack = F,do_cmsy_montecarlo = F,
                                do_expanded_montecarlo = F, mciterations = 250, elastic_demand = T, sp_group_demand = F)
  
{
  library(plyr)
  library(dplyr)
  library(lattice)
  library(rfishbase)
  library(stringr)
  library(RCurl)
  library(XML)
  library(MASS)
  library(zoo)
  library(proftools)
  library(snowfall)
  library(parallel)
  library(ggplot2)
  library(gridExtra)
  library(reshape2)
  
  sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
  
  BaselineYear <<- 2012
  if (do_iuu == T)
  {
    iuu_analysis <- run_iuu(runfolder = runfolder, NumCPUs = NumCPUs,
                            IUULevel = IUULevel, iterations = cmsy_iterations)
    show('Finished iuu analysis')
  }
  
  if (do_ind_jack == T)
  {
    individual_jackknife_analysis <- individual_jackknife(runfolder = runfolder, CPUs = NumCPUs, iterations = cmsy_iterations)
    show('Finished individual jackknife analysis')
    
  }
  if (do_reg_jack)
  {
    regional_jackknife_analysis <- regional_jackknife(runfolder = runfolder, CPUs = NumCPUs, iterations = cmsy_iterations)
    
    show('Finished regional jackknife analysis')
    
  }
  if (do_cmsy_montecarlo ==T)
  {
    cmsy_montecarlo_analysis <- cmsy_monte_carlo(runfolder = runfolder, CPUs = NumCPUs, mciterations = mciterations, real_elastic_demand = elastic_demand
                                                 , real_sp_group_demand = sp_group_demand)

    show('Finished cmsy montecarlo analysis')
    
  }
  if (do_expanded_montecarlo ==T)
  {
    expanded_montecarlo_analysis <- expanded_monte_carlo(runfolder = runfolder, CPUs = NumCPUs, mciterations = mciterations,real_elastic_demand = elastic_demand
                                                         , real_sp_group_demand = sp_group_demand)

    show('Finished expanded montecarlo analysis')
    
  }
  files <- ls(pattern = '_analysis')
  
  diagnostics <- list()
  
  for (f in 1:length(files))
  {
    eval(parse(text = paste('diagnostics$',files[f],'=',files[f], sep = '')))
  }
  return(diagnostics)
}