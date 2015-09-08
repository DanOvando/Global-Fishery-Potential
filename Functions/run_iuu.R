run_iuu <- function(runfolder,CPUs, IUULevel = 1.25, iterations)
{
  load(paste('Results/',runfolder,'/Data/Global Fishery Recovery Results.rdata', sep = ''))
  
#   load(paste('Results/',runfolder,'/Data/Global Fishery Recovery Complete Results.rdata', sep = ''))
  
  funcs <- as.vector(lsf.str())
  
  rm(list = funcs)
  
  sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
  
  NumCPUs <- CPUs
  
  FigureFolder <- paste('Results/',runfolder,'/Diagnostics/Regional Jackknife/',sep='')
  
  
  iuu_test <- run_iuu_diagnostic(Data = MsyData,Regressions = RealModels,IUULevel=IUULevel,
                                 NumCatchMSYIterations = iterations,BatchFolder = BatchFolder,
                                 SubSample = 0, RealModelSdevs = RealModelSdevs,NeiModelSdevs = NeiModelSdevs,
                                 NumCPUs = NumCPUs,FigureFolder = FigureFolder)
  
  
  return(iuu_test)
}