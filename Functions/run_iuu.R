run_iuu <- function(runfolder,NumCPUs, IUULevel = 1.25, iterations)
{
load(paste('Results/',runfolder,'/Data/Global Fishery Recovery Results.rdata', sep = ''))
# NumCPUs<- 2
# IUULevel<- 1.25
  sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)


iuu_test <- run_iuu_diagnostic(Data = MsyData,Regressions = RealModels,IUULevel=IUULevel,
                             NumCatchMSYIterations = iterations,BatchFolder = BatchFolder,
                             SubSample = 0, RealModelSdevs = RealModelSdevs,NeiModelSdevs = NeiModelSdevs)


return(iuu_test)
}