sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)

fuuuuck <- run_gfr_diagnostics(runfolder = '5.2 global demand common phi', do_expanded_montecarlo = T, mciterations = 2)