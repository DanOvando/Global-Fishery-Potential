sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)


diagnostics = run_gfr_diagnostics(runfolder = '5.1 global demand spcat phi', NumCPUs = 30, do_iuu = F, do_ind_jack = F, do_reg_jack = F, do_cmsy_montecarlo = F, do_expanded_montecarlo = T, mciterations = 50,elastic_demand = T, sp_group_demand = F)