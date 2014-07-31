.schaefer  <- function(theta) # Schaefer model
{
  with(as.list(theta), {  ## for all combinations of ri & ki
    bt=vector()
    ell = 0  ## initialize ell
    for (j in startbt)
    {
      if(ell == 0) 
      {
        bt[1]=j*k*exp(rnorm(1,0, sigR))  ## set biomass in first year
        for(i in 1:nyr) ## for all years in the time series
        {
          xt=rnorm(1,0, sigR)
          bt[i+1]=(bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
        }
        
        #Bernoulli likelihood, assign 0 or 1 to each combination of r and k
        ell = 0
        # show(paste('k is',k))
        # show(paste('lam1 is',lam1))
        # show(paste('lam2 is',lam2))
        
        if(bt[nyr+1]/k>=lam1 && bt[nyr+1]/k <=lam2 && min(bt) > 0 && max(bt) <=k && bt[which(yr==interyr)]/k>=interbio[1] && bt[which(yr==interyr)]/k<=interbio[2]) 
          ell = 1
      }	
    }
    return(list(ell=ell))
    
    
  })
}
