sraMSY  <-function(theta, N) #CatchMSY guts
{
  # theta=parbound
  # N=n
  #This function conducts the stock reduction
  #analysis for N trials
  #args:
  #	theta - a list object containing:
  #		r (lower and upper bounds for r)
  #		k (lower and upper bounds for k)
  #		lambda (limits for current depletion)
  
  
  with(as.list(theta), 
{
  # c(0.05,0.5)
  # ri = exp(runif(N, log(.05), log(0.5)))  ## get N values between r[1] and r[2], assign to ri
  ri = exp(runif(N, log(r[1]), log(r[2])))  ## get N values between r[1] and r[2], assign to ri
  ki = exp(runif(N, log(k[1]), log(k[2])))  ## get N values between k[1] and k[2], assing to ki
  itheta=cbind(r=ri,k=ki, lam1=lambda[1],lam2=lambda[2], sigR=sigR) ## assign ri, ki, and final biomass range to itheta
  M = apply(itheta,1,.schaefer) ## call Schaefer function with parameters in itheta
  i=1:N
  ## prototype objective function
  get.ell=function(i) M[[i]]$ell
  ell = sapply(i, get.ell) 
  return(list(r=ri,k=ki, ell=ell))	
})
}
