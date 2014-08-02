######################################
# Forward Simulation of Policy Function
# Inputs: polcy function, duration of simulation, and other model parameters
# Outputs: variables over time (f, b, yield (y), profit (pi)--------------------------------------------------
######################################

Sim_Forward= function(fpolicy,bvec,b0,T,p,MSY,c,r,beta,delta)
{  
  b = matrix(0,T,1)
  f = b
  pi = b
  y = b
  
  b[1] = b0;
  for (t in 1:T)
  {
    f[t] = approx(bvec,fpolicy,b[t])$y
    pi[t] = p*MSY*f[t]*b[t] - c*(f[t]*r/2)^beta
    y[t] = MSY*f[t]*b[t]
    if (t<T)
    {b[t+1] = b[t] + r*b[t]*(1-b[t]/2) - r/2*b[t]*f[t]}
  }
    
  Projection<- data.frame(f,b,y,pi)
  
  colnames(Projection)<- c('FvFmsy','BvBmsy','Yields','Profits')
  
  return(Projection)
}