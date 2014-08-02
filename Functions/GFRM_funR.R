######################################
# Internal Optimization Function 
# To be used with RunDynamicOptimization2.R
# Gives (negative) value function value for value function iteration code--------------------------------------------------
######################################

GFRM_funR= function(f,b,p,MSY,c,r,beta,V,bvec,delta)
{  
  profit= p*MSY*f*b - c*(f*r/2)^beta

bnext= max(min(bvec),b + r*b*(1-b/2) - r/2*b*f)
bnext=min(max(bvec),bnext)
out= approx(bvec,t(V),bnext) #spline(bvec,V,xout=bnext)
Vnext= out$y

negout= -(profit + delta*Vnext)
return(negout)
}