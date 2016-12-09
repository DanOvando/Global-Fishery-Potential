######################################
# Internal Optimization Function 
# To be used with RunDynamicOptimization2.R
# Gives (negative) value function value for value function iteration code--------------------------------------------------
######################################

GFRM_funR= function(f,b,p,MSY,c,r,beta,V,bvec,delta)
{  
profit= p*MSY*f*b - c*(f*r/2)^beta

bnext= b + r*b*(1-b/2) - r/2*b*f
out= approx(bvec,V,bnext) #spline(bvec,V,xout=bnext)
Vnext= out$y


negout= -(profit + delta*Vnext)
return(list(negout=negout))
}