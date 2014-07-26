######################################
# Run Dynamic Optimization --------------------------------------------------
# Solves for optimal policy function f (as function of bvec) given model parameters.
# last input argument "tol" is convergence tolerance (use tol=.01)
######################################

RunDynamicOpt2= function(MSY,r,p,c,beta,disc,bvec,tol)
{
  
  
delta= 1/(1+disc) #Discount parameter
t=0

f1= matrix(1,length(bvec),1)
Vnew= matrix(0,length(bvec),1)
diff= 10*tol
    
    while (t<4 | diff>tol)
    {
      t= t+1
      V= Vnew
      oldf1= f1
      
      for (i in 1:length(bvec))
      {
        b= bvec[i]
        if(i==1)
          {guess= 1}
        else
        {guess= f1[i-1]}
        
        FishOut= optim(par=guess,fn=GFRM_funR,lower=0,upper=1.99,b=b,p=p,MSY=MSY,c=c,r=r,beta=beta,V=V,bvec=bvec,delta=delta,method="L-BFGS-B")

        Vnew[i]= -FishOut$value
        f1[i]= FishOut$par

      
      } #Close bvec loop
    
      diff= sum(abs(f1-oldf1))
      
    }# Close while loop
    

  return(list(f1=f1))
  
} #Close function