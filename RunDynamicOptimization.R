######################################
# Run Dynamic Optimization --------------------------------------------------
# This is the wrapper code for Costello's dynamic control rule optimization
######################################

RunDynamicOpt<- function(MSY,r,BaseP,BaseC,f0,beta,disc,bvec,TT,CSPrices,CSCost)
{
  
#   %f1=max npv
#   %f2=status quo f0 forever
#   %f3=catch share
#   %f4=yield
  
  delta<- 1/(1+disc) #Discount parameter
  
  for (s in 1:3)
  {
    Vnew<- matrix(0,length(bvec),1)
    
    if (s==1) #Standard scenario
    {
      p=BaseP
      
      c=BaseC
      
      Place=1
    }
    else if (s==2) #Catch share scenario
    {
      p=BaseP.*CSPrice
      
      c=BaseC.*CSCost
      
      Place=3
    }
    else if (s==3)
    {
      p<- 1
      
      c<- 0

    }
    
    for (t in seq(from=TT,to=1,by=-1))
    {
      
      V<- Vnew
      
      for (i in 1:length(bvec))
      {
        b<- bvec[i]
        
        [AA, BB] = fminbnd('GFRM_fun',0,1.99,[],b,p,MSY,c,r,beta,V,bvec,delta);
      
        Vnew[i]<- -BB
      
      } #Close bvec loop
      
    }# Close time loop
    
  } #Close s loop
  
  f2<- f0*rep(1,length(bvec))
  
  return(list(f1=f1,f2=f2,f3=f3,f4=f4))
} #Close function