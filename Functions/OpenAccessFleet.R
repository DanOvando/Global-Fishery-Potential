OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
{
  
  if (t==1)
  {
    f=f
  }
  if (t>1)
  {
    f<- f+omega*(pi/MsyProfits)
  }
  return(f)
}