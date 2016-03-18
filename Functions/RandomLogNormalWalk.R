RandomWalk<- function(Length,Iterations,Start,Error,AC)
{
    
  Walk<- matrix(NA,nrow=Iterations,ncol=Length)
  
  Walk[,1]<- rnorm(Iterations,Start,Error)
  
  for (i in 2:Length)
  {
   Walk[,i]<- Walk[,i-1]*AC+rnorm(Iterations,0,Error) 
  }
 
  return(Walk)
}

Walk<- RandomWalk(1000,1,0,1,1)

plot(t(Walk/mean(Walk)))