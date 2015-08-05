find_phi <- function(phi_guess,target_msy_ratio)
{
  
  func <- function(phi,target_msy_ratio)
  {
  ratio <- 1/((phi +1)^(1 / phi))
  
  obj <- (target_msy_ratio - ratio) ^2
  return(obj)
  }
  
  phi = optim(phi_guess,func,target_msy_ratio = target_msy_ratio, lower = -1, upper = 20, method = 'L-BFGS-B')$par
  
  return(phi)
  
}