assign_phi <- function(data, taxdata, phidata, default_phi = 0.188, min_phi = .1)
{
  
  phidata$order <- tolower(phidata$order)
  
  data$phi <- NA
  
  for (p in 1:dim(phidata)[1])
  {
    phidata$phi[p] <- pmax(min_phi,find_phi(target_msy_ratio = phidata$msy_ratio[p]))
  }
  
  data$paste_name <- paste(data$SciName,data$CommName, sep = '-')
  
  taxdata$paste_name <- paste(taxdata$SciName,taxdata$CommName, sep = '-')
  
  taxdata <- taxdata[,c('paste_name','order')]
  
  data <- left_join(data, taxdata, by = 'paste_name')
  
  data$order <- tolower(data$order)
  
  data <- dplyr::select(data, -phi, -paste_name)
  
  phidata <- dplyr::select(phidata,order,phi)
  
  data <- left_join(data,phidata, by = 'order')
  
  data$phi[is.na(data$phi)] <- default_phi
  
  return(data)
  
}