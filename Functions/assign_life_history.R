#' assign_life_history
#' 
#' \code{assign_life_history} adds in and fills in missing life history information for PRM
#'
#' @param dat the data to be used in the regression etc
#' @param FishBase potential fishbase data
#' @param LifeHistoryVars 
#'
#' @return data frame with life history
#' @export
assign_life_history <- function(dat, FishBase = NA, LifeHistoryVars = c('MaxLength','AgeMat','VonBertK','Temp')){
 
  # Add in life history variables if missing --------------------------------
  
  missing <- LifeHistoryVars[!LifeHistoryVars %in% colnames(dat)] # find life history data needed but not present
  
  add_in <- as.data.frame(matrix(5, nrow = dim(dat)[1], ncol = length(missing)))
  
  colnames(add_in) <- missing
  
  out_dat <- cbind(dat, add_in) #tack on empty matrices for missing life history data
  
  # Fill in present but missing variables --------------------------------
  
   return(out_dat)
}


