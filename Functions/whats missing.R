whats_missing <- function(a,b)
{
   c <- colnames(a)[!(colnames(a) %in% colnames(b))]
   
   return(c)
}