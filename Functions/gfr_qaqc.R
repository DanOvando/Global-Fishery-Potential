gfr_qaqc <- function(ProjectionData,FigureFolder)
{
  library(knitr)
  pdf(paste(FigureFolder,'check parameters.pdf', sep = ''))
  par(mfrow = c(2,2))
  hist(ProjectionData$MSY)
  abline(v=0)
  hist(ProjectionData$g)
  abline(v=0)
  hist(ProjectionData$Price)
  abline(v=0)
  hist(ProjectionData$MarginalCost)
  abline(v=0)
  dev.off()
  
  summary_table <- ProjectionData %>%
    ungroup() %>%
    summarize(min_msy = min(MSY,na.rm = T),max_msy = max(MSY, na.rm = T),
              min_g = min(g,na.rm = T), max_g = max(g, na.rm = T),
              max_price = max(Price, na.rm = T), min_price = min(Price, na.rm = T),
              max_cost = max(MarginalCost, na.rm = T), min_cost = min(MarginalCost, na.rm =T),
              max_profits = max(Profits, na.rm = T), min_profits = min(Profits, na.rm = T),
              max_catch = max(Catch, na.rm = T), min_catch = min(Catch, na.rm = T)) %>%
    t() %>%
    as.data.frame()
   
  colnames(summary_table) <- 'value'
  
  show(kable((summary_table)))
  
  ids <- unique(ProjectionData$IdOrig)
  
  pdf(paste(FigureFolder,'check trajectories.pdf', sep = ''))
  for (i in 1:length(ids))
  {
    
    print(ggplot(subset(ProjectionData, IdOrig == ids[i] & Policy %in% c('Business As Usual','Business As Usual Pessimistic'
                                                                         ,'Catch Share Three','CatchShare','Fmsy','Fmsy Three','Historic')), 
                 aes(Year,Catch, size = Profits, fill = Policy)) + 
            geom_point(shape = 21, alpha = 0.6) + 
            geom_vline(xintercept = 2012) + 
            ggtitle(ids[i]))
    
  }
  dev.off()
  
}
