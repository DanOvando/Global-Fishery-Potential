apply_prm <- function(dat,reg,CatchLags = 4, LifeHistoryVars = c('MaxLength','AgeMat','VonBertK','Temp','SpeciesCat','SpeciesCatName','b_to_k_ratio'),
                      IdVar = 'IdOrig',   CatchVariables =  c('YearsBack','ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'MaxCatch','TimeToMaxCatch','InitialScaledCatchSlope'
                                                              ,'MeanScaledCatch','CatchToRollingMax'),
                      min_catch_years = 10){
  
  dat$BvBmsy <- NA #housekeeping
  
  # Filter out things that don't have enough catch years
  not_enough_catch  <- dat %>%
    group_by(IdOrig) %>%
    summarize(catch_years = sum(is.na(Catch) == F)) %>%
    subset(catch_years < min_catch_years)
  
  dat <- dat %>%
    filter(!IdOrig %in% not_enough_catch$IdOrig)
  
  Fisheries <- unique(dat$IdOrig)
  
  # Format data for regression
  formatted <- lapply(1:length(Fisheries),FormatForRegression, Data = dat, Fisheries = Fisheries, DependentVariable = 'BvBmsy',
                      CatchVariables = CatchVariables, CatchLags = 4, LifeHistoryVars = LifeHistoryVars, 
                      IsLog = T, IdVar = 'IdOrig') %>%
    bind_rows()
  
  # Add in life history
  formatted <- assign_life_history(dat = formatted,LifeHistoryVars = LifeHistoryVars)
  
  # Change species category factors to match model
  reg_factors <- reg$xlevels$SpeciesCatName
  
  AllPossible = formatted %>%
    select(SpeciesCatName, SpeciesCat) %>%
    unique()
  
  adjusted_data = AssignNearestSpeciesCategory(Data = formatted, AvailableCategories = reg_factors, AllCategories = AllPossible)
  
  # Predict log B/Bmsy
  predicted = predict.lm(reg,adjusted_data$Data, se.fit = T)
  
  formatted$LogBvBmsy = predicted$fit
  
  formatted$BvBmsySD = predicted$se.fit
  
  return(formatted)
}