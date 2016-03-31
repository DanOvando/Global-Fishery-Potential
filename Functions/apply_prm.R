apply_prm <- function(dat,reg,CatchLags = 4, LifeHistoryVars = c('MaxLength','AgeMat','VonBertK','Temp'),
                      IdVar = 'IdOrig',   CatchVariables =  c('YearsBack','ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'MaxCatch','TimeToMaxCatch','InitialScaledCatchSlope'
                                                              ,'MeanScaledCatch','CatchToRollingMax'),
                      min_catch_years = 10){
  
  dat$BvBmsy <- NA
  
  dat <- dat %>% dplyr::select(IdOrig,SciName,CommName,SpeciesCat,SpeciesCatName,Year,Catch,BvBmsy)
  
  not_enough_catch  <- dat %>%
    group_by(IdOrig) %>%
    summarize(catch_years = sum(is.na(Catch) == F)) %>%
    subset(catch_years < min_catch_years)
  
  dat <- dat %>%
    filter(!IdOrig %in% not_enough_catch$IdOrig)
  
  Fisheries <- unique(dat$IdOrig)
  
  formatted <- lapply(1:length(Fisheries),FormatForRegression, Data = dat, Fisheries = Fisheries, DependentVariable = 'BvBmsy',
                      CatchVariables = CatchVariables, CatchLags = 4, LifeHistoryVars = LifeHistoryVars, 
                      IsLog = T, IdVar = 'IdOrig') %>%
    bind_rows()
  
  formatted <- assign_life_history(dat = formatted)
  
  reg_factors <- reg$xlevels$SpeciesCatName
  
  AllPossible = formatted %>%
    select(SpeciesCatName, SpeciesCat) %>%
    unique()
  
  adjusted_data = AssignNearestSpeciesCategory(Data = formatted, AvailableCategories = reg_factors, AllCategories = AllPossible)
  
  predicted = predict(reg,adjusted_data$Data)
  
  formatted$LogBvBmsy = predicted
  
  return(formatted)
}