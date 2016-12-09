today <- subset(ProjectionData, Year == 2012) %>%
  group_by(Dbase) %>%
  summarise(numfish = length(unique(IdOrig)),
            total_catch = sum(Catch, na.rm = T),
            total_msy = sum(MSY, na.rm = T)) %>%
  mutate(perc_of_stocks = numfish/sum(numfish),
         perc_of_catch = total_catch/sum(total_catch),
         perc_of_msy = total_msy/sum(total_msy))

bytype <- subset(ProjectionData, Year == 2012) %>%
  group_by(IdLevel) %>%
  summarise(numfish = length(unique(IdOrig)),
            total_catch = sum(Catch, na.rm = T),
            total_msy = sum(MSY, na.rm = T)) %>%
  mutate(perc_of_stocks = numfish/sum(numfish),
         perc_of_catch = total_catch/sum(total_catch),
         perc_of_msy = total_msy/sum(total_msy))

today <- subset(ProjectionData, Year == 2012) %>%
  ungroup() %>%
  arrange(desc(Profits)) %>%
  mutate(cumuprofits = cumsum(Profits)) %>%
  dplyr::select(IdOrig,Dbase,CatchShare,Catch,Profits,cumuprofits)


wtf <- subset(ProjectionData, Year == 2012 & FvFmsy >100)

quartz()
(ggplot(today,aes(1:length(cumuprofits),cumuprofits,color = Dbase)) + geom_point(alpha = 0.2) + 
  geom_vline(aes(xintercept = .1*length(cumuprofits))))
