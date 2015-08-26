fuck <- lumped_schaefer %>%
  ungroup() %>%
  group_by(Year,Policy) %>%
  summarize(num_stocks = length(unique(IdOrig)), global_catch = sum(Catch, na.rm = T )) %>%
  subset(Policy == 'Historic' & Year == 2012)

fucker <- unlumped_schaefer %>%
  ungroup() %>%
  group_by(Year,Policy) %>%
  summarize(num_stocks = length(unique(IdOrig)), global_catch = sum(Catch, na.rm = T )) %>%
  subset(Policy == 'Historic' & Year == 2012)

missing <-  schaefer_pd$IdOrig[!(schaefer_pd$IdOrig %in% lumped_phi)]

sch_ids <- unique(sch_pd$IdOrig)

phi_ids 

Missing <- sch_ids[!(sch_ids %in% phi_ids) ]

fuck <- sch_pd[sch_pd$IdOrig %in% Missing,]

