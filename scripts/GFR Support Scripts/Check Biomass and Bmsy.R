colnames(FullData)


FullData$test <- FullData$Biomass/FullData$Bmsy

FullData %>%
  filter(Year == 2012) %>%
  ggplot(aes(BvBmsy, test, fill = Dbase)) + 
  geom_point(shape = 21) + 
  geom_abline(slope = 1, intercept = 0)


proj_bio <- ProjectionData %>%
  filter(Year == 2012) %>%
  select(IdOrig, Dbase,BvBmsy, Biomass, Bmsy, MSY) %>%
  rename(proj_BvBmsy = BvBmsy, proj_Biomass = Biomass, proj_Bmsy = Bmsy, proj_MSY = MSY) %>%
  filter(Dbase == 'RAM')

orig_bio <- FullData %>%
  filter(Year == 2012) %>%
  select(IdOrig,Dbase, BvBmsy, Biomass, Bmsy, MSY, SpeciesCatName) %>%
  rename(orig_BvBmsy = BvBmsy, orig_Biomass = Biomass, orig_Bmsy = Bmsy, orig_MSY = MSY) %>%
filter(Dbase == 'RAM')


orig_ram <- RAM %>%
  filter(Year == 2012) %>%
  select(IdOrig, BvBmsy, Biomass, Bmsy) %>%
  rename(ram_BvBmsy = BvBmsy, ram_Biomass = Biomass, ram_Bmsy = Bmsy)

check = left_join(orig_bio, proj_bio, by = 'IdOrig') %>%
  left_join(orig_ram) %>%
  mutate(is_missing = orig_Biomass == 0 | is.na(orig_Biomass),
         had_orig = is.na(orig_Biomass) == F & is.na(orig_Bmsy) == F,
         is_forage = SpeciesCatName == 'Herrings, sardines, anchovies') %>%
  filter(Dbase.x == 'RAM')

check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(orig_BvBmsy, proj_BvBmsy, fill = is_missing)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)

check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(orig_Biomass, proj_Biomass, fill = is_missing)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)

check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(orig_Biomass, ram_Biomass, fill = is_missing)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)

check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(orig_Biomass, ram_Biomass, fill = is_missing)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)



check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(orig_Bmsy, proj_Bmsy, fill = SpeciesCatName)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)

a = check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(pmin(2.5,orig_BvBmsy), orig_Biomass/orig_Bmsy, fill = orig_Bmsy == proj_Bmsy)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)

b = check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(proj_BvBmsy, proj_Biomass/proj_Bmsy, fill = orig_Bmsy == proj_Bmsy)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)

grid.arrange(a,b)

 check %>%
  filter(Dbase.x == 'RAM') %>%
  ggplot(aes(proj_BvBmsy, proj_Biomass/proj_Bmsy, fill = is_forage)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)
 
RamData %>%
  filter(Year == 2012) %>%
  ggplot(aes(BvBmsy, Biomass/Bmsy)) + 
  geom_point(shape = 21, alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0)


