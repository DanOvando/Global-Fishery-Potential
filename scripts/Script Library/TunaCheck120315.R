tunas<-unique(ProjectionData$SciName[ProjectionData$Dbase=='RAM' & ProjectionData$SpeciesCatName=='Tunas, bonitos, billfishes'])

tunaraw<-RawData %>%
  filter(Dbase=='FAO' & SpeciesCatName=='Tunas, bonitos, billfishes' & SciName %in% tunas) %>%
  select(Dbase,Country,SciName,CommName,Year,RegionFAO,Catch) %>%
  mutate(Source='Raw FAO')

tunadf<-ProjectionData %>%
  filter(SpeciesCatName=='Tunas, bonitos, billfishes' & Year<=2012) %>%
  select(Dbase,Country,SciName,CommName,Year,RegionFAO,Catch) %>%
  mutate(Source='Upsides Data')

both<-unique(tunaraw$SciName[tunaraw$SciName %in% tunas])

tunatest<-rbind(tunaraw,tunadf) %>%
  filter(SciName %in% both)

scatterTheme <- theme_bw() + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=12, vjust=.15),
                                   plot.margin=unit(c(1,1,1,1), "lines"),
                                   legend.title = element_text(size=12),
                                   legend.text= element_text(size=12),
                                   plot.title = element_text(lineheight=.8, size=12),
                                   strip.text.x = element_text(size = 12))

tunatest %>%
  group_by(Source,SciName,Year) %>%
  summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
  ungroup () %>%
  ggplot(aes(x=Year,y=TotalCatch,color=Source)) +
  geom_line() +
  facet_wrap(~SciName,scales = 'free') +
  scatterTheme

ggsave(file='Tuna Comparison.pdf', width=10,height=10)

check<-c("Xiphias gladius","Thunnus thynnus","Thunnus obesus","Thunnus maccoyii", 'Thunnus alalunga')

## Check specific species

# Swordfish
swordfish<-tunatest %>% filter(SciName=="Xiphias gladius")

unique(swordfish[,c('Source',"CommName",'RegionFAO')])

# Bluefin
bluefin<-tunatest %>% filter(SciName=="Thunnus thynnus")

unique(bluefin[,c('Source',"CommName",'RegionFAO')])

test<-bluefin %>% filter(Source=='Upsides Data' & CommName %in% c('Atlantic bluefin tuna'))

bluefin %>%
  filter(Source=='Raw FAO' | (Source=='Upsides Data' & CommName!='Atlantic bluefin tuna')) %>%
  group_by(Source,SciName,Year) %>%
  summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
  ungroup () %>%
  ggplot(aes(x=Year,y=TotalCatch,color=Source)) +
  geom_line()

# Southern Bluefin
soblue<-tunatest %>% filter(SciName=="Thunnus maccoyii")

unique(soblue[,c('Source',"CommName",'RegionFAO')])

soblue %>%
  filter(Source=='Raw FAO' | CommName=='Southern bluefin tuna Southern Oceans') %>%
  group_by(Source,SciName,Year) %>%
  summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
  ungroup () %>%
  ggplot(aes(x=Year,y=TotalCatch,color=Source)) +
  geom_line()

# Bigeye tuna
bigeye<-tunatest %>% filter(SciName=="Thunnus obesus")

unique(bigeye[,c('Source',"CommName",'RegionFAO')])

test<-bigeye %>% filter(Source=='Upsides Data' & CommName %in% c('Bigeye tuna Eastern Pacific','Bigeye tuna'))

bigeye %>%
  filter(Source=='Raw FAO' | (Source=='Upsides Data' & CommName!='Bigeye tuna')) %>%
  group_by(Source,SciName,Year) %>%
  summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
  ungroup () %>%
  ggplot(aes(x=Year,y=TotalCatch,color=Source)) +
  geom_line()

# Albacore
albacore<-tunatest %>% filter(SciName=="Thunnus alalunga")

unique(albacore[,c('Source',"CommName",'RegionFAO')])

albacore %>%
  filter(Source=='Raw FAO' | (Source=='Upsides Data')) %>%
  group_by(Source,SciName,Year) %>%
  summarize(TotalCatch=sum(Catch,na.rm=T)) %>%
  ungroup () %>%
  ggplot(aes(x=Year,y=TotalCatch,color=Source)) +
  geom_line()