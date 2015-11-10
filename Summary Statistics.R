gfr_summary <- function(runname)
{
  runname <- '6.0 global demand common phi'
  
  load(file = paste('Results',runname,'Data','ProjectionData Data.rdata', sep = '/'))
#   
  load(file = paste('Results',runname,'Data','MsyData.Rdata', sep = '/'))

  load(file = paste('Results',runname,'Data','Global Fishery Recovery Results.Rdata', sep = '/'))
  
  id <- sample(unique(ProjectionData$IdOrig), size = 1)
  
  ggplot(subset(ProjectionData,IdOrig == id), aes(Year,Catch, fill = Policy)) + geom_point(shape = 21)
  
  
 proj.dat <- UnlumpedProjectionData %>%
   subset(Year == 2012) %>%
   group_by(Dbase) %>%
   summarize(nstocks = length(unique(IdOrig)), total.catch = sum(Catch, na.rm = T))%>%
 ungroup() %>%
   mutate(perc.catch = total.catch/sum(total.catch))
 
 id <- sample(unique(CatchMSYPossibleParams$IdOrig), size = 1)
 
 ggplot(subset(CatchMSYPossibleParams, IdOrig == id), aes(g,K)) + geom_hex() + 
   scale_fill_gradient(low = 'blue',high = 'green')
 
 pdf(file = 'cmsy summary.pdf')
 cmsy_summary <- (ggplot(subset(CatchMSYPossibleParams, IdOrig == id), aes(g,K, fill = FinalBvBmsy, size = MSY)) + 
   geom_point(shape = 21, alpha = 0.6) + 
     scale_size_continuous() + 
   scale_fill_gradient(low = 'blue',high = 'green', name = 'B/Bmsy') + 
     theme(text = element_text(size = 16,family = 'Helvetica')) + 
   geom_point(aes(x = exp(mean(log(g))), y = exp(mean(log(K)))), fill = 'red', shape = 24, size = 6))
 
 cmsy_summary <- ggExtra::ggMarginal(cmsy_summary,type = "histogram")
 (cmsy_summary)
 dev.off()
 
 kobeplot <- (ggplot(subset(ProjectionData, Year == 2012), aes(BvBmsy,FvFmsy)) + 
     stat_density2d(aes(fill = ..level..), geom= 'polygon', n = 200)+
   scale_fill_gradient2(guide = F,low = 'skyblue1',high = 'gold1',mid = 'white',midpoint = 0.2) + 
   geom_vline(aes(xintercept = 1), linetype = 'dashed') + 
   geom_hline(aes(yintercept = 1), linetype = 'dashed'))
 
 kobeplot + 
   geom_point( alpha = 0.3,aes(color = factor(Dbase)))+
   scale_color_discrete(guide = F) + 
   ylim(0,4)+ 
   xlab('B/Bmsy')+
 ylab('F/Fmsy') + 
   theme_classic() + 
   theme(text = element_text(size = 24))
   
   

   
   cmsy_hex_summary <- (ggplot(subset(CatchMSYPossibleParams, IdOrig == id), aes(g,K)) + 
                      geom_hex() + 
                      scale_size_continuous(guide = F) + 
                      scale_fill_gradient(low = 'blue',high = 'green') + 
                      geom_point(aes(x = exp(mean(log(g))), y = exp(mean(log(K)))), fill = 'red', shape = 24, size = 6))
  
   
   cmsy_hex_summary<-  ggExtra::ggMarginal(cmsy_hex_summary,type = "histogram")

   
   
price.and.costs <- ProjectionData %>%
  subset(Year == 2012) %>%
  mutate(revenue = Catch*Price, cost = MarginalCost * (FvFmsy * g)^1.3,cost.to.revenue = cost/revenue) %>%
  ungroup() %>%
  group_by(SpeciesCatName) %>%
  summarise(mean.price = mean(Price, na.rm = T), mean.cvr = mean(cost.to.revenue, na.rm = T))

price.and.costs$SpeciesCatName <- reorder(price.and.costs$SpeciesCatName,price.and.costs$mean.price)

ggplot(price.and.costs, aes(x = SpeciesCatName, y = mean.price, fill = mean.price)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_gradient(low = 'orangered',high  = 'green') + 
  coord_flip() + 
  ylab('Mean Price Per Ton') + 
  xlab('')+
  theme(legend.position = 'none', text = element_text(size = 18)) + 
  scale_y_continuous(labels = dollar) 

price.and.costs$SpeciesCatName <- reorder(price.and.costs$SpeciesCatName,price.and.costs$mean.cvr)

ggplot(price.and.costs, aes(x = SpeciesCatName, y = mean.cvr, fill = mean.cvr)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_gradient(low = 'green',high  = 'orangered') + 
  coord_flip() + 
  ylab('Cost to Revenue Ratio') + 
  xlab('')+
  theme(legend.position = 'none', text = element_text(size = 18))

ggplot(price.and.costs, aes(x = mean.price, y = mean.cvr, fill = SpeciesCatName)) + 
  geom_point(shape = 21)


id <- sample(unique(PolicyStorage$IdOrig), size = 1)

pols <- PolicyStorage %>%
  subset(IdOrig %in% id) %>%
  ggplot(aes(b,CatchShare)) + 
  geom_point(shape = 21, fill = 'lightseagreen', size = 4) + 
#   facet_wrap(~IdOrig, scales = 'fixed') + 
  ylab('F/Fmsy') + 
  xlab('B/Bmsy') + 
  theme_classic() + 
  theme(text = element_text(size = 24))
  

pols <- PolicyStorage %>%
  subset(IdOrig %in% id) %>%
  ggplot(aes(b,Fmsy)) + 
  geom_point(shape = 21, fill = 'lightseagreen') + 
  facet_wrap(~IdOrig, scales = 'fixed') + 
  ylab('F/Fmsy') + 
  xlab('B/Bmsy')
  
oaplot <- ProjectionData %>%
  subset(IdOrig %in% id & Policy == 'StatusQuoOpenAccess') %>%
  ggplot(aes(BvBmsy, FvFmsy, fill = Year)) + 
  geom_point(shape = 21, size = 4) + 
#   facet_wrap(~IdOrig) + 
  scale_fill_continuous(low = 'green',high = 'blue') + 
  theme_classic() + 
  theme(text = element_text(size = 24),
        legend.text = element_text(size = 14)) + 
  xlab('B/Bmsy') + ylab('F/Fmsy')


}

