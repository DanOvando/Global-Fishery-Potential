############################################------------------------------------------------------
##
## 
## Build New Status Quo Policies
## 
## "Business As Usual Optimistic"
##   - RAM non-catch share stocks get 'Fmsy'
##   - Catch share stocks get 'Optimal'
## Business As Usual Pessimistic
##
############################################



BuildPolicyBAUs<-function(ProjectionData,BaselineYear, elastic_demand = T, elasticity = -0.7, Discount = 0,sp_group_demand = F)
{
  
  ### 1) "Business As Usual Pessimistic" Where all non RAM and Catch share stocks go to Open Access
  
  # RAM - F current forever
  # Catch shares - Opt
  # All others - Open Access
  # 
  if (elastic_demand == T)
  {
    if (sp_group_demand == T)
    {
      base_supply <- filter(ProjectionData,Year == 2012 & Policy =='Historic') %>%
        group_by(SpeciesCatName) %>%
        summarise(global_catch = sum(Catch, na.rm = T))
      
      ProjectionData <- join(ProjectionData,base_supply, by = 'SpeciesCatName')
      
    }
    
    if (sp_group_demand == F)
    {
      base_supply <- filter(ProjectionData,Year == 2012 & Policy =='Historic') %>%
        summarise(global_catch = sum(Catch, na.rm = T))
      
      ProjectionData$global_catch <- base_supply$global_catch
      
    }
    
    ProjectionData <- ProjectionData %>%
      group_by(IdOrig) %>%
      mutate(alpha = global_catch / Price^elasticity, 
             pricek = (1 / alpha)^(1 / elasticity)) %>%
      ungroup()
  }
  ram<-ProjectionData[ProjectionData$Policy=='StatusQuoFForever' & ProjectionData$Dbase=='RAM' & ProjectionData$CatchShare!=1,]
  
  ramids<-unique(ram$IdOrig)
  
  cs<-ProjectionData[ProjectionData$Policy=='Opt' & ProjectionData$CatchShare==1,]
  
  csids<-unique(cs$IdOrig)
  
  otherids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)))]
  
  other<-ProjectionData[(ProjectionData$IdOrig %in% c(otherids)) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  BAUpess<-rbind(ram,cs,other)
  
  BAUpess$Policy<-'Business As Usual Pessimistic'
  if (elastic_demand == T){
    
    elastic_BAUpess <- elastic_projection(poldata = BAUpess,oa_ids = otherids, elasticity = elasticity,
                                          discount = Discount, base_year = BaselineYear+1, sp_group_demand = sp_group_demand )  
  }
  #   nonelastic<- subset(BAUpess, (IdOrig %in% otherids))
  #   
  #   elastic<- subset(elastic_BAUpess, (IdOrig %in% otherids))
  #   
  #     browser()
  #   
  #     quartz()
  #     ggplot(nonelastic,aes(factor(Year),log(Price))) + geom_boxplot() + facet_wrap(~SpeciesCatName)
  #     
  #   elastic_summary <- elastic %>%
  #     group_by(Year) %>%
  #     summarize(TotalCatch = sum(Catch,na.rm = T), MeanPrice = mean(Price, na.rm = T))
  #   
  #   quartz()
  #   ggplot(elastic_summary,aes(TotalCatch,(MeanPrice))) + geom_point()
  #   
  
  ### 2) "Business As Usual Current Management"
  
  # RAM - Fmsy
  # Catch shares - Opt
  # Overfished and Overfishing - Open Access
  # Myctophids - StatusQuoBForever
  
  overFFids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                                          ((ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy<1) | (ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy>1) |
                                                                             (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy<1)))]
  
  overff<-ProjectionData[(ProjectionData$IdOrig %in% overFFids) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  mctofids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                                         (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy>1))]
  
  mctofid<-ProjectionData[(ProjectionData$IdOrig %in% mctofids) & ProjectionData$Policy=='StatusQuoBForever',]
  
  BAUoptim<-rbind(ram,cs,overff,mctofid)
  
  BAUoptim$Policy<-'Business As Usual'
  if (elastic_demand == T){
    
    elastic_BAUoptim <- elastic_projection(poldata = BAUoptim,oa_ids = overFFids, elasticity = elasticity, discount = Discount, base_year = BaselineYear+1,sp_group_demand = sp_group_demand)  
  }
  ### 3 & 4) "Catch Share Three" and "Fmsy Three" - Adjust results for CS and Fmsy policies so that the policy is not applied to underfished/underfishing stocks
  
  PolicyOverFFids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (ProjectionData$BvBmsy<1 | ProjectionData$FvFmsy>1)] # overfished/overfishing stocks
  
  PolicyUnderFFids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (ProjectionData$BvBmsy>1 & ProjectionData$FvFmsy<1)] # underfished/underfishing stocks
  
  PolicyMcTofids<-ProjectionData[(ProjectionData$IdOrig %in% PolicyUnderFFids) & ProjectionData$Policy=='StatusQuoBForever',] # mctofid subset set to B current forever
  
  # 3) Catch Share 
  
  CsThree<-ProjectionData[(ProjectionData$IdOrig %in% PolicyOverFFids) & ProjectionData$Policy=='CatchShare',]
  
  McRam<- subset(ProjectionData, (IdOrig %in% PolicyUnderFFids) & Policy=='StatusQuoFForever' & Dbase=='RAM' & CatchShare!=1)
  
  McCatchShares<-subset(ProjectionData, (IdOrig %in% PolicyUnderFFids) & Policy=='Opt' & CatchShare==1)
  
  Myctophids<- subset(ProjectionData, (IdOrig %in% PolicyUnderFFids) & Policy=='StatusQuoBForever' & CatchShare!=1 & Dbase!='RAM')
  
  CatchShareThree<-rbind(CsThree,McRam,McCatchShares,Myctophids) # combine Catch Share results for overfished/overfishing results with B current forever results for mctofids
  
  CatchShareThree$Policy<-'Catch Share Three'
  if (elastic_demand == T){
    
    elastic_CatchShareThree <- elastic_projection(poldata = CatchShareThree, oa_ids = 'none', elasticity = elasticity, discount = Discount, base_year = BaselineYear + 1,sp_group_demand = sp_group_demand)  
    
  }
  
  
  # 4) Fmsy 
  
  fThree<-ProjectionData[(ProjectionData$IdOrig %in% PolicyOverFFids) & ProjectionData$Policy=='Fmsy',]
  
  FmsyThree<-rbind(fThree,McRam,McCatchShares,Myctophids) # combine Catch Share results for overfished/overfishing results with B current forever results for mctofids
  
  FmsyThree$Policy<-'Fmsy Three'
  
  if (elastic_demand == T){
    
    elastic_FmsyThree <- elastic_projection(poldata = FmsyThree, oa_ids = 'none', elasticity = elasticity, discount = Discount, base_year = BaselineYear + 1, sp_group_demand = sp_group_demand )  
  }
  
  # Modify Catch Share Policy with Elastic Demand ---------------------------
  
  if (elastic_demand == T){
    
    f_for_elastic <-subset(ProjectionData,Policy=='Fmsy')
    
    f_for_elastic$Policy<-'Fmsy'
    
    elastic_Fmsy <- elastic_projection(poldata = f_for_elastic, oa_ids = 'none', elasticity = elasticity, discount = Discount, base_year = BaselineYear + 1, sp_group_demand = sp_group_demand)  
    
    catchshare_for_elastic <-subset(ProjectionData,Policy=='CatchShare')
    
    catchshare_for_elastic$Policy<-'CatchShare'
    
    elastic_catchshare <- elastic_projection(poldata = catchshare_for_elastic, oa_ids = 'none', elasticity = elasticity, discount = Discount, base_year = BaselineYear + 1, sp_group_demand = sp_group_demand)  
    
    
    #     darg <- ProjectionData %>%
    #       group_by(Policy,Year) %>%
    #       summarize(tc = sum(Catch,na.rm = T), tp = sum(Profits, na.rm = T))
    #     
    # 
    # damn <- catchshare_for_elastic %>%
    #   group_by(SpeciesCatName,Year) %>%
    #   summarize(TotalCatch = sum(Catch,na.rm = T), TotalProfits = sum(Profits, na.rm = T)) %>%
    #   ungroup() %>%
    #   group_by(SpeciesCatName) %>%
    #   summarize(CatchChange = 100*(TotalCatch[Year == 2050]/TotalCatch[Year == 2013] -1),
    #             ProfitChange = 100*(TotalProfits[Year == 2050]/TotalProfits[Year == 2013] -1)) %>%
    #   arrange(ProfitChange)
    # 
    # quartz()
    # ggplot(damn,aes(Year,TotalProfits, fill = SpeciesCatName)) + geom_bar(stat = 'identity')
    # 
    #     
    #     #     browser()
    # #     
    # #     
    #     huh <- data.frame(elastic_Fmsy$IdOrig,elastic_Fmsy$Year,elastic_Fmsy$Price,elastic_Fmsy$Catch,
    #                       f_for_elastic$Price,elastic_Fmsy$SpeciesCatName)
    #     
    #     colnames(huh) <- c('IdOrig','Year','elastic_price','Catch','inelastic_price','species_cat')
    #     
    #     huh <- huh %>%
    #       group_by(IdOrig) %>%
    #       mutate(CatchChange = 100*(Catch/Catch[1]))
    #     
    #     quartz()
    #     wtf <- ggplot(huh,aes(inelastic_price,elastic_price,color = species_cat)) + geom_point()
    #     ggsave('price change for Fmsy.pdf', plot = wtf)
    #     
    #     quartz()
    #     ggplot(huh,aes(Year,log(Catch),group = IdOrig,color = log(CatchChange))) + geom_point()
    #     
    #     
    #     huh <- subset(elastic_catchshare, Year == 2050) %>%
    #       group_by(Year,Policy) %>%
    #       summarize(NumStocks = length(unique(IdOrig)),TotalCatch = sum(Catch, na.rm = T), TotalProfits = sum(Profits, na.rm = T))
    
    ProjectionData <- filter(ProjectionData, Policy != 'CatchShare' & Policy != 'Fmsy')
    
  }
  if (elastic_demand == T)
  {
    ProjectionData <- ProjectionData %>%
      dplyr::select(-alpha,-pricek,-global_catch)
  }
  ### Bind all composite policies to ProjectionData
  if (elastic_demand == F){
    
    ProjectionData<-rbind(ProjectionData,BAUpess,BAUoptim,CatchShareThree,FmsyThree)
  }
  if (elastic_demand == T){
    ProjectionData<-rbind(ProjectionData,elastic_BAUpess,elastic_BAUoptim,elastic_CatchShareThree,elastic_FmsyThree,elastic_Fmsy,elastic_catchshare)
    
  }
  
  
  return(ProjectionData)
}


