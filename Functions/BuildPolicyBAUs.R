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



BuildPolicyBAUs<-function(ProjectionData,BaselineYear, elastic_demand = T, elasticity = -0.7, 
                          Discount = 0,sp_group_demand = F,omega = 0.1,beta = 1.3)
{
  ### 1) "Business As Usual Pessimistic" Where all non RAM and Catch share stocks go to Open Access
  
  # RAM - F current forever
  # Catch shares - Opt
  # All others - Open Access
  #
  commodity_names <- read.csv('Data/CommodityNames.csv', stringsAsFactors = F) %>%
    rename(SpeciesCatName = ISSCAAP)
  
  ProjectionData <- left_join(ProjectionData, commodity_names, by = "SpeciesCatName")
  
  ProjectionData$omega <- omega
  
  ProjectionData$beta <- beta
  
  if (elastic_demand == T)
  {
    if (sp_group_demand == T)
    {
      #       base_supply <- filter(ProjectionData,Year == 2012) %>%
      #         group_by(SpeciesCatName) %>%
      #         summarise(global_catch = sum(Catch, na.rm = T))
      
      base_supply <- filter(ProjectionData,Year == 2012 & Policy =='Historic') %>%
        ungroup() %>%
        group_by(CommodityName) %>%
        summarise(global_catch = sum(Catch, na.rm = T))
      
      ProjectionData <- left_join(ProjectionData,base_supply, by = 'CommodityName')
      
    }
    if (sp_group_demand == F)
    {
      #       base_supply <- filter(ProjectionData,Year == 2012) %>%
      #         summarise(global_catch = sum(Catch, na.rm = T))
      
      base_supply <- subset(ProjectionData,Year == 2012 & Policy =='Historic') %>%
        ungroup() %>%
        summarise(global_catch = sum(Catch, na.rm = T))
      
      ProjectionData$global_catch <- base_supply$global_catch
      
    }
    ProjectionData <- ProjectionData %>%
      ungroup() %>%
      group_by(IdOrig) %>%
      mutate(alpha = global_catch / Price^elasticity, 
             pricek = (1 / alpha)^(1 / elasticity)) %>%
      ungroup()
  }
  ram<-ProjectionData[ProjectionData$Policy=='StatusQuoFForever' & ProjectionData$Dbase=='RAM' & ProjectionData$CatchShare!=1,]
  
  ramids<-unique(ram$IdOrig)
  
  cs<-ProjectionData[ProjectionData$Policy=='Opt' & ProjectionData$CatchShare==1,]
  
  csids<-unique(cs$IdOrig)
  
  otherids<- unique(ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)))])
  
  other<-ProjectionData[(ProjectionData$IdOrig %in% c(otherids)) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  BAUpess<-rbind(ram,cs,other)
  
  if (elastic_demand == T){
    
    elastic_BAUpess <- elastic_projection(poldata = BAUpess,oa_ids = otherids, elasticity = elasticity,
                                          discount = Discount, base_year = BaselineYear+1, sp_group_demand = sp_group_demand,
                                          omega = BAUpess$omega,beta = BAUpess$beta )  
  
    elastic_BAUpess$Policy<-'Business As Usual Pessimistic'
    
    }
 
  BAUpess$Policy<-'Business As Usual Pessimistic'
  
  
  overFFids <- ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                                          ((ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy<1) | (ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy>1) |
                                                                             (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy<1)))]
  
  overff<-ProjectionData[(ProjectionData$IdOrig %in% overFFids) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  mctofids<- unique(ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy>1))])
  
  mctofid<-ProjectionData[(ProjectionData$IdOrig %in% mctofids) & ProjectionData$Policy=='StatusQuoBForever',]
  
  BAUoptim<-rbind(ram,cs,overff,mctofid)
  
  if (elastic_demand == T){
    
    elastic_BAUoptim <- elastic_projection(poldata = BAUoptim,oa_ids = overFFids, 
                                           elasticity = elasticity, discount = Discount, 
                                           base_year = BaselineYear+1,sp_group_demand = sp_group_demand,
                                           omega = BAUoptim$omega, beta = BAUoptim$beta)  
  
    elastic_BAUoptim$Policy<-'Business As Usual'
    
    }
  
  BAUoptim$Policy<-'Business As Usual'
  
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
    
    elastic_CatchShareThree <- elastic_projection(poldata = CatchShareThree, oa_ids = 'none',
                                                  elasticity = elasticity, discount = Discount, base_year = BaselineYear + 1,
                                                  sp_group_demand = sp_group_demand,omega = CatchShareThree$omega,
                                                  beta = CatchShareThree$beta)  
    
  }
  
  
  # 4) Fmsy 
  
  fThree<-ProjectionData[(ProjectionData$IdOrig %in% PolicyOverFFids) & ProjectionData$Policy=='Fmsy',]
  
  FmsyThree<-rbind(fThree,McRam,McCatchShares,Myctophids) # combine Catch Share results for overfished/overfishing results with B current forever results for mctofids
  
  FmsyThree$Policy<-'Fmsy Three'
  
  if (elastic_demand == T){
    
    elastic_FmsyThree <- elastic_projection(poldata = FmsyThree, oa_ids = 'none', 
                                            elasticity = elasticity, discount = Discount, 
                                            base_year = BaselineYear + 1, sp_group_demand = sp_group_demand,
                                            omega = FmsyThree$omega,beta = FmsyThree$beta)  
  }
  
  # Modify Catch Share Policy with Elastic Demand ---------------------------
  
  if (elastic_demand == T){
    
    f_for_elastic <-subset(ProjectionData,Policy=='Fmsy')
    
    f_for_elastic$Policy<-'Fmsy'
    
    elastic_Fmsy <- elastic_projection(poldata = f_for_elastic, oa_ids = 'none', 
                                       elasticity = elasticity, discount = Discount, 
                                       base_year = BaselineYear + 1, sp_group_demand = sp_group_demand,
                                       omega = f_for_elastic$omega,beta = f_for_elastic$beta)  
    
    catchshare_for_elastic <-subset(ProjectionData,Policy=='CatchShare')
    
    catchshare_for_elastic$Policy<-'CatchShare'
    
    elastic_catchshare <- elastic_projection(poldata = catchshare_for_elastic, oa_ids = 'none', 
                                             elasticity = elasticity, discount = Discount, 
                                             base_year = BaselineYear + 1, sp_group_demand = sp_group_demand,
                                             omega = catchshare_for_elastic$omega,beta = catchshare_for_elastic$beta)  
    
    ProjectionData <- subset(ProjectionData, Policy != 'CatchShare' & Policy != 'Fmsy')
    
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


