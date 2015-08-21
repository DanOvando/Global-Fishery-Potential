# Function to project stocks forward, 
# adjusting prices and open access to reflect supply, post BAU policy construction
elastic_projection <- function(poldata,oa_ids,elasticity = -.7, discount = 0.05, base_year = 2012,sp_group_demand = F)
{
  OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
  {
    #Function to adjust f in response to prior profits
    if (t==1)
    {
      f=f
    }
    if (t>1)
    {
      f<- pmin(4,pmax(f+omega*(pi/MsyProfits),.0001))
    }
    return(f)
  }
  
  # Set up base conditions -------
  years <- unique(poldata$Year)
  
  #   if (sp_group_demand == T)
  #   {
  #     base_supply <- filter(poldata,Year == years[1]) %>%
  #       group_by(SpeciesCatName) %>%
  #       summarise(global_catch = sum(Catch, na.rm = T))
  #     
  #     poldata <- join(poldata,base_supply, by = 'SpeciesCatName')
  #     
  #   }
  #   
  #   if (sp_group_demand == F)
  #   {
  #     base_supply <- filter(poldata,Year == years[1]) %>%
  #       summarise(global_catch = sum(Catch, na.rm = T))
  #     
  #     poldata$global_catch <- base_supply$global_catch
  #     
  #   }
  
  #   poldata <- poldata %>%
  #     group_by(IdOrig) %>%
  #     mutate(alpha = global_catch / Price^elasticity, 
  #            pricek = (1 / alpha)^(1 / elasticity)) %>%
  #     ungroup()
  
  oa <- subset(poldata, IdOrig %in% oa_ids)
  
  oa_msyprofits <- (oa$MSY * oa$Price - oa$MarginalCost * (oa$g)^beta)[oa$Year == years[1]]
  
  # loops!-------
  
  for (y in 2:length(years))
  {
    where_year <- oa$Year == years[y]
    
    last_oa_f <- oa$FvFmsy[oa$Year == years[y - 1]]
    
    last_oa_pi <- oa$Profits[oa$Year == years[y - 1]]
    
    last_oa_b <- oa$BvBmsy[oa$Year == years[y - 1]]
    
    oa_g <- oa$g[oa$Year == years[y - 1]]
    
    oa_phi <- oa$phi[oa$Year == years[y - 1]]
    
    current_oa_f <- OpenAccessFleet(f = last_oa_f,pi = last_oa_pi,t = y,omega = 0.1,MsyProfits = oa_msyprofits )
    
    current_oa_b <- pmax(min(bvec), last_oa_b + ((oa_phi+1)/oa_phi)*oa_g*last_oa_b*(1-last_oa_b^oa_phi/(oa_phi+1))
                         - oa_g*last_oa_b*last_oa_f)
    
    oa$BvBmsy[where_year] <- current_oa_b
    
    oa$Biomass[where_year] <- (oa$BvBmsy * oa$Bmsy)[where_year]
    
    oa$FvFmsy[where_year] <- current_oa_f
    
    oa$Catch[where_year] <- (oa$MSY * oa$FvFmsy * oa$BvBmsy)[where_year]
    
    if (sp_group_demand == T)
    {
      supply <- poldata %>%
        ungroup() %>%
        filter(Year == years[y]) %>%
        group_by(SpeciesCatName) %>%
        summarise(global_catch = sum(Catch, na.rm = T))
      
    }
    
    if (sp_group_demand == F)
    {
      supply <- poldata %>%
        ungroup() %>%
        subset(Year == years[y]) %>%
        summarise(global_catch = sum(Catch, na.rm = T))
    }
    
    poldata[poldata$IdOrig %in% oa_ids & poldata$Year == years[y],] <- oa[where_year,] #Put open access stocks in the given year back in the general population
    
    where_all_year <- poldata$Year == years[y]
    
    
    if (sp_group_demand == T)
    {
      poldata <- poldata %>%
        dplyr::select(-global_catch) %>%
        join(supply, by = 'SpeciesCatName')
      
    }
    
    if (sp_group_demand == F)
    {
      poldata <- poldata %>%
        dplyr::select(-global_catch)
      poldata$global_catch <- supply$global_catch
    }
    
    
    poldata$Price[where_all_year] <- (poldata$pricek * poldata$global_catch^(1/elasticity))[where_all_year] #adjust prices
    
    poldata$Profits[where_all_year] <- ((poldata$Price * poldata$MSY * poldata$FvFmsy * poldata$BvBmsy) 
                                        - poldata$MarginalCost * (poldata$FvFmsy * poldata$g)^beta )[where_all_year] #adjust profits
    
  }
  
  poldata <- poldata %>%
    group_by(Year) %>%
    mutate(DiscProfits = Profits * (1 + discount)^-(Year-base_year))
  
  poldata <- poldata %>%
    dplyr::select(-alpha,-pricek,-global_catch)
  
  return(poldata)  
}


