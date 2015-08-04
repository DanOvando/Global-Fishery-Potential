# Function to project stocks forward, 
# adjusting prices and open access to reflect supply, post BAU policy construction
elastic_projection <- function(poldata,oa_ids,elasticity)
{
  
  # Set up base conditions -------
  years <- unique(poldata$Year)
  
  base_supply <- filter(poldata,Year == years[1]) %>%
    summarise(total_catch = sum(Catch, na.rm = T))
  
  poldata <- poldata %>%
    group_by(IdOrig) %>%
    mutate(alpha = base_supply$total_catch / Price^elasticity, 
           pricek = (1 / alpha)^(1/elasticity))
  
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
    
    supply <- filter(poldata,Year == years[y]) %>%
      summarise(total_catch = sum(Catch, na.rm = T))
    
    poldata[poldata$IdOrig %in% oa_ids & poldata$Year == years[y],] <- oa[where_year,] #Put open access stocks in the given year back in the general population
    
    where_all_year <- poldata$Year == years[y]
    
    pi <- (poldata$pricek * supply$total_catch^(1/elasticity))[where_all_year] #adjust prices
    
    poldata$Price[where_all_year] <- pi
    
    poldata$Profits[where_all_year] <- ((poldata$Price * poldata$MSY * poldata$FvFmsy * poldata$BvBmsy) 
                                        - poldata$MarginalCost * (poldata$FvFmsy * poldata$g)^beta )[where_all_year] #adjust profits
    
  }
  poldata <- poldata %>%
    select(-alpha,-pricek)
  
  return(poldata)  
}


