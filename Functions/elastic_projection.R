# Function to project stocks forward,
# adjusting prices and open access to reflect supply, post BAU policy construction
elastic_projection <- function(poldata,oa_ids,elasticity = -.7, discount = 0.05,
                               base_year = 2013,sp_group_demand = F, beta = 1.3,bvec = seq(0.00000001,2.5,length.out=30),omega = 0.1)
{
  show(elasticity)
  OpenAccessFleet<- function(f,pi,t,omega,MsyProfits)
  {
    #Function to adjust f in response to prior profits
#     if (t==1)
#     {
#       f=f
#     }
#     if (t>1)
#     {
      f<- pmin(4,pmax(f+omega*(pi/MsyProfits),.0001))
    # }
    return(f)
  }
  poldata$beta <- beta
  
  poldata$omega <- omega
  # Set up base conditions -------
  years <- unique(poldata$Year)
  if (sp_group_demand == T)
  {
    supply <- poldata %>%
      ungroup() %>%
      filter(Year == min(Year, na.rm = T)) %>%
#       filter(Year == years[1]) %>%
      group_by(CommodityName) %>%
      summarise(global_catch = sum(Catch, na.rm = T))

    poldata <- poldata %>%
      dplyr::select(-global_catch) %>%
      join(supply, by = 'CommodityName')
  }

  if (sp_group_demand == F)
  {
    supply <- poldata %>%
      ungroup() %>%
      subset(Year == min(Year, na.rm = T)) %>%
      summarise(global_catch = sum(Catch, na.rm = T))

    poldata <- poldata %>%
      ungroup() %>%
      dplyr::select(-global_catch)
    
    poldata$global_catch <- supply$global_catch
  }

  poldata$Price[poldata$Year == base_year] <- (poldata$pricek * poldata$global_catch^(1/elasticity))[poldata$Year == base_year] #adjust prices

  poldata$Profits[poldata$Year == base_year] <- ((poldata$Price * poldata$MSY * poldata$FvFmsy * poldata$BvBmsy)
                                                 - poldata$MarginalCost * (poldata$FvFmsy * poldata$g)^poldata$beta )[poldata$Year == base_year] #adjust profits

  oa <- subset(poldata, IdOrig %in% oa_ids)

  # Seperate out NEIs -------------------------------------------------------

  nei <- subset(poldata, IdLevel == 'Neis')

  if (dim(nei)[1] > 0)
  {

    nei_ids <- unique(nei$IdOrig)

    nei_type_table<-unique(nei[c("CommName", "SciName","SpeciesCatName")]) # find unique combinations of nei stocks

    nei_type_table$TaxonLevel<-NA

    nei_type_table$TaxonLevel[grepl("spp",nei_type_table$SciName)==T]<-"Genus"

    nei_type_table$TaxonLevel[grepl("spp",nei_type_table$SciName)==F]<-"Non-Genus"

    nei_types <-unique(nei_type_table$SciName)

    species_types <- read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes

    nei_lookup_table <- lapply(1:length(nei_types),make_nei_lookup,nei_type_table = nei_type_table,species_types = species_types) %>% ldply()
  }
  # Back to open access -------------------------------------------------------

  oa <- subset(oa, IdLevel != 'Neis')

  oa_ids <- unique(oa$IdOrig)

  oa_msyprofits <- (oa$MSY * oa$Price - oa$MarginalCost * (oa$g)^oa$beta)[oa$Year == years[1]]

  # loops!-------
  for (y in 2:length(years))
  {
    where_year <- oa$Year == years[y]

    last_oa_f <- oa$FvFmsy[oa$Year == years[y - 1]]

    last_oa_pi <- oa$Profits[oa$Year == years[y - 1]]

    last_oa_b <- oa$BvBmsy[oa$Year == years[y - 1]]

    oa_g <- oa$g[oa$Year == years[y - 1]]

    oa_phi <- oa$phi[oa$Year == years[y - 1]]

    oa_omega <- oa$omega[oa$Year == years[y - 1]]
    
    current_oa_f <- OpenAccessFleet(f = last_oa_f,pi = last_oa_pi,t = y,omega = oa_omega,MsyProfits = oa_msyprofits )

    current_oa_b <- pmax(min(bvec), last_oa_b + ((oa_phi+1)/oa_phi)*oa_g*last_oa_b*(1-(last_oa_b^oa_phi)/(oa_phi+1))
                         - oa_g*last_oa_b*last_oa_f)

    oa$BvBmsy[where_year] <- current_oa_b

    oa$Biomass[where_year] <- (oa$BvBmsy * oa$Bmsy)[where_year]

    oa$FvFmsy[where_year] <- current_oa_f

    oa$Catch[where_year] <- (oa$MSY * oa$FvFmsy * oa$BvBmsy)[where_year]
    
    poldata[poldata$IdOrig %in% oa_ids & poldata$Year == years[y],] <- oa[where_year,] #Put open access stocks in the given year back in the general population
    
    current_non_neis <- poldata[poldata$Year == years[y] & poldata$IdLevel != 'Neis',]

    current_neis <- poldata[poldata$Year == years[y] & poldata$IdLevel == 'Neis',]
    
    if (dim(current_neis)[1]>0){
     
       current_nei_types <- unique(current_neis$SciName)

      for (n in 1:length(current_nei_types)){

        nei_type <- current_nei_types[n]

        compstocks <- nei_lookup_table$compstocks[nei_lookup_table$NeiCat == nei_type]
        
        comp_current_non_neis <- subset(current_non_neis,SciName %in% compstocks)
        
        results <- NULL
        
        results$BvBmsy25 <- quantile(comp_current_non_neis$BvBmsy,c(0.25),na.rm=T)
        
        results$FvFmsy75 =quantile(comp_current_non_neis$FvFmsy,c(0.75),na.rm=T)
        
        results$MedianG=median(comp_current_non_neis$g,na.rm=T)
        
        results$MedianK=median(comp_current_non_neis$k,na.rm=T)
        
        results$MedianPrice=median(comp_current_non_neis$Price,na.rm=T)
        
        results$MedianCost=median(comp_current_non_neis$MarginalCost,na.rm=T)
        
        results$JStocks=length(unique(comp_current_non_neis$IdOrig))
        
        where_nei_type <- current_neis$SciName == nei_type

        current_neis$BvBmsy[where_nei_type] <- results$BvBmsy25

        current_neis$FvFmsy[where_nei_type] <- results$FvFmsy75

        current_neis$Catch[where_nei_type] <- (current_neis$MSY * current_neis$BvBmsy * current_neis$FvFmsy)[where_nei_type]

#         current_neis$MarginalCost[where_nei_type] <- results$MedianCost
        
      } #close nei type loop

      poldata[poldata$Year == years[y] & poldata$IdLevel == 'Neis',] <- current_neis #Put open access stocks in the given year back in the general population
    } #close if nei statement
    
    
    if (sp_group_demand == T)
    {
      supply <- poldata %>%
        ungroup() %>%
        filter(Year == years[y]) %>%
        group_by(CommodityName) %>%
        summarise(global_catch = sum(Catch, na.rm = T))
      
    }
    
    if (sp_group_demand == F)
    {
      supply <- poldata %>%
        ungroup() %>%
        subset(Year == years[y]) %>%
        summarise(global_catch = sum(Catch, na.rm = T))
    }
    
    where_all_year <- poldata$Year == years[y]

    if (sp_group_demand == T)
    {
      poldata <- poldata %>%
        dplyr::select(-global_catch) %>%
        join(supply, by = 'CommodityName')

    }

    if (sp_group_demand == F)
    {
      poldata <- poldata %>%
        dplyr::select(-global_catch)
      poldata$global_catch <- supply$global_catch
    }


    poldata$Price[where_all_year] <- (poldata$pricek * poldata$global_catch^(1/elasticity))[where_all_year] #adjust prices

    poldata$Profits[where_all_year] <- ((poldata$Price * poldata$MSY * poldata$FvFmsy * poldata$BvBmsy)
                                        - poldata$MarginalCost * (poldata$FvFmsy * poldata$g)^poldata$beta )[where_all_year] #adjust profits

  }

  poldata <- poldata %>%
    ungroup() %>%
    group_by(Year) %>%
    mutate(DiscProfits = Profits * (1 + discount)^-(Year-(base_year-1)))

  poldata <- poldata %>%
    dplyr::select(-alpha,-pricek,-global_catch)

  return(poldata)
}




