# Function to project stocks forward, 
# adjusting prices and open access to reflect supply, post BAU policy construction
elastic_projection <- function(poldata,oa_ids,elasticity = -.7, discount = 0.05, 
                               base_year = 2013,sp_group_demand = F, beta = 1.3,bvec = seq(0.00000001,2.5,length.out=30))
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
  
  show(elasticity)
  if (sp_group_demand == T)
  {
    supply <- poldata %>%
      ungroup() %>%
      filter(Year == years[1]) %>%
      group_by(SpeciesCatName) %>%
      summarise(global_catch = sum(Catch, na.rm = T))
    
    poldata <- poldata %>%
      dplyr::select(-global_catch) %>%
      join(supply, by = 'SpeciesCatName')
  }
  
  if (sp_group_demand == F)
  {
    supply <- poldata %>%
      ungroup() %>%
      subset(Year == years[1]) %>%
      summarise(global_catch = sum(Catch, na.rm = T))
    
    poldata <- poldata %>%
      dplyr::select(-global_catch)
    poldata$global_catch <- supply$global_catch
  }
  
  
  poldata$Price[poldata$Year == base_year] <- (poldata$pricek * poldata$global_catch^(1/elasticity))[poldata$Year == base_year] #adjust prices
  
  poldata$Profits[poldata$Year == base_year] <- ((poldata$Price * poldata$MSY * poldata$FvFmsy * poldata$BvBmsy) 
                                                 - poldata$MarginalCost * (poldata$FvFmsy * poldata$g)^beta )[poldata$Year == base_year] #adjust profits
  
  oa <- subset(poldata, IdOrig %in% oa_ids)
  
  # Seperate out NEIs -------------------------------------------------------
  
  nei <- subset(poldata, IdLevel == 'Neis')
  
  nei_ids <- unique(nei$IdOrig)
  
  nei_type_table<-unique(nei[c("CommName", "SciName","SpeciesCatName")]) # find unique combinations of nei stocks  
  
  nei_type_table$TaxonLevel<-NA
  
  nei_type_table$TaxonLevel[grepl("spp",nei_type_table$SciName)==T]<-"Genus"  
  
  nei_type_table$TaxonLevel[grepl("spp",nei_type_table$SciName)==F]<-"Non-Genus"
  
  nei_types <-unique(nei_type_table$SciName)
  
  species_types <- read.csv("Data/ASFIS_Feb2014.csv",stringsAsFactors=F) # list of ASFIS scientific names and corressponding ISSCAAP codes
  
  make_nei_lookup <- function(n,nei_type_table, species_types){
    NeiCat <- nei_type_table$SciName[n]
    
    tax_level <- nei_type_table$TaxonLevel[n]
    
    if(tax_level =="Genus") # find scientific names for all genus level nei stocks
    {
      
      Genus<-unlist(str_split(NeiCat,pattern=" "))[1] # pull out genus
      
      WhereComp<-grepl(Genus,species_types$Species_AFSIS) # search for species names in that genus
      
      compstocks<-unique(species_types$Species_AFSIS[WhereComp]) # pull out species names
      
    } 
    
    if (tax_level =="Non-Genus" & (NeiCat %in% species_types$Family)) # determine if non-genus stock is a family name
    {  
      family<-NeiCat # if so find species within that family
      
      WhereComp<-species_types$Family==family
      
      compstocks<-unique(species_types$Species_AFSIS[WhereComp])
    }
    
    if(tax_level == "Non-Genus" & (tolower(NeiCat) %in% tolower(species_types$Order))) # determine if non-genus stock is an order name
    {  
      
      order<-toupper(NeiCat) # order of nei stock (translate to uppercase to match sheet)
      
      WhereComp<-species_types$Order==order
      
      compstocks<-unique(species_types$Species_AFSIS[WhereComp])
    }
    
    lookup_table <- data.frame(NeiCat,compstocks, stringsAsFactors = F)
    
    return(lookup_table)
    
  } #close make nei function
  
  nei_lookup_table <- lapply(1:length(nei_types),make_nei_lookup,nei_type_table = nei_type_table,species_types = species_types) %>% ldply()
  
  # Back to open access -------------------------------------------------------
  
  oa <- subset(oa, IdLevel != 'Neis')
  
  oa_ids <- unique(oa$IdOrig)
  
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
    
    current_non_neis <- poldata[poldata$Year == years[y] & poldata$IdLevel != 'Neis',]
    
    current_neis <- poldata[poldata$Year == years[y] & poldata$IdLevel == 'Neis',]
    
    current_nei_types <- unique(current_neis$SciName)
    
    for (n in 1:length(current_nei_types)){
      
      nei_type <- current_nei_types[n]
      
      compstocks <- nei_lookup_table$compstocks[nei_lookup_table$NeiCat == nei_type]
      
      results<- current_non_neis %>%
        subset(SciName %in% compstocks) %>%
        summarize(BvBmsy25=quantile(BvBmsy,c(0.25),na.rm=T),FvFmsy75=quantile(FvFmsy,c(0.75),na.rm=T),
                  MedianG=median(g,na.rm=T),MedianK=median(k,na.rm=T),MedianPrice=median(Price,na.rm=T)
                  ,MedianCost=median(MarginalCost,na.rm=T)
                  ,JStocks=length(unique(IdOrig)),VarBvBmsy=var(BvBmsy,na.rm=T),VarFvFmsy=var(FvFmsy,na.rm=T))
      
      where_nei_type <- current_neis$SciName == nei_type
      
      current_neis$BvBmsy[where_nei_type] <- results$BvBmsy25
      
      current_neis$FvFmsy[where_nei_type] <- results$FvFmsy75
      
      current_neis$Catch[where_nei_type] <- (current_neis$MSY * current_neis$BvBmsy * current_neis$FvFmsy)[where_nei_type]
      
    } #close nei type loop
    
    poldata[poldata$Year == years[y] & poldata$IdLevel == 'Neis',] <- current_neis #Put open access stocks in the given year back in the general population
    
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
    ungroup() %>%
    group_by(Year) %>%
    mutate(DiscProfits = Profits * (1 + discount)^-(Year-(base_year-1)))
  
  poldata <- poldata %>%
    dplyr::select(-alpha,-pricek,-global_catch)
  
  return(poldata)  
}




