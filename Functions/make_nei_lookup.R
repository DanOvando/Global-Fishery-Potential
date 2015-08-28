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
