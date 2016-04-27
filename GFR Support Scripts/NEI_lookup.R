###########################################################################
##
## Generate lookup table for NEI fisheries
##
## This code will find all species that fall within the classification of 
## a given NEI stock in the global upside model
##
###########################################################################

# library(dplyr)
# library(tidyr)

# This ASFIS file is the one containing the taxonomic codes for each species class
# asfis<-read.csv(file = '../Global Fishery Potential/Data/TaxonCodes.csv', stringsAsFactors = F)

NEI_lookup <- function(df = ProjectionData, asfis) {
  ### Extract NEI data and build lookup table of species to use for applying results ----
  
  # process asfis data to match columns
  asfis <- asfis %>%
    tbl_df() %>%
    rename(
      CommName   =  English_name,
      SciName    =  Scientific_name,
      SpeciesCat =  ISSCAAP,
      TaxCode    =  TAXOCODE
    ) %>%
    select(-X3A_CODE)
  
  # Extract nei info from main data
  neidata <- df %>%
    tbl_df() %>%
    filter(IdLevel == 'Neis' & Year == 2012) %>%
    select(IdOrig,
           CommName,
           SciName,
           SpeciesCatName,
           SpeciesCat,
           IdLevel,
           Catch)
  
  # find unique nei categories
  nei_types <-
    data.frame(CommName = unique(neidata$CommName),
               stringsAsFactors = F)
  
  # join nei types data with asfis data to get higher taxa info
  nei_types <- nei_types %>%
    tbl_df() %>%
    left_join(asfis, by = 'CommName')
  
  # subset asfis for just species level stocks
  sps <- asfis %>%
    filter(grepl('X', TaxCode) == F) %>%
    rename(TaxCodeNumbs = TaxCode)
  
  # Make new code for each nei stock that has X's stripped from end
  nei_types$TaxCodeNumbs <-
    gsub('*X', replacement = '', nei_types$TaxCode)
  
  # for each nei, find all species that fall within that code of that nei. This includes any stock with at least
  # the same numerical digits in the code
  
  # loop over nei types and find comp stocks
  neis <- unique(nei_types$CommName)
  
  # initialize lookup list for nei species lookups
  nei_lookup <- list()
  
  for (a in 1:length(neis)) {
    neicode <-
      nei_types$TaxCodeNumbs[nei_types$CommName == neis[a]] # pull out nei code
    
    out <-
      sps[grepl(neicode, sps$TaxCodeNumbs), ] # find all species with that initial code sequence
    
    if (nrow(out) == 0) {
      # fill with NAs if no comp stocks available
      out[1, ] <- NA
    }
    
    out$NeiCommName <- neis[a] # add in name of nei
    
    nei_lookup[[neis[a]]] <- out # store matches in list with each list named after the nei
  }
  
  return(nei_lookup) # return nei_lookup list
  
}


