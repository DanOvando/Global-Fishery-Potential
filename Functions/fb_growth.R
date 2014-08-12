fb_growth <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.org/')
{
  require("XML")
  require("stringr")
  require('RCurl')
  if (is.na(idFB)) 
  {
    ids <- fb_ids(Genus=Genus, Species=Species, server = server)
    idFB <- ids$idFB
  }
  if ((is.na(idFB)) & (is.na(Genus) | is.na(Species)))
  {
    warning("idFB or Genus and Specie are required!")
  }
 
    url <- paste(server, "PopDyn/PopGrowthList.php?ID=", idFB, sep = "")
    g <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
    if (length(g) == 0)
    {
      gu <- c(as.character(idFB), as.character(Genus), as.character(Species),t(rep(NA, 13)))
      names(gu) <-   c('idFB', 'Genus', 'Species', 'Linf', 'LengthType', 'k', 't0', 'sex', 
                       'M', 'Temp', 'Lm', 'phi', 'Country', 'Locality', 'Questionable', 
                       'Captive')
      gu <- t(gu)
    }
    if (!is.null(g$dataTable))
    {
      gu <- cbind(as.character(idFB), as.character(Genus), as.character(Species),g$dataTable[,-1])
      names(gu) <-  c('idFB', 'Genus', 'Species', 'Linf', 'LengthType', 'k', 't0', 'sex',
                      'M', 'Temp', 'Lm', 'phi', 'Country', 'Locality', 'Questionable', 
                      'Captive')
      gu$idFB <- as.character(gu$idFB)
      gu$Genus <- as.character(gu$Genus)
      gu$Species <- as.character(gu$Species)
      gu$Linf <- as.character(gu$Linf)
      gu$LengthType <- as.character(gu$LengthType)
      gu$k <- as.character(gu$k)
      gu$t0 <- as.character(gu$t0)
      gu$sex <- as.character(gu$sex)
      gu$M <- as.character(gu$M)
      gu$Temp <- as.character(gu$Temp)
      gu$Lm <- as.character(gu$Lm)
      gu$phi <- as.character(gu$phi)
      gu$Country <- as.character(gu$Country)
      gu$Locality <- as.character(gu$Locality)
      gu$Questionable <- as.character(gu$Questionable)
      gu$Captive <- as.character(gu$Captive)
    }
  
  return(data.frame(gu))
}
