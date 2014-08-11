fb_ids <-
function (Genus = NA, Species = NA, server = "http://www.fishbase.us/", 
          ...) 
{
  require("XML")
  require("stringr")
  require("RCurl")
  #require("tm")
  if (is.na(Genus) | is.na(Species)) {
    warning("Genus and Specie are required!")
  }
  if (!is.na(Genus) & !is.na(Species)) {
    url <- paste(server, "Summary/", Genus, "-", Species, 
                 ".html", sep = "")
    g <- htmlParse(url)
    g2 <- toString.XMLNode(g)
    g3 <- toString(str_replace_all(str_replace_all(str_replace_all(g2, 
                                                                   "\t", ""), "\n", ""), "\r", ""))
    g4 <- strsplit(g3, "StockCode=")[[1]][2]
    g5 <- strsplit(g4, "\"")[[1]][1]

    
    idFB <- NA
    StockCode <- NA
    
    doc2 <- str_split(g2, "SpeciesSummary.php?")[[1]]
    if (length(doc2) > 1) {
      doc3 <- str_split(doc2[2], "id=")[[1]][2]
      idFB <- as.numeric(str_split(doc3, "&amp")[[1]][1]) 
      g3 <- toString(str_replace_all(str_replace_all(str_replace_all(g2, 
                                                                     "\t", ""), "\n", ""), "\r", ""))
      g4 <- strsplit(g3, "StockCode=")[[1]][2]
      StockCode <- strsplit(g4, "\"")[[1]][1]
    }
    ids <- data.frame(Genus = Genus, Species = Species, idFB = as.numeric(idFB), 
                      StockCode = as.numeric(StockCode))
  }
  return(as.data.frame(ids))
}
