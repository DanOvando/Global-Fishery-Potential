fb_maturity <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.org/')
	{
require("XML")
require("stringr")
require('RCurl')
	if (is.na(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server= server)
		idFB <- ids$idFB
		}
  if ((is.na(idFB)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB or Genus and Specie are required!")
    }
		url <- paste(server, "Reproduction/MaturityList.php?ID=", idFB, sep = "")
		Mat <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (length(Mat)==0)
			{
			Maturity <- data.frame(idFB = idFB, Genus = Genus, Species = Species, na0 = NA, lm = NA, 
                      length_min = NA, na1 = NA, length_max = NA, Age_min = NA, na2 = NA, 
                      Age_max = NA, tm = NA, Sex = NA, Country = NA, Locality = NA)
			#Maturity <- t(Maturity)
			}
  if (is.null(dim(Mat$dataTable)))
    {
    Maturity <- data.frame(idFB = idFB, Genus = Genus, Species = Species, na0 = NA, lm = NA, 
                         length_min = NA, na1 = NA, length_max = NA, Age_min = NA, na2 = NA, 
                         Age_max = NA, tm = NA, Sex = NA, Country = NA, Locality = NA)
    #Maturity <- t(Maturity)
    }
		if (!is.null(dim(Mat$dataTable)))
			{
			Maturity <- cbind(as.numeric(as.character(idFB)), as.character(Genus), as.character(Species), Mat$dataTable)
			names(Maturity) <- c('idFB', 'Genus', 'Species', 'na0', 'lm', 'length_min', 'na1', 'length_max', 'Age_min', 'na2', 'Age_max', 'tm', 'Sex', 'Country', 'Locality')
			}
  Maturity <- Maturity[, names(Maturity) %in% c('idFB', 'Genus', 'Species', 'lm', 'length_min', 'length_max',
                                                'Age_min', 'Age_max', 'tm', 'Sex', 'Country', 'Locality')]
	return(as.data.frame(Maturity))
	}
