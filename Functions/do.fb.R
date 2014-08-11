do.fb <-
function(fun, data, iStart=1, wait=10, server='http://www.fishbase.us/')
  {
  miles <- 0
  count <- 0
  Server <- server
  pb <- txtProgressBar(min = 0, max = length(data$Genus), style = 3)
  for (i in iStart:length(data$idFB))
    {
    miles <- miles + 1
    if ('idFB' %in% names(data))
      {   
      temp2 <<- fun(idFB = data$idFB[i], Genus = data$Genus[i], Species = data$Species[i], server=Server)
      }
    else 
      {
      temp2 <<- fun(Genus = data$Genus[i], Species = data$Species[i])#, server=Server)
      }
    #print(paste(i, length(data$Genus)-i, data$Genus[i], data$Species[i], temp2$URL[1]))
    if (i == 1) {ids <<- temp2} else  {ids <<- rbind(ids, temp2)}
    count <- count + 1
    miles <- miles + 1
  #  if (count == 5) 
  #    {
  #    count <- 0
  #    if (server == 'http://www.fishbase.org/') {server <- 'http://www.fishbase.us/'} else {server <- 'http://www.fishbase.org/'}
  #    }
    if (miles == 100) 
      {
      print(paste('Wait: ', wait, 'seconds'))
      Sys.sleep(time=wait)
      miles <- 0
      }
    setTxtProgressBar(pb, i)
   }

  return(ids)
  #write.csv(ids, file=paste(paste(fun), '.csv', sep=''))
  }
