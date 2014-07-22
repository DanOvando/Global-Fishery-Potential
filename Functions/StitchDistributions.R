StitchDistributions<- function(Data)
{
  
  
  
  Data<- rbind(FaoSpeciesDist,FaoMiscDist)
  
  jtemp<- apply(exp(Data),2,median,na.rm=T) #calculate median of each column
  
  jsort<- sort(jtemp) #Sort medians
  
  itemp<- apply(exp(jstore),1,mean,na.rm=T) #store results for individual fisheries (using MEAN FOR NOW)
  
  isort<- t(apply(exp(jstore),1,sort))
  
  top<- isort[,ceiling(0.975*dim(isort)[2])]
  
  bot<- isort[,ceiling(0.025*dim(isort)[2])]
  
  med<- mean(jsort,na.rm=T) #mean of medians
 
  top<- jsort[ceiling(0.975*length(jsort))] #upper CI
  
  bot<- jsort[ceiling(0.025*length(jsort))] #lower CI
  
  box<- boxplot(jsort,plot=F)
  
  c[y,]<- c(years[y],med,top,bot,box$stats[4],box$stats[2])
  
}