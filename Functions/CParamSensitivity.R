####################################
##
## Function to examine c parameter sensitivity to BOA
##
####################################

# Data<-MsyData

CParamSensitivity<-function(Data,BaselineYear,beta)
{
  # unique species categories
  cats<-unique(Data$SpeciesCatName[Data$Year==BaselineYear])
  
  # loop over species categories
  for(a in 1:length(cats))
  {
#     show(a)
    
    # subset MSY data to species category
    temp<-Data[Data$SpeciesCatName==cats[a] & Data$Year==BaselineYear,c('IdOrig','SpeciesCatName','BvBmsyOpenAccess','MSY','r','Price')]
    
    # loop over BOA values from 0.1-1
    boa<-seq(from=0.1,to=1,by=0.1)
    
    for(b in 1:length(boa))
    {
    temp2<-temp
      
    temp2$c_num<- temp2$Price* (2-boa[b]) * boa[b] * temp2$MSY*2^beta  
    
    temp2$c_den<- ((2-boa[b])*temp2$r)^beta
    
    temp2$Cost<- temp2$c_num/temp2$c_den
    
    result<-temp2[,c('IdOrig','SpeciesCatName','Cost')]
    
    result$BOA<-boa[b]
    
    if(a==1 & b==1){SensitivityC<-result}
    if(a==1 & b>1 | a>1){SensitivityC<-bind_rows(SensitivityC,result)}
    } # close BOA loop
  } # close cats loop

  # Plot results
  pdf(file=paste(FigureFolder,"C Parameter Sensitivity.pdf",sep=''),width=16,height=10)  
  
  print(ggplot(SensitivityC,aes(x=factor(BOA),y=log(Cost))) +
    geom_boxplot() +
    theme(text=element_text(size=20)) +
    labs(x='Open Access B/Bmsy',y='Log of Marginal Cost')) 
  
  dev.off()
  
  ggplot(SensitivityC,aes(x=factor(BOA),y=log(Cost))) +
    geom_boxplot() +
    facet_wrap(~SpeciesCatName) +
    theme(text=element_text(size=15)) +
    labs(x='Open Access B/Bmsy',y='Log of Marginal Cost')
   
  return()
  }

