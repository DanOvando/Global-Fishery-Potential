################################################################################################
##
##
## Chris find MEY code in function form
## 04/26/16
##
################################################################################################


findMEY<-function(df) {
  
  Fisheries = unique(df$IdOrig)
  MEYdata = data.frame(matrix(NA,nrow=length(Fisheries),ncol=7))
  current_b = vector()
  current_f = vector()
  current_b_mey = vector()
  current_f_mey = vector()
  b_mey = vector()
  f_mey = vector()
  grow_disc = vector()
  
  
  for (i in seq(1,length(Fisheries),1))
  {
    print(i)
    DANsub=subset(df,df$IdOrig==Fisheries[i])
    bvec = DANsub$b
    fvec = DANsub$Opt
    
    phi = DANsub$phi[1]
    g = DANsub$g[1]
    
    if (is.na(phi))
    {phi=.188}
    
    fpt = ((phi+1)/phi)*(1 - bvec^phi/(phi+1))
    
    bmey = fzero(DiffF,1.2,bgrid=bvec,f0=fvec,f1=fpt)
    fmey = ((phi+1)/phi)*(1 - bmey$x^phi/(phi+1)) 
    
    current_b[i] = DANsub$StatusQuoBForever[1] #current B/Bmsy
    current_f[i] = DANsub$StatusQuoFForever[1] #current F/Fmsy
    b_mey[i] = bmey$x #Bmey/Bmsy
    f_mey[i] = fmey #Fmey/Fmsy
    current_b_mey[i] = DANsub$StatusQuoBForever[1]/bmey$x #B/Bmey
    current_f_mey[i] = DANsub$StatusQuoFForever[1]/fmey #F/Fmey
    grow_disc[i] = ((phi+1)/phi)*g
  }
  
  MEYdata = data.frame(Fisheries,current_b,current_f,b_mey,f_mey,current_b_mey, current_f_mey,stringsAsFactors = F)
  
  return(MEYdata)
}