############################################------------------------------------------------------
##
## 
## Build New Status Quo Policies
## 
## "Business As Usual Optimistic"
##   - RAM non-catch share stocks get 'Fmsy'
##   - Catch share stocks get 'Optimal'
## Business As Usual Pessimistic
##
############################################



BuildPolicyBAUs<-function(ProjectionData,BaselineYear)
{
  
  # 1) "Business As Usual Pessimistic" Where all non RAM and Catch share stocks go to Open Access
  
  # RAM - Fmsy
  # Catch shares - Opt
  # All others - Open Access
  
  ram<-ProjectionData[ProjectionData$Policy=='StatusQuoFForever' & ProjectionData$Dbase=='RAM' & ProjectionData$CatchShare!=1,]
  
  ramids<-unique(ram$IdOrig)
  
  cs<-ProjectionData[ProjectionData$Policy=='Opt' & ProjectionData$CatchShare==1,]
  
  csids<-unique(cs$IdOrig)
  
  otherids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)))]
  
  other<-ProjectionData[(ProjectionData$IdOrig %in% c(otherids)) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  BAUpess<-rbind(ram,cs,other)
  
  BAUpess$Policy<-'BAU Pessimistic'
  
  
  # 2) "Business As Usual Current Management"
  
  # RAM - Fmsy
  # Catch shares - Opt
  # Overfished and Overfishing - Open Access
  # Myctophids - StatusQuoBForever
  
  overFFids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                              ((ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy<1) | (ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy>1) |
                                                                 (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy<1)))]
  
  overff<-ProjectionData[(ProjectionData$IdOrig %in% overFFids) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  mctofids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                             (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy>1))]
  
  mctofid<-ProjectionData[(ProjectionData$IdOrig %in% mctofids) & ProjectionData$Policy=='StatusQuoBForever',]
  
  BAUoptim<-rbind(ram,cs,overff,mctofid)
  
  BAUoptim$Policy<-'Current Management'
  
  # 3) "Business As Usual Current Fishing Mortality" - Ram stocks projected under current fishing mortality
  
  # RAM - StatusQuoFForever (only change from 'Current Management')
  # Catch shares - Opt
  # Overfished and Overfishing - Open Access
  # Myctophids - StatusQuoBForever
  
  # just change policy used for ram stocks and bind to cs, overff, and mctofid
#   ramF<-ProjectionData[ProjectionData$Policy=='StatusQuoFForever' & ProjectionData$Dbase=='RAM' & ProjectionData$CatchShare!=1,]
#   
#   BAUcurrF<-rbind(ramF,cs,overff,mctofid)
#   
#   BAUcurrF$Policy<-'BAU Current Fishing Mortality'
  
  # Bind New policies to original Projection Data
  
  ProjectionData<-rbind(ProjectionData,BAUpess,BAUoptim)
  
  return(ProjectionData)
}