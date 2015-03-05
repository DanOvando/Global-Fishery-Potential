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
  
  # 1) Where all non RAM and Catch share stocks go to Open Access
  
  ram<-ProjectionData[ProjectionData$Policy=='Fmsy' & ProjectionData$Dbase=='RAM' & ProjectionData$CatchShare!=1,]
  
  ramids<-unique(ram$IdOrig)
  
  cs<-ProjectionData[ProjectionData$Policy=='Fmsy' & ProjectionData$CatchShare==1,]
  
  csids<-unique(cs$IdOrig)
  
  otherids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)))]
  
  other<-ProjectionData[(ProjectionData$IdOrig %in% c(otherids)) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  BAUsevere<-rbind(ram,cs,other)
  
  BAUsevere$Policy<-'Business As Usual Pessimistic'
  
  
  # 2)
  
  overFFids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                              ((ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy<1) | (ProjectionData$FvFmsy>1 & ProjectionData$BvBmsy>1) |
                                                                 (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy<1)))]
  
  overff<-ProjectionData[(ProjectionData$IdOrig %in% overFFids) & ProjectionData$Policy=='StatusQuoOpenAccess',]
  
  mctofids<-ProjectionData$IdOrig[ProjectionData$Year==BaselineYear & (!(ProjectionData$IdOrig %in% c(ramids,csids)) & 
                                                             (ProjectionData$FvFmsy<1 & ProjectionData$BvBmsy>1))]
  
  mctofid<-ProjectionData[(ProjectionData$IdOrig %in% mctofids) & ProjectionData$Policy=='StatusQuoBForever',]
  
  BAUoptim<-rbind(ram,cs,overff,mctofid)
  
  BAUoptim$Policy<-'Business As Usual Optimistic'
  
  # Bind New policies to original Projection Data
  
  ProjectionData<-rbind(ProjectionData,BAUsevere,BAUoptim)
  
  return(ProjectionData)
}