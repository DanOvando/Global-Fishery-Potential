#######################################################
#
# Quality control on RAM Scientific names that are not found in FAO (some are synonyms to FAO names)
#
#######################################################

# Data<-FullData
# VersionToUse<-'SciNameToUse' # indicates using the FAO match
 

RamSciNameAdjuster<-function(Data,VersionToUse)
{
  
  NamesToChange<-read.csv('Data/Ram_SciNames_NoFAOMatch.csv',stringsAsFactors=F)
  
  ids<-unique(NamesToChange$assessid)
  
  for(d in 1:length(ids))
  {
    where<-Data$IdOrig==ids[d]
    
    if(is.na(NamesToChange[NamesToChange$assessid==ids[d],VersionToUse])==F) # don't change for sci names with no good equivalent
    {
      Data$SciName[where]<-NamesToChange[NamesToChange$assessid==ids[d],VersionToUse]
    }
    
  }
  return(Data)
}
