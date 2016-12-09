# GlobalUpsideAll<-UnlumpedUpsideAllStocks$GlobalUpside
# GlobalUpsideOverF<-UnlumpedUpsideOverfishOnly$GlobalUpside

TrevorDenominator<-function(GlobalUpsideOverF,GlobalUpsideAll,Discount)  
{
  
Over<-GlobalUpsideOverF[GlobalUpsideOverF$Policy %in% c('Fmsy','Opt','CatchShare'),
                        c('Policy','AbsChangeFromSQTotalCatch','AbsChangeFromSQTotalBiomass','TotalProfits','TotalCatchSQ','TotalBiomassSQ',
                          'AbsChangeTotalProfits','AbsChangeTotalBiomass','AbsChangeTotalCatch','TotalNPV','TotalBaselineProfits','AbsChangeFromSQNPV')]

Trevor<-GlobalUpsideAll[GlobalUpsideAll$Policy %in% c('Fmsy','Opt','CatchShare'),
                        c('TotalBaselineCatch','TotalBaselineProfits','TotalBaselineBiomass','TotalCatchSQ',
                          'TotalProfitsSQ','TotalBiomassSQ','TotalNPVSQ')]

                          
GlobalUpsidesTrevor<-data.frame(matrix(nrow=3,ncol=1))
colnames(GlobalUpsidesTrevor)<-c('Policy')

GlobalUpsidesTrevor$Policy[1:3]<-c('CatchShare','Fmsy','Opt')

# Lower left panel - Overfish delta over all stocks current

GlobalUpsidesTrevor$PercTDCatchCurrent[1:3]<-100*(Over$AbsChangeTotalCatch/Trevor$TotalBaselineCatch)

GlobalUpsidesTrevor$PercTDBiomassCurrent[1:3]<-100*(Over$AbsChangeTotalBiomass/Trevor$TotalBaselineBiomass)


# Lower right panel - Overfish difference in SQ and Policy in 2048 over SQ harvest of all stocks in 2048

GlobalUpsidesTrevor$PercTDCatchSQ[1:3]<-100*(Over$AbsChangeFromSQTotalCatch/Trevor$TotalCatchSQ)

GlobalUpsidesTrevor$PercTDBiomassSQ[1:3]<-100*(Over$AbsChangeFromSQTotalBiomass/Trevor$TotalBiomassSQ)

GlobalUpsidesTrevor$PercTDNPVSQ[1:3]<-100*(Over$AbsChangeFromSQNPV/Trevor$TotalNPVSQ)

# Calculate annuity and add to lower left panel numbers

Annuity<-(GlobalUpsideOverF$TotalNPV[GlobalUpsideOverF$Policy %in% c('CatchShare','Fmsy','Opt')]*Discount)/(1-(1+Discount)^(-36))

GlobalUpsidesTrevor$PercTDNPVCurrent[1:3]<-100*((Annuity-Over$TotalBaselineProfits)/Trevor$TotalBaselineProfits)

AnnuityOver<-(GlobalUpsideOverF$TotalNPV*Discount)/(1-(1+Discount)^(-36))

GlobalUpsideOverF$PercChangeNPV<-100*((AnnuityOver-GlobalUpsideOverF$TotalBaselineProfits)/GlobalUpsideOverF$TotalBaselineProfits)

AnnuityAll<-(GlobalUpsideAll$TotalNPV*Discount)/(1-(1+Discount)^(-36))

GlobalUpsideAll$PercChangeNPV<-100*((AnnuityAll-GlobalUpsideAll$TotalBaselineProfits)/GlobalUpsideAll$TotalBaselineProfits)


# Subset data sets and write csvs

GlobalUpsideAll<-GlobalUpsideAll[GlobalUpsideAll$Policy %in% c('CatchShare','Opt','Fmsy'),]

GlobalUpsideOverF<-GlobalUpsideOverF[GlobalUpsideOverF$Policy %in% c('CatchShare','Opt','Fmsy'),]

# write.csv(file=paste(ResultFolder,'Global Upsides All Stocks.csv',sep=''),GlobalUpsideAll)
# 
# write.csv(file=paste(ResultFolder,'Global Upsides Overfish Stocks.csv',sep=''),GlobalUpsideOverF)
# 
# write.csv(file=paste(ResultFolder,'Global Upsides Overfish Trevor Denominator.csv',sep=''),GlobalUpsidesTrevor)

return(GlobalUpsidesTrevor)

}
