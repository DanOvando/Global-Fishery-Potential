#################################################################################################
#START [call data and function]
##################################################################################################

# UnlumpedData<-UnlumpedProjectionData
# LumpedData<-ProjectionData
# BaselineYear <- 2012

RenSummaryTable<-function(UnlumpedData,LumpedData,BaselineYear,ResultFolder,FigureFolder)
{
  
################## Checking for duplicates [Unlumped only. This can be done for lumped]
Checkfile<-subset(UnlumpedData, Year==2012,select=c(Country, SciName,CommName,RegionFAO,Dbase,Catch))
for ( i in 1:dim(Checkfile)[1] )
{
y<-subset(Checkfile, Checkfile$Country==Checkfile$Country[i] & Checkfile$SciName==Checkfile$SciName[i] & Checkfile$RegionFAO==Checkfile$RegionFAO[i],select=c(Country, SciName,CommName,RegionFAO,Dbase,Catch))
size<-dim(y)[1]
if (i==1){x<-y}
if (size>1){x<-rbind(x,y)}
}
z<-x[-1,]#deleting first row
zs<-subset(z, Dbase=="FAO",select=c(Country, SciName,CommName,RegionFAO,Dbase,Catch))
zu<-unique(zs)
zu
#write.csv(zu,"//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/DuplicatesUnlumped.csv")
################

# library(plyr)

UnlumpedUpsideAllStocks<-FisheriesUpsideV3_REN(UnlumpedData,BaselineYear,DenominatorPolicy='Business As Usual',RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='All Stocks')
LumpedUpsideAllStocks<-FisheriesUpsideV3_REN(LumpedData,BaselineYear,DenominatorPolicy='Business As Usual',RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='All Stocks')

####################################################################################################

#for global baseline -- Unlumped
GlobalU<-UnlumpedUpsideAllStocks$GlobalBaseline
CountryU<-UnlumpedUpsideAllStocks$CountryBaseline
GlobalU <- cbind(Country="Global", GlobalU)
AllU <- rbind(GlobalU,CountryU)
AllU$Policy <- NULL
colnames(AllU)[1] <- "BASELINE"
write.table(t(AllU),paste(ResultFolder,'GlobalBaselineUnlumped.csv',sep=''),sep=",",col.names=FALSE)

#for global results -- Unlumped
GlobeResU<-UnlumpedUpsideAllStocks$GlobalResults
GlobeResU <- cbind(Country=c("Global","Global","Global","Global","Global","Global","Global","Global","Global","Global","Global"), GlobeResU)
CountResU<-UnlumpedUpsideAllStocks$CountryResults
AllResU <- rbind(GlobeResU,CountResU)
colnames(AllResU)[1] <- "RESULTS"
write.csv(AllResU,paste(ResultFolder,'GlobalResultsUnlumped.csv',sep=''),row.names=FALSE)

#for global baseline -- Lumped
GlobalL<-LumpedUpsideAllStocks$GlobalBaseline
CountryL<-LumpedUpsideAllStocks$CountryBaseline
GlobalL <- cbind(Country="Global", GlobalL)
AllL <- rbind(GlobalL,CountryL)
AllL$Policy <- NULL
colnames(AllL)[1] <- "BASELINE"
write.table(t(AllL),paste(ResultFolder,'GlobalBaselineLumped.csv',sep=''),sep=",",col.names=FALSE)


#for global results -- Lumped
GlobeResL<-LumpedUpsideAllStocks$GlobalResults
GlobeResL <- cbind(Country=c("Global","Global","Global","Global","Global","Global","Global","Global","Global","Global","Global"), GlobeResL)
CountResL<-LumpedUpsideAllStocks$CountryResults
AllResL <- rbind(GlobeResL,CountResL)
colnames(AllResL)[1] <- "RESULTS"
write.csv(AllResL,paste(ResultFolder,'GlobalResultsLumped.csv',sep=''),row.names=FALSE)


####BAR PLOT Lumped and Unlumped Global and per country
library(gridExtra)
UniqName<-unique(AllResU$RESULTS)#186 names
UniqPolicy<-unique(AllResL$Policy)
ResNames<-c("AnnuityOutTo2050","ProfitIn2050","BiomassIn2050","HarvestIn2050")
ylabs<-c("US dollar","US dollar","metric tons","metric tons")

pdf(paste(FigureFolder,"Ren_Barplots.pdf",sep=''), onefile = TRUE)
for (i in UniqName) {
	par(oma=c(0,0,2,0))
	par(mfrow=c(2,2))
		count<-0
		for (j in ResNames){
		count<-count+1

		if(count==1){
dummyL<-AllResL$AnnuityOutTo2050[AllResL$RESULTS==i]
dummyU<-AllResU$AnnuityOutTo2050[AllResU$RESULTS==i]
}
		if(count==2){
			dummyL<-AllResL$ProfitIn2050[AllResL$RESULTS==i]
dummyU<-AllResU$ProfitIn2050[AllResU$RESULTS==i]
			limY<-AllU$Profit_in_2012[AllU$BASELINE==i]}
		if(count==3){dummyL<-AllResL$BiomassIn2050[AllResL$RESULTS==i]
dummyU<-AllResU$BiomassIn2050[AllResU$RESULTS==i]
			limY<-AllU$Biomass_in_2012[AllU$BASELINE==i]
			}
		if(count==4){dummyL<-AllResL$HarvestIn2050[AllResL$RESULTS==i]
dummyU<-AllResU$HarvestIn2050[AllResU$RESULTS==i]
			limY<-AllU$Harvest_in_2012[AllU$BASELINE==i]
			}

if (length(dummyL)!=0){
names(dummyL)<- UniqPolicy}else{
dummyL=c(0,0,0,0,0,0,0,0,0,0,0)
names(dummyL)<- UniqPolicy
}

names(dummyU)<- UniqPolicy 
	op <- par(mar = c(10,4,4,2) + 0.1)   

dummy <- rbind(dummyL,dummyU)

if (count==1){
barplot(dummy,beside=T,las = 2,ylab=ylabs[count],cex.lab=0.8,cex.names=0.8,cex=0.6,cex.main=1.0,main=j,col=c("grey44","grey99"))
legend("topright", c("Lumped","Unlumped"), cex=0.6, bty="n",fill=c("grey44","grey99"))
}else{
barplot(dummy,beside=T,las = 2,ylab=ylabs[count],cex.lab=0.8,cex.names=0.8,cex=0.6,cex.main=1.0,main=j,ylim=c(min(0,min(dummyU),limY,min(dummyL)),max(max(dummyU),limY,max(dummyL))),col=c("grey44","grey99"))
legend("topright", c("Lumped","Unlumped"), cex=0.6, bty="n",fill=c("grey44","grey99"))
}
if(count==2){
panel.first = abline(h=AllL$Profit_in_2012[AllL$BASELINE==i], col = "black",lwd=1,lty=2)
panel.first = abline(h=AllU$Profit_in_2012[AllU$BASELINE==i], col = "gray",lwd=1.5,lty=2)
}
if(count==3){
panel.first = abline(h=AllL$Biomass_in_2012[AllL$BASELINE==i], col = "black",lwd=1,lty=2)
panel.first = abline(h=AllU$Biomass_in_2012[AllU$BASELINE==i], col = "gray",lwd=1.5,lty=2)
}
if(count==4){
panel.first = abline(h=AllL$Harvest_in_2012[AllL$BASELINE==i], col = "black",lwd=1,lty=2)
panel.first = abline(h=AllU$Harvest_in_2012[AllU$BASELINE==i], col = "gray",lwd=1.5,lty=2)
}

title(main=i,outer=T)
	par(op) 
}}
dev.off()


##################################
###Calculate percentages [5-8]

#UNLUMPED
AA<-UnlumpedUpsideAllStocks$GlobalPercentage
BB<-UnlumpedUpsideAllStocks$Denom_GlobalPercentage
GlobePersU<-AA/BB
CC<-UnlumpedUpsideAllStocks$CountryPercentage
#CC$Country #182 entries
#write.table(CC$Country,"//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/name1.csv",sep=",",col.names=FALSE)
DDU<-UnlumpedUpsideAllStocks$Denom_CountryPercentage
#DDU$Country #185 entries
#write.table(DDU$Country,"//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/name2.csv",sep=",",col.names=FALSE)

DD2<-DDU[DDU$Country %in% CC$Country,]

CountryPersU<-CC[,colnames(CC)!='Country']/DD2[,colnames(DD2)!='Country']
CountryPersU$Country<-DD2$Country
GlobePersU <- cbind(Country="Global", GlobePersU)
GlobePersU$CanProject<-NULL
AllPersU <- rbind(GlobePersU,CountryPersU)
colnames(AllPersU)[1] <- "PERCENTCHANGE"
write.table(t(AllPersU),paste(ResultFolder,'Percentages_Unlumped.csv',sep=''),sep=",",col.names=FALSE)


m <- matrix(0, ncol = 4, nrow = 32)
m <- data.frame(m)
names(m) <- c("description1","description2","Unlumped","Lumped")

#######################
#9 to 20 --- Table
#########################

m$description1[9]<-"reforming fisheries currently at-risk could generate increases of:"
m$description2[9]<-"38% in fish biomass"
m$description2[10]<-"19% in harvest"
m$description2[11]<-"112% in fisheries profit compared to current levels"

m$Unlumped[9]<-GlobePersU$Biomass_CatchShareVsToday_All
m$Unlumped[10]<-GlobePersU$Harvest_CatchShareVsToday_All
m$Unlumped[11]<-GlobePersU$Annuity_CatchShareVsToday_All

###---
m$description1[12]<-"4,373 fisheries"
m$description1[13]<-"394 fisheries included from the RAM Legacy database"
m$description1[14]<-"3,979 unassessed fisheries"
m$description1[15]<-"SOFIA"
m$description1[16]<-"global MSY is 104 MMT (current catch is 80MMT)"
m$description1[17]<-"median F/Fmsy of 1.4"
m$description1[18]<-"median B/Bmsy of 0.77"
m$description1[19]<-"76% of global fisheries are currently at-risk ? those with B/Bmsy<1 and/or F/Fmsy>1."
#m$description1[20]<-"Relative to BAU, this fraction rises to 93% of countries (Figure 1, right panels)."

m$Unlumped[12]<-GlobalU$N_Fisheries
m$Unlumped[13]<-GlobalU$N_RAM
m$Unlumped[14]<-GlobalU$N_FAO
m$Unlumped[15]<-GlobalU$N_SOFIA

m$Unlumped[16]<-GlobalU$TotalMSY
m$Unlumped[17]<-GlobalU$MedianFvFmsy2012
m$Unlumped[18]<-GlobalU$MedianBvBmsy2012
m$Unlumped[19]<-GlobalU$FisheriesAtRisk
#m$Unlumped[20]<-

##########Triple-bottom-line [ALL COUNTRY] [row 1-4]
m$description1[1]<-"Triple-bottom-line [ALL COUNTRIES]"

m$description2[1]<-"Catch Share vs. Today, relative to rebuilt only"
m$Unlumped[1]<-length(CountryPersU$Country[CountryPersU$Biomass_CatchShareVsToday_Rebuild>0 & CountryPersU$Harvest_CatchShareVsToday_Rebuild>0 & CountryPersU$Annuity_CatchShareVsToday_Rebuild>0])*100/dim(CountryPersU)[1]

m$description2[2]<-"Catch Share vs. BAU, relative to rebuilt only"
m$Unlumped[2]<-length(CountryPersU$Country[CountryPersU$Biomass_CatchShareVsBAU_Rebuild>0 & CountryPersU$Harvest_CatchShareVsBAU_Rebuild>0 & CountryPersU$Annuity_CatchShareVsBAU_Rebuild>0])*100/dim(CountryPersU)[1]

m$description2[3]<-"Catch Share vs. Today, relative to all stocks"
m$Unlumped[3]<-length(CountryPersU$Country[CountryPersU$Biomass_CatchShareVsToday_All>0 & CountryPersU$Harvest_CatchShareVsToday_All>0 & CountryPersU$Annuity_CatchShareVsToday_All>0])*100/dim(CountryPersU)[1]

m$description2[4]<-"Catch Share vs. BAU, relative to all stocks"
m$Unlumped[4]<-length(CountryPersU$Country[CountryPersU$Biomass_CatchShareVsBAU_All>0 & CountryPersU$Harvest_CatchShareVsBAU_All>0 & CountryPersU$Annuity_CatchShareVsBAU_All>0])*100/dim(CountryPersU)[1]


####Top 30 fishing countries only_UNLUMPED [row 5 to 8]
m$description1[5]<-"Triple-bottom-line [TOP 30 COUNTRIES]"
CountrySort<-CountryU[order(-CountryU$Harvest_in_2012),] 
CountrySort<-CountrySort[which(CountrySort$Country != "High Seas Tuna and Billfish" & CountrySort$Country!="Multinational" & CountrySort$Country %in% CountryPersU$Country),]
CountryTop30<-CountrySort[1:30,]
CountryTop30$Country

CountryPersUTOP30<- CountryPersU[which(CountryPersU$Country=="China"),]
for (i in CountryTop30$Country){
CountryPersUTOP30<-rbind(CountryPersUTOP30,CountryPersU[which(CountryPersU$Country==i & CountryPersU$Country != "High Seas Tuna and Billfish" & CountryPersU$Country!="Multinational"),])
}
CountryPersUTOP30<-CountryPersUTOP30[-1,]#deleting first row
CountryPersUTOP30$Country

dim(CountryPersUTOP30)[1]

#Catch Share vs. Today, relative to rebuilt only
m$Unlumped[5]<-length(CountryPersUTOP30$Country[CountryPersUTOP30$Biomass_CatchShareVsToday_Rebuild>0 & CountryPersUTOP30$Harvest_CatchShareVsToday_Rebuild>0 & CountryPersUTOP30$Annuity_CatchShareVsToday_Rebuild>0])*100/dim(CountryPersUTOP30)[1]

#Catch Share vs. BAU, relative to rebuilt only
m$Unlumped[6]<-length(CountryPersUTOP30$Country[CountryPersUTOP30$Biomass_CatchShareVsBAU_Rebuild>0 & CountryPersUTOP30$Harvest_CatchShareVsBAU_Rebuild>0 & CountryPersUTOP30$Annuity_CatchShareVsBAU_Rebuild>0])*100/dim(CountryPersUTOP30)[1]

#Catch Share vs. Today, relative to all stocks
m$Unlumped[7]<-length(CountryPersUTOP30$Country[CountryPersUTOP30$Biomass_CatchShareVsToday_All>0 & CountryPersUTOP30$Harvest_CatchShareVsToday_All>0 & CountryPersUTOP30$Annuity_CatchShareVsToday_All>0])*100/dim(CountryPersUTOP30)[1]

#Catch Share vs. BAU, relative to all stocks
m$Unlumped[8]<-length(CountryPersUTOP30$Country[CountryPersUTOP30$Biomass_CatchShareVsBAU_All>0 & CountryPersUTOP30$Harvest_CatchShareVsBAU_All>0 & CountryPersUTOP30$Annuity_CatchShareVsBAU_All>0])*100/dim(CountryPersUTOP30)[1]

m$description2[5]<-"Catch Share vs. Today, relative to rebuilt only"
m$description2[6]<-"Catch Share vs. BAU, relative to rebuilt only"
m$description2[7]<-"Catch Share vs. Today, relative to all stocks"
m$description2[8]<-"Catch Share vs. BAU, relative to all stocks"

m$description1[25]<-"Fish catch can increase under Catch Shares by 11 MMT (relative to current catch)"
m$description2[26]<-"and by 14.5 MMT (relative to BAU);"
m$description1[27]<-"While profits are already 34% higher under Catch Shares than under Fmsy (Figure 3),"
m$description2[28]<-"this wedge would be even larger (44%) had we allowed all fisheries (not just those at-risk) to benefit from the pecuniary effects of Catch Shares."

m$Unlumped[25]<-GlobeResU$HarvestIn2050[GlobeResU$Policy=="Catch Share Three"]-GlobalU$Harvest_in_2012
m$Unlumped[26]<-GlobeResU$HarvestIn2050[GlobeResU$Policy=="Catch Share Three"]-GlobeResU$HarvestIn2050[GlobeResU$Policy=="Business As Usual"]

m$Unlumped[27]<-(GlobeResU$AnnuityOutTo2050[GlobeResU$Policy=="Catch Share Three"]-GlobeResU$AnnuityOutTo2050[GlobeResU$Policy=="Fmsy Three"])*100/GlobeResU$AnnuityOutTo2050[GlobeResU$Policy=="Fmsy Three"]
m$Unlumped[28]<-(GlobeResU$AnnuityOutTo2050[GlobeResU$Policy=="CatchShare"]-GlobeResU$AnnuityOutTo2050[GlobeResU$Policy=="Fmsy"])*100/GlobeResU$AnnuityOutTo2050[GlobeResU$Policy=="Fmsy"]


m$description1[29]<-"under BAU, 73% of global fisheries are likely to be in need of recovery by 2050"
m$description2[30]<-"(this rises to 92% under the pessimistic BAU).?"

BvBMSYNeedrecoveredU<- UnlumpedData %>%
  group_by(Policy) %>%
  summarize(needrecovered=length(IdOrig[Year==2050 & BvBmsy<0.8])*100/length(IdOrig[Year==2050]),meantimerecov= mean(c(min(Year[BvBmsy>=0.8])))     )
  
#   ddply(UnlumpedData,c('Policy'),summarize,needrecovered=length(IdOrig[Year==2050 & BvBmsy<0.8])*100/length(IdOrig[Year==2050]),meantimerecov= mean(c(min(Year[BvBmsy>=0.8])))       )

# BvBMSYNeedrecoveredU<-ddply(UnlumpedData,c('Policy'),summarize,needrecovered=length(IdOrig[Year==2050 & BvBmsy<0.8])*100/length(IdOrig[Year==2050]),meantimerecov= mean(c(min(Year[BvBmsy>=0.8])))       )


# BvBMSYNeedrecoveredU

####
m$Unlumped[29]<-BvBMSYNeedrecoveredU$needrecovered[BvBMSYNeedrecoveredU$Policy=="Business As Usual"]
###
m$Unlumped[30]<-BvBMSYNeedrecoveredU$needrecovered[BvBMSYNeedrecoveredU$Policy=="Business As Usual Pessimistic"]

m$description1[31]<-"In contrast, if reform efforts are put in place now, the mean time to recovery would be < 11 years,"
m$description2[32]<-"and by mid-century, the vast majority (84%) of stocks would be biologically healthy."

BvBMSYU<- UnlumpedData %>%
  group_by(IdOrig) %>%
  summarize(meantimerecov= mean(c(min(Year[BvBmsy>=0.8 & Policy == "Catch Share Three"])),na.rm=T),included=sum(Year[Year==2012 & BvBmsy<0.8 & Policy == "Historic"]))

# BvBMSYU<-ddply(UnlumpedData,c('IdOrig'),summarize,meantimerecov= mean(c(min(Year[BvBmsy>=0.8 & Policy == "Catch Share Three"])),na.rm=T),included=sum(Year[Year==2012 & BvBmsy<0.8 & Policy == "Historic"]))


BvBMSYU$meantimerecov[!is.finite(BvBMSYU$meantimerecov)] <- NA
m$Unlumped[31]<-mean(c(BvBMSYU$meantimerecov[BvBMSYU$included==2012]),na.rm=TRUE)-2012
m$Unlumped[32]<-100-BvBMSYNeedrecoveredU$needrecovered[BvBMSYNeedrecoveredU$Policy=="Catch Share Three"]


#LUMPED####
AA<-LumpedUpsideAllStocks$GlobalPercentage
BB<-LumpedUpsideAllStocks$Denom_GlobalPercentage
GlobePersL<-AA/BB
CC<-LumpedUpsideAllStocks$CountryPercentage
#CC$Country
#write.table(CC$Country,"//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/name1.csv",sep=",",col.names=FALSE)
DD<-LumpedUpsideAllStocks$Denom_CountryPercentage

DDL<-DD[DD$Country %in% CC$Country,]
#DD$Country
#write.table(DD$Country,"//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/name2.csv",sep=",",col.names=FALSE)

CountryPersL<-CC[,colnames(CC)!='Country']/DDL[,colnames(DDL)!='Country']
# CountryPersL<-CC/DDL
CountryPersL$Country<-DDL$Country
GlobePersL <- cbind(Country="Global", GlobePersL)
GlobePersL$CanProject<-NULL
AllPersL <- rbind(GlobePersL,CountryPersL)
colnames(AllPersL)[1] <- "PERCENTCHANGE"
write.table(t(AllPersL),paste(ResultFolder,'Percentages_Lumped.csv",sep='),sep=",",col.names=FALSE)

m$Lumped[9]<-GlobePersL$Biomass_CatchShareVsToday_All
m$Lumped[10]<-GlobePersL$Harvest_CatchShareVsToday_All
m$Lumped[11]<-GlobePersL$Annuity_CatchShareVsToday_All
m$Lumped[12]<-GlobalL$N_Fisheries
m$Lumped[13]<-GlobalL$N_RAM
m$Lumped[14]<-GlobalL$N_FAO
m$Lumped[15]<-GlobalL$N_SOFIA
m$Lumped[16]<-GlobalL$TotalMSY
m$Lumped[17]<-GlobalL$MedianFvFmsy2012
m$Lumped[18]<-GlobalL$MedianBvBmsy2012
m$Lumped[19]<-GlobalL$FisheriesAtRisk


#Catch Share vs. Today, relative to rebuilt only
m$Lumped[1]<-length(CountryPersL$Country[CountryPersL$Biomass_CatchShareVsToday_Rebuild>0 & CountryPersL$Harvest_CatchShareVsToday_Rebuild>0 & CountryPersL$Annuity_CatchShareVsToday_Rebuild>0])*100/dim(CountryPersL)[1]

#Catch Share vs. BAU, relative to rebuilt only
m$Lumped[2]<-length(CountryPersL$Country[CountryPersL$Biomass_CatchShareVsBAU_Rebuild>0 & CountryPersL$Harvest_CatchShareVsBAU_Rebuild>0 & CountryPersL$Annuity_CatchShareVsBAU_Rebuild>0])*100/dim(CountryPersL)[1]

#Catch Share vs. Today, relative to all stocks
m$Lumped[3]<-length(CountryPersL$Country[CountryPersL$Biomass_CatchShareVsToday_All>0 & CountryPersL$Harvest_CatchShareVsToday_All>0 & CountryPersL$Annuity_CatchShareVsToday_All>0])*100/dim(CountryPersL)[1]

#Catch Share vs. BAU, relative to all stocks
m$Lumped[4]<-length(CountryPersL$Country[CountryPersL$Biomass_CatchShareVsBAU_All>0 & CountryPersL$Harvest_CatchShareVsBAU_All>0 & CountryPersL$Annuity_CatchShareVsBAU_All>0])*100/dim(CountryPersL)[1]

####Top 30 fishing countries only_LUMPED
CountrySortL <- CountryL[order(-CountryL$Harvest_in_2012),] 
CountrySortL<-CountrySortL[which(CountrySortL$Country != "High Seas Tuna and Billfish" & CountrySortL$Country!="Multinational" & CountrySortL$Country %in% CountryPersL$Country),]
CountryTop30<-CountrySortL[1:30,]
CountryTop30$Country

CountryPersLTOP30<- CountryPersL[which(CountryPersL$Country=="China"),]
for (i in CountryTop30$Country){
CountryPersLTOP30<-rbind(CountryPersLTOP30,CountryPersL[which(CountryPersL$Country==i & CountryPersL$Country != "High Seas Tuna and Billfish" & CountryPersL$Country!="Multinational"),])
}
CountryPersLTOP30<-CountryPersLTOP30[-1,]#deleting first row
CountryPersLTOP30$Country

dim(CountryPersLTOP30)[1]
#Catch Share vs. Today, relative to rebuilt only
m$Lumped[5]<-length(CountryPersLTOP30$Country[CountryPersLTOP30$Biomass_CatchShareVsToday_Rebuild>0 & CountryPersLTOP30$Harvest_CatchShareVsToday_Rebuild>0 & CountryPersLTOP30$Annuity_CatchShareVsToday_Rebuild>0])*100/dim(CountryPersLTOP30)[1]

#Catch Share vs. BAU, relative to rebuilt only
m$Lumped[6]<-length(CountryPersLTOP30$Country[CountryPersLTOP30$Biomass_CatchShareVsBAU_Rebuild>0 & CountryPersLTOP30$Harvest_CatchShareVsBAU_Rebuild>0 & CountryPersLTOP30$Annuity_CatchShareVsBAU_Rebuild>0])*100/dim(CountryPersLTOP30)[1]

#Catch Share vs. Today, relative to all stocks
m$Lumped[7]<-length(CountryPersLTOP30$Country[CountryPersLTOP30$Biomass_CatchShareVsToday_All>0 & CountryPersLTOP30$Harvest_CatchShareVsToday_All>0 & CountryPersLTOP30$Annuity_CatchShareVsToday_All>0])*100/dim(CountryPersLTOP30)[1]

#Catch Share vs. BAU, relative to all stocks
m$Lumped[8]<-length(CountryPersLTOP30$Country[CountryPersLTOP30$Biomass_CatchShareVsBAU_All>0 & CountryPersLTOP30$Harvest_CatchShareVsBAU_All>0 & CountryPersLTOP30$Annuity_CatchShareVsBAU_All>0])*100/dim(CountryPersLTOP30)[1]



m$Lumped[25]<-GlobeResL$HarvestIn2050[GlobeResL$Policy=="Catch Share Three"]-GlobalL$Harvest_in_2012
m$Lumped[26]<-GlobeResL$HarvestIn2050[GlobeResL$Policy=="Catch Share Three"]-GlobeResL$HarvestIn2050[GlobeResL$Policy=="Business As Usual"]

m$Lumped[27]<-(GlobeResL$AnnuityOutTo2050[GlobeResL$Policy=="Catch Share Three"]-GlobeResL$AnnuityOutTo2050[GlobeResL$Policy=="Fmsy Three"])*100/GlobeResL$AnnuityOutTo2050[GlobeResL$Policy=="Fmsy Three"]
m$Lumped[28]<-(GlobeResL$AnnuityOutTo2050[GlobeResL$Policy=="CatchShare"]-GlobeResL$AnnuityOutTo2050[GlobeResL$Policy=="Fmsy"])*100/GlobeResL$AnnuityOutTo2050[GlobeResL$Policy=="Fmsy"]

# BvBMSYNeedrecoveredL<-ddply(LumpedData,c('Policy'),summarize,needrecovered=length(IdOrig[Year==2050 & BvBmsy<0.8])*100/length(IdOrig[Year==2050]))

BvBMSYNeedrecoveredL<- LumpedData %>%
  group_by(Policy) %>%
  summarize(needrecovered=length(IdOrig[Year==2050 & BvBmsy<0.8])*100/length(IdOrig[Year==2050]))

# BvBMSYNeedrecoveredL
####
m$Lumped[29]<-BvBMSYNeedrecoveredL$needrecovered[BvBMSYNeedrecoveredL$Policy=="Business As Usual"]
###
m$Lumped[30]<-BvBMSYNeedrecoveredL$needrecovered[BvBMSYNeedrecoveredL$Policy=="Business As Usual Pessimistic"]

BvBMSYL<- LumpedData %>%
  group_by(IdOrig) %>%
  summarise(meantimerecov= mean(c(min(Year[BvBmsy>=0.8 & Policy == "Catch Share Three"])),na.rm=T),included=sum(Year[Year==2012 & BvBmsy<0.8 & Policy == "Historic"]))
  

# BvBMSYL<-ddply(LumpedData,c('IdOrig'),summarize,meantimerecov= mean(c(min(Year[BvBmsy>=0.8 & Policy == "Catch Share Three"])),na.rm=T),included=sum(Year[Year==2012 & BvBmsy<0.8 & Policy == "Historic"]))



BvBMSYL$meantimerecov[!is.finite(BvBMSYL$meantimerecov)] <- NA
m$Lumped[31]<-mean(c(BvBMSYL$meantimerecov[BvBMSYL$included==2012]),na.rm=TRUE)-2012
m$Lumped[32]<-100-BvBMSYNeedrecoveredL$needrecovered[BvBMSYNeedrecoveredL$Policy=="Catch Share Three"]

m[m == 0] <- ""
write.csv(m,paste(ResultFolder,'FinalResultMatrix.csv',sep=''))

### Top 30 countries code:
  
  ###CS3 FMSY3 BAU
  #head(CountResU) #Just get the top 30 countries
#   TOP30C<-c("China","Peru","USA","Indonesia","Japan","India","Russian Federation","Chile","Philippines","Norway","Republic of Korea","Argentina","Thailand","Morocco","Malaysia","South Africa","Mexico","Denmark","Iceland","Canada","Viet Nam","Senegal","Namibia","United Kingdom","Spain","Angola","Taiwan Province of China","Brazil","New Zealand","Iran")
TOP30C<-unique(CountryPersUTOP30$Country)

#SUBCountResU<-CountResU[which(CountResU$Country %in% TOP30C),]
T30 <- matrix(0, ncol = 10, nrow = 30)
T30 <- data.frame(T30)
names(T30) <- c("Country","CatchShare3_BiomassIn2050","FMSY3_BiomassIn2050","BAU_BiomassIn2050","CatchShare3_HarvestIn2050","FMSY3_HarvestIn2050","BAU_HarvestIn2050","CatchShare3_ProfitIn2050","FMSY3_ProfitIn2050","BAU_ProfitIn2050")
T30$Country<-TOP30C

SUB<-CountResU[which(CountResU$Country==TOP30C[1]),]
for (i in TOP30C[2:30]){
  SUB2<-CountResU[which(CountResU$Country==i),]
  SUB<-rbind(SUB,SUB2)
}

T30$CatchShare3_BiomassIn2050<-SUB$BiomassIn2050[SUB$Policy== "Catch Share Three"]
T30$FMSY3_BiomassIn2050<-SUB$BiomassIn2050[SUB$Policy== "Fmsy Three"]
T30$BAU_BiomassIn2050<-SUB$BiomassIn2050[SUB$Policy== "Business As Usual"]

T30$CatchShare3_HarvestIn2050<-SUB$HarvestIn2050[SUB$Policy== "Catch Share Three"]
T30$FMSY3_HarvestIn2050<-SUB$HarvestIn2050[SUB$Policy== "Fmsy Three"]
T30$BAU_HarvestIn2050<-SUB$HarvestIn2050[SUB$Policy== "Business As Usual"]

T30$CatchShare3_ProfitIn2050<-SUB$ProfitIn2050[SUB$Policy== "Catch Share Three"]
T30$FMSY3_ProfitIn2050<-SUB$ProfitIn2050[SUB$Policy== "Fmsy Three"]
T30$BAU_ProfitIn2050<-SUB$ProfitIn2050[SUB$Policy== "Business As Usual"]

write.csv(T30,paste(ResultFolder,'STEVE_CS3_Fmsy3_BAU.csv',sep=''))


###CS FMSY BAU
#head(CountResU) #Just get the top 30 countries

#SUBCountResU<-CountResU[which(CountResU$Country %in% TOP30C),]
T30 <- matrix(0, ncol = 10, nrow = 30)
T30 <- data.frame(T30)
names(T30) <- c("Country","CatchShare_BiomassIn2050","FMSY_BiomassIn2050","BAU_BiomassIn2050","CatchShare_HarvestIn2050","FMSY_HarvestIn2050","BAU_HarvestIn2050","CatchShare_ProfitIn2050","FMSY_ProfitIn2050","BAU_ProfitIn2050")
T30$Country<-TOP30C

SUB<-CountResU[which(CountResU$Country==TOP30C[1]),]
for (i in TOP30C[2:30]){
  SUB2<-CountResU[which(CountResU$Country==i),]
  SUB<-rbind(SUB,SUB2)
}

T30$CatchShare_BiomassIn2050<-SUB$BiomassIn2050[SUB$Policy== "CatchShare"]
T30$FMSY_BiomassIn2050<-SUB$BiomassIn2050[SUB$Policy== "Fmsy"]
T30$BAU_BiomassIn2050<-SUB$BiomassIn2050[SUB$Policy== "Business As Usual"]

T30$CatchShare_HarvestIn2050<-SUB$HarvestIn2050[SUB$Policy== "CatchShare"]
T30$FMSY_HarvestIn2050<-SUB$HarvestIn2050[SUB$Policy== "Fmsy"]
T30$BAU_HarvestIn2050<-SUB$HarvestIn2050[SUB$Policy== "Business As Usual"]

T30$CatchShare_ProfitIn2050<-SUB$ProfitIn2050[SUB$Policy== "CatchShare"]
T30$FMSY_ProfitIn2050<-SUB$ProfitIn2050[SUB$Policy== "Fmsy"]
T30$BAU_ProfitIn2050<-SUB$ProfitIn2050[SUB$Policy== "Business As Usual"]

write.csv(T30,paste(ResultFolder,'STEVE_CS_Fmsy_BAU.csv',sep=''))

###############################
###PlotPercentages
###############################

# library(gridExtra)
# UniqName<-unique(DDU$Country)

W<-list("Biomass_CatchShareVsToday_Rebuild",
"Harvest_CatchShareVsToday_Rebuild",
"Annuity_CatchShareVsToday_Rebuild",
"Biomass_CatchShareVsBAU_Rebuild",
"Harvest_CatchShareVsBAU_Rebuild",
"Annuity_CatchShareVsBAU_Rebuild",
"Biomass_CatchShareVsToday_All",
"Harvest_CatchShareVsToday_All",
"Annuity_CatchShareVsToday_All",
"Biomass_CatchShareVsBAU_All",
"Harvest_CatchShareVsBAU_All",
"Annuity_CatchShareVsBAU_All")

# pdf(file=paste(FigureFolder,"Plot_Percentages_Lumped.pdf",sep=''), onefile = TRUE)
# for (i in UniqName) {
# par(oma=c(8,0,0,0))
# 
# if (i %in% AllPersU$PERCENTCHANGE){ 
# subdatU <- subset(AllPersU,AllPersU$PERCENTCHANGE == i)
# subdat2U <- t(subdatU)
# subdat2U<-subdat2U[-1,]} 
# else {
# subdat2U=list(0,0,0,0,0,0,0,0,0,0,0,0)
# names(subdat2U)<- W
# }
# 
# if (i %in% AllPersL$PERCENTCHANGE){
# subdatL <- subset(AllPersL,AllPersL$PERCENTCHANGE == i)
# subdat2L <- t(subdatL)
# subdat2L<-subdat2L[-1,]}
# else{
# subdat2L=list(0,0,0,0,0,0,0,0,0,0,0,0)
# names(subdat2L)<- W
# }
# 
# barplot(t(subdat2L),las=2,cex.names=0.8,ylab="Percent Change",main=i)
# }
# dev.off()
# 
# pdf(file=paste(FigureFolder,"Plot_Percentages_Unlumped.pdf",sep=''), onefile = TRUE)
# browser()
# for (i in UniqName) {
# par(oma=c(8,0,0,0))
# 
# if (i %in% AllPersU$PERCENTCHANGE){ 
# subdatU <- subset(AllPersU,AllPersU$PERCENTCHANGE == i)
# subdat2U <- t(subdatU)
# subdat2U<-subdat2U[-1,]} 
# else {
# subdat2U=list(0,0,0,0,0,0,0,0,0,0,0,0)
# names(subdat2U)<- W
# }
# 
# if (i %in% AllPersL$PERCENTCHANGE){
# subdatL <- subset(AllPersL,AllPersL$PERCENTCHANGE == i)
# subdat2L <- t(subdatL)
# subdat2L<-subdat2L[-1,]}
# else{
# subdat2L=list(0,0,0,0,0,0,0,0,0,0,0,0)
# names(subdat2L)<- W
# }
# 
# barplot(t(subdat2U),las=2,cex.names=0.8,ylab="Percent Change",main=i)
# }
# dev.off()

# return values table

return(ValuesForPaper=m)

}
