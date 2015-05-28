# setwd("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode")
# getwd()

#################################################################################################
#START [call data and function]
##################################################################################################

##UNLUMPED
# UnlumpedProjectionData <- read.csv("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/Unlumped Projection Data.csv")
# ##LUMPED
# LumpedProjectionData <- read.csv("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/Lumped Projection Data.csv")
# 
# BaselineYear <- 2012
# library(plyr)
# 
# FisheriesUpsideV3 <- dget("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/FisheriesUpsideV3.R")
LumpedProjectionData<-ProjectionData

RenUnlumpedUpsideAllStocks<-FisheriesUpsideV3_REN(UnlumpedProjectionData,BaselineYear,DenominatorPolicy='Business As Usual',RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='All Stocks')
LumpedUpsideAllStocks<-FisheriesUpsideV3_REN(LumpedProjectionData,BaselineYear,DenominatorPolicy='Business As Usual',RecoveryThreshold=0.8,LumpedName='UnLumped Projection Data',SubsetName='All Stocks')

####################################################################################################

#for global baseline -- Unlumped
GlobalU<-RenUnlumpedUpsideAllStocks$GlobalBaseline
CountryU<-RenUnlumpedUpsideAllStocks$CountryBaseline
GlobalU <- cbind(Country="Global", GlobalU)
AllU <- rbind(GlobalU,CountryU)
AllU$Policy <- NULL
colnames(AllU)[1] <- "BASELINE"
write.table(t(AllU),paste(ResultFolder,"LumpedGlobalBaseline.csv",sep=''),sep=",",col.names=FALSE)

#for global results -- Unlumped
GlobeResU<-RenUnlumpedUpsideAllStocks$GlobalResults
GlobeResU<-cbind(Country=c(rep("Global",nrow(GlobeResU))),GlobeResU)
CountResU<-RenUnlumpedUpsideAllStocks$CountryResults
AllResU <- rbind(GlobeResU,CountResU)
colnames(AllResU)[1] <- "RESULTS"
write.csv(AllResU,paste(ResultFolder,"ResultsUnlumped.csv",sep=''),row.names=FALSE)


#for global baseline -- Lumped
GlobalL<-LumpedUpsideAllStocks$GlobalBaseline
CountryL<-LumpedUpsideAllStocks$CountryBaseline
GlobalL <- cbind(Country="Global", GlobalL)
AllL <- rbind(GlobalL,CountryL)
AllL$Policy <- NULL
colnames(AllL)[1] <- "BASELINE"
write.table(t(AllL),paste(ResultFolder,"UnlumpedGlobalBaseline.csv",sep=''),sep=",",col.names=FALSE)

#for global results -- Lumped
GlobeResL<-LumpedUpsideAllStocks$GlobalResults
GlobeResL<-cbind(Country=c(rep("Global",nrow(GlobeResL))),GlobeResL)
CountResL<-LumpedUpsideAllStocks$CountryResults
AllResL <- rbind(GlobeResL,CountResL)
colnames(AllResL)[1] <- "RESULTS"
write.csv(AllResL,paste(ResultFolder,"ResultsLumped.csv",sep=''),row.names=FALSE)


#UNLUMPED_PLOT_%biomass_%catch
#uname<-unique(AllResU$RESULTS)
# upolicy<-unique(AllResU$Policy)
# par(mfrow=c(2,5))
# for (i in 1:length(upolicy)){
# subResU<-subset(AllResU, Policy==Policy[i],select=c(RESULTS, BiomassIn2050,HarvestIn2050))
# subBasU<-AllU[c("BASELINE","Biomass_in_2012","Harvest_in_2012")]
# colnames(subBasU)[1] <- "RESULTS"
# Merged <- merge(subResU,subBasU,by="RESULTS")
# plot((Merged$BiomassIn2050-Merged$Biomass_in_2012)*100/Merged$Biomass_in_2012,(Merged$HarvestIn2050-Merged$Harvest_in_2012)*100/Merged$Harvest_in_2012,main=upolicy[i],ylab="% change in harvest",xlab="% change in biomass")
# # write.csv(Merged,paste("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/PercentCalc",upolicy[i],".csv"),row.names=FALSE)
# }

# x11()##will push the system to create another window for plotting

#LUMPED_PLOT_%biomass_%catch
#uname<-unique(AllResL$RESULTS)
# upolicy<-unique(AllResL$Policy)
# par(mfrow=c(2,5))
# for (i in 1:length(upolicy)){
# subResL<-subset(AllResL, Policy==Policy[i],select=c(RESULTS, BiomassIn2050,HarvestIn2050))
# subBasL<-AllL[c("BASELINE","Biomass_in_2012","Harvest_in_2012")]
# colnames(subBasL)[1] <- "RESULTS"
# Merged <- merge(subResL,subBasL,by="RESULTS")
# plot((Merged$BiomassIn2050-Merged$Biomass_in_2012)*100/Merged$Biomass_in_2012,(Merged$HarvestIn2050-Merged$Harvest_in_2012)*100/Merged$Harvest_in_2012,main=upolicy[i],ylab="% change in harvest",xlab="% change in biomass")
# #write.csv(Merged,paste("//babylon/gaineslab/rcabral/Desktop/Costello Upside Paper/plotcode/revcode/PercentCalc",upolicy[i],".csv"),row.names=FALSE)
# }

##Bind Results
#BindRes <- merge(AllResU,AllResL,by=c("RESULTS","Policy"),sort = FALSE,suffixes = c(".U",".L"))
#unique(BindRes$RESULTS)
##BindBase
#BindBase <- merge(AllU,AllL,by="BASELINE",sort = FALSE,suffixes = c(".U",".L"))


# BARPLOTS 
# library(gridExtra)
#UniqName<-unique(AllResL$RESULTS)#169 names
UniqName<-unique(AllResU$RESULTS)#186 names
UniqPolicy<-unique(AllResL$Policy)
#UniqName<-unique(BindRes$RESULTS)
#UniqPolicy<-unique(BindRes$Policy)
ResNames<-c("AnnuityOutTo2050","ProfitIn2050","BiomassIn2050","HarvestIn2050")
#ResNames<-c("NPVoutTo2050.U","ProfitIn2050.U","BiomassIn2050.U","HarvestIn2050.U","NPVoutTo2050.L","ProfitIn2050.L","BiomassIn2050.L","HarvestIn2050.L")
ylabs<-c("US dollar","US dollar","metric tons","metric tons")

pdf(paste(FigureFolder,"Global and Country Result Barplots.pdf",sep=''), onefile = TRUE)
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
dummyL=c(rep(0,nrow(GlobeResL)))
#dummyL=c(0,0,0,0,0,0,0,0,0)
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
AA<-RenUnlumpedUpsideAllStocks$GlobalPercentage
BB<-RenUnlumpedUpsideAllStocks$Denom_GlobalPercentage
GlobePers<-AA/BB
CC<-RenUnlumpedUpsideAllStocks$CountryPercentage
#CC$Country #182 entries

DDU<-RenUnlumpedUpsideAllStocks$Denom_CountryPercentage
#DDU$Country #185 entries

DD2<-DDU[-19,]#%for British Indian Ocean Ter
DD2<-DD2[-158,]#%for Sudan
DD2<-DD2[-166,]#%for Tokelau
CountryPers<-CC/DD2
CountryPers$Country<-DD2$Country
GlobePers <- cbind(Country="Global", GlobePers)
GlobePers$CanProject<-NULL
AllPersU <- rbind(GlobePers,CountryPers)
colnames(AllPersU)[1] <- "PERCENTCHANGE"
write.table(t(AllPersU),paste(ResultFolder,"All Percentages Unlumped.csv",sep=''),sep=",",col.names=FALSE)

#LUMPED####
AA<-LumpedUpsideAllStocks$GlobalPercentage
BB<-LumpedUpsideAllStocks$Denom_GlobalPercentage
GlobePers<-AA/BB
CC<-LumpedUpsideAllStocks$CountryPercentage
#CC$Country
DD<-LumpedUpsideAllStocks$Denom_CountryPercentage
#DD$Country
DD<-DD[-12,]#%for Bangladesh! #change this into function
CountryPers<-CC/DD
CountryPers$Country<-DD$Country
GlobePers <- cbind(Country="Global", GlobePers)
GlobePers$CanProject<-NULL
AllPersL <- rbind(GlobePers,CountryPers)
colnames(AllPersL)[1] <- "PERCENTCHANGE"
write.table(t(AllPersL),paste(ResultFolder,"All Percentages Lumped.csv",sep=''),sep=",",col.names=FALSE)


###############################
###PlotPercentages
###############################

library(gridExtra)
UniqName<-unique(DDU$Country)

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

pdf(paste(FigureFolder,"Plot Percentages Lumped.pdf",sep=''), onefile = TRUE)
for (i in UniqName) {
par(oma=c(8,0,0,0))

if (i %in% AllPersU$PERCENTCHANGE){ 
subdatU <- subset(AllPersU,AllPersU$PERCENTCHANGE == i)
subdat2U <- t(subdatU)
subdat2U<-subdat2U[-1,]} 
else {
subdat2U=list(0,0,0,0,0,0,0,0,0,0,0,0)
names(subdat2U)<- W
}

if (i %in% AllPersL$PERCENTCHANGE){
subdatL <- subset(AllPersL,AllPersL$PERCENTCHANGE == i)
subdat2L <- t(subdatL)
subdat2L<-subdat2L[-1,]}
else{
subdat2L=list(0,0,0,0,0,0,0,0,0,0,0,0)
names(subdat2L)<- W
}

barplot(t(subdat2L),las=2,cex.names=0.8,ylab="Percent Change",main=i)
}
dev.off()

pdf(paste(FigureFolder,"Plot Percentages Unlumped.pdf",sep=''), onefile = TRUE)
for (i in UniqName) {
par(oma=c(8,0,0,0))

if (i %in% AllPersU$PERCENTCHANGE){ 
subdatU <- subset(AllPersU,AllPersU$PERCENTCHANGE == i)
subdat2U <- t(subdatU)
subdat2U<-subdat2U[-1,]} 
else {
subdat2U=list(0,0,0,0,0,0,0,0,0,0,0,0)
names(subdat2U)<- W
}

if (i %in% AllPersL$PERCENTCHANGE){
subdatL <- subset(AllPersL,AllPersL$PERCENTCHANGE == i)
subdat2L <- t(subdatL)
subdat2L<-subdat2L[-1,]}
else{
subdat2L=list(0,0,0,0,0,0,0,0,0,0,0,0)
names(subdat2L)<- W
}

barplot(t(subdat2U),las=2,cex.names=0.8,ylab="Percent Change",main=i)
}
dev.off()
