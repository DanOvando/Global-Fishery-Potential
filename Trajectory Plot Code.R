# ### Make Trajectory Plots ###
# TrajTime<- 10

# TrajBioMatrix<- as.data.frame(matrix(NA,nrow=0,ncol=TrajTime+3))

# TrajYieldMatrix<- as.data.frame(matrix(NA,nrow=0,ncol=TrajTime+3))

# TrajCount<- 0

# Stocks<- unique(rMatrix$stock,na.rm=T)

# Stocks<- Stocks[is.na(Stocks)==F]

# for (s in 1:length(Stocks))
# {
	# Where<- rMatrix$stock==Stocks[s] & is.na(rMatrix$stock)==F & rMatrix$bvbmsy <1
	
	
	# show(paste(round(100*(s/length(Stocks)),2),'% Done',sep=''))
	
	# if (sum(Where,na.rm=T)>0)
	# {
	
	# for (p in 1:3)
	# {
		
		# TempTraj<- sim_HB(rMatrix[Where,4:(SampleLength+3)],kMatrix[Where,4:(SampleLength+3)],msyMatrix[Where,4:(SampleLength+3)],rMatrix$bvbmsy[Where],TrajTime,p)
		
		# TrajBioMatrix2<- data.frame(Stocks[s],p,1:dim(TempTraj$B)[1],TempTraj$B[1: TrajTime], stringsAsFactors=FALSE)

		# TrajBioMatrix<- rbind(TrajBioMatrix, TrajBioMatrix2)

		# TrajYieldMatrix2<- data.frame(Stocks[s],p,1:dim(TempTraj$B)[1],TempTraj$H, stringsAsFactors=FALSE)
		
		# TrajYieldMatrix<- rbind(TrajYieldMatrix, TrajYieldMatrix2)
				
	# }
	# }
# }


# colnames(TrajBioMatrix)<- c('stock','policy','it',paste('year',1:(TrajTime),sep=''))
# colnames(TrajYieldMatrix)<- c('stock','policy','it',paste('year',1:(TrajTime),sep=''))


# BioPlotMatrix<- as.data.frame(matrix(NA,nrow=0,ncol=4))

# YieldPlotMatrix<- as.data.frame(matrix(NA,nrow=0,ncol=4))

# Policies<- unique(TrajBioMatrix$policy)

# YearColumns<- grepl('year',colnames(TrajBioMatrix))

# Bottom<- ceiling(.025*SampleLength)

# Top<- ceiling(.975*SampleLength)

# for (p in Policies)
# {


# ItBioMeans<- as.data.frame(matrix(NA,nrow=SampleLength,ncol=TrajTime))

# ItYieldMeans<- as.data.frame(matrix(NA,nrow=SampleLength,ncol=TrajTime))


# for (i in 1:SampleLength)
# {
	# Where<- TrajBioMatrix$it==i	& 	TrajBioMatrix$policy==p

	# ItBioData<- TrajBioMatrix[Where, YearColumns]
		
	# ItBioMeans[i,]<- colMeans(ItBioData,na.rm=T)

	# ItYieldData<- TrajYieldMatrix[Where, YearColumns]
	
	# ItYieldMeans[i,]<- colMeans(ItYieldData,na.rm=T)

# }


# ItMeans<- apply(ItBioMeans,2,sort)
# YearMean<- colMeans(ItMeans,na.rm=T)
# YearBot<- ItMeans[Bottom,]
# YearTop<- ItMeans[Top,]

# TempBio<- data.frame(1:TrajTime,p,YearMean,YearBot,YearTop)

# BioPlotMatrix<- rbind(BioPlotMatrix,TempBio)


# ItMeans<- apply(ItYieldMeans,2,sort)
# YearMean<- colMeans(ItMeans,na.rm=T)
# YearBot<- ItMeans[Bottom,]
# YearTop<- ItMeans[Top,]

# TempYields<- data.frame(1:TrajTime,p,YearMean,YearBot,YearTop)

# YieldPlotMatrix<- rbind(YieldPlotMatrix, TempYields)

# }
# # }

# colnames(BioPlotMatrix)<- c('Year','Policy','Mean','Bottom','Top')

# colnames(YieldPlotMatrix)<- c('Year','Policy','Mean','Bottom','Top')


 # panel.bands <-
    # function(x, y, upper, lower,
             # subscripts, ..., font, fontface)
 # {
    # upper <- upper[subscripts]
    # lower <- lower[subscripts]
    # panel.polygon(c(x, rev(x)), c(upper, rev(lower)),...)
 # }

# Cols<-   scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1)


# BioPlotMatrix$Policy<- as.factor(BioPlotMatrix$Policy)

# levels(BioPlotMatrix$Policy)<- c('Policy 1','Policy 2','Policy 3')

# pdf(file=paste(FigureFolder,'/Biomass Trajectory.pdf',sep=''),width=8,height=6)
# par(family='Bookman')
 # xyplot(Mean ~ Year | Policy, group = Policy, data = BioPlotMatrix,layout=c(1,3),ylim=c(0.4,1.1),index.cond=list(c(3,2,1)), type = 'l',lwd=2,xlab='Year',ylab='B/Bmsy',
       # upper = BioPlotMatrix$Top,
       # lower = BioPlotMatrix$Bottom,col='gray80',col.line=Cols(6),border='transparent',
       # panel = function(x, y, ...){
           # panel.superpose(x, y, panel.groups = 'panel.bands', ...)
           # panel.abline(h=1,lty=2)
           # panel.xyplot(x, y, ...)
       # })
# dev.off()
 

# YieldPlotMatrix$Policy<- as.factor(YieldPlotMatrix$Policy)

# levels(YieldPlotMatrix$Policy)<- c('Policy 1','Policy 2','Policy 3')

# pdf(file=paste(FigureFolder,'/Yield Trajectory.pdf',sep=''),width=8,height=6)
# par(family='Bookman')
 # xyplot(Mean ~ Year | Policy, group = Policy, data = YieldPlotMatrix,layout=c(1,3),index.cond=list(c(3,2,1)), type = 'l',lwd=4,xlab='Year',ylab='Y/MSY',
       # upper = YieldPlotMatrix$Top,
       # lower = YieldPlotMatrix$Bottom,col='gray80',col.line=Cols(6),border='transparent',
       # panel = function(x, y, ...){
           # panel.superpose(x, y, panel.groups = 'panel.bands', ...)
           # panel.xyplot(x, y, ...)
           # panel.abline(h=1,lty=2)

       # })
# dev.off()