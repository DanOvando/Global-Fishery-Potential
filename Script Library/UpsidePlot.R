#####################
##
## Upside Plot
##
#####################

UpsidePlot<-function(CumulativesFinal,FinalYearFinal,Policy,Limit)

  
AsianCountries<-c('Afghanistan','Bahrain','Bangladesh', 'Bhutan','Brunei Darussalam','Cambodia','China', 
                  'Timor-Leste','Viet Nam', 'Japan','Tajikistan','Turkmenistan','Iran','Iraq','Israel','Jordan','Kazakhstan',
                    'Kuwait','Myanmar','Indonesia','Thailand', 'India','Philipines','Republic of Korea','Malaysia','Taiwan Province of China',
                  'Sri Lanka','Maldives','Nepal','Uzbekistan','Kyrgyzstan','Malaysia','Mongolia','Oman','Pakistan','Qatar','Russian Federation',
                  'Saudi Arabia','Singapore','Syrian Arab Republic','Turkey','United Arab Emirates','Yemen')

DotSize<-'Food'
XVar<-'PercChangeFromSQTotalBiomass'
YVar<-'NPV'
Limit<-300
Policy<-'CatchShare'
  
Cumulatives<-CumulativesFinal[CumulativesFinal$Policy==Policy,]

FinalYear<-FinalYearFinal[FinalYearFinal$Policy==Policy,]

# Build dataframe to plot based on axis inputs and country
c<-Cumulatives$Country

if(DotSize=="Food") { s<-Cumulatives$Food}
if(DotSize=="NPV"){s<-Cumulatives$NPV}
if(DotSize=="Fish"){s<-Cumulatives$Fish}

if(YVar=="NPV") { y<-Cumulatives$NPV}
if(YVar=="PercChangeTotalProfits"){y<-FinalYear$PercChangeTotalProfits}
if(YVar=="PercChangeFromSQMedianProfits"){y<-FinalYear$PercChangeFromSQMedianProfits}
if(YVar=="PercChangeMedianProfits"){y<-FinalYear$PercChangeMedianProfits}

if(XVar=='PercChangeFromSQTotalBiomass'){x<-FinalYear$PercChangeFromSQTotalBiomass} 
if(XVar=='PercChangeTotalBiomass'){x<-FinalYear$PercChangeTotalBiomass}
if(XVar=='PercChangeFromSQMedianBiomass'){x<-FinalYear$PercChangeFromSQMedianBiomass}
if(XVar=='PercChangeMedianBiomass'){x<-FinalYear$PercChangeMedianBiomass}
if(XVar=='PercChangeTotalCatch'){x<-FinalYear$PercChangeTotalCatch}
if(XVar=="Food") { x<-Cumulatives$Food}

PlotData<-data.frame(c,x,y,s)
colnames(PlotData)<-c("Country","xVar","yVar","Size")

PlotData$xVar[PlotData$xVar>300]<-300 # change value to limit variable?
PlotData$yVar[PlotData$yVar>300]<-300

pdf(file='Asian Upside Plot.pdf',height=10,width=14,pointsize=6)
ggplot(PlotData,aes(xVar,yVar,size=Size)) +
  geom_point(aes(color=Country)) +
  guides(color=FALSE) +
  coord_cartesian(xlim=c(-70,320),ylim=c(-30,330)) +
  scale_size_continuous(range=c(6,12)) +
  theme(text=element_text(size=20)) +
  geom_abline(intercept=0,slope=0) +
  geom_vline(xintercept=0) +
  labs(title="Upside Percentages for Asian Countries", x = "Percent Change from SQ Total Biomass",
       y = "Percent Change from SQ NPV",size="% Change\n SQ Food")
dev.off()

