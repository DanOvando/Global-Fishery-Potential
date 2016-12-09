
Prices<-read.csv('Data/New_Prices_Analysis.csv')

Prices<-Prices[Prices$best_fit_AgCommodity_0711!='Atlantic(Thunnus thynnus) and Pacific(Thunnus orientalis)bluefin tuna',]

Prices$LogTrue<-log(Prices$Wted_avg_sp_price)

Prices$LogNew<-log(Prices$New_Five_yr_stock_price)

Prices$PropError<-(Prices$New_Five_yr_stock_price/Prices$Wted_avg_sp_price)-1

Prices$AllData<-'All Species'

# write.csv(Prices,file='ExVessel Price Analysis Version One.csv')

panelA<-ggplot(Prices,aes(x=LogTrue,y=LogNew)) +
  coord_cartesian(xlim=c(2,10),ylim=c(2,10)) +
  geom_point(size=4,alpha=0.9) +
  geom_abline(yintercept=0,slope=1) +
  theme(text=element_text(size=18)) +
  labs(x='Observed Log Price',y='Predicted Log Price')

panelB<-ggplot(Prices,aes(y=PropError,x=AllData)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(-2,2)) +
  geom_hline(yintercept=0,linetype=2) +
  theme(text=element_text(size=18)) +
  labs(x='All Prices',y='Proportional Error in Log Price')
  
pdf(file='SOM Plots/Price Database Analysis.pdf',width=10,height=10)

print(grid.arrange(arrangeGrob(panelA,panelB,ncol=2))) 

dev.off()

