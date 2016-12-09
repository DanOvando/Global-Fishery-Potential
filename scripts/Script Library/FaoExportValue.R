
# First dataset is value by commodity category. 
# Create new dataset that has production by commodity in order to match value to quantity

# Notes: 
# 1) value is in $1000 USD


### FAO Export Value csv
FaoValue<-read.csv('Data/fao_value_111214.csv',stringsAsFactors=F, na.strings=c("...","-","0 0"))
FaoValue[,6:41]<-apply(FaoValue[,6:41],2,function(y) as.numeric(gsub(" F","",y))) # remove " F" from certain data points and convert catch record to numeric

# Name columns
colnames(FaoValue)[1:5]<-c('Country','Commodity','SpeciesCatName','SpeciesCat','Trade')

# convert to long format and subset to most recent year
FaoValue<-reshape(FaoValue,varying=6:41,direction="long", v.names="Value", timevar="Year",times=1976:2011,)
FaoValue<-FaoValue[FaoValue$Year==2011 & is.na(FaoValue$Value)==F,1:7]

# multiply value by 1000 to go from units of 1000 USD to USD
FaoValue$Value<-FaoValue$Value*1000

### FAO Export Quantity csv
FaoQuantity<-read.csv('Data/fao_quantity_111214.csv',stringsAsFactors=F, na.strings=c("...","-","0 0"))
FaoQuantity[,6:41]<-apply(FaoQuantity[,6:41],2,function(y) as.numeric(gsub(" F","",y))) # remove " F" from certain data points and convert catch record to numeric

# Name columns
colnames(FaoQuantity)[1:5]<-c('Country','Commodity','SpeciesCatName','SpeciesCat','Trade')

# convert to long format and subset out unwanted columns
FaoQuantity<-reshape(FaoQuantity,varying=6:41,direction="long", v.names="Value", timevar="Year",times=1976:2011,)
FaoQuantity<-FaoQuantity[FaoQuantity$Year==2011 & is.na(FaoQuantity$Value)==F,1:7]

### Match values to quantities
FaoValue$Quantity<-NA

for(a in 1:nrow(FaoValue))
{
  where<-FaoQuantity$Country==FaoValue$Country[a] & FaoQuantity$Commodity==FaoValue$Commodity[a]
  
  if(any(where)==T) {FaoValue$Quantity[a]<-FaoQuantity$Value[where]} 
}

# Calculate export value per ton
FaoValue$ValuePerTon<-FaoValue$Value/FaoValue$Quantity

# Aggregate by ISSCAAP category
FaoGlobalValues<-ddply(FaoValue[is.na(FaoValue$ValuePerTon)==F,],c('SpeciesCatName'),summarize,MeanValue=mean(ValuePerTon,na.rm=T))


