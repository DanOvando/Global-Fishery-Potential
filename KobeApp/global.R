Data<-readRDS("data/KobeAppData.rds")

source("shinyKobe.R")

KobeColors<- colorRampPalette(c("lightskyblue", "white",'gold1'))

DbaseColors<- colorRampPalette(c('Red','Black','Green'))