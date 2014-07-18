Test<- read.csv('Data/TestRegression.csv')

RegVars<- c('Dependent','Ind1','Ind2','ind5')

Where<- colnames(Test) %in% RegVars

TestReg<- lm(Dependent~., data=Test[1:2,])

summary(TestReg)

predict(TestReg,Test)

a<- c(10,10,20)

b<- c(20,7,10,11,12)

b%in%a