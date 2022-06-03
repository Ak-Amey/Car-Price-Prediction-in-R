
library(ggplot2)
library(dplyr) #sorting or arrange
library(tidyr)
library(lubridate)
library(MASS)
library(stringr)
library(dummies)
library(GGally)
library(gplots)
library(car)

#Reading data----
carprice=read.csv("car_data.csv")
str(carprice)


#Years to Factors 

carprice$Mfg_Year <- factor(carprice$Mfg_Year)


#Excluded:-ID,Model,Mfg_month,Mfg_Year,color,Cylinders
#Age in months is similar to  Mfg_Month and Mfg_Year


carprice_1 <- dummy.data.frame(carprice[,-c(1,2,5,6,11,15)])
#print(carprice_1)
#cat("\n\n")

#After analysing Variables which are not important seeing the Correlation matrix excluded-
#Radio_Cassette,Tow_Bar, Parking_Assistant,Mettalic_Rim, Backseat_Divider
#Sports_Model,Mistlamps,BOVAG_Guarantee, Mfr_Guarantee, Automatic, Met_Color
#Radio and radio_cassette have high correlation 0.99 correlation so only radio is considered
carprice_2 <- carprice_1[,-c(8,9,15,16,29,30,31,32,33,34,35)]
#print(carprice_2)

#Heatmap to analyse all data
heatmap.2(cor(carprice_2), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(carprice_2),2),
         notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# All continuous variables
Toyota_Co.df <- carprice_2[,c(1,2,3,7,10,11,12,13,14,17)]
print(Toyota_Co.df)
str(Toyota_Co.df)

#Heat Map for only continuous variables


heatmap.2(cor(Toyota_Co.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_Co.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))




#After analysing data other highly Correalted variables which are dropped:-
#CC, Doors, Gears, Guarantee period

Toyota_Drop.df <- Toyota_Co.df[,-c(5,6,7,10)]
#print(Toyota_Drop.df)


#Heat Map of only relevant continuous variables
#Continous Variables relevant- Price,Age,KM,Quaterly_Tax,Weight,HP

heatmap.2(cor(Toyota_Drop.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_Drop.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))



#Only Continuous variables for Correlation matrix:-
cat("\nCorrelation Matrix\n")
Cor_Matrix <- round(cor(Toyota_Drop.df),2)
print(Cor_Matrix)
cat("\n")

#Matrix scatter plot Continous variables 
m1=ggpairs(Toyota_Drop.df)
print(m1)


#Executing the t Linear Model:----

#randomly shuffle data
carprice_2.shuffled=carprice_2[sample(nrow(carprice_2)),]


cat("Summary Of Linear Regression using variable KM\n")
model_01=lm(Price~KM,carprice_2.shuffled)
pre1=predict(model_01,carprice_2.shuffled)
a=summary(model_01)
print(a)
cat("\n\n")


p0=ggplot(carprice_2.shuffled,aes(x=KM,y=Price))+geom_point()
p0=p0+geom_smooth(method = "lm",formula = y~x,col="violet",se=F)
p0=p0+ggtitle("Linear Regression Line using KM variable")
print(p0)

cat("Summary Of Linear Regression using variable Age_08_04\n")
model_1=lm(Price~Age_08_04,carprice_2.shuffled)
pre2=predict(model_1,carprice_2.shuffled)
a=summary(model_1)
print(a)
cat("\n\n")

x=1:length(carprice_2.shuffled$Price)
plot(x, carprice_2.shuffled$Price,pch=19, col="blue",main="Plot Using KM variable")
lines(x,pre1 , col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)


p=ggplot(carprice_2.shuffled,aes(x=Age_08_04,y=Price))+geom_point()
p=p+geom_smooth(method = "lm",formula = y~x,col="violet",se=F)
p=p+ggtitle("Linear Regression Line using Age_08_04 variable")
print(p)

x1=1:length(carprice_2.shuffled$Price)
plot(x1, carprice_2.shuffled$Price,pch=19, col="blue",main="Plot Using Age_08_04 variable")
lines(x1, pre2, col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)

#LR Prediction Model
#Using KM variable
se0=data.frame(KM=c(200000,100000,50000,10000,1000))

p3=predict(model_01,se0)
b1=cbind(se0,p3)
cat("Prediction using Linear Regression Model with KM variable\n")
print(b1)
cat("\n\n")

# Using variable Age_08_04
se1=data.frame(Age_08_04=c(80,60,40,30,10))

p4=predict(model_1,se1)
b2=cbind(se1,p4)
cat("Prediction using Linear Regression Model with Age_08_04 variable\n")
print(b2)
cat("\n\n")


#plotting Polynomial Regression line for KM vs Price-----

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 5

#create k equal-sized folds
folds <- cut(seq(1,nrow(carprice_2.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <-carprice_2.shuffled[testIndexes, ]
  trainData <- carprice_2.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(Price ~ poly(Age_08_04,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$Price)^2) 
  }
}
#find MSE for each degree 
a1=colMeans(mse)
#print(a1)

#fit best model using KM variable
best0 = lm(Price ~ poly(KM,4, raw=T), data=carprice_2.shuffled)

#view summary of best model
cat("\nSummary Of Polynomial Regression using KM variable\n")
a02=summary(best0)
print(a02)
cat("\n\n")

#plotting
p4=ggplot(carprice_2.shuffled, aes(x=KM, y=Price)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ poly(x,4), size = 1) + 
  xlab('KM') +
  ylab('Price')
p4=p4+ggtitle("Polynomial Regression Line using KM variable")
print(p4)



#PLR
pred1 <- predict(best0, data = carprice_2.shuffled)
se2=data.frame(KM=c(200000,100000,50000,10000,1000))

p4=predict(best0,se2)
b02=cbind(se2,p4)
cat("Prediction using polynomial Regression Model with KM variable\n")
print(b02)

#create data frame of actual and predicted values using KM variable
cat("\nActual vs predicted values using PLR Model with KM variable\n")
values_0 <- data.frame(actual=carprice_2.shuffled$Price, predicted=pred1)
h1=head(values_0)
print(h1)



#fit best model using Age_08_04
best1 = lm(Price ~ poly(Age_08_04,5, raw=T), data=carprice_2.shuffled)

#view summary of best model
cat("\nSummary Of Polynomial Regression using Age_08_04\n")
a03=summary(best1)
print(a03)
cat("\n\n")

#plotting
p5=ggplot(carprice_2.shuffled, aes(x=Age_08_04, y=Price)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ poly(x,4), size = 1) + 
  xlab('KM') +
  ylab('Price')
p5=p5+ggtitle("Polynomial Regression Line using Age_08_04")
print(p5)

#PLR
pred2 <- predict(best1, data = carprice_2.shuffled)
se3=data.frame(Age_08_04=c(80,60,40,30,10))
pred3=predict(best1,carprice_2.shuffled)

pr1=predict(best1,se3)
b6=cbind(se3,pr1)
cat("\nPrediction using polynomial Regression Model with Age_08_04\n")
print(b6)

#create data frame of actual and predicted values using KM variable
cat("\nActual and predicted values using Age_08_04 variable of Polynomial Regression\n")
values1 <- data.frame(actual=carprice_2.shuffled$Price, predicted=pred3)
h3=head(values1)
print(h3)
cat("\n")

#MLR Prediction
cat("MLR Summary using some variable\n")
m=lm(Price~KM+Weight+Age_08_04+HP+Mfr_Guarantee+Automatic_airco,carprice_2.shuffled)
avPlots(m)
a5=summary(m)
print(a5)
se1=data.frame(KM=c(200000,150000,100000,50000,10000,5000),Weight=c(1200,1200),Age_08_04=c(80,70,60,40,25,10),HP=c(110,110),Mfr_Guarantee=c(1,1),Automatic_airco=c(1,1))
p5=predict(m,se1)
b3=cbind(se1,p5)
cat("\nPrediction using MLR Regression Model\n")
print(b3)

#create data frame of actual and predicted values
cat("\nActual and Predicted values using MLR some variable\n")
values3 <- data.frame(actual=carprice_2.shuffled$Price, predicted=predict(m))
h5=head(values3)
print(h5)
cat("\n")

z1=ggplot(values3,                                     # Draw plot using ggplot2 package
          aes(x = predicted,
              y = actual)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)+ggtitle("Actual vs Predicted Plot")
print(z1)

x5=1:length(carprice_2.shuffled$Price)
plot(x5, carprice_2.shuffled$Price,pch=19, col="blue",main="MLR Plot Using some variable")
lines(x5,predict(m) , col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)


# MLR using All Variables
cat("Summary MLR using all variables")
g1=lm(Price~Age_08_04+KM +Fuel_TypeDiesel+HP+Met_Color+Doors+Gears+Quarterly_Tax+Weight+Mfr_Guarantee+ABS+Airbag_1+Airbag_2+Airco+Automatic_airco+Boardcomputer+CD_Player+Central_Lock+Powered_Windows+Power_Steering+Radio+Mistlamps,carprice_2.shuffled)
s1=summary(g1)
print(s1)
m1=predict(g1,carprice_2.shuffled)
plot(m1,main="MLR plot using all variables")
plot(m1,carprice_2.shuffled$Price,main="Actual vs Predicted MLR Plot")

cat("\nActual and Predicted values using MLR with all variables\n")
values5 <- data.frame(actual=carprice_2.shuffled$Price, predicted=predict(g1))
h7=head(values5)
print(h7)
cat("\n")

x5=1:length(carprice_2.shuffled$Price)
plot(x5, carprice_2.shuffled$Price,pch=19, col="blue",main="MLR Plot Using all variable")
lines(x5,m1 , col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)


#KNN Functions----
library(class)
car_mod=carprice_2.shuffled[c('Price','KM','Weight','Age_08_04','HP','Mfr_Guarantee','Automatic_airco')]
head(car_mod)

#Normalize
n=function(b){
  return ((b-min(b))/(max(b)-min(b)))
}
car_new=as.data.frame(lapply(car_mod[,2:7],n))
car_new

set.seed(123) #to get some random sample

data=sample(1:nrow(car_new),size=nrow(car_new)*0.9,replace = FALSE)#random selection
train_price=car_mod[data,]# 90% training data
test_price=car_mod[-data,] #remaianing 10% data

#Now Creating seperate dataframe "Price"
train_price_labels=car_mod[data,1]
test_price_labels=car_mod[-data,1]


NROW(train_price_labels) #to find the number of observations
#1292 dataset so we basically take the square root of dataset value as k

knn_1=knn(train_price, test=test_price,train_price_labels,k=1)

acc.1=100*sum(test_price_labels==knn_1)/NROW(test_price_labels)
print(acc.1)


cat("\nTable of actual and predicted car price using knn\n")
val <- data.frame(actual=test_price_labels, predicted=knn_1)
h9=head(val)
print(h9)
cat("\n")

#Binding all the values and printing it
cat("\nTable of all predicted values using all algorithms\n\n")

cb1=cbind.data.frame(values_0,values1,values3,values5)
colnames(cb1) <- c("PLR","KM","PLR"," Age_08_04 "," MLR"," some  variable "," MLR","  all  variable ")
he1=head(cb1)
print(he1)


