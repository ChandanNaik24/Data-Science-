### Installation of packages ###

install.packages("corrplot")
install.packages("car")
install.packages("perturb")
install.packages("ggplot2")
install.packages("MASS")
install.packages("Kernlab")
install.packages("caret")

##$ Loading Libraries ###

library(corrplot)
library(car)
library(perturb)
library(ggplot2)
library(MASS)
library(kernlab)
library(caret)
library(plyr)

### Loadung the dataset ###

Forest<- read.csv(file.choose())
View(Forest)
str(Forest)
attach(Forest) 

###EDA(exploratory data analysis) ###

mean(Forest$FFMC)
mean(Forest$DMC)
mean(Forest$DC)
mean(Forest$ISI)
mean(Forest$ISI)
mean(Forest$temp)
mean(Forest$RH)
mean(Forest$wind)
mean(Forest$rain) 

## Visualization of the data with some of the variable ##

hist(Forest$area)
rug(Forest$area)
hist(Forest$FFMC)
hist(Forest$DMC)
hist(Forest$DC)
hist(Forest$ISI)
hist(Forest$temp)
hist(Forest$RH)
colnames(Forest)

## Checking summary of any missing values

sum(is.na(Forest))
set.seed(1234)
forest1<- mutate(Forest, y= log(area+1))
hist(forest1$y)
View(forest1)
summary(Forest)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Forest$temp = normalize(Forest$temp)
Forest$RH = normalize(Forest$RH)
Forest$wind = normalize(Forest$wind)
Forest$rain = normalize(Forest$rain)


ind<- sample(2, nrow(Forest), replace = TRUE, prob = c(0.7,0.3))
forest_train<- Forest[ind==1,]
fact_train<- as.factor(forest_train)

forest_test<- Forest[ind==2,]
fact_test<- as.factor(forest_test)
library(e1071)

fact<- as.factor(Forest$size_category)
model1<- ksvm(fact~temp + rain + wind + RH, data = fact_train, kernel= "vanilladot")
View(model1)

### number of support vector created is 298 ##

area_pred<- predict(model1,forest_test)
table(area_pred,forest_test$size_category) 

## area_pred large small ###

large 0 0
small 50 107
agreement<- area_pred==forest_test$size_category
agreement
table(agreement)

# # FALSE TRUE

50  107
prop.table(table(agreement)) 

### FALSE TRUE ###

0.3184713 0.6815287
pred_model1<- predict(model1, newdata = forest_test)
mean(pred_model1==forest_test$size_category)

# mean = 0.6815

##Using different type of kernel ###

model2<- ksvm(fact~temp + rain + wind + RH, data = fact_train, kernel= "rbfdot")
pred_model2<- predict(model2, newdata = forest_test)
mean(pred_model2==forest_test$size_category) 

model3<- ksvm(fact~temp + rain + wind + RH, data = fact_train, kernel= "besseldot")
pred_model3<- predict(model3, newdata = forest_test)
mean(pred_model3==forest_test$size_category)

model4<- ksvm(fact~temp + rain + wind + RH, data = fact_train, kernel= "polydot")
pred_model4<- predict(model4, newdata = forest_test)
mean(pred_model4==forest_test$size_category) 

model5<- ksvm(fact~temp + rain + wind + RH, data = fact_train, kernel= "anovadot")
pred_model5<- predict(model5, newdata = forest_test)
mean(pred_model5==forest_test$size_category) 

model6<- ksvm(fact~temp + rain + wind + RH, data = fact_train, kernel= "laplacedot")
pred_model6<- predict(model6, newdata = forest_test)
mean(pred_model6==forest_test$size_category) 

## the best fitted model here is "rbfdot" and "laplacedot"as it gives the higher mean value.
