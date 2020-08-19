## Loading the CSV file ##

Fish1 <- read.csv('C:/Users/User/Downloads/Fish.csv')
Fish1

## Checking the Summary of Dataset ##

summary(Fish1)

## Checking the Outliers ##

boxplot(Fish1$Length1)
boxplot(Fish1$Weight)
boxplot(Fish1$Length2)
boxplot(Fish1$Length3)
boxplot(Fish1$Height)

## Plot of the Dataset Weight VS Height ##

plot(Fish1$Weight,Fish1$Height, main = 'Weight vs Height',
    xlab = 'Weight', ylab = 'Height', col = 'green')

## PLot of Dataset Lenght1 vs Lenght2 ##

plot(Fish1$Length1,Fish1$Length2,main = 'Lenght1 vs Lenght2',
     xlab = 'Lenght1', ylab = 'Lenght2', col = 'red')

## Bar Plot ##

barplot(Fish1$Height, col = 'Yellow')
barplot(Fish1$Width, col = 'purple')

## Calling the maxmium value ##

max(Fish1$Weight)
max(Fish1$Height)

## Calling the minimum value ##

min(Fish1$Length1)
min(Fish1$Length3)

## Splitting the data in 80:20 tain:test ratio ##

library(caTools)

set.seed(2)
split <- sample.split(Fish1, SplitRatio = 0.8)
split
train <- subset(Fish1, split = T)
test <- subset(Fish1, split = F)
train
test

## Creating regression model Width & Height ##

model <- lm(Width~Height,data = Fish1)
summary(model)

## Creatig regression model for Lenght 1 & Lenght3 ##

model <- lm(Length3~Length1, data = Fish1)
summary(model)

## Prediction ##

predict <- predict(model,test)
predict

## Prediction ##

predict <- predict(model)
predict

## COmparing ##

plot(test$Weight,type = 'l', col = 'cyan')
plot(test$Height, type = 'l',col = 'red')
lines(test$Length1,type = 'l', col ='green')
plot(predict,type = 'l',col = 'purple')
lines(predict, type = 'l', col = 'yellow')

## Accuracay by root mean square error ##

rms <- sqrt(mean(predict-Fish1$Weight)^2)
rms

rms <- sqrt(mean(predict-Fish1$Height)^2)
rms

## Predicting Weight of Fish ##

y = 25792.2 + (Fish1$Weight)
y

## Predicting Width of Fish ##

y = 25792.2 + (Fish1$Width)
y

## Predicting Height of Fish ##

y = 26792.2 + (Fish1$Height)
y
