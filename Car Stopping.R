datasets::cars

car <- datasets::cars
summary(car)

## Otliers Plotting ##

boxplot(car$speed)
boxplot(car$dist)

## Different types Plots ##

## Bar Plots ##

barplot(car$speed, col = 'blue')
barplot(car$dist, col = 'red')

## Histogram Plots ##

hist(car$speed, col = 'yellow')
hist(car$dist, col = 'purple')

## Speed VS Dist ##

plot(car$speed,car$dist,main = 'Speed VS Dist',
     xlab = 'Speed', ylab = 'Dist', col = 'red')

## Relationship between two variable ##

plot(car$speed,car$dist,main = 'Car Stopping Distance',
     xlab = 'Speed', ylab = 'Dist', type = 'l', col = 'red')

## Train-Test Split ##

library(caTools)
set.seed(2)
split <- sample.split(car, SplitRatio = 0.7)
split
train <- subset(car, split = T)
test <- subset(car, split = F)
train
test

## Regression Model ##

model <- lm(dist~., data = train)
model
summary(model)

## Pridiction ##

pred <- predict.lm(model,test)
pred

## Accuracy ##

rmse <- sqrt(mean(pred-car$dist)^2)
rmse

## Plotting actual & prediction ##

plot(car$dist, type = 'l', col = 'blue')
lines(pred, type = 'l', col = 'red')

## Scatterpolt ##

scatter.smooth(car$speed,car$dist)

## Regression Value y = b+mx for 100 speed ##

y <- 17.5791+(3.9324*100)
y
