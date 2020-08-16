### Loading the CVS file to R Studio cloud ###

data<-WineRed

### Checking the summary ###

summary(data)

### Checking outliers ###

boxplot(data$fixed.acidity)
boxplot(data$citric.acid)
boxplot(data$volatile.acidity)
boxplot(data$residual.sugar)
boxplot(data$chlorides)

### Checking NA Valures ###

sum(is.na(data))

### Histogram PLot ###

hist(data$fixed.acidity, col = 'red')
hist(data$volatile.acidity,col = 'orange')

### PLot Dataset ###

plot(data)
plot(data$alcohol,data$density,main = 'Alchol vs Density',
     xlab = 'Alchol', ylab = 'Density', col = 'blue')

### Inverse Plotting ###

plot(data$density,data$alcohol, main = 'Density vs Alchol',
     xlab = 'Density', ylab = 'Alchol', type = 'l', col = 'green')

### Sulphates Vs Chlorides plot ###

plot(data$sulphates,data$chlorides, main = 'Sulphates vs Chlorides',
     xlab = 'Sulphates', ylab = 'Chlorides',col = 'yellow')

### Regression between Alcohol & Density ###

model <- lm(density~alcohol, data=data)
summary(model)

### Regression between Alcohol & Density ###

model <- lm(density~alcohol, data=data)
summary(model)

### Predectiopn ###

new_alochol=data.frame=c(11.73,17.5)
pred <- predict.lm(model,newdata = new_alochol)
pred

### Likewise checking the alcohol content with measuring density ###

model <- lm(alcohol~density,data=data)
summary(data)

### Prediction ###

new_density = data.frame(density=c(0.75,0.332,0.99,1.00))
pred1 <- predict.lm(model,newdata = new_density)
pred1

### Similiarly checking for regression for sugar & alochol ###

colnames(WineRed)
x <- data$residual.sugar
model <- lm(alcohol~x,data=data)
summary(model)

### Perdiction ###

new_sugar <- data.frame(x=c(3.0,1.1,0.95,1.2))
predict2 <- predict.lm(model,newdata = new_sugar)
predict2

### Plotting the relation of prediction ###

plot(x,WineRed$alcohol,type = 'b', col = 'green')
lines(predict2,type = 'l', col = 'red')

### Comparing old data with predict ###

predict3 <- predict.lm(model, data=x)
predict3


plot(x,WineRed$alcohol,type = 'b', col = 'yellow')
lines(predict3,type = 'l',col ='green')
