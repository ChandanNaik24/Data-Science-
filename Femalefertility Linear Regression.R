### Loading the CVS file to R Studio cloud ###

data <- SLR2

### EDA ###

summary(data)
dataframe1 <- summary

### Plot ###

plot(data)
dataframe2 <- plot

### Checking for outliers ###

boxplot(data$worker.percent,horizontal = T)
boxplot(data$fertility.rate, horizontal = T)
boxplot(data$fertility.rate,data$worker.percent, horizontal = T)

### CHecking the Normality ###

qqnorm(data$fertility.rate)
qqnorm(data$worker.percent)

### Assigning Variables ###

y <- data$worker.percent
x <- data$fertility.rate

### Linear Regression ###

model <- lm(y~x, data = data)
summary(model)

### Prediction ###

new_data = data.frame(x=c(3,9,1,2,8))
pred <- predict.lm(model,newdata = new_data)
pred
