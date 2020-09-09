#### Instalation of Packages #####

install.packages("ggplot2")
install.packages("kernlab")
install.packages("corrplot")
install.packages("car")
install.packages("perturb")
install.packages("caret")
install.packages("plyr")
install.packages("e1071")
install.packages("caTools")

##### Loading the libraries #####

library('ggplot2')
library('kernlab')
library('corrplot')
library('car')
library('perturb')
library('caret')
library('plyr')
library('caTools')

##### Loading the dataset #####

Car <- read.csv("C:/Users/User/Downloads/Cars.csv")
View(Car)

##### First 10 row's #####

head(Car,10)
head(Car$HP,10)
head(Car$MPG,10)

##### Last 10 row's #####

tail(Car,10)
tail(Car$VOL,10)
tail(Car$SP,10)

##### EDA #####

mean(Car$HP)
mean(Car$MPG)
mean(Car$VOL)
mean(Car$SP)
mean(Car$WT)

##### Plots #####

plot(Car)
plot(Car$HP, main = "HP", col = 'red')
plot(Car$MPG,main = "MPG", col = 'blue')
plot(Car$VOL, main = "VOL", col = 'green')
plot(Car$HP, Car$MPG, main = "HP vs MPG", xlab = 'HP', ylab = 'MPG', col = 'red')
plot(Car$VOL, Car$WT, main = "VOL vs WT", xlab = 'VOL', ylab = 'WT', col = 'blue')

##### Checking for outliers #####

boxplot(Car)
boxplot(Car$HP)
boxplot(Car$MPG)
boxplot(Car$VOL)

##### Bar plot's #####

barplot(Car$HP, main = "HP", col = 'blue')
barplot(Car$MPG, main = "MPG", col = 'purple')
barplot(Car$VOL,main = "VOL",col = 'green')
barplot(Car$SP, main = "SP", col = 'yellow')
barplot(Car$HP, Car$MPG, main = "HP vs MPG", xlab = "HP", ylab = "MPG", col = 'red')
barplot(Car$VOL, Car$SP, main = "VOL vs SP", xlab = "VOL", ylab = "SP", col = 'green')

##### Histogram Plot #####we

hist(Car$WT, main = "WT", col = 'orange')
hist(Car$SP, main = "SP", col = 'maroon')
hist(Car$VOL, main = "VOL", col = 'yellow')

##### Checking summary of any missing values #####

sum(is.na(Car))
set.seed(1234)
Car<- mutate(Car, y= log(HP+1))
hist(Car$y, col = 'pink')
View(Car)
summary(Car)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

Car$HP = normalize(Car$HP)
Car$MPG = normalize(Car$MPG)
Car$VOL = normalize(Car$VOL)
Car$SP = normalize(Car$SP)
Car$WT = normalize(Car$WT)


##### Visualization and univarient, Bivarient plot #####

ggplot(data=Car_train,aes(x=Car_train$HP, y = Car_train$MPG, fill = Car_train$HP)) +
geom_boxplot() +
ggtitle("Box Plot")

ggplot(data=Car_train,aes(x=Car_train$HP, y = Car_train$VOL, fill = Car_train$HP)) +
geom_boxplot() +
ggtitle("Box Plot")

ggplot(data=Car_train,aes(x=Car_train$HP, y = Car_train$SP, fill = Car_train$HP)) +
geom_boxplot() +
ggtitle("Box Plot")

ggplot(data=Car_train,aes(x=Car_train$HP, y = Car_train$WT, fill = Car_train$HP)) +
geom_boxplot() +
ggtitle("Box Plot")


##### Linear Regression between HP & MPG #####

CarLinear1 <- lm(MPG~HP, data = Car)
summary(CarLinear1)

##### Linear Regression between VOL & SP #####

CarLinear2 <- lm(SP~VOL, data = Car)
summary(CarLinear2)

##### Multi Regression Model #####

modelCar1 <- lm(HP~MPG+VOL+SP+WT, data = Car)
summary(modelCar1)

modelCar2 <- lm(HP~., data = Car)
summary(modelCar2)

modelCar3 <- lm(MPG~ HP+VOL+SP+WT, data = Car)
summary(modelCar3)

modelCar4 <- lm(VOL~ HP+MPG+SP+WT, data = Car)
summary(modelCar4)

modelCar5 <- lm(SP~ HP+MPG+VOL+WT, data = Car)
summary(modelCar5)

modelCar6 <- lm(WT~ HP+MPG+VOL+SP, data = Car)
summary(modelCar6)

##### standard logistic regression #####

model_std_Car <- glm(HP~, family = binomial(link = "logit"), data = Car)
summary(model_std_Car) 

##### Prediction and misclassification #####

predictions <- predict.glm(model_std_Car,data=Car$HP, type= "response")
predictions[predictions > 0.5] <- 1
predictions[predictions <= 0.5] <- 0  
matrix<- table(predictions, Car$HP) 
matrix

##### Accuracy = 0.88% #####

predictions <- predict.glm(model_std_Car, data=Car$HP, type= "response")
predictions[predictions > 0.1] <- 1
predictions[predictions <= 0.1] <- 0  
matrix<- table(predictions, Car$HP)
matrix
