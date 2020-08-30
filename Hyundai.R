##### Installing the packages #####

install.packages("caTools")
install.packages("ggplot2")
install.packages("glmnet")


##### Loading libraries #####

library("caTools")
library("ggplot2")
library("glmnet")
l

##### Loading datasset #####

Hyundai <- read.csv("F:/Huyndai.csv")
View(Hyundai)
summary(Hyundai)

##### Explaintary Data Analysis #####

##### First 10 columns #####

head(Hyundai,10)
head(Hyundai$price, 10)
head(Hyundai$transmission,10)

##### Last 10 columns #####

tail(Hyundai,10)
tail(Hyundai$engineSize,10)
tail(Hyundai$tax.Â..,10)

##### Plot #####

plot(Hyundai$price, main = "Price", col = "blue")
plot(Hyundai$mileage, main = "Milegae", col = "red")
plot(Hyundai$price, Hyundai$mileage, main = "Price vs Mileage",
     xlab = "Mileage", ylab = "Price", col = "purple")
plot(Hyundai$mpg, Hyundai$mileage, main = "MPG vs Milegae",
     xlab = "MPG", ylab = "Mileage", col = "darkgreen")

##### Checking outlier #####

boxplot(Hyundai$price)
boxplot(Hyundai$mileage)
boxplot(Hyundai$tax.Â..)

##### Bar plot #####

barplot(Hyundai$price, main = "Price")
barplot(Hyundai$mileage, main = "mileage")
barplot(Hyundai$mpg, main = "MPG")

##### Histogram Plot #####

hist(Hyundai$price, main = "Price", col = "red")
hist(Hyundai$mileage, main = "Mileage", col = "green")
hist(Hyundai$tax.Â.., main = "Tax", col = "purple")


##### Model Building #####

split <- sample.split(Hyundai, SplitRatio = 0.7)
split   

##### Creatinf Train & Test Dataset #####

train <- subset(Hyundai, split== 'TRUE')

test <- subset(train <- subset(Hyundai, split== 'TRUE')
               
##### Linear Regression #####
               
HyunLinear1 <- lm(mileage~price, data = Hyundai)
summary(HyunLinear1)
               
HyunLinear2 <- lm(mpg~mileage, data = Hyundai)
summary(HyunLinear2)
               
HyunLinear3 <- lm(year~model, data = Hyundai)
summary(HyunLinear3)
               
Hyunlinear4 <- lm(price~model, data = Hyundai)
summary(Hyunlinear4)
               
##### Multiple Regression #####
               
HyunMult1 <- lm(price~.,data = Hyundai) 
summary(HyunMult1) 
               
HyunMult2 <- lm(mileage~., data = Hyundai)
summary(HyunMult2)
               
HyunMult3 <- lm(year~., data = Hyundai)
summary(HyunMult3)


               

