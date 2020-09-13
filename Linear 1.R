## Loading the data set ##
  
MyData <- read.csv("F:/Calories.csv")
  
## Plots ##
  
plot(Calorie)
plot(Calorie$Weight.Gained..grams.)
plot(Calorie$Calories.Consumed)

## Box plot ##

boxplot(Calorie$Weight.Gained..grams.)
boxplot(Calorie$Calories.Consumed)


## Bar Plot ##

barplot(Calorie$Weight.Gained..grams., main = 'Weight Gained', col = 'red')
barplot(Calorie$Calories.Consumed, main = 'Calorie Consumed', col = 'green')
barplot(Calorie$Weight.Gained..grams., Calorie$Calories.Consumed, main = 'Weight Gained vs Calorie     Consumed',
        xlab = 'Weight Gained', ylab = 'Calorei Consumed', col = 'purple')


## Histogram plot ##

hist(Calorie$Weight.Gained..grams., main = 'Weight Gained', col = 'blue')
hist(Calorie$Calories.Consumed, main = 'Calories Consumed', col = 'yellow')

## Regression Model ##

model <- lm(Weight.gained..grams.~Calories.Consumed) 
summary(model)

# # Logrithamic Model##

reg_log <- lm(Weight.gained..grams. ~ log(Calories.Consumed)) 
summary(reg_log), 

# Exponential Model 
reg_exp <- lm(log(Weight.gained..grams.)~Calories.Consumed) 
summary(reg_exp) 


  