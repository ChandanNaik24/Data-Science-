## Loading the data set ##

Forest <- read.csv("C:/Users/User/Downloads/forestfires.csv")
View(Forest)
class(Forest)

## Installation of packages ##

install.packages("neuralnet")
install.packages("nnet")
install.packages("NeuralNetTools")
install.packages("plyr")

library(neuralnet) 
library(nnet)
library(NeuralNetTools)
library(plyr)

## Exploratory data Analysis ##

plot(Forest$month)

## Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) ## 

pairs(Startups)

## Correlation coefficient - Strength & Direction of correlation ##

cor(Startups)
summary(Startups)

## Apply Normalization technique to the whole dataset ##

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit)

## Orginal profit value ##

summary(Startups$profit) 

## Data Partition ##

set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]

## Creating a neural network model on training data ##


startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)
plot(startups_model, rep = "best")
summary(startups_model)

par(mar = numeric(4), family = 'serif')
plotnet(startups_model, alpha = 0.6)

## Evaluating model performance ##

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result
summary(predicted_profit)

## Predicted profit Vs Actual profit of test data ##

cor(predicted_profit,startups_test$Profit)

## since the prediction is in Normalized form, we need to de-normalize it ##
## to get the actual prediction on profit ##

str_max <- max(Startups$Profit)
str_min <- min(Startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

## Improve the model performance ##

set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")
summary(Startups_model2)

model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
summary(predicted_Profit2,startups_test$Profit)

plot(predicted_Profit2,startups_test$Profit)

par(mar = numeric(4), family = 'serif')
plotnet(Startups_model2, alpha = 0.6)
