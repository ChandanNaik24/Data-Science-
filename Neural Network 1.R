## Installation of packages ##

install.packages("neuralnet")
install.packages("ggvis")
install.packages("psych")
install.packages("knitr")

## Loading the libraries ##

library(neuralnet)
library(ggvis) 
library(psych) 
library(knitr)

## Loading data set ##

concrete <- read.csv("C:/Users/User/Downloads/concrete.csv")


## Partial Table Review ##

knitr::kable(head(concrete), caption = "Partial Table Preview")

## First & Last ten data sets ##

head(concrete,10)
tail(concrete,10)

## Plots ##

plot(concrete)
plot(concrete$cement)
plot(concrete$ash)

## Box plot ##

boxplot(concrete$cement)
boxplot(concrete$water)

## Bar plot ##

barplot(concrete$superplastic)
barplot(concrete$fineagg)

## Histogram plot ##

hist(concrete$age)
hist(concrete$strength)
concrete %>% ggvis(x = ~strength, fill:= "#27bc9c") %>% layer_histograms() %>% layer_paths(y = ~strength, 35.82, stroke := "red")

## Scatter plot Matricies ##

pairs(concrete[c("cement", "slag", "ash", "strength")])

## Normalisation ##

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
kable(round(head(concrete_norm), digits = 3), caption = "Normalized Data Preview")

## Traning Set ##

concrete_train <- concrete_norm[1:773, ]  
concerete_train

## Test set ##

concrete_test <- concrete_norm[774:1030, ]
concerete_test

## Build a neural network with one hidden layer ##

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age , data = concrete_train, hidden = 1)

## Visulisation Neural Network ##

plot(concrete_model)

## Building the predictor, exclude the target variable column ##

model_results <- compute(concrete_model, concrete_test[1:8])
model_results

## Store the net.results column ##

predicted_strength <- model_results$net.result
predicted_strength

## Model Accuracy ##

cor(predicted_strength, concrete_test$strength)

## Building the new model ##

concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5 )
concrete_model2
plot(concrete_model2)

## Nuilding the new predictor ##

model_results2 <- compute(concrete_model2, concrete_test[1:8])
model_results2

## Predicting Strength ##

predicted_strength2 <- model_results2$net.result
predicted_strength2
