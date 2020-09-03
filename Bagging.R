## Loading the data ##

datasets::iris
data('iris')

## Install the requried packages## 

install.packages("caret")
install.packages("C50")

## Library Invoke ##

library(caret)
library(C50)

## To make results consistent across the run ##

attach(iris)
set.seed(7)

## Data Partition ##

inTraininglocal <- createDataPartition(iris$Species, p = .70, list = F)
training <- iris[inTraininglocal,]
testing <- iris[-inTraininglocal,]

## Model Building ##

model <- C5.0(Species~., data = training)

## Generate the Model Summary ##

summary(model)

## Predict for test Dataset ##

pred <- predict.C5.0(model, testing[-5])
pred

## type = "prob" ##
## Accuracy of the Algorithm ##

a<- table(testing$Species, pred)
sum(diag(a)/sum(a))

## Visualization of Decession Tree ##

plot(model)

## Bagging Algorithm ##
## Using the Ipred bagged decession tree ##

install.packages("ipred")
library(ipred)
set.seed(7)
mybag <- bagging(iris$Species~., data = iris, nbagg = 2)
pred <- predict(mybag, iris)
table(pred, iris$Species)

## Bagging Using Caret Bagged Tress ##

library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
model_cv <- train(Species~., data = iris, method = "treebag", trControl = ctrl)
pred <- predict(model_cv,testing)
table(pred, testing$Species)

