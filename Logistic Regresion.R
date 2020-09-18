## Installation of packages ##

install.packages("glmnet")
install.packages("class")
ininstall.packages("ggplot2")
install.packages("dplyr")
install.packages("doParallel")

## Loading the libraries ##

library(ggplot2)
library(dplyr)
library(stringr)
library(glmnet)
library(doParallel)
library(class)



## Loading data set ##

termdp<- read.csv(file.choose(),sep = ';')
View(termdp)
str(termdp)
attach(termdp)
summary(termdp)


#### Cheking NA values #### 

na_value<- colSums(is.na.data.frame(termdp))
na_value
term_factor<- as.factor(termdp)


#### plotting the data ###

ggplot(termdp, aes(x= duration, fill = y))+ geom_boxplot(bins = 30)

ggplot(termdp, aes(x= duration, fill = y))+ geom_histogram(bins = 30)

ggplot(termdp, aes(x= age, fill = y))+ geom_histogram(bins = 30)

ggplot(termdp, aes(x= day, fill = y))+ geom_histogram(bins = 30)   

ggplot(termdp, aes(x= balance, fill = y))+ geom_histogram(bins = 30)


#### character data into factor format ####

termdp$job <- as.numeric(as.factor(termdp$job))

termdp$marital <- as.numeric(as.factor(termdp$marital))

termdp$education <- as.numeric(as.factor(termdp$education))

termdp$default<- ifelse(termdp$default == "yes", 1, 0)

termdp$housing <- ifelse(termdp$housing== "yes", 1, 0)  
termdp$loan<- ifelse(termdp$loan== "yes", 1, 0)

termdp$month <- as.numeric(as.factor(termdp$month))

termdp$contact <- as.numeric(as.factor(termdp$contact))

termdp$poutcome <- as.numeric(as.factor(termdp$poutcome))

termdp$y <- ifelse(termdp$y== "yes", 1, 0)


#### normalization ####

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
bank<- as.data.frame(lapply(termdp,normalize))


### Creating design matrix and target vector ###

mydata.X <- model.matrix(y ~ -1+., data= termdp )
mydata.X <- as.data.frame(mydata.X)
mydata.Y <- bank$y


### Now we split the data into training and test ###

cuts <- c(training = .8, test = .2)  
g <- sample(cut(seq(nrow(mydata.X)), nrow(mydata.X)*cumsum(c(0,cuts)), labels = names(cuts)))
final.X <- split(mydata.X, g)
final.Y <- split(mydata.Y, g)


#### Ridge regression method ####

bank.ridge <- cv.glmnet(x= as.matrix(final.X$training), y = as.matrix(final.Y$training), nfolds=10,
type.measure="class", family='binomial', alpha = 0, nlambda=100)
print(bank.ridge$lambda.min)
plot(bank.ridge)   


### Create a dataframe with the coefficient values ###

ridge.coefs <- as.data.frame(as.vector(coef(bank.ridge, s = bank.ridge$lambda.min)),
row.names = rownames(coef(bank.ridge)))
names(ridge.coefs) <- 'coefficient'


### regresssion using LASSO ###

bank.lasso <- cv.glmnet(x = as.matrix(final.X$training), y = as.matrix(final.Y$training), nfolds=10,   
type.measure="class", parallel=TRUE, family='binomial', alpha = 1, nlambda=100)
print(bank.lasso$lambda.min) 
plot(bank.lasso)


### Creates a new matrix with only the non-zero features ###
### Create a dataframe with the coefficient values ###

lasso.coefs <- as.data.frame(as.vector(coef(bank.lasso, s = bank.lasso$lambda.min)),
row.names = rownames(coef(bank.lasso)))
print(lasso.coefs)
lasso_bank <- bank[, intersect(colnames(bank),features)]   
names(lasso.coefs) <- 'coefficient'
features <- rownames(lasso.coefs)[lasso.coefs != 0]
print(features)


### Re-do the split into training and test ###

bank <- as.matrix(lasso_bank)
bank <- as.data.frame(bank)
bank$Y <- mydata.Y  
bank_1 <- split(bank, g)


### standard logistic regression ###

model_std <- glm(Y ~ ., family = binomial(link = "logit"), data = bank_1$training)
summary(model_std) 


### Prediction and misclassification ###

predictions <- predict.glm(model_std, newdata=bank_1$test, type= "response")
predictions[predictions > 0.5] <- 1
predictions[predictions <= 0.5] <- 0  
matrix<- table(predictions, bank_1$test$Y) 


##### Accuracy = 0.88%

redictions <- predict.glm(model_std, newdata=bank_1$test, type= "response")
predictions[predictions > 0.1] <- 1
predictions[predictions <= 0.1] <- 0  
matrix<- table(predictions, bank_1$test$Y)
matrix 






