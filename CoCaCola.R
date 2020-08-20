###### Installation of packages ######

install.packages("readr")
library(readr)

###### Loading the dataset ######

CoCaCola <- read.csv("C:/Users/User/Downloads/CocaCola_Sales_Rawdata.csv")
View(CoCaCola)
windows()
plot(CoCaCola$Sales,type="o")

###### Creating 4 Quarters ######

Q1 <-  ifelse(grepl("Q1",CoCaCola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CoCaCola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CoCaCola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CoCaCola$Quarter),'1','0')

###### Creating dummies for 12 months ######

CoCaCola<-cbind(CoCaCola,Q1,Q2,Q3,Q4)
View(CoCaCola)
colnames(CoCaCola)

CoCaCola["t"]<- 1:42
View(CoCaCola)
CoCaCola["log_Sales"]<-log(CoCaCola["Sales"])
CoCaCola["t_square"]<-CoCaCola["t"]*CoCaCola["t"]
attach(CoCaCola)

###### Train & Test Model ######

train<-CoCaCola[1:36,]

test<-CoCaCola[37:40,]

###### Linear Model ######

linear_model_CoCaCola1<-lm(Sales~t,data=train)
summary(linear_model_CoCaCola1)

###### Prediction of Linear Model ######

linear_pred<-data.frame(predict(linear_model_CoCaCola1,interval='predict',newdata =test))
View(linear_pred)

###### RMSE Linear Model ######

rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 

###### Exponential Model ######

expo_model_CoCaCola2<-lm(log_Sales~t,data=train)
summary(expo_model_CoCaCola2)

###### Prediction of Exponential Model ######

expo_pred<-data.frame(predict(expo_model_CoCaCola2,interval='predict',newdata=test))
summary(expo_pred)

###### RMSE of Expo Model ######

rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

###### Quadratic Model ######

Quad_model_CoCaCola3<-lm(Sales~t+t_square,data=train)
summary(Quad_model_CoCaCola3)

###### Prediction Quadratic Model ######

Quad_pred<-data.frame(predict(Quad_model_CoCaCola3,interval='predict',newdata=test))
summary(Quad_pred)

###### RMSE Quad ######

rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

###### Additive Seasonality ######

sea_add_model_CoCaCola001<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model_CoCaCola001)

###### Prediction of Add Season ######

sea_add_pred<-data.frame(predict(sea_add_model_CoCaCola001,newdata=test,interval='predict'))
summary(sea_add_pred)

###### RMSE of Add Season ######

rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

###### Additive Seasonality with Linear Model ######

Add_sea_Linear_model_CoCaCola4<-lm(Sales~+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model_CoCaCola4)

###### Prediction Additive Seasonality Linear ######

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model_CoCaCola4,interval='predict',newdata=test))
summary(Add_sea_Linear_pred)

###### RMSE of Add Seasonality Linear ######

rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

###### Additive Seasonality with Quadratic ######

Add_sea_Quad_model_CoCaCOla5<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model_CoCaCOla5)

###### Prediction of Add Season Quad ######

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model_CoCaCOla5,interval='predict',newdata=test))
summary(Add_sea_Quad_pred)

###### RMSE of Add Sea Quad ######

rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

###### Mutiplicative Seaconality ######

multi_sea_model_CoCaCola6<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model_CoCaCola6)

###### Prediction of Multi Season ######

multi_sea_pred<-data.frame(predict(multi_sea_model_CoCaCola6,newdata=test,interval='predict'))
summary(multi_sea_pred)

###### RMSE of Multi Season ######

rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

###### Multiplicative Seasonality Linear trend ######

multi_add_sea_model_CoCaCola7<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model_CoCaCola7) 

###### Prediction Multi Season Linear ######

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model_CoCaCola7,newdata=test,interval='predict'))
summary(multi_add_sea_pred)

###### RMSE Multi Season Linear ######

rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 

###### Preparing table on model and it's RMSE values ######

table_rmse1<-data.frame(c("rmse_linear ","rmse_expo","rmse_Quad","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),
                       c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse1)
colnames(table_rmse1)<-c("model","RMSE")
View(table_rmse1)

###### Additive seasonality with Quadratic has least RMSE value ######

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CoCaCola)
new_model_pred<-data.frame(predict(new_model,newdata=CoCaCola,interval='predict'))

new_model_fin <- new_model$fitted.values

View(new_model_fin)


###### pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12) ######

Quarter <- as.data.frame(CoCaCola$Quarter)

Final <- as.data.frame(cbind(Quarter,CoCaCola$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 
