##### Installation of packages #####

install.packages("caTools")
install.packages("psych")

##### Loading the data set #####

COVID19 <- read.csv("F:/Data Science/R Code dataset/COVID State.csv")
View(COVID19)

##### Fisrt 10 columns #####

head(COVID19)
head(COVID19$State,10)
head(COVID19$Confirmed,10)

##### Last 10 columns #####

tail(COVID19,10)
tail(COVID19$Active,10)
tail(COVID19$State,10)

##### Checking outliers #####

boxplot(COVID19$Recovered)
boxplot(COVID19$Deaths)
boxplot(COVID19$Confirmed,main='Confirmed')

##### Plots #####

plot(COVID19)
plot(COVID19$Confirmed, col = 'red')
plot(COVID19$Recovered.1, col = 'green')
plot(COVID19$Active,COVID19$Deaths,type = "p", col = 'red')

##### Bar plot #####

barplot(COVID19$Confirmed, main = "Confirmed",col = 'red')
barplot(COVID19$Deaths, main = "Deaths",col = 'purple')
barplot(COVID19$Recovered,COVID19$Death, main = 'Recovered vs Death',
        xlab = 'State',ylab = 'Confirmed', col = 'blue')
barplot(COVID19$Confirmed,COVID19$Active, main = 'Confirmed vs Active',
        xlab = 'Confirmed', ylab = 'Active', col = 'yellow')

##### Histogram Plot #####

hist(COVID19$Confirmed.1, col = 'blue')
hist(COVID19$Recovered.1, col = 'yellow')
hist(COVID19$Confirmed,xlab = 'Confirmed',col = 'green')

##### Regression MOdel #####

##### Linear regression between Confirmed & Recovered #####

COVID19_model_Linear <- lm(COVID19$Recovered~COVID19$Confirmed)
summary(COVID19_model_Linear)
View(COVID19_model_Linear)
plot(COVID19_model_Linear)
plot
barplot(COVID19_model_Linear)
barplot

##### Multiple Regressions #####

COVID19_model_Multiple1 <- lm(COVID19$Recovered~COVID19$Confirmed)
summary(COVID19_model_Multiple1)

COVID19_model_Multiple2 <- lm(COVID19$Recovered.1~COVID19$Confirmed.1)
summary(COVID19_model_Multiple2)

COVID19_model_Multiple3 <- lm(COVID19$Deaths~COVID19$Active)
summary(COVID19_model_Multiple3)
