
datasets::airquality

airquality <- datasets::airquality

##### Column #####
### 1st 10 rows and last 10 rows #####

head(airquality)
head(airquality,10)

tail(airquality)
tail(airquality,10)

###### Rows & Columns #####

airquality[,c(1,2)] 
airquality[c(2,4),]
airquality[,c(1,2,3)]
airquality[c(1,2,3),]

df1 <-airquality[,c(1,2,3)]
df2 <- airquality[c(1,2,3),]
df3 <- head(airquality,10)
df4 <- tail(airquality,10)

### Summary ####

summary(airquality)
summary(airquality[,c(2,4)])
df5 <- summary(airquality)
summary(airquality,4)
df6 <- summary(airquality,4)

### Selecting the particular column #####
airquality$Ozone
summary(airquality$Ozone)

##### Plots #####
plot(airquality$Ozone)
plot(airquality$Wind)
plot(airquality$Ozone,airquality$Wind)
plot(airquality$Wind, type= "l") # p: points, l: lines,b: both 
plot(airquality$Wind, xlab = 'ozone Concentration',ylab = 'No of Instances', main = 'Ozone levels in NY city', col = 'blue')

##### Bar Plots ####
barplot(airquality$Temp)
barplot(airquality$Day)
barplot(airquality$Ozone)
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',ylab = 'ozone levels', col= 'blue',horiz = F,axes=T)

##### Histogram Plot ####
hist(airquality$Temp)
hist(airquality$Temp, main = 'Solar Radiation values in air', xlab = 'Solar rad.', col='yellow')

##### Box Plot ######
boxplot(airquality$Month)
boxplot(airquality[,1:4],main='Multiple')
par(mfrow=c(3,3),mar=c(2,5,2,1),  las=0, bty="o") 

#### ALL the PLots #######
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality$Ozone, type= "l") 
plot(airquality$Ozone, type= "l")
plot(airquality$Ozone, type= "l")
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',xlab = 'ozone levels', col='green',horiz = TRUE)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')   
