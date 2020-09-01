### Loading the Dataset ###

equakes <- datasets::quakes

### Exploring the first 10 dataset's ###

head(quakes,10)
dataframe1 <- head(quakes,10)

### Exploring the Last 10 Dataset's ###

tail(quakes,10)
dataframe2 <- tail(quakes,10)

### Summary of Dataet ###

summary(equakes)
dataframe3 <- summary(equakes)

### Selection of particular column

equakes$lat
head(equakes$lat,10)
tail(equakes$lat,10)

### Plotting dataset graph ###

plot(equakes)

### Plotting boxplot for each variable to figure out the outliers ###

#Latitude#
boxplot(equakes$lat)

#Magnitude#
boxplot(equakes$mag)

#Longitude#
boxplot(equakes$long)

#Depth#
boxplot(equakes$depth)

#Stations#
boxplot(equakes$stations)

### Plotting Hitogram plot ###
#Depth#
hist(equakes$depth)
hist(equakes$depth, main = 'depth histogram',xlab ='depth',col = 'green')

### Plotting Histogram Plot ###
#Stations#
hist(equakes$stations)
hist(equakes$stations, main = 'stations histogram',xlab ='stations',col = 'purple')

### Plotting relationship between Longitude and Magnitude ###

plot(equakes$long,equakes$mag, main = 'Longitude vs Magnitude', 
     xlab = 'Longitude', ylab = 'Magnitude',col = 'cyan')

### Extrating values from 440 to 563 ###

equakes[440:563,]
dataframe4 <- equakes[440:563,]

### Seeking maximum & minium value of Stations ###

max(equakes$stations)
min(equakes$stations)

### Relation between Latitude & Magnitude ###

plot(equakes$lat,equakes$mag, xlab = 'Latitude', ylab = 'Mgnitude', type = 'p')

### Simple Plot ###

plot(equakes$depth, type = 'l')
plot(equakes$stations, type = 'l')
plot(equakes$depth,equakes$stations, type = 'p',
     main = 'depth vs stations', xlab = 'depth', ylab = 'stations', col='orange')

### Compiling all the plots in one freame ###

par(mfrow=c(3,3),mar=c(2,5,2,2), las=0, bty='n')
plot(equakes$lat,equakes$depth,main = 'Lat vs Dpth')
plot(equakes$long,equakes$mag,main = 'Long vs Mag')
plot(equakes$stations,equakes$mag,main = 'Stat vs Mag')
plot(equakes$lat,equakes$mag,main = 'Lat vs Mag')
plot(equakes$long,equakes$depth,main = 'Long vs Dpth')
plot(equakes$long,equakes$stations,main = 'Long vs Dpth')
plot(equakes$lat,equakes$stations,main = 'Lat vs Stat')
plot(equakes$lat,equakes$long)
plot(equakes$depth,equakes$mag,main = 'Dpth vs Mag')
