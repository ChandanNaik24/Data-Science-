### Loading the CSV file on R studio cloud ###

data <- read.csv('NewspaperData.csv')
data

### Visualization ###

install.packages("lattice")
library(lattice)

### Dot plot of Sunday & Daily Circulation ###

dotplot(ND$sunday, main = "Dot plot of sunday circulation", col= 'blue')
dotplot(ND$daily, main = "Dot plot of daily circulation", col= 'green')

### Checking the summary of the data set ###

summary(data)

### Boxplot for checking outliers ###

boxplot(ND$sunday, col = 'blue')
boxplot(ND$daily, col = 'orange')

### Plot of the dataset ###

plot(ND$sunday,ND$daily,main = "Newspaper", xlab = 'sunday', ylab = 'daily', col = 'brown')

### Barplot ###

barplot(ND$sunday,main = "Newspaper",xlab = 'sunday', ylab = 'daily',col = 'yellow')
barplot(ND$daily, col = "purple")

### Maximum Fnc ###

max(ND$sunday)
max(ND$daily)

### Minimum Fnc ###

min(ND$sunday)
min(ND$daily)

### Creating Regression Model ###

model <- lm(sunday~daily, data = ND)
summary(model)
new_daily = data.frame(daily = c(200,300))
sun1 = predict(model,newdata = new_daily)
sun1

### Prediction ###

pred <- predict(model)
pred

