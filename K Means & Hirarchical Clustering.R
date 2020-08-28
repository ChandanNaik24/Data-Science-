##### installation packages #####

install.packages("animation")

##### Loading dataset #####

Wine1 <- read.csv("C:/Users/User/Downloads/wine.csv")

##### First & Last 10 values #####

head(Wine1,10)
head(Wine1$Alcohol,10)
head(Wine1$Ash,10)
tail(Wine1,10)
tail(Wine1$Alcalinity,10)
tail(Wine1$Phenols,10)

##### Plots #####

plot(Wine1$Alcohol, main = "Alcohol", col = 'red')
plot(Wine1$Ash, main = "Ash",col = 'blue')
plot(Wine1$Alcohol, Wine1$Ash, main = "Alcohol vs Ash",
     xlab = 'Alcohol', ylab = 'Ash', col = 'green')

##### Bar Plot #####

barplot(Wine1$Malic, main = "Malic", col = 'orange')
barplot(Wine1$Alcalinity, main = "Alcalinity", col = 'red')
barplot(Wine1$Malic, Wine1$Alcalinity, main = "Malic vs Alcalinity", 
        xlab = 'Malic', ylab = 'Alcalinity', col = 'green')

##### Checking for Outliers #####

boxplot(Wine1$Alcohol)
boxplot(Wine1$Proline)
boxplot(Wine1$Phenols)

##### Histogram plot #####

hist(Wine1$Alcohol,main = "Alcohol", col = 'blue')
hist(Wine1$Ash, main = "Ash",col = 'purple')
hist(Wine1$Magnesium, main = "Magnesium", col = 'yellow')

##### The first column in Wine1 #####

View(Wine1[-1]) 

#### Wine[-1] -> Considering only numerical values for applying PCA ####

Wine1data1 <- Wine1[,-1]
attach(Wine1data1)
cor(Wine1data1)

#### cor = TRUE use correlation matrix for getting PCA scores ####

?princomp
pcaWine1<-princomp(Wine1data1, cor = TRUE, scores = TRUE, covmat = NULL)
str(pcaWine1)

#### princomp(data1, cor = TRUE) not_same_as prcomp(data1, scale=TRUE); similar, but different ####

summary(pcaWine1)
str(pcaWine1)
loadings(pcaWine1)

#### Graph showing importance of principal components ####

plot(pcaWine1)

#### Comp.1 having highest importance (highest variance) ####

biplot(pcaWine1)

##### K means Clustering #####

pcaWine1$scores[,1:3]
wine_data <-cbind(Wine1,pcaWine1$scores[,1:3]) 
View(wine_data)

library('animation') 
normalised_data<- scale(wine_data) 
View(normalised_data) 

km<- kmeans.ani(normalised_data)
wss= (nrow(normalised_data)-1)*(sum(apply(normalised_data,2,var))) 
for (i in 1:3) wss[i] = sum(kmeans(normalised_data, centers=i)$withinss) 
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
title(sub = "K-Means Clustering Scree-Plot") 

##### Hierarchial Clustering #####

View(Wine1)
summary(pcaWine1) 
str(pcaWine1) 
loadings(pcaWine1) 
plot(pcaWine1) 
biplot(pcaWine1)

pcaWine1$scores[,1:3] 
Wine1data2 <-cbind(Wine1,pcaWine1$scores[,1:3]) 
View(Wine1data2) 
clus_data <- Wine1data2[,8:10] 
nom_clust <- scale(clus_data) 

##### Eucliidean Distance #####

dist1 <- dist(nom_clust, method= "euclidean") 
fit1<-hclust(dist1,method="complete") 
plot(fit1)


groups<-cutree(fit1,4) 
cluster<-as.matrix(groups) 
View(cluster) 
final1<-cbind(cluster,Wine1data2) 
View(final1)








