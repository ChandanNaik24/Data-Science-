#### Loading the dataset ####

Wine <- read.csv("C:/Users/User/Downloads/wine.csv")
View(Wine)

#### First & Last 10 values ####

head(Wine, 10)
head(Wine$Type)
head(Wine$Alcohol)
tail(Wine$Malic)
tail(Wine$Ash)

#### Plots ####

plot(Wine$Type)
plot(Wine$Alcalinity)
plot(Wine$Dilution)

#### Box Plot to check putliers ####

boxplot(Wine$Alcohol)
boxplot(Wine$Type)
boxplot(Wine$Magnesium)

#### Bar Plot ####

barplot(Wine$Flavanoids, col = 'blue')
barplot(Wine$Phenols, col = 'orange')
barplot(Wine$Hue, col = 'yellow')

#### Histogram Plot ####

hist(Wine$Alcohol, col = 'green')
hist(Wine$Ash, col = 'purple')
hist(Wine$Color, col = 'red')

#### The first column in mydata has university names ####

View(Wine[-1]) 

#### Wine[-1] -> Considering only numerical values for applying PCA ####

data1 <- Wine[,-1]
attach(data1)
cor(data1)

#### cor = TRUE use correlation matrix for getting PCA scores ####

?princomp
pcaWine<-princomp(data1, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaWine)

#### princomp(data1, cor = TRUE) not_same_as prcomp(data1, scale=TRUE); similar, but different ####

summary(pcaWine)
str(pcaWine)
loadings(pcaWine)

#### graph showing importance of principal components ####

plot(pcaWine)

#### Comp.1 having highest importance (highest variance) ####

biplot(pcaWine)

#### Showing the increase of variance with considering principal components ####
#### Which helps in choosing number of principal components ####

plot(cumsum(pcaWine$sdev*pcaWine$sdev)*100/(sum(pcaWine$sdev*pcaWine$sdev)),type="b")

#### Top 3 PCA Scores which represents the whole data ####

pcaWine$scores[,1:3]

#### cbind used to bind the data in column wise ####
#### Considering top 3 principal component scores and binding them with data2 b ####

data2 <-cbind(data1,pcaWine$scores[,1:3])
View(data2)

#### preparing data for clustering (considering only pca scores as they represent the entire data) ####

clus_data <-data2[,8:10]

#### Normalizing the data ####
#### Scale function is used to normalize data ####

norm_clus<-scale(clus_data)

#### method for finding the Elucidian distance ####

dist1<-dist(norm_clus,method = "euclidean")
summary(dist1)

#### Clustering the data using hclust function --> Hierarchical ####

fit1<-hclust(dist1,method="complete")
summary(fit1)

### # Displaying Dendrogram ####

plot(fit1)

#### Cutting the dendrogram for 5 clusters ####

groups<-cutree(fit1,5)

#### cluster numbering ####

membership_1<-as.matrix(groups)
View(membership_1)

#### binding column wise with orginal data ####

final1<-cbind(wine)
View(final1)

#### Inferences can be drawn from the aggregate of the Wine data on membership_1 ####

View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean))

write.csv(final1,file ="Wine_clustered.csv",row.names = F,col.names = F)
getwd()
