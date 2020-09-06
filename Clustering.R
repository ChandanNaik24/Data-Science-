### Loading Data ###

mydata1<-read.csv("F:/Data Science/R Code dataset/Universities.csv")

### data standardization ###

mydata <- scale(mydata1[,2:7])

### Computing the distance ### 

d <- dist(mydata, method = "euclidean") 

# Building the algorithm # 

fit <- hclust(d, method="average")

# display dendogram ## 

plot(fit)

### cut tree into 4 clusters ###

clusters <- cutree(fit, k=4)

# draw dendogram with red borders around the 4 clusters 

rect.hclust(fit, k=4, border="red")

#Attach the cluster numbers to Uni

Final_output=data.frame('Uni'=mydata1[,1],'Cluster' =clusters)

View(Final_output)


