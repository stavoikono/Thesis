mydata <- read.csv("C:/Users/tit0v/R/PTUXIAKH/utilities.csv")

scaled <- as.data.frame(scale(mydata[,-1]))

dist_mat <- dist(scaled, method = 'euclidean')
hclust_com <- hclust(dist_mat, method = 'complete')
plot(hclust_com,labels=mydata$Company)
rect.hclust(hclust_com , k = 3, border = 2:6)


wss <- (nrow(scaled)-1)*sum(apply(scaled,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(scaled, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", col="red",
     main="The Elbow Rule")

