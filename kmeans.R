
library(ggplot2)
library(cluster) 
library(factoextra)
data("USArrests")

k2 <- kmeans(USArrests, centers = 2, nstart = 25)

clust <- as.factor(k2$cluster)
centers <- as.data.frame(k2$centers[,1:2])

ggplot(USArrests,aes(x=Assault,y=Murder,colour=clust)) + geom_point() + theme_bw() + 
  geom_point(aes(centers[1,2],centers[1,1]), colour='Blue', size=4) +
  geom_point(aes(centers[2,2],centers[2,1]), colour='Magenta', size=4)


fviz_nbclust(USArrests, kmeans, method = "wss")
