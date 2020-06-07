library(class)

binary <- read.csv("C:/Users/tit0v/R/PTUXIAKH/binary.csv")

binary$admit <- as.factor(binary$admit)

set.seed(1234)
ind <- sample(2,nrow(binary),replace=T,prob=c(0.8,0.2))
train<- binary[ind==1,]
test <- binary[ind==2,]

train[,-1] <- apply(train[,-1], 2, function(x) (x-min(x))/(max(x)-min(x)))
test[,-1] <-  apply(test[,-1], 2, function(x) (x-min(x))/(max(x)-min(x)))
train$rank<- as.factor(train$rank)
test$rank <- as.factor(test$rank)

knn_pred <- knn(train[,-1],test[,-1], cl=train[,1], k=5)

knn_cm <- table(knn_pred,test$admit)
knn_cm

accuracy_knn <- sum(diag(knn_cm)) / sum(knn_cm)
accuracy_knn
