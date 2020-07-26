library(naivebayes)

binary <- read.csv("C:/Users/tit0v/R/PTUXIAKH/binary.csv")

binary$admit <- as.factor(binary$admit)
binary$rank <- as.factor(binary$rank)

set.seed(1234)
ind <- sample(2,nrow(binary),replace=T,prob=c(0.8,0.2))
train<- binary[ind==1,]
test <- binary[ind==2,]

nb_model <- naive_bayes(admit~.,data=train)
summary(nb_model)


nb_pred <- predict(nb_model, newdata = test, type='class')
nb_pred2 <- predict.naive_bae


cm_nb <- table(nb_pred,test$admit)
cm_nb

accuracy_nb <- sum(diag(cm_nb)) / sum(cm_nb)
accuracy_nb

set.seed(1928)
cvfolds1 <- createMultiFolds(binary$admit,k=5,times=20)

tcontrol1 <- trainControl(method = "repeatedcv", number = 5, repeats = 20, index=cvfolds1)

nb2 <- train(admit ~ ., data = binary, method = "nb", trControl = tcontrol1, 
             tuneGrid = data.frame(fL=0, usekernel=T, adjust = 1))

nb2$results
t.test(nb2$resample$Accuracy)
summary(nb2$resample$Accuracy)