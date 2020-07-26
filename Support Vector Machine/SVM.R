library(e1071)

dataset <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Social_Network_Ads.csv")
dataset <- dataset[3:5]
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

set.seed(1234)
ind <- sample(2,nrow(iris),replace=T,prob=c(0.8,0.2))
train<- dataset[ind==1,]
test <- dataset[ind==2,]

train[-3] = scale(train[-3]) 
test[-3] = scale(test[-3]) 

### SVM Linear

svm_linear <- svm(Purchased~.,data=train,type='C-classification',kernel='linear')

pred_linear <- predict(svm_linear,newdata = test[-3])

cm_linear<- table(pred_linear,test$Purchased)
cm_linear

accuracy_svm_linear <- sum(diag(cm_linear)) / sum(cm_linear)
accuracy_svm_linear

plot(svm_linear,data=test,Age~EstimatedSalary)
summary(svm_linear)

confusionMatrix(pred_linear,test$Purchased)


#### SVM RADIAL

svm_radial <- svm(Purchased~.,data=train,type='C-classification',kernel='radial')

pred_radial <- predict(svm_radial,newdata = test[-3])

cm_radial<- table(pred_radial,test$Purchased)
cm_radial


accuracy_svm_radial <- sum(diag(cm_radial)) / sum(cm_radial)
accuracy_svm_radial

summary(svm_radial)
plot(svm_radial,data=test,Age~EstimatedSalary)


confusionMatrix(pred_radial,test$Purchased)


#### SVM SIGMOID

svm_sigm <- svm(Purchased~.,data=train,type='C-classification',kernel='sigmoid')

pred_sigm <- predict(svm_sigm,newdata = test[-3])

cm_sigm<- table(pred_sigm,test$Purchased)
cm_sigm


accuracy_svm_sigm <- sum(diag(cm_sigm)) / sum(cm_sigm)
accuracy_svm_sigm

summary(svm_sigm)
plot(svm_sigm,data=test,Age~EstimatedSalary)


confusionMatrix(pred_sigm,test$Purchased)


#### SVM POLYNOMIAL

svm_3d <- svm(Purchased~.,data=train,type='C-classification',kernel='polynomial')

pred_3d <- predict(svm_3d,newdata = test[-3])

cm_3d<- table(pred_3d,test$Purchased)
cm_3d


accuracy_svm_3d <- sum(diag(cm_3d)) / sum(cm_3d)
accuracy_svm_3d

summary(svm_3d)
plot(svm_3d,data=test,Age~EstimatedSalary)


confusionMatrix(pred_3d,test$Purchased)


########## CV

set.seed(1928)
cvfolds2 <- createMultiFolds(dataset$Purchased ,k=5,times=20)

tcontrol2 <- trainControl(method = "repeatedcv", number = 5, repeats = 20, index=cvfolds2)

svml2 <- train(Purchased ~ ., data = dataset, method = "svmLinear", trControl = tcontrol2)

svml2$results
t.test(svml2$resample$Accuracy)
summary(svml2$resample$Accuracy)

svmr2 <- train(Purchased ~ ., data = dataset, method = "svmRadial", trControl = tcontrol2)

svmr2$results
t.test(svmr2$resample$Accuracy)
summary(svmr2$resample$Accuracy)

svmp2 <- train(Purchased ~ ., data = dataset, method = "svmPoly", trControl = tcontrol2,
               tuneGrid = data.frame(degree=3,scale=0.100,C=1))
svmp2$results
t.test(svmp2$resample$Accuracy)
summary(svmp2$resample$Accuracy)