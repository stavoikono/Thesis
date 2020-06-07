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

svm_linear <- svm(Purchased~.,data=train,type='C-classification',kernel='linear')

pred_linear <- predict(svm_linear,newdata = test[-3])

cm_linear<- table(pred_linear,test$Purchased)
cm_linear

accuracy_svm_linear <- sum(diag(cm_linear)) / sum(cm_linear)
accuracy_svm_linear

plot(svm_linear,data=test,Age~EstimatedSalary)
summary(svm_linear)
####

svm_radial <- svm(Purchased~.,data=train,type='C-classification',kernel='radial')

pred_radial <- predict(svm_radial,newdata = test[-3])

cm_radial<- table(pred_radial,test$Purchased)
cm_radial


accuracy_svm_radial <- sum(diag(cm_radial)) / sum(cm_radial)
accuracy_svm_radial

summary(svm_radial)
plot(svm_radial,data=test,Age~EstimatedSalary)
