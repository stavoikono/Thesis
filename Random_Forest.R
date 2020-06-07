library(randomForest)
data("iris")

ind_rf <- sample(2,nrow(iris),replace=T,prob=c(0.8,0.2))
train_rf<- iris[ind_rf==1,]
test_rf <- iris[ind_rf==2,]

rf_model <- randomForest(Species~., data = train_rf, ntree=500, mtry=2)
pred_rf <- predict(rf_model,test_rf)

cm_rf<-table(pred_rf,test_rf$Species)
cm_rf

rf_accuracy <- sum(diag(cm_rf)) / sum(cm_rf)
rf_accuracy
