library(dplyr)
library(caret)
library(randomForest)
library(MASS)
library(klaR)
library(kernlab)
library(AMR)
library(factoextra)
library(pca3d)

can <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Breast_Cancer_Wisconsin.csv")

head(can)
str(can)
summary(can)
tail(can)
sapply(can, function(x) sum(is.na(x)))

can <- can[,c(-1,-33)]

set.seed(1234)
indi <- sample(2,nrow(can),replace=T,prob=c(0.8,0.2))
trainset<- can[indi==1,]
testset <- can[indi==2,]

### Cross validation - We will be using K- fold cross validation, with number of folds (k) set at 10.
tcontrol <- trainControl(method = "cv", number = 10)

### Classification models

# K-Nearest Neighbors
modelKNN <- train(diagnosis ~ ., data = trainset, method = "knn", 
                  preProcess = c("center", "scale"), trControl = tcontrol)
# Naive Bayes
modelNB <- train(diagnosis ~ ., data = trainset, method = "nb", trControl = tcontrol)
# Random Forest
modelRF <- train(diagnosis ~ ., data = trainset, method = "rf", ntree = 100, 
                 importance = T, trControl = tcontrol)
# Logisitic Regression
modelLG <- train(diagnosis ~ ., data = trainset, method = "glm", family = binomial, 
                 trControl = tcontrol)
# Desicion Tree
modelDT <- train(diagnosis ~ ., data = trainset, method = "rpart", trControl = tcontrol)
# SVM Linear
modelSVML <- train(diagnosis ~ ., data = trainset, method = "svmLinear", trControl = tcontrol)
# SVM Radial
modelSVMR <- train(diagnosis ~ ., data = trainset, method = "svmRadial", trControl = tcontrol)
                 


##### prediction
# KNN
pKNN <- predict(modelKNN, testset)
# Naive Bayes
pNB <- predict(modelNB, testset)
# Random Forest
pRF <- predict(modelRF, testset)
# Logistic Regression
pLG <- predict(modelLG, testset)
# Desicion Tree
pDT <- predict(modelDT,testset)
# SVM Linear
pSVML <- predict(modelSVML, testset)
# SVM Radial
pSVMR <- predict(modelSVMR, testset)


#### CM
cmKNN <- confusionMatrix(testset$diagnosis, pKNN)
# Naive Bayes
cmNB <- confusionMatrix(testset$diagnosis, pNB)
# Random Forest
cmRF <- confusionMatrix(testset$diagnosis, pRF)
# Logisitic Regression
cmLG <- confusionMatrix(testset$diagnosis, pLG)
# Desicion Tree
cmDT <- confusionMatrix(testset$diagnosis, pDT)
# SVM Linear
cmSVML <- confusionMatrix(testset$diagnosis, pSVML)
# SVM Radial
cmSVMR <- confusionMatrix(testset$diagnosis, pSVMR)



#### TABLE
Model_Type <- c("K nearest neighbor", "Naive Bayes", "Random forest", "Logistic regression", "Desicion Tree",
               "SVM Linear", "SVM Radial") 

Train_Accuracy <- c(max(modelKNN$results$Accuracy), max(modelNB$results$Accuracy), 
                   max(modelRF$results$Accuracy), max(modelLG$results$Accuracy),
                   max(modelDT$results$Accuracy), max(modelSVML$results$Accuracy),
                   max(modelSVMR$results$Accuracy))


Test_Accuracy <- c(cmKNN$overall[1], cmNB$overall[1], cmRF$overall[1], 
                        cmLG$overall[1], cmDT$overall[[1]], cmSVML$overall[[1]],
                        cmSVMR$overall[[1]])


Test_missclass_Error <- 1 - Test_Accuracy
Test_sensitivity <- c(cmKNN$byClass[[1]],cmNB$byClass[[1]],cmRF$byClass[[1]],
                      cmLG$byClass[[1]],cmDT$byClass[[1]],cmSVML$byClass[[1]],
                      cmSVMR$byClass[[1]])
Test_specificity <- c(cmKNN$byClass[[2]],cmNB$byClass[[2]],cmRF$byClass[[2]],
                      cmLG$byClass[[2]],cmDT$byClass[[2]],cmSVML$byClass[[2]],
                      cmSVMR$byClass[[2]])

metrics <- data.frame(Model_Type, Train_Accuracy, Test_Accuracy, 
                      Test_missclass_Error, Test_sensitivity, Test_specificity)
knitr::kable(metrics, digits = 5)

cmSVMR

### PCA

all_pca <- prcomp(can[,-1], center = TRUE, scale. = TRUE)
summary(all_pca)

fviz_pca_biplot(all_pca, col.ind = can$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)

pca3d(all_pca, group=can$diagnosis, legend="bottomleft")

## CV 100
set.seed(1928)
cvfolds7 <- createMultiFolds(can$diagnosis ,k=10,times=10)
tcontrol7 <- trainControl(method = "repeatedcv", number = 10,
                          repeats = 10, index=cvfolds7)

# K-Nearest Neighbors
modelKNN2 <- train(diagnosis ~ ., data = can, method = "knn", 
                  preProcess = c("center", "scale"), trControl = tcontrol7)
# Naive Bayes
modelNB2 <- train(diagnosis ~ ., data = can, method = "nb", trControl = tcontrol7)
# Random Forest
modelRF2 <- train(diagnosis ~ ., data = can, method = "rf", ntree = 100, 
                 importance = T, trControl = tcontrol7)
# Logisitic Regression
modelLG2 <- train(diagnosis ~ ., data = can, method = "glm", family = binomial, 
                 trControl = tcontrol7)
# Desicion Tree
modelDT2 <- train(diagnosis ~ ., data = can, method = "rpart", trControl = tcontrol7)
# SVM Linear
modelSVML2 <- train(diagnosis ~ ., data = can, method = "svmLinear", trControl = tcontrol7)
# SVM Radial
modelSVMR2 <- train(diagnosis ~ ., data = can, method = "svmRadial", trControl = tcontrol7)

modelSVMR2$results
t.test(modelSVMR2$resample$Accuracy)
summary(modelSVMR2$resample$Accuracy)
