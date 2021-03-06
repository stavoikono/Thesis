binary <- read.csv("C:/Users/tit0v/R/PTUXIAKH/binary.csv")

binary$admit <- as.factor(binary$admit)
binary$rank <- as.factor(binary$rank)

set.seed(1234)
ind <- sample(2,nrow(binary),replace=T,prob=c(0.8,0.2))
train<- binary[ind==1,]
test <- binary[ind==2,]

glm_model <- glm(admit~.,data = train, family='binomial')

summary(glm_model)

glm_pred <- predict(glm_model, newdata = test, type = 'response')

glm_prediction <- ifelse(glm_pred > 0.5,1,0)

cm_glm <- table(glm_prediction, test$admit)
cm_glm

accuracy_glm <- sum(diag(cm_glm)) / sum(cm_glm)
accuracy_glm

confusionMatrix(as.factor(glm_prediction),test$admit)

set.seed(1928)
cvfolds1 <- createMultiFolds(binary$admit,k=5,times=20)

tcontrol1 <- trainControl(method = "repeatedcv", number = 5, repeats = 20, index=cvfolds1)

glm2 <- train(admit ~ ., data = binary, method = "glm", family = binomial, 
              trControl = tcontrol1)

glm2$results
t.test(glm2$resample$Accuracy)
summary(glm2$resample$Accuracy)
sd(glm2$resample$Accuracy)
