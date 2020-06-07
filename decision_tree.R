### party

library(party)
data("iris")
ind <- sample(2,nrow(iris),replace=T,prob=c(0.8,0.2))
train<- iris[ind==1,]
test <- iris[ind==2,]

tree <- ctree(Species~.,train)

plot(tree, type='simple')

pred <- predict(tree, test)
cm <- table(pred,test$Species)
cm

### rpart

library(rpart)
library(rpart.plot)
data("iris")

ind <- sample(2,nrow(iris),replace=T,prob=c(0.8,0.2))
train<- iris[ind==1,]
test <- iris[ind==2,]

fit <- rpart(Species~., data = train, method = 'class')
rpart.plot(fit)

predict <-predict(fit, test, type = 'class')

cm<-table(predict,test$Species)
cm

accuracy <- sum(diag(cm)) / sum(cm)
accuracy






