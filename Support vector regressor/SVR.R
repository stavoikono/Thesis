library(e1071)
library(ggplot2)

df <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Salary_Data.csv")


set.seed(1234)
ind <- sample(2,nrow(df),replace=T,prob=c(2/3,1/3))
train<- df[ind==1,]
test <- df[ind==2,]

svr_model <- svm(Salary~., data=df, type="eps-regression")

svr_pred <- predict(svr_model, newdata = test)

## train
ggplot() +
  geom_point(aes(x = train$YearsExperience, y = train$Salary),
             colour = 'red') +
  geom_line(aes(x = df$YearsExperience, y = predict(svr_model, newdata = df)),
            colour = 'blue') +
  ggtitle('Support Vector Regressor') +
  xlab('Level') +
  ylab('Salary') + theme_bw()

## test
ggplot() +
  geom_point(aes(x = test$YearsExperience, y = test$Salary),
             colour = 'red') +
  geom_line(aes(x = df$YearsExperience, y = predict(svr_model, newdata = df)),
            colour = 'blue') +
  ggtitle('Support Vector Regressor') +
  xlab('Level') +
  ylab('Salary') + theme_bw()

## pred

svr_pred <- predict(svr_model, data.frame(YearsExperience=7))
