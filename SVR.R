library(e1071)
library(ggplot2)

dataset <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Position_Salaries.csv")
dataset <- dataset[2:3]

svr_model <- svm(Salary~., data=dataset, type="eps-regression")

svr_pred <- predict(svr_model, data.frame(Level=6.5))

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(svr_model, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Support Vector Regressor') +
  xlab('Level') +
  ylab('Salary') + theme_bw()
