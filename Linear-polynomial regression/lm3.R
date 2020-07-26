library(ggplot2)
dataset <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Position_Salaries.csv")
dataset <- dataset[,2:3]

poly1_model <- lm(Salary~Level,data=dataset)

poly2_model <- lm(Salary~poly(Level, 2, raw = TRUE), data=dataset)

poly3_model <- lm(Salary ~ poly(Level, 3, raw = TRUE), data = dataset)

poly4_model <- lm(Salary ~ poly(Level, 4, raw = TRUE), data = dataset)

poly1_pred <- predict(poly1_model, data.frame(Level=6.5))
poly2_pred <- predict(poly2_model, data.frame(Level=6.5))
poly3_pred <- predict(poly3_model, data.frame(Level=6.5))
poly4_pred <- predict(poly4_model, data.frame(Level=6.5))

tbl123 <- data.frame(Linear=poly1_pred,Quant=poly2_pred,Third=poly3_pred)
tbl123 

ggplot(dataset,aes(x=Level,y=Salary))+geom_point(colour=I("Red")) + geom_smooth(method="lm", se=F, colour="Green")+
  theme_bw() + geom_smooth(method="lm", formula = y ~ poly(x, 3), se = F) + 
  geom_smooth(method="lm", formula = y ~ poly(x, 2),se=F, colour="Magenta") +
  geom_smooth(method="lm", formula = y ~ poly(x, 4), se = F, colour="orange")


dataset[dataset$Level==7,"Salary"]
predict(poly1_model, data.frame(Level=7))
predict(poly2_model, data.frame(Level=7))
predict(poly3_model, data.frame(Level=7))
predict(poly4_model, data.frame(Level=7))


sqrt(mean((poly1_model$fitted.values-dataset$Salary)^2))
sqrt(mean((poly2_model$fitted.values-dataset$Salary)^2))
sqrt(mean((poly3_model$fitted.values-dataset$Salary)^2))
sqrt(mean((poly4_model$fitted.values-dataset$Salary)^2))
