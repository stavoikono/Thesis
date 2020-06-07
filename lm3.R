library(ggplot2)
dataset <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Position_Salaries.csv")
dataset <- dataset[,2:3]

poly1_model <- lm(Salary~Level,data=dataset)

poly2_model <- lm(Salary~poly(Level, 2, raw = TRUE), data=dataset)

poly3_model <- lm(Salary ~ poly(Level, 3, raw = TRUE), data = dataset)

poly1_pred <- predict(poly1_model, data.frame(Level=6.5))
poly2_pred <- predict(poly2_model, data.frame(Level=6.5))
poly3_pred <- predict(poly3_model, data.frame(Level=6.5))

tbl123 <- data.frame(Linear=poly1_pred,Quant=poly2_pred,Third=poly3_pred)
tbl123 

ggplot(dataset,aes(x=Level,y=Salary))+geom_point(colour=I("Red")) + geom_smooth(method="lm", se=F, colour="Green")+
  theme_bw() + geom_smooth(method="lm", formula = y ~ poly(x, 3), se = F) + 
  geom_smooth(method="lm", formula = y ~ poly(x, 2),se=F, colour="Magenta")
