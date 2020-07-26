dataf <- read.csv("C:/Users/tit0v/R/PTUXIAKH/50_Startups.csv")

set.seed(1234)
ind <- sample(2,nrow(dataf),replace=T,prob=c(0.8,0.2))
train<- dataf[ind==1,]
test <- dataf[ind==2,]

mlm_model <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State, data = train)
summary(mlm_model)

mlm_pred <- predict(mlm_model,newdata = test)

tbl<-data.frame(prediction=mlm_pred,real=test$Profit,diff=mlm_pred-test$Profit)

knitr::kable(tbl)
