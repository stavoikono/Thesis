library(ggplot2)

df <- read.csv("C:/Users/tit0v/R/PTUXIAKH/Salary_Data.csv")


set.seed(1234)
ind <- sample(2,nrow(df),replace=T,prob=c(2/3,1/3))
train<- df[ind==1,]
test <- df[ind==2,]

lm_model <- lm(Salary~YearsExperience, data = train)
summary(lm_model)

lm_pred <- predict(lm_model, newdata = test)

ggplot(train,aes(x=YearsExperience,y=Salary))+geom_point(colour=I("Red")) + geom_smooth(method="lm", se=F)+
  theme_bw() + ggtitle("Γραμμική παλινδρόμηση-δεδομένα εκπαίδευσης")
  

ggplot(test,aes(x=YearsExperience,y=Salary))+geom_point(colour=I("Red")) + geom_smooth(method="lm", se=F)+
  theme_bw() + ggtitle("Γραμμική Παλινδρόμηση στα δεδομένα δοκιμής")
