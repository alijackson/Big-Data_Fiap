# base script

install.packages("randomForest")
library('randomForest') # classification algorithm

install.packages("e1071")
library(e1071)

install.packages("party")
library(party)


train <- read.csv("train.csv")
test <- read.csv("test.csv")


mod <- lm(desconto~margem, data=train)

cor(train$desconto,train$margem)
p <- predict(mod, newdata=test)
summary(p)
p <- as.data.frame(cbind(1:31,p))

train$date

boxplot(train$margem~train$weekday)
summary(p)
#names(p)<-c("id","venda")
#write.csv(p, file="predict.csv", row.names=FALSE)


mod <- lm(venda~desconto+margem, data=train)
p <- predict(mod, newdata=test)

val <- read.csv("validate.csv")
RMSE<-sqrt(mean((p-val$venda)^2))
RMSE
