# base script
train <- read.csv("train.csv")
test <- read.csv("test.csv")

mod <- lm(venda~outmg, data=train)
p <- predict(mod, newdata=test)

p <- as.data.frame(cbind(1:31,p))
names(p)<-c("id","venda")
write.csv(p, file="predict.csv", row.names=FALSE)
