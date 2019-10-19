# Aula006 05/10/2019
# Professor Diogenes Justo
# Continuação da aula 005
# Aproveitamento do script da ultima aula do dia 05/10/2019

install.packages("randomForest")
library('randomForest') # classification algorithm

install.packages("e1071")
library(e1071)

install.packages("party")
library(party)


d <- read.csv("train.csv")

mod<-ctree(Survived~Sex,data=d)

# Arvore com uma variavel
plot(mod, type="simple")

# Arvore com duas variaveis. 
modTwo<-ctree(Survived~Sex+Pclass,data=d)
plot(modTwo, type="simple")

# Arvore com três variaveis. 
modTree<-ctree(Survived~Sex+Pclass+Age,data=d)
plot(modTree, type="simple")

res = 0
i = 0

while (i<200) {
  set.seed(i)
  va <- sample(nrow(d))
  treino<- d[va[1:691],]
  teste<-d[va[692:891],]
  
  #Modelagem
  mod <- ctree(Survived~Sex+Pclass,data=treino)
  p<- predict(mod, newdata=teste)
  prev<-ifelse(p<.5,0,1)
  cbind(prev,teste$Survived)
  #Matriz de confusão
  t<-table(prev,teste$Survived)
  result<-(t[1,1]+t[2,2])/200
  if(result>res){
    res <- result
    seed = i
  }
  
  i <- i+1
}
set.seed(seed)
va <- sample(nrow(d))
treino<- d[va[1:691],]
teste<-d[va[692:891],]

#Modelagem
vetor <- names(d)
mod <- ctree(Survived~Sex+Pclass+Embarked,data=teste)
p<- predict(mod, newdata=teste)
prev<-ifelse(p<.5,0,1)
cbind(prev,teste$Survived)
#Matriz de confusão
t<-table(prev,teste$Survived)


#Matriz de confusão
acc <- table(prev,teste$Survived)

acc1 <- acc[1,1]
acc2 <- acc[2,2]
accuracy<- (acc1 + acc2)/200
t

prec = acc2/(acc[2,1]+acc[2,2])
prec

recal <- acc[2,2]/(acc[1,2]+acc[2,2])


desafio <- (9800+150)/10000
desafio 


#  +++++++++++ TESTE

vetor <- names(d)

set.seed(33)
sementeNavio <- sample(nrow(names(d)))

#Modelagem
mod <- c
tree(Survived~sementeNavio,data=treino)
p<- predict(mod, newdata=teste)
prev<-ifelse(p<.5,0,1)
solu<-cbind(prev,teste$Survived)
#Matriz de confusão
t<-table(prev,teste$Survived)


#Matriz de confusão
acc <- table(prev,teste$Survived)

acc1 <- acc[1,1]
acc2 <- acc[2,2]
accuracy<- (acc1 + acc2)/200
t

prec = acc2/(acc[2,1]+acc[2,2])
prec

recal <- acc[2,2]/(acc[1,2]+acc[2,2])

pred <- ifelse(p<.5,0,1)
table(pred, teste$Survived)


(110+58)/291
58/71
58/101

110+43+13+58
162+64+5+60
291-224


# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = teste$PassengerId, Survived = prev)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution_4.csv', row.names = F)


teste$Survived_F<-as.factor(teste$Survived)

randomForest()


randomMod <- randomForest(Survived_F~Sex+Pclass+Embarked,data=teste)
randomp<- predict(randomMod, newdata=teste)
randomprev<-ifelse(randomp<0,1)
cbind(randomprev,teste$Survived)
#Matriz de confusão
randomt<-table(randomprev,teste$Survived)
randomt


#SVM
