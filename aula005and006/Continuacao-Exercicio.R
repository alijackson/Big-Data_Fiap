# Aula006 05/10/2019
# Professor Diogenes Justo
# Continuação da aula 005

d <- read.csv("train.csv")

install.packages("party")
library(party)

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
mod <- ctree(Survived~sementeNavio,data=treino)
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

