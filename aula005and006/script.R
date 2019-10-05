# Aula 005 01/10/2019

# Professor Diogenes Justo 

m <- mtcars

# Gráfico de disperssão 
plot(m$mpg~m$wt)

# Semente de aletoriedade
set.seed(33)
va <- sample(32)
#Definir base treino e base teste.
treino<- m[va[1:24],]
teste<- m[va[25:32],]

# Mod. Regressao linear 
# mod<- lm(mpg~wt,data=treino)
mod<- lm(mpg~log(wt),data=treino)
mod<- lm(mpg~poly(wt,2),data=treino)
mod<- lm(mpg~poly(wt,3),data=treino)
mod<- lm(mpg~poly(wt,4),data=treino)
mod<- lm(mpg~poly(wt,5),data=treino)
mod<- lm(mpg~poly(wt,6),data=treino)
mod<- lm(mpg~poly(wt,10),data=treino)

# Analise de parametro de regressao
summary(mod)

# PRevisão em Teste
p <- predict(mod, newdata = teste)
# Comparação, previsto VS real VS Erro
cbind(p, teste$mpg, p - teste$mpg)
# SSE 
sse <- sum((p-teste$mpg)^2)
sse

# ============
# Tranformação Linear
# Hipotese: WT tem o comportamento exponencial
# Para linearizar WT utilizaremos log,inverter
cor(m$mpg, m$wt)
cor(m$mpg, log(m$wt))


# ======== Variaveis Categoricas

plot(m$mpg~m$wt,col=m$cyl)

# == Variaveis cilindros como categorica

mod<- lm(mpg~wt+as.factor(cyl),data=treino)
