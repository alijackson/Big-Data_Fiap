# Aula 2 - Gera��o de amostra

install.packages("ggplot2")
#   Instanciar o ggplot na memoria. 
library(ggplot2)

# Entrada e saida de dados 
# write.csv(d.file="d.csv") # Saida de csv
# d1 <- read.csv("d.csv") # Entrada de dados atrav�z do csv

# Example de l�gica
#if(mA1 != mA2){
#  teste <- mA1+ mA2  
#}

# Tabela de dados de diamantes -> DataFrame
d <- diamonds
# +++++ Valores reais 
# Calcular media
mean(d$price)
# Calcular Tendencia central (Mediana)
median(d$price)
# Baseado nos dados apresentandos, 
# � bem provavel que temos valores outlier
# Desvio padrao 
sd(d$price)

# Gera��o de amostra
# ==== Amostra 01
a1 <- d[1:3000,]
# ==== Amostra 02
a2 <- d[3001:6000,]
# ==== Dados Amostra 01
mA1 <- mean(a1$price)
median(a1$price)
sd(a1$price)
# ==== Dados Amostra 02
mA2 <- mean(a2$price)
median(a2$price)
sd(a2$price)


# Gerar numeros rondom
sample(10)

# Definicao de semente de aleatoriedade
# Exemplo: Semente � o 3
# Para for�ar a repeti��o de uma sequencia 
# Tomar cuidado com a caracteristica fisica do pc
set.seed(4)
sample(3)

set.seed(33)
va <- sample(53940)
d[1:2]
d[2:1,]

# Geracao de amostra aleatoria 
a3 <- d[va[1:3000],]
mean(a3$price)
median(a3$price)
sd(a3$price)


# Histograma 
hist(d$price)

# Visualizacao de 4 graficos 
par(mfrow=c(2,2))
# Reset de grafico
par(mrflow=c(1,1))

# Comparativo de distribuicao
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Exemplos de dados box ploit,
# Pois puxa para mais uma lado, do que para outro 
# Apenas um valor extremo n�o atende, tem que ter dados ate o extremo
# Descri��o de uma amostra
summary(d)
summary(a3)

# Exemplos de dados box ploit,
# Pois puxa para mais uma lado, do que para outro 
# Apenas um valor extremo n�o atende, tem que ter dados ate o extremo
# Examplo de box ploit
boxplot(d$price)
boxplot(d$price~d$cut)
boxplot(d$price~d$color)

# ======> A parti desse momento vamos usar outro DataFrame.
# =====> Sobre veiculos
# Trabalhando com graficos de dispersao
m <- mtcars

# Estudar se os carros mais levas sao mais economico
plot(m$mpg~m$wt)

# Correla��o linear ==> Coef, Correl, Linear
# (Person)
cor(m$mpg,m$wt)
