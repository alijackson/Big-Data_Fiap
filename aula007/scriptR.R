# Aula 007 18102019
# Professor Diogenes Justo
# Assunto = Clusterização 

m <- mtcars
plot(m$mpg~m$wt)

set.seed(33)
m$wt_2 <- m$wt*6
# Clusterização das variaveis wt e mpg
k<-kmeans(m[,c("mpg", "wt_2")],4)#Numero de cluster

k$cluster
#Colorir grafico do cluster
plot(m$mpg~m$wt, col=k$cluster,pch=k$cluster)

summary(lm(mpg~wt,data=m))
summary(lm(mpg~wt+as.factor(k$cluster),data = m))
