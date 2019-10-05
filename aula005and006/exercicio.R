# Exercicio do Titanic

d <- read.csv("train.csv")


# Probabilidade de sobrevivencia 
nrow(d[d$Survived==1,])/891

# Trabalhando com SQL no R
install.packages("sqldf")
library(sqldf)

#  Fazer contagem usando linguagem SQL
result <- sqldf("select survived,count(*) from d group by survived")

# Fazer contagem usando table nativa do R
table(d$Survived)
prop.table(d$Survived)

# Calcular porcentual com PROP table
prop.table(table(d$Survived))

sqldf("select Sex.count(*) from d group by Sex")


prop.table(table(d$Sex))

prop.table(table(d.Sex))

table(d[,c("Sex","Survived")])
