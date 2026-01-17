
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir mostram diversas técnicas vistas em aula para
# análises exploratórias bivariadas para variáveis qualitativas e 
# quantitativas
#
# Execute os comandos, discuta o que eles fazem, comente o código.
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------

dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", 
                  "instrucao", "filhos", "salario", 
                  "anos", "meses", "regiao")

head(dados)
summary(dados)
names(dados)

#-----------------------------------------------------------------------

dados$estado_civil
dados$instrucao

#-----------------------------------------------------------------------

table(dados$estado_civil, dados$instrucao)
table(dados$instrucao, dados$estado_civil)

#-----------------------------------------------------------------------

tabela1 <- table(dados$estado_civil, 
                 dados$instrucao)
tabela1
sum(tabela1)

addmargins(tabela1)

t(tabela1)

#-----------------------------------------------------------------------

prop.table(tabela1)
tabela2 <- prop.table(tabela1)
tabela2
sum(tabela2)
addmargins(tabela2)
t(tabela2)

#-----------------------------------------------------------------------

prop.table(tabela1, margin = 1)
tabela3 <- prop.table(tabela1, margin = 1)
tabela3
addmargins(tabela3)
t(tabela3)

#-----------------------------------------------------------------------

prop.table(tabela1, margin = 2)
tabela4 <- prop.table(tabela1, margin = 2)
tabela4
addmargins(tabela4)

#-----------------------------------------------------------------------

tabela1

barplot(tabela1, beside = F, legend.text = T)
barplot(t(tabela1), beside = F, legend.text = T)

barplot(tabela1, beside = T, legend.text = T)
barplot(t(tabela1), beside = T, legend.text = T)

barplot(t(tabela3), legend.text = T)
barplot(tabela4, legend.text = T)

#-----------------------------------------------------------------------

tabela1
tabela1_margens <- addmargins(tabela1)
tabela1_margens

esperados <- (tabela1_margens[1:2,4] %*% t(tabela1_margens[3,1:3]))/sum(tabela1)
sum(((tabela1 - esperados)^2)/(esperados))

#-----------------------------------------------------------------------

cor(dados$salario, dados$anos)
cor(dados$salario, dados$anos, method = "pearson")
cor(dados$salario, dados$anos, method = "spearman")
cor(dados$salario, dados$anos, method = "kendall")

plot(salario ~ anos, data = dados)
lm(salario ~ anos, data = dados)
coeficientes <- lm(salario ~ anos, data = dados)
abline(coeficientes, col = 2)

#-----------------------------------------------------------------------

tapply(X = dados$salario,
       INDEX = dados$instrucao,
       FUN = mean)

tapply(X = dados$salario,
       INDEX = dados$instrucao,
       FUN = sd)

tapply(X = dados$salario,
       INDEX = dados$instrucao,
       FUN = summary)

boxplot(salario~instrucao, data = dados)

grau1 <- subset(dados, instrucao == "1o Grau")
grau2 <- subset(dados, instrucao == "2o Grau")
grau3 <- subset(dados, instrucao == "Superior")

par(mfrow = c(1,3))
hist(grau1$salario)
hist(grau2$salario)
hist(grau3$salario)

plot(density(grau1$salario))
plot(density(grau2$salario))
plot(density(grau3$salario))

par(mfrow = c(1,1))
plot(density(grau1$salario), xlim = c(0,30))
lines(density(grau2$salario), col= 2)
lines(density(grau3$salario), col = 4)

#----------------------------------------------------------------------
