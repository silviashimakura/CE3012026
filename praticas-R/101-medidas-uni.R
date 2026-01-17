
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir mostram diversas técnicas vistas em aula para
# análise exploratória univariada usando medidas resumo para variáveis
# qualitativas e quantitativas
#
# Execute os comandos, discuta o que eles fazem, comente o código.
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------
dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", "instrucao", 
                  "filhos", "salario", "anos", "meses", "regiao")

#-----------------------------------------------------------------------

dados$salario

sum(dados$salario)/length(dados$salario)
mean(dados$salario)

median(dados$salario)

quantile(dados$salario)
quantile(dados$salario, seq(0,1,0.1))
quantile(dados$salario)[4] - quantile(dados$salario)[1]

min(dados$salario)
max(dados$salario)
range(dados$salario)
max(dados$salario) - min(dados$salario)
diff(range(dados$salario))

dados$salario - mean(dados$salario)
dados$salario - median(dados$salario)

da_media <- abs(dados$salario - mean(dados$salario))
da_mediana <- abs(dados$salario - median(dados$salario))

mean(da_media)
mean(da_mediana)

var(dados$salario)
sqrt(var(dados$salario))
sd(dados$salario)

sd(dados$salario)/var(dados$salario)

(dados$salario - mean(dados$sal))/sd(dados$salario)
scale(dados$salario)
escore <- scale(dados$salario)
mean(escore)
sd(escore)

dados$instrucao
tabela <- table(dados$instrucao)
pi <- prop.table(tabela)
log_pi <- log(pi)
h1 <- -(sum(pi*log_pi))
h1

#-----------------------------------------------------------------------
