###################################################

###################################################
## Importando os dados
milsa <-
  read.table("http://www.leg.ufpr.br/~paulojus/dados/milsa.dat", head=T)

###################################################
head(milsa)
milsa$civil <- factor(milsa$civil, label=c("solteiro", "casado"), levels=1:2)
milsa$instrucao <- factor(milsa$instrucao, label=c("1oGrau", "2oGrau", "Superior"), lev=1:3, ord=T)
milsa$regiao <- factor(milsa$regiao, label=c("capital", "interior", "outro"), lev=c(2,1,3)) 
head(milsa)

###################################################
milsa <- transform(milsa, idade = ano + mes/12)
milsa$idade

###################################################
head(milsa)
is.data.frame(milsa) # conferindo se é um data-frame
names(milsa)         # vendo o nome das variáveis
dim(milsa)           # vendo as dimensões do data-frame


###################################################
with(milsa, civil)
is.factor(milsa$civil)
with(milsa, is.factor(civil))

###################################################
civil.tb <- with(milsa, table(civil))
civil.tb
with(milsa, 100*table(civil)/length(civil))
prop.table(civil.tb)

###################################################
with(milsa, barplot(table(civil)))

###################################################
civil.mo <- names(civil.tb)[which.max(civil.tb)]
civil.mo

###################################################
milsa$instrucao
is.factor(milsa$instrucao)

###################################################
instrucao.tb <- with(milsa, table(instrucao))
instrucao.tb                               
prop.table(instrucao.tb)

###################################################
barplot(instrucao.tb)

###################################################
instrucao.mo <- names(instrucao.tb)[which.max(instrucao.tb)]
instrucao.mo

with(milsa, median(as.numeric(instrucao))) # só calcula mediana de variáveis numéricas
levels(milsa$instrucao)[median(as.numeric(milsa$instrucao))]

###################################################
with(milsa, filhos)
with(milsa, is.factor(filhos))
with(milsa, is.numeric(filhos))

###################################################
filhos.tb <- with(milsa, table(filhos))
filhos.tb
filhos.tbr <- prop.table(filhos.tb)
filhos.tbr

###################################################
plot(filhos.tb)            # gráfico das frequências absolutas

###################################################
par(mar=c(3,3,0.5,0.5), mgp=c(2,0.8,0), mfrow=c(1,2)) ## mudando espeçao entre gráficos na janela gráfica
plot(filhos.tbr)
filhos.fac <- cumsum(filhos.tbr)
filhos.fac                 
plot(filhos.fac, type="S") 

###################################################
filhos.mo <- names(filhos.tb)[which.max(filhos.tb)]
filhos.mo                  # moda

filhos.md <- with(milsa, median(filhos, na.rm=T))
filhos.md                  # mediana

filhos.me <- with(milsa, mean(filhos, na.rm=T))
filhos.me                  # média

filhos.me <- with(milsa, mean(filhos, trim=0.1, na.rm=T))
filhos.me                  # média

filhos.qt <- with(milsa, quantile(filhos, na.rm=T))

###################################################
with(milsa, range(filhos, na.rm=T))
filhos.A <- with(milsa, diff(range(filhos, na.rm=T))) 
filhos.A

with(milsa, var(filhos, na.rm=T))       # variância
filhos.dp <- with(milsa, sd(filhos, na.rm=T))   # desvio padrão
filhos.dp

filhos.cv <- 100 * filhos.dp/filhos.me  # coeficiente de variação
filhos.cv

filhos.qt <- with(milsa, quantile(filhos, na.rm=T))
filhos.ai <- filhos.qt[4] - filhos.qt[2] # amplitude interquartílica 
filhos.ai

###################################################
with(milsa, summary(filhos))            # várias medidas
with(milsa, fivenum(filhos))

###################################################
with(milsa, salario)
with(milsa, is.factor(salario))
with(milsa, is.numeric(salario))

###################################################
with(milsa, range(salario))          # máximo e mínimo
with(milsa, nclass.Sturges(salario)) # número de classes pelo critério de Sturges
args(cut)
args(cut.default)
salario.tb <- with(milsa, table(cut(salario, seq(3.5,23.5,l=8))))
prop.table(salario.tb)

###################################################
par(mar=c(3,3,0.5,0.5), mgp=c(2,0.8,0), mfrow=c(1,2))
with(milsa, hist(salario, main=""))
with(milsa, boxplot(salario))

###################################################
with(milsa, stem(salario))

###################################################
salario.md <- with(milsa, median(salario, na.rm=T))
salario.md                # mediana

salario.me <- with(milsa, mean(salario, na.rm=T))
salario.me                # média

with(milsa, range(salario, na.rm=T))
salario.A <- with(milsa, diff(range(salario, na.rm=T))) # amplitude
salario.A

with(milsa, var(salario, na.rm=T))     # variância
salario.dp <- with(milsa, sd(salario, na.rm=T))   # desvio padrão
salario.dp

salario.cv <- 100 * salario.dp/salario.me  # coeficiente de variação
salario.cv

salario.qt <- with(milsa, quantile(salario, na.rm=T))
salario.ai <- with(milsa, salario.qt[4] - salario.qt[2]) # amplitude interquartílica 
salario.ai

with(milsa, summary(salario))          # várias medidas
with(milsa, fivenum(salario))

###################################################
civ.gi.tb <- with(milsa, table(civil, instrucao)) # frequências absolutas
civ.gi.tb
addmargins(civ.gi.tb)

###################################################
prop.table(civ.gi.tb)
prop.table(civ.gi.tb, margin=1)
prop.table(civ.gi.tb, margin=2)

###################################################
par(mar=c(3,3,0.5,0.5), mgp=c(2,0.8,0), mfrow=c(2,2))
barplot(civ.gi.tb, legend=T)
barplot(t(civ.gi.tb), legend=T)
barplot(civ.gi.tb, beside=T, legend=T)
barplot(t(prop.table(civ.gi.tb)), beside=T, legend=T)

###################################################
summary(civ.gi.tb)   # resumo incluindo o teste Chi-quadrado
names(summary(civ.gi.tb))   # resumo incluindo o teste Chi-quadrado
chisq <- summary(civ.gi.tb)$stat
chisq
n <- sum(civ.gi.tb)
n
C <- sqrt(chisq/(chisq+n))
C
t <- min(dim(civ.gi.tb))
C1 <- C/((t-1)/t)^2
C1

###################################################
temp <- with(milsa, ifelse(instrucao == "1oGrau", 1, 2))
milsa$instrucao1 <- factor(temp, label=c("1oGrau", "2o+Superior"), lev=1:2, ord=T)
rm(temp)
with(milsa, table(instrucao1))
with(milsa, table(civil, instrucao1))
with(milsa, summary(table(civil, instrucao1)))

###################################################
with(milsa, quantile(salario))
salario.cl <- with(milsa, cut(salario, quantile(salario), include.lowest=T))
ins.sal.tb <- with(milsa, table(instrucao, salario.cl))
ins.sal.tb
prop.table(ins.sal.tb, margin=1)

###################################################
with(milsa, boxplot(salario~instrucao))

###################################################
with(milsa, tapply(salario, instrucao, mean))
with(milsa, tapply(salario, instrucao, sd))
with(milsa, tapply(salario, instrucao, quantile))

## fazer com aggregate()

###################################################
idade.cl <- with(milsa, cut(idade, quantile(idade), include.lowest=T))
table(idade.cl)
salario.cl <- with(milsa, cut(salario, quantile(salario), include.lowest=T))
table(salario.cl)
table(idade.cl, salario.cl)
prop.table(table(idade.cl, salario.cl), mar=1)

###################################################
idade.cl1 <- with(milsa, cut(idade, quantile(idade, seq(0,1,len=4)), include.lowest=T))
salario.cl1 <- with(milsa, cut(salario, quantile(salario, seq(0,1,len=4)), include.lowest=T))
table(idade.cl1, salario.cl1)
prop.table(table(idade.cl1, salario.cl1), mar=1)

###################################################
with(milsa, plot(idade, salario))

###################################################
with(milsa, cor(idade, salario))
with(milsa, cor(idade, salario, method="kendall"))
with(milsa, cor(idade, salario, method="spearman"))

###################################################
with(milsa, tapply(salario, instrucao, mean))
with(milsa, tapply(salario, civil, mean))
with(milsa, tapply(salario, interaction(instrucao, civil), mean))
with(milsa, tapply(salario, list(instrucao, civil), mean))

## tentar com alternativa: aggregate()

###################################################
with(milsa, tapply(salario, instrucao, function(x) sum(x >= 10)))
with(milsa, tapply(salario, civil, function(x) sum(x >= 10)))
with(milsa, tapply(salario, interaction(instrucao, civil), function(x) sum(x >= 10)))

## alternativa: aggregate

###################################################
## data()
## data(women)  # carrega o conjunto de dados women
## women        # mostra os dados
## help(woman)  # mostra a documentação destes dados


## {Exercícios}

## Experimente as funções \code{mean()}, \code{var()}, \code{sd()}, \code{median()}, \code{quantile()} nos dados mostrados anteriormente.
## Veja a documentação das funções e as opções de uso.

## Faça uma análise descritiva adequada do conjunto de dados \code{women}.

## Carregue o conjunto de dados \code{USArrests} com o comando \code{data(USArrests)}.
Examine a sua documentação com \code{help(USArrests)} e responda as perguntas a seguir.
\begin{enumerate}
## qual o número médio e mediano de cada um dos crimes?
## encontre a mediana e quartis para cada crime.
## encontre o número máximo e mínimo para cada crime.
## faça um gráfico adequado para o número de assassinatos ({\it murder}).
## faça um diagrama ramo-e-folhas para o número de estupros ({\it rape}).
## verifique se há correlação entre os diferentes tipos de crime.
## verifique se há correlação entre os crimes e a proporção de população urbana.
## encontre os estados com maior e menor ocorrência de cada tipo de crime.
## encontre os estados com maior e menor ocorrência per capta de cada tipo de crime.
## encontre os estados com maior e menor ocorrência do total de crimes.

