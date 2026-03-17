x = c(48, 56, 60, 65, 77, 80, 81, 94, 95, 97)
mean(x)

stripchart(x, method = "stack", offset = 0.5, at = .15, pch = 16, col = "black", xlim = c(40, 100),axes = FALSE,cex=1.5)
axis(1)
points(mean(x),0.10,col="darkred",pch=17,cex=1.5)
segments(48,0.15,97,0.15,col="black",lwd=1,lty=3)


# Lista Análise exploratória de dados
# Exercício 20
# Vinte e cinco residências de um certo bairro foram sorteadas e visitadas por um entrevistador que, entre outras
# questões, perguntou sobre o número de televisores. Os dados foram os seguintes:
#  2, 2, 2, 3, 1, 2, 1, 1, 1, 1, 0, 1, 2, 2, 2, 2, 3, 1, 1, 3, 1, 2, 1, 0 e 2.
# Organize os dados numa tabela de frequências e determine as diversas medidas de posição

y <- c(2, 2, 2, 3, 1, 2, 1, 1, 1, 1, 0, 1, 2, 2, 2, 2, 3, 1, 1, 3, 1, 2, 1, 0, 2)
length(y)
table(y)
mean(y)
sort(y)
fivenum(y)
q1 <- fivenum(y)[2]
q2 <- fivenum(y)[3]
q3 <- fivenum(y)[4]
aiq <- q3 - q1
ls <- q3 + 1.5*aiq
li <- q1 - 1.5*aiq
boxplot(y)
