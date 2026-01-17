##
## Exemplo 1
##
## fonte:
## https://silviashimakura.github.io/CE3012025/listas/203-exercicios-distribuicoes.pdf


## Y ~ Bin(n = 15, p = 0.80)

##a) P(Y = 15)
choose(15, 15) * (0.80)^15 * (1 - 0.80)^(15 - 15)
args(dbinom)
dbinom(15, size = 15, prob = 0.80)

##b) P(Y <= 13) = 1 - P(Y = 14) - P(Y = 15)
(b14 <- choose(15, 14) * (0.80)^14 * (1 - 0.80)^(15 - 14))
(b15 <- choose(15, 15) * (0.80)^15 * (1 - 0.80)^(15 - 15))
1 - b14 - b15

dbinom(14, size = 15, prob = 0.80)
dbinom(15, size = 15, prob = 0.80)
dbinom(14:15, size = 15, prob = 0.80)
1 - sum(dbinom(14:15, size = 15, prob = 0.80))

pbinom(13, size = 15, prob = 0.80)


##c) P(Y>=10) = P(Y=10) + P(Y=11) + P(Y=12) + P(Y=13) + P(Y=14) + P(Y=15) 
##            = 1 - P(Y <= 9)
ys <- 10:15
choose(15, ys) * (0.80)^ys * (1 - 0.80)^(15 - ys)
sum(choose(15, ys) * (0.80)^ys * (1 - 0.80)^(15 - ys))

dbinom(ys, size = 15, prob = 0.80)
sum(dbinom(ys, size = 15, prob = 0.80))

pbinom(9, size = 15, prob = 0.80)
pbinom(9, size = 15, prob = 0.80, lower = FALSE)

## Mediana
qbinom(0.5, size = 15, prob = 0.80)
## quartis
qbinom(c(0.25, 0.5, 0.75), size = 15, prob = 0.80)
## Quantil 0.35:  P(Y < q) = 0.35
qbinom(0.35, size = 15, prob = 0.80)

## Esperança e variância 
ys <- 0:15
Pys <- dbinom(ys, size = 15, prob = 0.80)
cbind(ys, Pys)

(EY <- sum(ys*Pys))
15 * 0.80
(VY <- sum((ys^2)*Pys) - EY^2)
15 * 0.80 * (1-0.80)

Fys <- pbinom(ys, size = 15, prob = 0.80)
cbind(ys, Pys, Fys)

par(mfrow = c(1,2), mar = c(3.5,3.5,0.5,0.5), mgp = c(2,1,0))
plot(ys, Pys, type = "h")
points(ys, Pys, pch = 19)

plot(ys, Fys, type = "s")
points(ys, Fys, pch = 19)

## O "quarteto fantástico:
## dbinom()
## pbinom()
## qbinom()
## ***rbinom()***

rbinom(1, size = 15, prob = 0.80)
rbinom(1, size = 15, prob = 0.80)
rbinom(1, size = 15, prob = 0.80)
rbinom(1, size = 15, prob = 0.80)
rbinom(1, size = 15, prob = 0.80)

rbinom(5, size = 15, prob = 0.80)
rbinom(50, size = 15, prob = 0.80)

(ysim <- rbinom(10000, size = 15, prob = 0.80))
table(ysim)
(Psim <- prop.table(table(ysim)))

ysimf <- factor(ysim, levels = 0:15)
(Psimf <- prop.table(table(ysimf)))

cbind(ys, Pys, Psimf)
cbind(ys, round(Pys, dig = 2), Psimf)

barplot(
    rbind(round(Pys, dig = 2), Psimf),
    beside = TRUE,
    legend.text = c("Teo","Sim")
)

## recalculando (agora estimando por simulação) as probabilidades pedidas)
## a) P(Y = 15)
mean(ysim == 15) ## compare com calculado anteriormente
## b) P(Y <= 13)
mean(ysim <= 13) ## compare com calculado anteriormente
## c) P(Y >= 10)
mean(ysim >= 10) ## compare com calculado anteriormente

## e a esperança e variância
mean(ysim)
var(ysim)

## agora volte na simulação e troque 100 por 10000 e repita comandos
## (use agora round(Pys, dig = 4))

rm(ys, Pys, Fys, EY, VY, ysim, Psim, ysimf, Psimf)
##
## Fim do exemplo 1
##

##
## Exercícios: 13, 5, 8 
##  responder questões propostas e também fazer gráficos e cálculos
##  adicionais análogos ao feito no Exemplo 1


##
## Exemplo 2
##
## fonte:
## https://silviashimakura.github.io/CE3012025/listas/203-exercicios-distribuicoes.pdf

## Y ~ Exp(1)
args(dexp)
curve(dexp(x, rate = 1), from = 0, to = 6)
curve(dexp(x, rate = 0.5), from = 0, to = 6, add = TRUE, lty = 2)
curve(dexp(x, rate = 2), from = 0, to = 6, add = TRUE, lty = 3)

curve(pexp(x, rate = 1), from = 0, to = 6)
curve(pexp(x, rate = 0.5), from = 0, to = 6, add = TRUE, lty = 2)
curve(pexp(x, rate = 2), from = 0, to = 6, add = TRUE, lty = 3)

## a) P(0 < Y < 2).
pexp(2, rate = 1)
## b) P(Y < 2).
pexp(2, rate = 1)
## c) P(1 < Y < 4).
pexp(4, rate = 1) - pexp(1, rate = 1)
diff(pexp(c(1, 4), rate = 1))
## d) P(Y > 3).
pexp(3, rate = 1, lower = FALSE)
## e) P(Y < 2|Y > 1) = P(1 < Y < 2)/P(Y > 1)
diff(pexp(c(1, 2), rate = 1))/pexp(1, rate = 1, lower = FALSE)
## f) P(Y < a1) = 0,65  a1 = ?
qexp(0.65, rate = 1)
## g) P(Y > a3) = 0,55  a2 = ?
qexp(0.45, rate = 1)
qexp(0.55, rate = 1, lower = FALSE)


ysim <- rexp(1000, rate = 1)
mean(ysim < 2)
mean(ysim > 1 &  ysim < 4)
mean(ysim > 3)
mean(ysim[ysim > 1] < 2)    
quantile(ysim, prob = c(0.65, 0.45))

mean(ysim)
var(ysim)


rm(ysim)
##
## Exercicios: 17, 27, 22
##
