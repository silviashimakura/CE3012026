##
## Exemplo 1
##
## fonte:
## https://silviashimakura.github.io/CE3012025/listas/202-exercicios-variaveis-aleatorias.pdf
## Exercício 5

tempo <- 3:9
Ptempo <- c(0.1, 0.1, 0.2, 0.2, 0.2, 0.1, 0.1)
rbind(tempo, Ptempo)

## P[T = t]  qual gráfico é o mais adequado?
plot(tempo, Ptempo)
plot(tempo, Ptempo, type = "l")
plot(tempo, Ptempo, type = "b")
plot(tempo, Ptempo, type = "h")

plot(tempo, Ptempo, type = "h", ylim = c(0, 0.20))

## F(t) = P[T <= t]
Ftempo <- cumsum(Ptempo)
Ftempo
rbind(tempo, Ptempo, Ftempo)

## F(T) = P[T <= t]  qual gráfico é o mais adequado?
plot(tempo, Ftempo)
plot(tempo, Ftempo, type = "l")
plot(tempo, Ftempo, type = "b")
plot(tempo, Ftempo, type = "h")
plot(tempo, Ftempo, type = "S")
plot(tempo, Ftempo, type = "s")

plot(tempo, Ftempo, type = "s", ylim = c(0, 1))

plot(c(2, tempo, 10), c(0, Ftempo, 1), type = "n", xlab = "tempo", ylab = "F(temp0)")
points(tempo, Ftempo, type = "p", pch = 19)
segments(c(2, tempo), c(0, Ftempo), c(tempo, 10), c(0, Ftempo), pch = 19)

par(mfrow = c(1,2), mar = c(3.5, 3.5, 0.5, 0.5), mgp = c(2,1,0))
plot(tempo, Ptempo, type = "h", ylim = c(0, 0.20))

plot(c(2, tempo, 10), c(0, Ftempo, 1), type = "n", xlab = "tempo", ylab = "F(temp0)")
points(tempo, Ftempo, type = "p", pch = 19)
segments(c(2, tempo), c(0, Ftempo), c(tempo, 10), c(0, Ftempo), pch = 19)

(Etempo <- sum(tempo * Ptempo))
(Vtempo <- sum(((tempo - Etempo)^2) * Ptempo))
## ou
(Vtempo <- sum((tempo^2) * Ptempo) - Etempo^2)

## Enunciado define uma outro variável: pontos
pontos <- 10:4
Ppontos <- Ptempo
rbind(pontos, Ppontos)

pontos <- rev(pontos)
Ppontos <- rev(Ppontos)
rbind(pontos, Ppontos)

Fpontos <- cumsum(Ppontos)
rbind(pontos, Ppontos, Fpontos)

(Epontos <- sum(pontos * Ppontos))
(Vpontos <- sum(((pontos - Epontos)^2) * Ppontos))

## Perguntas adicionais
## P[pontos = 4]
Ppontos[pontos == 4]

## P[pontos = 4 ou pontos = 7]
sum(Ppontos[pontos == 4 | pontos == 7])

## P[pontos <= 7]
sum(Ppontos[pontos <= 7]) 
Fpontos[pontos == 7]

## P[pontos > 6]
sum(Ppontos[pontos > 6])
1 - Fpontos[pontos == 6]

##
par(mfrow = c(1,1))
rm(tempo, Ptempo, Ftempo, Etempo, Vtempo, pontos, Ppontos, Epontos, Vpontos)
##
## Fim do Exemplo 1
##

##
## Exercício: Fazer versão computacional dos exercícios 2, 3, 6, 11
##

##
## Exemplo 2
##
## fonte:
## https://silviashimakura.github.io/CE3012025/listas/202-exercicios-variaveis-aleatorias.pdf
## Exercício 13
fy <- function(y){
    res <- numeric(length(y))
    ind1 <- y >= 0 & y < 2
    ind2 <- y >= 2 & y <= 6
    res[ind1] <- 1/4
    res[ind2] <- 1/8
    return(res)
}
curve(fy, from = -0.2, to = 6.2)
(2-0)*(1/4) + (6-2)*(1/8)
integrate(fy, 0, 6)$value

Fy <- function(y){
    res <- numeric(length(y))
    ind1 <- y >= 0 & y < 2
    ind2 <- y >= 2 & y <= 6
    res[ind1] <- (1/4)*(y[ind1])
    res[ind2] <- (1/2) + (1/8)*(y[ind2] - 2)
    res[y > 6] <- 1
    return(res)
}
curve(Fy, from = -0.2, to = 6.2)


## a) P[Y > 3]
(6-3)*(1/8)
integrate(fy, 3, 6)$value
1 - Fy(3)

## b) P[1 < Y <= 4]
(2-1)*(1/4) + (4-2)*(1/8)
integrate(fy, 1, 4)$value
Fy(4) - Fy(1)

## c) P[Y < 3| Y >= 1] = P[1 <= Y < 3]/P[Y >= 1] 
((2-1)*(1/4) + (3-2)*(1/8))
(1-(1-0)*(1/4))
((2-1)*(1/4) + (3-2)*(1/8))/(1-(1-0)*(1/4))
integrate(fy, 1, 3)$value
integrate(fy, 1, 6)$value
integrate(fy, 1, 3)$value/integrate(fy, 1, 6)$value
(Fy(3)-Fy(1))/(1 - Fy(1))

## d) b tal que P[Y > b] = 0.6
##    b é o *quantil* 0.4  (P[Y < b] = 0.4)
invFy <- function(p){
    if(any(p < 0 | p > 1)) stop("valor de p não permitido")
    y <- numeric(length(p))
    ind1 <- p >= 0 & p < 1/2
    ind2 <- p >= 1/2 & p <= 1
    y[ind1] <- 4*(p[ind1])
    y[ind2] <- 2 + 8*(p[ind2] - 0.5)
    return(y)
}
invFy(0.40)
Fy(1.6)

## outros exemplos
Fy(1.5)
invFy(0.375)
Fy(4.5)
invFy(0.8125)
invFy(c(0.25, 0.50, 0.75))

## e) Valor esperado e variância
Efun <- function(y) y * fy(y)
(EY <- integrate(Efun, lower = 0, upper = 6)$value)
E2fun <- function(y) y^2 * fy(y)
(EY2 <- integrate(E2fun, lower = 0, upper = 6)$value)
(VarY <- EY2 - EY^2)
## ou
Vfun <- function(y) (y-EY)^2 * fy(y)
integrate(Vfun, lower = 0, upper = 6)

rm(fy, Fy, EY, EY2, VarY)
##
## fim exemplo 2
##

##
## Exercício: Fazer versão computacional do exercício 8
##

##
## Exemplo 3
##
## fonte:
## https://silviashimakura.github.io/CE3012025/listas/202-exercicios-variaveis-aleatorias.pdf
## Exercício 14

fy <- function(y){
    res <- numeric(length(y))
    ind <- y >= 0.5 & y < 2
    res[ind] <- (8/9)*y[ind] - 4/9
    res
}

fy(1) 
MASS::fractions(fy(1))
fy(c(0.3, 0.5, 1, 1.5, 2, 2.5))

y.seq <- seq(0.4, 2.1, length = 101)
fy.seq <- fy(y.seq)
plot(y.seq, fy.seq, type = "l")
## forma laternativa de produzir o gráfico:
curve(fy(x), from = 0.4, to = 2.1)

integrate(fy, 0.5, 2)$value

## F(y) = (8/9)[y^2/2 - 0.5^2/2] - (4/9)(x - 0.5)
Fy <- function(y){
    res <- numeric(length(y))
    ind <- y >= 0.5 & y < 2
    res[ind] <- (8/9)*((y[ind]^2)/2- (0.5^2)/2) - (4/9)*(y[ind]-0.5)
    res[y >= 2] <-  1    
    return(res)
}
curve(Fy(x), from = 0.4, to = 2.1)

## a) P[Y < 0.8]
(0.8-0.5)*fy(0.8)/2
integrate(fy, 0.5, 0.8)$value
Fy(0.8)

## b) P[Y > 1.5 | Y >= 1] = P[Y > 1.5 & Y >= 1]/P[Y >= 1] = P[Y > 1.5]/P[Y >= 1] 
1 - (1.5 - 0.5)*fy(1.5)/2  ## P[Y > 1.5]
1 - (1.0 - 0.5)*fy(1.1)/2  ## P[Y > 1.0]
(1 - (1.5 - 0.5)*fy(1.5)/2)/(1 - (1.0 - 0.5)*fy(1.0)/2)  ## P[Y > 1,5] / P[Y > 1.0]
integrate(fy, 1.5, 2)$value/integrate(fy, 1, 2)$value
(1-Fy(1.5))/(1-Fy(1.0))

## c) Valor esperado e variância
Efun <- function(y) y * fy(y)
(EY <- integrate(Efun, lower = 0.5, upper = 2)$value)
E2fun <- function(y) y^2 * fy(y)
(EY2 <- integrate(E2fun, lower = 0.5, upper = 2)$value)
(VarY <- EY2 - EY^2)
## ou
Vfun <- function(y) (y-EY)^2 * fy(y)
integrate(Vfun, lower = 0.5, upper = 2)$value

rm(fy, Fy, y.seq, fy.seq, EY, EY2, VarY)
##
## Fim do exemplo 3
##

##
## Exercício: Fazer versão computacional dos exercícios 12 e 16
##            - definir e plotar a função de densidade f(y)

