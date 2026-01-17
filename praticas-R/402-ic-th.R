
#----------------------------------------------------------------------

# Para cada um dos problemas a seguir:

## a) Faça uma análise exploratória dos dados.

## b) Obtenha estimativas pontuais e intervalares das quantidades de 
## interesse e interprete os resultados.

## c) Proceda o teste de hipóteses adequado ao problema 
## (defina hipóteses, estatística de teste, distribuição amostral,
## região de rejeição, p-valor e conclusão).

#----------------------------------------------------------------------

# Média com variância populacional conhecida

## Um pesquisador deseja estudar o efeito de certa substância no tempo de 
## reação de seres vivos a um certo tipo de estímulo. Ele desconfia que o 
## tempo médio sofre alteração por influência da substância. Para avaliar 
## sua suposição, um experimento foi desenvolvido com cobaias que 
## receberam a substância e logo em seguida foram submetidas a um 
## estímulo elétrico. Para cada cobaia foi avaliado o tempo de reação em 
## segundos. Admite-se que, em situações típicas, o tempo de reação 
## segue o modelo Normal com média 8 e desvio padrão 2 segundos. 
## A um nível de significância de 5%, podemos concluir 

set.seed(1)
cobaias <- round(rnorm(100, 9, 1), 2)

### Exploratória
summary(cobaias)
hist(cobaias)

### Estimativa pontual e intervalar
alpha <- 0.05
y_barra <- mean(cobaias)
z_intervalo <- qnorm((1-(alpha/2)))
sigma <- 2
n <- length(cobaias)

y_barra - (z_intervalo*(sigma/sqrt(n)))
y_barra + (z_intervalo*(sigma/sqrt(n)))

### Teste de hipóteses
#### Hipóteses: H0: mu = 8 x H1: mu != 8

#### z calculado
mu0 <- 8
z_calculado <- (y_barra - mu0)/(sigma/sqrt(n))

#### z critico
z_critico_inferior <- qnorm(((alpha/2)))
z_critico_superior <- qnorm((1-(alpha/2)))

#### Confrontando valor calculado com região crítica
z_calculado < z_critico_inferior
z_calculado > z_critico_superior

#### p-valor
(1-pnorm(z_calculado))*2

#### De forma visual
plot(seq(-6,6, 0.01), 
     dnorm(seq(-6,6, 0.01)), 
     type = 'l',
     xlab = 'Z',
     ylab = 'Densidade')

segments(x0 = z_critico_inferior, 
         y0 = 0, 
         x1 = z_critico_inferior, 
         y1 = dnorm(z_critico_inferior))

segments(x0 = z_critico_superior, 
         y0 = 0, 
         x1 = z_critico_superior, 
         y1 = dnorm(z_critico_superior))

points(z_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta

library(BSDA)

z.test(
  x = cobaias,
  mu = mu0,
  sigma.x = sigma,
  alternative = "two.sided",
  conf.level = 0.95
)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para média com variância populacional desconhecida

## Deseja-se investigar se uma certa moléstia que ataca o rim altera o 
## consumo de oxigênio desse orgão. Para indivíduos sadios, admite-se 
## que esse consumo tem distribuição Normal com média $12cm^3/min$. 
## Qual seria a conclusão ao nível de significância de 1%?

set.seed(2)
consumo <- round(rnorm(50, 13, 0.82), 2)

### Exploratória
summary(consumo)
hist(consumo)

### Estimativa pontual e intervalar
alpha <- 0.01
n <- length(consumo)
y_barra <- mean(consumo)
t_intervalo <- qt((1-(alpha/2)), df = n-1)
s <- sd(consumo)

y_barra - (t_intervalo*(s/sqrt(n)))
y_barra + (t_intervalo*(s/sqrt(n)))

### Teste de hipóteses
#### Hipóteses: H0: mu = 12 x H1: mu != 12

#### t calculado
mu0 <- 12
t_calculado <- (y_barra - mu0)/(s/sqrt(n))

#### t critico
t_critico_inferior <- qt(((alpha/2)), df = n-1)
t_critico_superior <- qt((1-(alpha/2)), df = n-1)

#### Confrontando valor calculado com região crítica
t_calculado < t_critico_inferior
t_calculado > t_critico_superior

#### p-valor
2*(1-pt(t_calculado, df = n-1))

#### De forma visual
plot(seq(-9,9, 0.01), 
     dt(seq(-9,9, 0.01),
        df = n-1), 
     type = 'l',
     xlab = 't',
     ylab = 'Densidade')

segments(x0 = t_critico_inferior, 
         y0 = 0, 
         x1 = t_critico_inferior, 
         y1 = dt(t_critico_inferior,
                 df = n-1))

segments(x0 = t_critico_superior, 
         y0 = 0, 
         x1 = t_critico_superior, 
         y1 = dt(t_critico_superior,
                 df = n-1))

points(t_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
t.test(x = consumo,
       alternative = "two.sided",
       mu = mu0, 
       paired = FALSE, 
       conf.level = 0.99)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para uma proporção

## Um antigo relatório de uma companhia afirma que 40% de toda a 
## água obtida por meio de poços artesianos em determinada região 
## é imprópria para consumo. Devido a uma série de mudanças na região 
## existe uma forte suspeita de que este percentual seja maior do que 
## 40%. Para dirimir as dúvidas, 400 poços foram sorteados.
## Qual seria a conclusão, ao nível de 1% de significância?
## Considere 1: contaminado e 0: não contaminado.

set.seed(3)
pocos <- sample(0:1, 400, replace = T, prob = c(0.54, 0.46))

### Exploratória
table(pocos)
barplot(table(pocos))

### Estimativa pontual e intervalar
alpha <- 0.01
n <- length(pocos)
p_chapeu <- sum(pocos)/length(pocos)
z_intervalo <- qnorm((1-(alpha/2)))

p_chapeu - (z_intervalo*(sqrt((p_chapeu*(1-p_chapeu))/n)))
p_chapeu + (z_intervalo*(sqrt((p_chapeu*(1-p_chapeu))/n)))

### Teste de hipóteses
#### Hipóteses: H0: p = 0.4 x H1: p > 0.4

#### z calculado
p0 <- 0.4
z_calculado <- (p_chapeu - p0)/(sqrt((p0*(1-p0))/n))

#### z critico
z_critico <- qnorm((1-(alpha)))

#### Confrontando valor calculado com região crítica
z_calculado > z_critico

#### p-valor
1-pnorm(z_calculado)

#### De forma visual
plot(seq(-6,6, 0.01), 
     dnorm(seq(-6,6, 0.01)), 
     type = 'l',
     xlab = 'Z',
     ylab = 'Densidade')

segments(x0 = z_critico, 
         y0 = 0, 
         x1 = z_critico, 
         y1 = dnorm(z_critico))

points(z_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
prop.test(x = sum(pocos),
          n = length(pocos),
          p = 0.4,
          alternative = "greater",
          correct = F,
          conf.level = 0.99)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para uma variância

## Um analista da qualidade está avaliando a variabilidade do 
## tamanho de peças na produção de um componente. Caso a variância 
## exceda 2 unidades de medida, existirá uma proporção inaceitável de 
## peças que que causarão problemas. Para avaliar se a variabilidade 
## está dentro do controle, uma amostra foi tomada.
## Assumindo que o tamanho das peças segue distribuição normal e considerando 
## um nível de significância de 5%, pode-se afirmar que a variabilidade está 
## acima do aceitável?

set.seed(4)
pecas <- round(rnorm(100, 100, sqrt(2)), 2)

### Exploratória
summary(pecas)
boxplot(pecas)

### Estimativa pontual e intervalar
alpha <- 0.05
n <- length(pecas)
s <- sd(pecas)
chi_intervalo_inferior <- qchisq((alpha/2), n-1)
chi_intervalo_superior <- qchisq(1-(alpha/2), n-1)

((n-1)*s^2)/chi_intervalo_superior
((n-1)*s^2)/chi_intervalo_inferior

### Teste de hipóteses
#### Hipóteses: H0: sigma = 2 x H1: sigma > 2

#### chi calculado
chi0 <- 2
chi_calculado <- ((n-1)*s^2)/chi0

#### z critico
chi_critico <- qchisq((1-(alpha)), df = n-1)

#### Confrontando valor calculado com região crítica
chi_calculado > chi_critico

#### p-valor
1-pchisq(chi_calculado, df = n-1)

#### De forma visual
plot(seq(60,200, 0.01), 
     dchisq(seq(60,200, 0.01), 
            df = n-1), 
     type = 'l',
     xlab = 'chi^2',
     ylab = 'Densidade')

segments(x0 = chi_critico, 
         y0 = 0, 
         x1 = chi_critico, 
         y1 = dchisq(chi_critico, df = n-1))

points(chi_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta

library(EnvStats)

varTest(x = pecas, 
        alternative = "greater", 
        sigma.squared =  2, 
        conf.level = (1 - alpha))

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para duas médias independentes com variância 
# populacional conhecida

## Um grupo de pesquisadores conduziu um estudo para avaliar o efeito de 
## atividade física nos batimentos cardíacos (em minutos). Foram definidos 
## 2 grupos. O primeiro grupo era composto por indivíduos ativos e o segundo 
## grupo composto por indivíduos sedentários. 
## Assumindo que a distribuição dos batimentos para ambos os grupos é normal 
## com desvio padrão igual a 10 para ativos e igual a 12 para sedentários,
## existe evidência suficiente nos dados que permita afirmar que há diferença
## entre as médias dos dois grupos? Considere um nível de significância de 1%.

set.seed(5)
ativos <- rnorm(50, 91, 10)
sedentarios <- rnorm(60, 126, 12)

### Exploratória
summary(ativos)
summary(sedentarios)

plot(density(ativos), 
     xlim = range(c(ativos, sedentarios)),
     ylim = c(0, 0.04))

lines(density(sedentarios), col = 2)

### Estimativa pontual e intervalar
alpha <- 0.01

y_barra_ativos <- mean(ativos)
y_barra_sedentarios <- mean(sedentarios)

diferenca <- y_barra_ativos-y_barra_sedentarios

z_intervalo <- qnorm((1-(alpha/2)))

sigma_ativos <- 10
sigma_sedentarios <- 12

n_ativos <- length(ativos)
n_sedentarios <- length(ativos)

diferenca - (z_intervalo*
               sqrt((sigma_ativos^2/n_ativos) + 
                      (sigma_sedentarios^2/n_sedentarios)))

diferenca + (z_intervalo*
               sqrt((sigma_ativos^2/n_ativos) + 
                      (sigma_sedentarios^2/n_sedentarios)))


### Teste de hipóteses
#### Hipóteses: H0: mu = 0 x H1: mu != 0

#### z calculado
mu0 <- 0

z_calculado <- diferenca/sqrt((sigma_ativos^2/n_ativos) + 
                                (sigma_sedentarios^2/n_sedentarios))

#### z critico
z_critico_inferior <- qnorm(((alpha/2)))
z_critico_superior <- qnorm((1-(alpha/2)))

#### Confrontando valor calculado com região crítica
z_calculado < z_critico_inferior
z_calculado > z_critico_superior

#### p-valor
(pnorm(z_calculado))*2

#### De forma visual
plot(seq(-16,16, 0.01), 
     dnorm(seq(-16,16, 0.01)), 
     type = 'l',
     xlab = 'Z',
     ylab = 'Densidade')

segments(x0 = z_critico_inferior, 
         y0 = 0, 
         x1 = z_critico_inferior, 
         y1 = dnorm(z_critico_inferior))

segments(x0 = z_critico_superior, 
         y0 = 0, 
         x1 = z_critico_superior, 
         y1 = dnorm(z_critico_superior))

points(z_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta

library(BSDA)

z.test(
  x = ativos,
  y = sedentarios,
  sigma.x = sigma_ativos,
  sigma.y = sigma_sedentarios,
  mu = 0,
  alternative = "two.sided",
  conf.level = 0.99
)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para duas médias independentes com variância 
# populacional desconhecida

set.seed(6)

#----------------------------------------------------------------------

# Teste de hipóteses para duas médias independentes com variância 
# populacional desconhecida mas variâncias amostrais iguais

set.seed(7)

#----------------------------------------------------------------------

# Teste de hipóteses para diferença de médias (amostras pareadas)

## Um estudo tinha como objetivo avaliar o efeito de uma aula no 
## conhecimento dos alunos. Para isso, um grupo de alunos de uma 
## escola foi sorteado ao acaso e submetidos a uma avaliação de um 
## tema que não tinham conhecimento prévio. Após esta avaliação, os 
## alunos tiveram uma aula do tema e foram submetidos a uma segunda 
## avaliação. Considerando um nível de significância de 10%, podemos 
## afirmar que após a aula a nota dos alunos foi superior?

set.seed(8)
antes <- round(rnorm(40, 45, 13))
depois <- round(rnorm(40, 69, 12))

### Exploratória
summary(antes)
summary(depois)

plot(density(antes), 
     xlim = range(c(antes, depois)),
     ylim = c(0, 0.035))

lines(density(depois), col = 2)

### Estimativa pontual e intervalar
alpha <- 0.1
n <- length(antes)

diferenca <- antes-depois
y_barra_diferenca <- mean(diferenca)
s_diferenca <- sd(diferenca)

t_intervalo <- qt((1-(alpha/2)), df = n-1)

y_barra_diferenca - (t_intervalo*(s_diferenca/sqrt(n)))
y_barra_diferenca + (t_intervalo*(s_diferenca/sqrt(n)))

### Teste de hipóteses
#### Hipóteses: H0: mu = 0 x H1: mu < 0

#### t calculado
mu0 <- 0
t_calculado <- (y_barra_diferenca - mu0)/(s_diferenca/sqrt(n))

#### t critico
t_critico <- qt(alpha, df = n-1)

#### Confrontando valor calculado com região crítica
t_calculado < t_critico

#### p-valor
pt(t_calculado, df = n-1)

#### De forma visual
plot(seq(-8,8, 0.01), 
     dt(seq(-8,8, 0.01),
        df = n-1), 
     type = 'l',
     xlab = 't',
     ylab = 'Densidade')

segments(x0 = t_critico, 
         y0 = 0, 
         x1 = t_critico, 
         y1 = dt(t_critico,
                 df = n-1))

points(t_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
t.test(x = antes,
       y =  depois,
       alternative = "less", 
       paired = TRUE, 
       conf.level = 0.90)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para diferença de proporções

## Considere uma empresa que faz vendas de produtos usando uma página web. 
## Algumas análises mostraram que uma determinada seção do site vem sendo 
## menos acessada do que o esperado. A equipe de marketing digital da empresa 
## quer avaliar se uma mudança de cores nesta parte específica da página faz 
## com que o número de acessos aumente. 

## Para avaliar o efeito da mudança dos elementos na página os dois cenários 
## foram colocados em produção sem que os usuários soubessem e foi avaliado,
## para cada usuário se ele acessou ou não a seção de interesse.

## Existe evidência de que a alteração no esquema de cores causou aumento no 
## número de acessos? Proceda o teste de hipóteses adequado a um nível de 
## significância de 5%.

## Considere 1: acessou e 0: não acessou.

set.seed(9)
cores1 <- sample(0:1, 500, replace = T, prob = c(0.53, 0.47))
cores2 <- sample(0:1, 500, replace = T, prob = c(0.45, 0.55))  

### Exploratória
table(cores1)
table(cores2)

barplot(table(cores1))
barplot(table(cores2))

### Estimativa pontual e intervalar
alpha <- 0.05

n1 <- length(cores1)
n2 <- length(cores2)

p_chapeu1 <- sum(cores1)/length(cores1)
p_chapeu2 <- sum(cores2)/length(cores2)

z_intervalo <- qnorm((1-(alpha/2)))

(p_chapeu1-p_chapeu2)-z_intervalo*sqrt(((p_chapeu1*(1-p_chapeu1))/n1) + ((p_chapeu2*(1-p_chapeu2))/n2))
(p_chapeu1-p_chapeu2)+z_intervalo*sqrt(((p_chapeu1*(1-p_chapeu1))/n1) + ((p_chapeu2*(1-p_chapeu2))/n2))

### Teste de hipóteses
#### Hipóteses: H0: p1-p2 = 0 x H1: p1-p2 < 0

#### z calculado
p0 <- 0
p_barra <- (sum(cores1) + sum(cores2))/(n1+n2)

z_calculado <- (p_chapeu1-p_chapeu2)/sqrt((p_barra*(1-p_barra))*((1/n1)+(1/n2)))

#### z critico
z_critico <- qnorm(alpha)

#### Confrontando valor calculado com região crítica
z_calculado < z_critico

#### p-valor
pnorm(z_calculado)

#### De forma visual
plot(seq(-6,6, 0.01), 
     dnorm(seq(-6,6, 0.01)), 
     type = 'l',
     xlab = 'Z',
     ylab = 'Densidade')

segments(x0 = z_critico, 
         y0 = 0, 
         x1 = z_critico, 
         y1 = dnorm(z_critico))

points(z_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
prop.test(x = c(sum(cores1), sum(cores2)),
          n = c(length(cores1), length(cores2)),
          alternative = "less",
          correct = F,
          conf.level = 0.95)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para razão de variâncias

## Queremos verificar se duas máquinas produzem peças com a mesma 
## homogeneidade quanto à resistência à tensão. Para isso, sorteamos 
## duas amostras de peças de cada máquina e obtivemos as resistências. 
## Considerando que as resistências seguem distribuição normal, existe 
## evidência para afirmar que a variabilidade das duas máquinas difere 
## a um nível de significância de 10%?

set.seed(10)
maquina1 <- round(rnorm(100, 138, 35), 2)
maquina2 <- round(rnorm(95, 135, 40), 2)

### Exploratória
summary(maquina1)
summary(maquina2)

par(mfrow = c(1,2))
boxplot(maquina1, ylim = c(40,220))
boxplot(maquina2, ylim = c(40,220))

### Estimativa pontual e intervalar
alpha <- 0.1

n1 <- length(maquina1)
n2 <- length(maquina2)

s1 <- sd(maquina1)
s2 <- sd(maquina2)

(1/qf(alpha/2, df1 = n1-1, df2 = n2-1)) * ((s1^2)/(s2^2))
(qf(alpha/2, df1 = n2-1, df2 = n1-1)) * ((s1^2)/(s2^2))

### Teste de hipóteses
#### Hipóteses: H0: sigma1/sigma2 = 1 x H1: sigma1/sigma2 != 1

#### F calculado
F_calculado <- s1^2/s2^2

#### z critico
F_critico_inferior <- qf(alpha/2, df1 = n1-1, df2 = n2-1)
F_critico_superior <- qf((1-(alpha/2)), df1 = n1-1, df2 = n2-1)

#### Confrontando valor calculado com região crítica
F_calculado > F_critico_superior
F_calculado < F_critico_inferior

#### p-valor
2*pf(F_calculado, df1 = n1-1, df2 = n2-1)

#### De forma visual
plot(seq(0.5,3, 0.01), 
     df(seq(0.5,3, 0.01), 
        df1 = n1-1,
        df2 = n2-1), 
     type = 'l',
     xlab = 'chi^2',
     ylab = 'Densidade')

segments(x0 = F_critico_inferior, 
         y0 = 0, 
         x1 = F_critico_inferior, 
         y1 = df(F_critico_inferior, df1 = n1-1, df2 = n2-1))

segments(x0 = F_critico_superior, 
         y0 = 0, 
         x1 = F_critico_superior, 
         y1 = df(F_critico_superior, df1 = n1-1, df2 = n2-1))

points(F_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
var.test(maquina1, 
         maquina2, 
         ratio = 1,
        alternative = "two.sided",
        conf.level = 0.90)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para associação entre variáveis qualitativas

## Em um estudo para verificar a relação entre crises de asma e 
## incidência de gripe, um grupo de crianças foi escolhido ao acaso 
## dentre aquelas acompanhadas por um posto de saúde.
## A um nível de significância de 5%, existe evidência de que as 
## ocorrências de asma e gripe são independentes?  

set.seed(11)
asma <- rbinom(100,1,0.6)
gripe <- rbinom(100,1,0.6)

### Exploratória
tabela <- table(asma, gripe)
tabela

plot(tabela)

### Obtenção dos totais linha e coluna
total_linha <- addmargins(tabela)[1:2,3]
total_coluna <- addmargins(tabela)[3,1:2]

### Obtenção da matriz de valores esperados
esperados <- outer(total_linha,
                   total_coluna)/sum(tabela)

### Cálculo do valor de chi
chi_calculado <- sum(((tabela - esperados)^2)/(esperados))

### Teste de hipóteses
#### Hipóteses: H0: chi = 0 x H1: chi > 0
alpha <- 0.05
chi_critico <- qchisq(1-alpha, 
                      df = (nrow(tabela)-1) * (ncol(tabela)-1))

chi_calculado > chi_critico

#### p-valor
1-pchisq(chi_calculado, df = (nrow(tabela)-1) * (ncol(tabela)-1))

#### De forma visual
plot(seq(0,5, 0.01), 
     dchisq(seq(0,5, 0.01), 
        df = (nrow(tabela)-1) * (ncol(tabela)-1)), 
     type = 'l',
     xlab = 'chi^2',
     ylab = 'Densidade')

segments(x0 = chi_critico, 
         y0 = 0, 
         x1 = chi_critico, 
         y1 = dchisq(chi_critico, 
                     df = (nrow(tabela)-1) * (ncol(tabela)-1)))

points(chi_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
chisq.test(tabela, correct = F)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipóteses para aderência

## Um dado foi fabricado com o centro em madeira leve e cada face com 
## uma chapa metálica porém de diferentes características 
## (espessura/densidade) em cada face. Este dado foi lançado diversas 
## vezes e a face resultante foi anotada.
## A um nível de significância de 5%, existe evidência para crer que o 
## dado é viciado?

set.seed(12)
faces <- sample(1:6, size = 350, replace = T)

### Exploratória
tabela <- table(faces)
tabela
plot(tabela)

### Obtenção dos valores esperados
esperados <- sum(tabela)/6

### Cálculo do valor de chi
chi_calculado <- sum(((tabela - esperados)^2)/(esperados))

### Teste de hipóteses
#### Hipóteses: H0: chi = 0 x H1: chi > 0
alpha <- 0.05
chi_critico <- qchisq(1-alpha, 
                      df = length(tabela)-1)

chi_calculado > chi_critico

#### p-valor
1-pchisq(chi_calculado, df = length(tabela)-1)

#### De forma visual
plot(seq(0,15, 0.01), 
     dchisq(seq(0,15, 0.01), 
            df = length(tabela)-1), 
     type = 'l',
     xlab = 'chi^2',
     ylab = 'Densidade')

segments(x0 = chi_critico, 
         y0 = 0, 
         x1 = chi_critico, 
         y1 = dchisq(chi_critico, 
                     df = length(tabela)-1))

points(chi_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
chisq.test(tabela, correct = F)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipótese para correlação (rho0 = 0)

## Uma empresa que fabrica automóveis está fazendo testes em um modelo 
## para avaliar modificações a serem feitas na próxima geração. Havia uma 
## suspeita de que ocorria uma alteração no consumo médio de acordo com 
## a quilometragem dos veículos, de tal modo que quanto maior a 
## quilometragem, maior o consumo médio. Para avaliar se isso ocorria,
## uma amostra de veículos foi selecionada ao acaso e anotou-se a
## quilometragem e o consumo médio de combustível. Com base nos dados,
## a suspeita do fabricante é válida ou não? Considere um nível de 
## significância de 1%.

set.seed(13)  

v1 = round(rnorm(100, 100, 20))
v2 = round(rnorm(100, 20, 10))

km <- v1*300
consumo <- round((v1+v2)/13,2)

### Exploratória
summary(km)
summary(consumo)

cor(km,consumo)
plot(consumo~km)

### Estimativa pontual e intervalar
alpha <- 0.01
n <- length(consumo)
correlacao <- cor(km,consumo)
z_intervalo <- qnorm((1-(alpha/2)))

tanh(atanh(correlacao) - (z_intervalo/sqrt(n-3)))
tanh(atanh(correlacao) + (z_intervalo/sqrt(n-3)))

### Teste de hipóteses
#### Hipóteses: H0: rho = 0 x H1: rho > 12

#### t calculado
rho0 <- 0
t_calculado <- (correlacao*sqrt(n-2))/(sqrt(1-correlacao^2))

#### t critico
t_critico <- qt((1-alpha), df = n-2)

#### Confrontando valor calculado com região crítica
t_calculado > t_critico

#### p-valor
1-pt(t_calculado, df = n-2)

#### De forma visual
plot(seq(-5,17, 0.01), 
     dt(seq(-5,17, 0.01),
        df = n-2), 
     type = 'l',
     xlab = 't',
     ylab = 'Densidade')

segments(x0 = t_critico, 
         y0 = 0, 
         x1 = t_critico, 
         y1 = dt(t_critico,
                 df = n-2))

points(t_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta
cor.test(consumo, km,
         alternative = "greater",
         method = "pearson",
         conf.level = 0.95, 
         continuity = FALSE)

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------

# Teste de hipótese para correlação (rho0 != 0)

## Em uma indústria é realizado um curso de treinamento após 1 ano de 
## admissão dos operários. Historicamente, o coeficiente de correlação 
## entre a nota final do curso e a produtividade dos operários após 6 
## meses do curso é 0,4. 

## Para uma nova versão, foram introduzidas modificações no curso, com 
## o objetivo de aumentar a produtividade. 

## Em uma amostra de operários submetidos ao novo modelo, foi verificada
## a nota no curso e um índice de produtividade 6 meses depois do curso.

## Você diria que os objetivos das alterações no curso foram atingidos? 
## Proceda o teste de hipóteses adequado considerando um nível de 
## significância de 10%.

set.seed(14)

v1 = round(rnorm(100, 67, 10))
v2 = round(rnorm(100, 7, 1))

nota <- v1*300
produtividade <- (v1*v2)/10

### Exploratória
summary(nota)
summary(produtividade)

cor(nota,produtividade)
plot(produtividade~nota)

### Estimativa pontual e intervalar
alpha <- 0.1
n <- length(nota)
correlacao <- cor(nota,produtividade)
z_intervalo <- qnorm((1-(alpha/2)))

tanh(atanh(correlacao) - (z_intervalo/sqrt(n-3)))
tanh(atanh(correlacao) + (z_intervalo/sqrt(n-3)))

### Teste de hipóteses
#### Hipóteses: H0: rho = 0.4 x H1: rho > 0.4

#### t calculado
rho0 <- 4
z_calculado <- (atan(correlacao) - atan(rho0))*sqrt(n-3)

#### z critico
z_critico <- qnorm(1-alpha)

#### Confrontando valor calculado com região crítica
z_calculado > z_critico

#### p-valor
1-pnorm(z_calculado)

#### De forma visual
plot(seq(-8,8, 0.01), 
     dnorm(seq(-8,8, 0.01)), 
     type = 'l',
     xlab = 'z',
     ylab = 'Densidade')

segments(x0 = z_critico, 
         y0 = 0, 
         x1 = z_critico, 
         y1 = dnorm(z_critico))

points(z_calculado,0, pch = 19, col = 4, cex = 1.5)

# Usando uma função pronta

#----------------------------------------------------------------------
rm(list = ls())
#----------------------------------------------------------------------
