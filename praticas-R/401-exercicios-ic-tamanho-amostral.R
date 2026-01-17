
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Estimação pontual, intervalar e tamanho amostral
#-----------------------------------------------------------------------

# 1.

## Um pesquisador deseja estudar o efeito de certa 
## substância no tempo de reação de seres vivos a um 
## certo tipo de estímulo.

## Um experimento foi desenvolvido com cobaias que 
## recebem a substância e logo em seguida são 
## submetidas a um estímulo elétrico. 

## Para cada cobaia foi avaliado o tempo de reação em 
## segundos. 

## O tempo de reação considerado normal é de 8 segundos e
## estudos anteriores afirmam que o desvio padrão é igual 
## a 2.

## O pesquisador desconfia que o tempo médio sofre 
## alteração por influência da substância. 

## Obtenha um intervalo com 95% de confiança e interprete
## o resultado.

tempos <- 
  c(13.4, 9.6, 4.8, 8.6, 7.6, 11.2, 9.1,
    8.5, 7.8, 10.3, 7.9, 5.5, 11.6, 10.2,
    9.8, 8.6, 9.3, 10.8, 8.0, 6.7, 6.3,
    11.2, 9.8, 10.7, 8.1, 11.8, 9.7, 9.8,
    7.6, 8.8, 9.7, 10.2, 10.1, 6.7, 7.5,
    9.5, 9.3, 6.7, 4.6, 10.3, 7.1, 12.7,
    10.4, 10.7, 11.1, 8.6, 10.6, 9.2, 9.6)

#-----------------------------------------------------------------------

# 2.

## Deseja-se investigar se uma certa moléstia que ataca o 
## rim altera o consumo de oxigênio desse orgão. 

## Para indivíduos sadios, admite-se que esse consumo tem 
## distribuição Normal com média 12cm^3/min. 

## Os valores medidos em pacientes com a moléstia são: 

valores <- 
  c(12.5, 13.2, 12.3, 14.3, 13.3, 
    12.3, 13.4, 13.6, 13.5, 12.7)

## Obtenha um intervalo com 90% de confiança e interprete
## o resultado.

#-----------------------------------------------------------------------

# 3.

## Um antigo relatório de uma companhia afirma que 40% de toda 
## a água obtida por meio de poços artesianos em determinada 
## região é imprópria para consumo. 

## Devido a uma série de mudanças na região existe uma forte 
## suspeita de que este percentual seja maior do que 40%. 

## Para dirimir as dúvidas, 400 poços foram sorteados e 
## observou-se que em 184 deles a água era imprópria. 

## Obtenha intervalos com 99% de confiança para os cenários
## otimista e conservativo e interprete os resultados.

#-----------------------------------------------------------------------

# 4.

## Um analista da qualidade está avaliando a variabilidade do 
## tamanho de peças na produção de um componente. 

## Caso a variância exceda 2 unidades de medida, existirá uma 
## proporção inaceitável de peças que que causarão problemas. 

## Para avaliar se a variabilidade está dentro do controle, 
## uma amostra foi tomada. 
  
tamanho <- 
  c(103.23,  98.31,  99.02,  99.42,  98.63,
    98.66, 101.06,  99.83, 100.22, 103.10,
    100.50, 103.84, 103.23, 100.46, 102.68)

## Assuma que o tamanho das peças segue distribuição normal.

## Obtenha um intervalo com 80% de confiança e interprete 
## os resultados.

#-----------------------------------------------------------------------

# 5.

## Uma escola para crianças com necessidades especiais deseja 
## estudar características como capacidade de leitura e 
## interpretação de texto. 

## A ideia é aplicar uma prova e avaliar as notas desta prova. 

## Existem evidências para crer que a nota tem desvio padrão igual 
## a 3.5 unidades. 

## Quantos alunos a escola deve amostrar para que, com 90% de 
## confiança, a média amostral esteja a menos de 1 ponto da média 
## populacional.

#-----------------------------------------------------------------------

# 6. 

## Um médico tem razões para crer que existe um percentual de 
## crianças que apresenta uma doença em uma região. 

## Considere que há interesse em estimar esse percentual para 
## fins de logística operacional do tratamento. 

## Quantas crianças precisarão ser amostradas para que a estimativa 
## apresente um erro máximo de 5% com um nível de confiança de 99%?

## Obtenha uma estimativa otimista, considerando existe evidência para 
## crer que o verdadeiro percentual é algo próximo de 10%.

## Obtenha também uma estimativa conservativa, isto é, considere que 
## não há qualquer informação a priori que possa ser usada a respeito 
## do verdadeiro percentual.

#-----------------------------------------------------------------------

# 7.

## Um pesquisador tem interesse em estimar o gasto médio com mercado 
## de trabalhadores de determinado ramo. 

## Sabe-se a priori que os gastos variam de R$850,00 a R$2500,00.

## Quantos trabalhadores devem ser selecionados para termos 95% de 
## confiança de que a média amostral de gastos esteja a menos de 
## R$100,00 da média populacional.

#-----------------------------------------------------------------------