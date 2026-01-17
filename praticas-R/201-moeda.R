
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir simulam o lançamento de uma moeda diversas vezes
# e verifica as alterações das frequências relativas conforme o número
# de lançamentos aumenta.
#
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------
library(gganimate)
library(ggplot2)
#-----------------------------------------------------------------------
# Faces
faces <- c('cara', 'coroa')
#-----------------------------------------------------------------------
# Número de lançamentos
n_lancamentos <- 1000
#-----------------------------------------------------------------------
# Jogos

## Moeda honesta
lancamentos <- sample(faces, 
                      n_lancamentos, 
                      replace = TRUE)

## Moeda desonesta
#lancamentos <- sample(faces, 
#                      n_lancamentos, 
#                      replace = TRUE,
#                      prob = c(0.7, 0.3))
#-----------------------------------------------------------------------
# Organizando resultados
resultados <- data.frame(tentativa = 1:n_lancamentos,
                    resultado = lancamentos)

resultados$caras <- cumsum(ifelse(resultados$resultado == 'cara',1,0))/resultados$tentativa
resultados$coroas <- cumsum(ifelse(resultados$resultado == 'coroa',1,0))/resultados$tentativa
#-----------------------------------------------------------------------
# Plotando os resultados
legenda <- c("Cara" = 2, 
             "Coroa" = 4)

ggplot(resultados, aes(x = tentativa)) +
  geom_line(aes(y = caras, color = "Cara"), size = 1) +
  geom_line(aes(y = coroas, color = "Coroa"), size = 1) +
  geom_hline(yintercept=0.5, 
             linetype="dashed", color = "black") +
  ylim(0,1) +
  #transition_reveal(tentativa) +
  labs(x = "Lançamentos",
       y = "Freq. Relativa",
       color = "") +
  scale_color_manual(values = legenda) + 
  theme_bw() + 
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +
  ggtitle("Moeda")

#-----------------------------------------------------------------------
# Comparando as frequências relativas com as probabilidades esperadas
resultados[nrow(resultados),]
#-----------------------------------------------------------------------