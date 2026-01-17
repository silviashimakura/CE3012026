
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir simulam o lançamento de um dado diversas vezes
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
faces <- 1:6
#-----------------------------------------------------------------------
# Número de lançamentos
n_lancamentos <- 3500
#-----------------------------------------------------------------------
# Jogos

## Dado honesto
lancamentos <- sample(faces, 
                      n_lancamentos, 
                      replace = TRUE)

## Dado desonesto
# lancamentos <- sample(faces, 
#                       n_lancamentos, 
#                       replace = TRUE,
#                       prob = c(0.01,0.05,0.1,0.1,0.3,0.44))

#-----------------------------------------------------------------------
# Organizando resultados
resultados <- data.frame(tentativa = 1:n_lancamentos,
                         resultado = lancamentos)

resultados$face1 <- cumsum(ifelse(resultados$resultado == 1,1,0))/resultados$tentativa
resultados$face2 <- cumsum(ifelse(resultados$resultado == 2,1,0))/resultados$tentativa
resultados$face3 <- cumsum(ifelse(resultados$resultado == 3,1,0))/resultados$tentativa
resultados$face4 <- cumsum(ifelse(resultados$resultado == 4,1,0))/resultados$tentativa
resultados$face5 <- cumsum(ifelse(resultados$resultado == 5,1,0))/resultados$tentativa
resultados$face6 <- cumsum(ifelse(resultados$resultado == 6,1,0))/resultados$tentativa

legenda <- c("1" = 2, 
             "2" = 3,
             "3" = 4, 
             "4" = 5, 
             "5" = 6, 
             "6" = 7)
#-----------------------------------------------------------------------
# Plotando os resultados
ggplot(resultados, aes(x = tentativa)) +
  geom_line(aes(y = face1, color = "1"), size = 1) +
  geom_line(aes(y = face2, color = "2"), size = 1) +
  geom_line(aes(y = face3, color = "3"), size = 1) +
  geom_line(aes(y = face4, color = "4"), size = 1) +
  geom_line(aes(y = face5, color = "5"), size = 1) +
  geom_line(aes(y = face6, color = "6"), size = 1) +
  geom_hline(yintercept=1/6, 
             linetype="dashed", color = "black") +
  ylim(0,1) +
  #transition_reveal(tentativa) +
  labs(x = "Lançamentos",
       y = "Freq. Relativa",
       color = "") +
  scale_color_manual(values = legenda)+ 
  theme_bw() + 
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +
  ggtitle("Dado")

#-----------------------------------------------------------------------
# Comparando as frequências relativas com as probabilidades esperadas
resultados[nrow(resultados),]
#-----------------------------------------------------------------------
