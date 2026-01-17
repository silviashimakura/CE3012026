
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir simulam o segundo jogo do problema de Chavalier:
# lançam-se dois dados até no máximo 24 vezes e ganha se obtiver um 
# duplo seis.
# 
# O jogo é simulado diversas vezes e são calculadas as frequências 
# relativas de vitórias e derrotas conforme o número de lançamentos 
# aumenta.
#
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------
library(gganimate)
library(ggplot2)
#-----------------------------------------------------------------------
# Número de jogadas
n_jogos <- 3500
#-----------------------------------------------------------------------
# Jogadas
rodada <- c()
jogos <- c()

for (j in 1:n_jogos) {
  for (i in 1:24) {
    rodada[i] <- ifelse(sum(c(sample(1:6, 1), sample(1:6, 1)) == 6) == 2,
                        'vitória', 'derrota')
  }
  
  jogos[j] <- ifelse(sum(rodada == 'vitória') >= 1, 'vitória', 'derrota')
  
}
#-----------------------------------------------------------------------
# Organizando resultados
resultados <- data.frame(tentativa = 1:n_jogos,
                         resultado = jogos)

resultados$vitória <- cumsum(ifelse(resultados$resultado == 'vitória',1,0))/resultados$tentativa
resultados$derrota <- cumsum(ifelse(resultados$resultado == 'derrota',1,0))/resultados$tentativa
#-----------------------------------------------------------------------
# Plotando resultados
legenda <- c("vitória" = 4, 
             "derrota" = 2)

ggplot(resultados, aes(x = tentativa)) +
  geom_line(aes(y = vitória, color = "vitória"), size = 1) +
  geom_line(aes(y = derrota, color = "derrota"), size = 1) +
  geom_hline(yintercept=1-(35/36)^24, 
             linetype="dashed", color = "black") +
  ylim(0,1) +
  transition_reveal(tentativa) +
  labs(x = "Jogos",
       y = "Freq. Relativa",
       color = "") +
  scale_color_manual(values = legenda) + 
  theme_bw() + 
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +
  labs(title = "Chavalier",
       subtitle = 'Obter um duplo 6 em até 24 lançamentos')

#-----------------------------------------------------------------------
# Comparando as frequências relativas com as probabilidades esperadas
resultados[nrow(resultados),]
#-----------------------------------------------------------------------
