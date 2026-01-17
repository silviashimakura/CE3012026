#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Considere que foi coletada uma amostra de animais. 
# Cada um destes animais foi categorizado de acordo com o sexo (macho 
# ou fêmea) e com a espécie (A, B ou C). 
#
# O código R a seguir gera o conjunto de dados associado ao problema.
#
# Com base na tabela, responda aos itens.
#
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------
set.seed(1)
#-----------------------------------------------------------------------

sexo <- sample(c("Macho", "Fêmea"), size = 500, replace = T)
especie <- sample(c("A", "B", "C"), size = 500, replace = T)

dados <- data.frame(sexo,
                    especie)

tabela <- addmargins(table(dados))

row.names(tabela)[3] <- "Soma"
colnames(tabela)[4] <- "Soma"

tabela
#-----------------------------------------------------------------------

# 1. Defina os eventos.

#-----------------------------------------------------------------------

# 2. Qual a probabilidade de escolhermos ao acaso uma fêmea?
p_femea <- tabela[1,4]/tabela[3,4]

# 3. Qual a probabilidade de escolhermos ao acaso um macho?
246/500
tabela[2,4]/tabela[3,4]
p_macho <- 1-p_femea

# 4. Qual a probabilidade de escolhermos ao acaso um animal da 
## espécie A?

# 5. Qual a probabilidade de escolhermos ao acaso um animal da 
# espécie B?

# 6. Qual a probabilidade de escolhermos ao acaso um animal da 
# espécie C?

# 7. Qual a probabilidade de escolhermos ao acaso um macho ou 
# uma fêmea?

#-----------------------------------------------------------------------

# 8. Qual a probabilidade de escolhermos um animal da espécie A ou da 
# espécie B?

# 9. Qual a probabilidade de escolhermos um animal da espécie A ou da 
# espécie C?

# 10. Qual a probabilidade de escolhermos um animal da espécie B ou da 
# espécie C?

# 11. Qual a probabilidade de escolhermos um animal da espécie 
# A ou B ou C?

#-----------------------------------------------------------------------

# 12. Qual a probabilidade de escolhermos uma fêmea ou um animal da 
# espécie A?

# 13. Qual a probabilidade de escolhermos uma fêmea ou um animal da 
# espécie B?

# 14. Qual a probabilidade de escolhermos uma fêmea ou um animal da 
# espécie C?

# 15. Qual a probabilidade de escolhermos um macho ou um animal da 
# espécie A?

# 16. Qual a probabilidade de escolhermos um macho ou um animal da 
# espécie B?

# 17. Qual a probabilidade de escolhermos um macho ou um animal da 
# espécie C?

#-----------------------------------------------------------------------

# 18. Sabendo que é uma fêmea, qual a probabilidade de ser da espécie A?

# 19. Sabendo que é uma fêmea, qual a probabilidade de ser da espécie B?

# 20. Sabendo que é uma fêmea, qual a probabilidade de ser da espécie C?

# 21. Sabendo que é um macho, qual a probabilidade de ser da espécie A?

# 22. Sabendo que é um macho, qual a probabilidade de ser da espécie B?

# 23. Sabendo que é um macho, qual a probabilidade de ser da espécie C?

#-----------------------------------------------------------------------

# 24. Sabendo que é da espécie A, qual a probabilidade de ser uma fêmea?

# 25. Sabendo que é da espécie B, qual a probabilidade de ser uma fêmea?

# 26. Sabendo que é da espécie C, qual a probabilidade de ser uma fêmea?

# 27. Sabendo que é da espécie A, qual a probabilidade de ser um macho?

# 28. Sabendo que é da espécie B, qual a probabilidade de ser um macho?

# 29. Sabendo que é da espécie C, qual a probabilidade de ser um macho?

#-----------------------------------------------------------------------

# 30. Qual a probabilidade de ser uma fêmea e da espécie A?

# 31. Qual a probabilidade de ser uma fêmea e da espécie B?

# 32. Qual a probabilidade de ser uma fêmea e da espécie C?

# 33. Qual a probabilidade de ser uma macho e da espécie A?

# 34. Qual a probabilidade de ser uma macho e da espécie B?

# 35. Qual a probabilidade de ser uma macho e da espécie C?

#-----------------------------------------------------------------------

# 36. O que podemos concluir a respeito da independência das variáveis?
## P(A|B) = P(A) & P(B|A) = P(B)

#-----------------------------------------------------------------------