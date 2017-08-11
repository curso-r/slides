library(MASS)
library(tidyverse)
library(randomForest)
library(forcats)

# banco de dados de letras
letras <- "201705_pp/data/letras.rds" %>% 
  readRDS() %>% 
  mutate(y = factor(y))

View(letras)

#-------------------------------------------------------------
# Descritiva - Matriz de dispersão e correlação linear
letras %>%
  ggplot(aes(x = y)) +
  geom_bar() +
  theme_bw()

letras %>%
  ggplot(aes(x = fct_reorder(y, ataque), y = ataque)) +
  geom_boxplot() +
  theme_bw()

#-------------------------------------------------------------
# dividir a base em partes de treino e teste
set.seed(19880923)
ids <- sample(1:nrow(letras), nrow(letras) * 7 / 10, replace = FALSE)
letras_treino <- letras[ids,]
letras_teste <- letras[-ids,]

modelo_arvore <- rpart::rpart(y ~ . - id, 
                          data = letras_treino)

rpart.plot::rpart.plot(modelo_arvore)
#-------------------------------------------------------------
# random forest
modelo_rf <- randomForest(y ~ . - id, 
                          data = letras_treino, 
                          ntree = 800)
varImpPlot(modelo_rf)

# resultados -------------------------------------------------
letras_teste_com_predicoes <- letras_teste %>%
  mutate(pred = predict(modelo_rf, newdata = .))

# preditos ---------------------------------------------------
letras_teste_com_predicoes %>% 
  count(y, pred) %>% 
  spread(pred, n, fill = '.') %>% 
  View()
#-------------------------------------------------------------
