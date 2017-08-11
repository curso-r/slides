# install.packages("GGally")
# install.packages("jpeg")
# install.packages("rpart.plot")

# https://www.youtube.com/watch?v=CrBFNLvoL6A

# pacotes necessários
library(MASS)
library(jpeg)
library(mgcv)
library(boot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(GGally)
library(magrittr)
library(purrr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(glmnet)

# ------------------------------------------------------------------------------
# magick::image_read('201704_cea/purple_wave.jpg') %>%
#   magick::image_noise() %>%
#   magick::image_noise() %>%
#   magick::image_noise() %>%
#   magick::image_noise() %>%
#   magick::image_noise() %>%
#   magick::image_write('201704_cea/purple_wave2.jpg')

# carregar uma imagem jpeg no R
onda_roxa_jpeg <- readJPEG("201704_cea/purple_wave2.jpg") 

# transformar o array da imagem em data.frame com infos de posicao (x,y) e cor (r,g,b)
# dimensões da imagem
onda_roxa_dm <- dim(onda_roxa_jpeg)

# RGB para data.frame
onda_roxa <- data_frame(
  x = rep(1:onda_roxa_dm[2], each = onda_roxa_dm[1]),
  y = rep(onda_roxa_dm[1]:1, onda_roxa_dm[2]),
  r = as.vector(onda_roxa_jpeg[,,1]),
  g = as.vector(onda_roxa_jpeg[,,2]),
  b = as.vector(onda_roxa_jpeg[,,3])
) %>%
  mutate(apenas_azul = rgb(0, 0, b),
         original = rgb(r, g, b),
         id = 1:n())

onda_roxa

# Visualização - imagem original / imagem apenas com o azul
onda_roxa_para_grafico <- onda_roxa %>%
  gather(imagem, cor, apenas_azul, original) %>%
  mutate(imagem = factor(imagem, levels = c("original", "apenas_azul"), 
                         labels = c("Original", "Apenas azul")),
         cor = as.character(cor))
cores <- unique(onda_roxa_para_grafico$cor)
names(cores) <- cores
onda_roxa_orig_azul <- onda_roxa_para_grafico %>% 
  ggplot(aes(x = x, y = y, colour = cor)) +
  facet_wrap(~ imagem) +
  geom_point(show.legend = FALSE) +
  scale_colour_manual(values = cores) +
  labs(x = "x", y = "y") +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(strip.text = element_text(size = 14))

onda_roxa_orig_azul

# Treino/teste =================================================================
# dividir o data.frame em partes de treino e teste.
set.seed(19880923)

# adicionando variável noise
onda_roxa$noise1 <- rnorm(nrow(onda_roxa))
onda_roxa$noise2 <- rnorm(nrow(onda_roxa))
onda_roxa$noise3 <- rnorm(nrow(onda_roxa))

onda_roxa_treino <- onda_roxa %>% sample_frac(1 / 10)
onda_roxa_teste <- onda_roxa %>% anti_join(onda_roxa_treino, 'id')

# Descritiva ===================================================================
set.seed(19880923)

onda_roxa_ggpairs <- onda_roxa %>% 
  sample_n(500) %>% 
  select(-original, -apenas_azul, -id) %>% 
  ggpairs()

onda_roxa_ggpairs

# Modelagem ====================================================================
set.seed(19880923)

f <- b ~ x + y + r + g + noise1 + noise2 + noise3
# cenário I - regressão linear  selecionado SEM cross-validation (stepwise)-----
onda_roxa_lm <- glm(f, data = onda_roxa_treino)
onda_roxa_lm_sem_cv <- onda_roxa_lm %>% stepAIC(trace = TRUE)

# cenário II - regressão linear selecionado COM cross-validation ---------------
variaveis <- c("x", "y", "r", "g", "noise1", "noise2", "noise3")
n_vars_no_modelo <- 0:4

gera_formulas <- function(n_vars) {
  combn(variaveis, n_vars, FUN = function(vars) {
    if(length(vars) == 0) vars <- "1"
    explicativas <- paste0(vars, collapse = " + ")
    formula <- paste0("b ~ ", explicativas)
    return(formula)
  })
}

onda_roxa_lms_com_cv <- n_vars_no_modelo %>% 
  map(gera_formulas) %>% 
  unlist() %>%
  tibble::tibble(formula = .) %>%
  mutate(modelo = map(formula, ~glm(as.formula(.x), data = onda_roxa_treino))) %>%
  mutate(erro_cv = map_dbl(modelo, ~cv.glm(onda_roxa_treino, .x, K = 5)$delta[1])) %>%
  arrange(erro_cv)

onda_roxa_lm_com_cv <- onda_roxa_lms_com_cv %>%
  slice(which.min(erro_cv)) %>%
  with(modelo[[1]])

# cenário III - árvore de decisão COM cross-validation -------------------------
onda_roxa_tree_sem_cv <- rpart(f, data = onda_roxa_treino, 
                               xval = 10, 
                               minbucket = 100,
                               minsplit = 100,
                               maxdepth = 30,
                               cp = 0.001)
rpart.plot(onda_roxa_tree_sem_cv)
printcp(onda_roxa_tree_sem_cv)

onda_roxa_tree_com_cv <- prune(
  onda_roxa_tree_sem_cv, 
  cp = onda_roxa_tree_sem_cv$cptable[onda_roxa_tree_sem_cv$cptable[,"nsplit"] == 8, "CP"]
)
rpart.plot(onda_roxa_tree_com_cv)

# cenário IV - LASSO com CV ----------------------------------------------------
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
m <- onda_roxa_treino %>% 
  select(r, x, y, g, noise1, noise2, noise3) %>% 
  as.matrix()

onda_roxa_lasso_com_cv <- cv.glmnet(m, onda_roxa_treino$b)
plot(onda_roxa_lasso_com_cv)
coef(onda_roxa_lasso_com_cv)

# melhor lambda
lambda <- onda_roxa_lasso_com_cv$lambda.min
mm <- as.matrix(select(onda_roxa_teste, r, x, y, g, noise1, noise2, noise3))
p_lasso <- predict(onda_roxa_lasso_com_cv, type = 'link', s = lambda, newx = mm)


# cenário V - GAM --------------------------------------------------------------
onda_roxa_gam_sem_cv <- mgcv::gam(b ~ r + x + s(y) + g + noise1+ noise2 + noise3, 
                                  data = onda_roxa_treino)

# Resultados ===================================================================
# onda_roxa_teste com os azuis preditos
onda_roxa_teste_com_predicoes <- onda_roxa_teste %>%
  tibble::as_tibble() %>%
  mutate(lm_sem_cv = predict(onda_roxa_lm_sem_cv, newdata = .),
         lm_com_cv = predict(onda_roxa_lm_com_cv, newdata = .),
         tree_com_cv = predict(onda_roxa_tree_com_cv, newdata = .),
         tree_sem_cv = predict(onda_roxa_tree_sem_cv, newdata = .),
         gam_sem_cv = predict(onda_roxa_gam_sem_cv, newdata = .),
         lasso_com_cv = p_lasso
  ) 

# erros preditivos
onda_roxa_teste_com_predicoes %>%
  mutate_if(is.numeric, as.numeric) %>% 
  gather(metodo_de_selecao, b_predito, matches("_cv$")) %>%
  mutate(residuo = b - b_predito) %>%
  group_by(metodo_de_selecao) %>%
  summarise(mse_1000 = mean(residuo^2) * 1000) %>% 
  arrange(mse_1000)

# gráfico
onda_roxa_teste_com_predicoes_para_grafico <- onda_roxa_teste_com_predicoes %>%
  gather(imagem, azul_predito, b, contains("selecionado")) %>%
  mutate(cor = rgb(0, 0, azul_predito) %>% as.character)

cores <- onda_roxa_teste_com_predicoes_para_grafico$cor %>% unique
names(cores) <- cores
onda_roxa_azuis_preditos <- onda_roxa_teste_com_predicoes_para_grafico %>%
  ggplot(aes(x = x, y = y, colour = cor)) +
  facet_wrap(~ imagem) +
  geom_point(show.legend = FALSE) +
  scale_colour_manual(values = cores) +
  labs(x = "x", y = "y") +
  coord_fixed(ratio = 1) +
  theme_bw() 

onda_roxa_azuis_preditos

# intuição
onda_roxa %>%
  mutate(r_cat = predict(rpart(b ~ r, data = onda_roxa), newdata = onda_roxa)) %>%
  arrange(b) %>%
  ggplot() +
  geom_point(aes(x = r, y = b)) +
  stat_smooth(aes(x = r, y = b), method = "lm") +
  geom_step(aes(x = r_cat, y = b), colour = "red") +
  theme_bw()

