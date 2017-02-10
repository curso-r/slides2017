# pacotes necessários
library(MASS)
library(jpeg)
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








#------------------------------------------#
# carregar uma imagem jpeg no R
xadrez_jpeg <- readJPEG("aula6/xadrez_colorido.jpg") 

# transformar o array da imagem em data.frame com infos de posicao (x,y) e cor (r,g,b)
# dimensões da imagem
xadrez_dm <- dim(xadrez_jpeg)

# RGB para data.frame
xadrez <- data_frame(
  x = rep(1:xadrez_dm[2], each = xadrez_dm[1]),
  y = rep(xadrez_dm[1]:1, xadrez_dm[2]),
  r = as.vector(xadrez_jpeg[,,1]),
  g = as.vector(xadrez_jpeg[,,2]),
  b = as.vector(xadrez_jpeg[,,3])
) %>%
  mutate(apenas_azul = rgb(0, 0, b),
         original = rgb(r, g, b),
         id = 1:n())

xadrez







#---------------------------------------------#
# dividir o data.frame em partes de treino e teste.
set.seed(19880923)

xadrez_treino <- xadrez %>% sample_frac(7/10)

xadrez_teste <- xadrez %>% 
  filter(id %in% xadrez_treino$id %>% not)







#--------------------------------------------#
# Visualização - imagem original / imagem apenas com o azul
xadrez_para_grafico <- xadrez %>%
  gather(imagem, cor, apenas_azul, original) %>%
  mutate(imagem = factor(imagem, levels = c("original", "apenas_azul"), labels = c("Original", "Apenas azul")),
         cor = cor %>% as.character)

cores <- xadrez_para_grafico$cor %>% unique
names(cores) <- cores
xadrez_orig_azul <- ggplot(data = xadrez_para_grafico, aes(x = x, y = y, colour = cor)) +
  facet_wrap(~ imagem) +
  geom_point(show.legend = FALSE) +
  scale_colour_manual(values = cores) +
  labs(x = "x", y = "y") +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(strip.text = element_text(size = 14))

xadrez_orig_azul





#-------------------------------------------------------------#
# Descritiva - Matriz de dispersão e correlação linear
set.seed(19880923)

xadrez_ggpairs <- xadrez %>% 
  sample_n(500) %>% 
  select(-original, -apenas_azul, -id) %>% 
  ggpairs

xadrez_ggpairs






#-------------------------------------------------------------#
# Modelagem 
set.seed(19880923)

# regressão linear - modelo completo
xadrez_lm <- glm(b ~ x + y + r + g, data = xadrez_treino)

# cenário I - regressão linear  selecionado SEM cross-validation (stepwise)
xadrez_lm_sem_cv <- xadrez_lm %>% stepAIC(trace=FALSE)





# cenário II - regressão linear selecionado COM cross-validation
variaveis <- c("x", "y", "r", "g")
n_vars_no_modelo <- 0:4

gera_formulas <- function(n_vars) {
  combn(variaveis, n_vars, FUN = function(vars) {
    if(length(vars) == 0) vars <- "1"
    
    explicativas <- paste0(vars, collapse = " + ")
    formula <- paste0("b ~ ", explicativas)
    return(formula)
  })
}

# leva aproximadamente 1 minuto
xadrez_lms_com_cv <- n_vars_no_modelo %>% 
  map(gera_formulas) %>% 
  unlist %>%
  data_frame(formula = .) %>%
  mutate(modelo = formula %>% map(~ .x %>% 
                                    as.formula %>% 
                                    glm(data = xadrez_treino))
  ) %>%
  mutate(erro_cv = modelo %>% map_dbl(~ cv.glm(.x, K = 5, data = xadrez_treino)$delta[2])) %>%
  arrange(erro_cv)

xadrez_lm_com_cv <- xadrez_lms_com_cv %>%
  filter(erro_cv == min(erro_cv)) %$%
  modelo[[1]]


# cenário III - árvore de decisão COM cross-validation

xadrez_tree_sem_cv <- rpart(b ~ r + x + y + g, 
                               data = xadrez_treino, 
                               xval = 10, 
                               minbucket = 100,
                               minsplit = 100,
                               maxdepth = 30,
                               cp = 0.0001)
rpart.plot(xadrez_tree_sem_cv)
printcp(xadrez_tree_sem_cv)
plotcp(xadrez_tree_sem_cv)

xadrez_tree_com_cv <- prune(xadrez_tree_sem_cv, 
                               cp = xadrez_tree_sem_cv$cptable[xadrez_tree_sem_cv$cptable[,"nsplit"] == 7, "CP"])
rpart.plot(xadrez_tree_com_cv)





# xadrez_teste com os azuis preditos
xadrez_teste_com_predicoes <- xadrez_teste %>%
  tbl_df %>%
  mutate(lm_selecionado_sem_cv = predict(xadrez_lm_sem_cv, newdata = .),
         lm_selecionado_com_cv = predict(xadrez_lm_com_cv, newdata = .),
         tree_selecionado_com_cv = predict(xadrez_tree_com_cv, newdata = .)) 

# erros preditivos
xadrez_teste_com_predicoes %>%
  gather(metodo_de_selecao, b_predito, contains("selecionado")) %>%
  mutate(residuo = b - b_predito) %>%
  group_by(metodo_de_selecao) %>%
  summarise(mse = mean(residuo^2))

# gráfico
xadrez_teste_com_predicoes_para_grafico <- xadrez_teste_com_predicoes %>%
  gather(imagem, azul_predito, b, contains("selecionado")) %>%
  mutate(cor = rgb(0, 0, azul_predito) %>% as.character)

cores <- xadrez_teste_com_predicoes_para_grafico$cor %>% unique
names(cores) <- cores
xadrez_azuis_preditos <- xadrez_teste_com_predicoes_para_grafico %>%
  ggplot(aes(x = x, y = y, colour = cor)) +
  facet_wrap(~ imagem) +
  geom_point(show.legend = FALSE) +
  scale_colour_manual(values = cores) +
  labs(x = "x", y = "y") +
  coord_fixed(ratio = 1) +
  theme_bw() 

xadrez_azuis_preditos







#-------------------------------------------------------------#
# EXTRA!
# Modelagem - regressão linear com criação de variáveis
# xadrez_treino %<>%
#   mutate(x_cat = cut(x, breaks = c(-Inf, 73, 75+16.67, 100+8.33, 125,Inf)),
#          y_cat = cut(y, breaks = c(-Inf, range(y) %>% diff %>% divide_by(9) * 1:9, Inf)))
