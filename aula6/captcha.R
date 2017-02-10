library(MASS)
library(jpeg)
library(boot)
library(stringr)
library(tibble)
library(rpart)
library(rpart.plot)
library(randomForest)
library(magrittr)
library(purrr)
library(ggplot2)
library(forcats)
library(tidyr)
library(dplyr)
library(captchaReceitaAudio)

# https://github.com/decryptr




# banco de dados de letras
letras <- readRDS("aula6/letras.RDS") %>% mutate(y = factor(y))


letras %<>% 
  mutate(x1 = runif(n()),
         x2 = runif(n()),
         x3 = runif(n()),
         x4 = runif(n()),
         x5 = runif(n()),
         x6 = runif(n()),
         x7 = runif(n()),
         x8 = runif(n()),
         x9 = runif(n()),
         x10 = runif(n()),
         x11= runif(n()),
         x12 = runif(n()))
View(letras)



#-------------------------------------------------------------#
# Descritiva - Matriz de dispersão e correlação linear
letras %>%
  ggplot +
  geom_bar(aes(x = y))

letras %>%
  ggplot +
  geom_boxplot(aes(x = fct_reorder(y, comprimento), y = comprimento))


#---------------------------------------------#
# dividir o data.frame em partes de treino e teste.
set.seed(19880923)

letras_treino <- letras %>% sample_frac(7/10)

letras_teste <- letras %>% 
  filter(rownames(.) %in% rownames(letras_treino) %>% not)





#---------------------------------------------#
# arvore de decisao
letras_tree_sem_cv <- rpart(y ~ . - id, 
                            data = letras_treino, 
                            xval = 10, 
                            minbucket = 1,
                            minsplit = 1,
                            maxdepth = 30,
                            cp = 0.0001)
rpart.plot(letras_tree_sem_cv, box.palette=0)
printcp(letras_tree_sem_cv)
plotcp(letras_tree_sem_cv)

letras_tree_com_cv <- prune(letras_tree_sem_cv, 
                            cp = letras_tree_sem_cv$cptable[letras_tree_sem_cv$cptable[,"nsplit"] == 56, "CP"])
rpart.plot(letras_tree_com_cv, box.palette=0)
printcp(letras_tree_com_cv)
plotcp(letras_tree_com_cv)


#---------------------------------------------#
# random forest
letras_rf_com_cv <- randomForest(y ~ . - id, 
                                 data = letras_treino,
                                 ntree = 800)

varImpPlot(letras_rf_com_cv)





# resultados -----------------------------#
letras_teste_com_predicoes <- letras_teste %>%
  mutate(tree_selecionado_com_cv = predict(letras_tree_com_cv, newdata = ., type = 'class'),
         rf_selecionado_com_cv = predict(letras_rf_com_cv, newdata = .))

# erros preditivos
letras_teste_com_predicoes %>%
  gather(metodo_de_selecao, y_predito, dplyr::contains("selecionado")) %>%
  mutate(erro_de_classif = y != y_predito) %>%
  group_by(metodo_de_selecao) %>%
  summarise(erro_de_classif = mean(erro_de_classif))

# Visualizar uma matriz de confusao
letras_rf_com_cv$confusion %>% 
  apply(2, str_replace, pattern = "^0$", replace = ".") %>% 
  View



