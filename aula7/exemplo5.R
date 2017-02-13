library(purrr)

# validação cruzada -------------------------------------------------------
dataset_sem_na <- titanic %>% 
  filter(complete.cases(.)) %>% 
  mutate_if(is.character, funs(as.factor(.)))

cross_val_logistica  <- function(formula, dataset, prop = 0.75, repet = 1) {
  ajustar_logistica <- function() {
    indices <- sample(1:nrow(dataset), nrow(dataset) * prop)
    treino <- dataset[indices,]
    teste <- dataset[-indices,]
    modelo <- glm(formula, data = treino, family = 'binomial')
    predicao <- predict(modelo, newdata = teste, type = 'response') %>% 
      round() %>% 
      factor(levels = c(0, 1))
    
    erro <- sum(predicao == teste$Survived, na.rm = TRUE) / nrow(teste)
    return(erro)
  }
  
  erro <- map_dbl(1:repet, ~ajustar_logistica())
  list(modelo = formula, erro_cv = mean(erro))
}

cross_val_logistica(Survived ~ Sex + Age + Fare, 
                    dataset = dataset_sem_na, 
                    prop = 0.75, 
                    repet = 10)

# carregar formulas no exemplo2.R

ajustes <- map(formulas, 
               cross_val_logistica, 
               dataset = dataset_sem_na, 
               prop = 0.75, 
               repet = 10)

modelos_ordenados <- ajustes %>%
  sort_by(~-.x$erro_cv) %>% 
  map_chr(~.x$modelo)

modelos_ordenados


