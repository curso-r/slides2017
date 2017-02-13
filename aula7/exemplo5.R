library(purrr)
# validação cruzada -------------------------------------------------------

cross_val_logistica  <- function(formula, dataset, prop = 0.75, repet = 1){
  
  dataset_sem_na <- dataset %>% 
    filter(complete.cases(.))
  
  ajusta_1_logistica <- function(){
    
    indices <- sample(1:nrow(dataset), nrow(dataset)*prop)
    
    treino <- dataset_sem_na[indices,]
    teste <- dataset_sem_na[-indices,]
  
    modelo <- glm(formula, data = treino, family = 'binomial')
    predicao <- predict(modelo, newdata = teste, type = 'response') %>% 
      round %>% 
      factor(levels = c(0,1))

    erro <- sum(predicao==teste$Survived, na.rm = T)/nrow(teste)
    
    return(erro)
  }
  
  erro <- map_dbl(1:repet, ~ajusta_1_logistica())
  
  list(modelo = formula, erro_cv = mean(erro))
}

cross_val_logistica(Survived ~ Sex + Age + Fare, dataset = titanic, prop = 0.75, repet = 10)

formulas <- map2(list(names(titanic)), #primeiro argumento
     c(1:5), #segundo argumento
     gera_formulas, #função que será aplicada
     response_var = "Survived", #parâmetros adicionais
     exclude_vars = c("Name","Cabin","Ticket","PassengerId","Embarked") # parâmetros adicionais
) %>% 
  unlist()

titanic <- titanic %>% 
  mutate_if(is.character, funs(as.factor(.)))

titanic$Age <- as.numeric(titanic$Age)
titanic$Fare <- as.numeric(titanic$Fare)
titanic$Parch <- as.numeric(titanic$Parch)
titanic$SibSp <- as.numeric(titanic$SibSp)

ajustes <- map(formulas, cross_val_logistica, dataset = titanic, prop = 0.75, repet = 10)

modelos_ordenados <- ajustes %>%
  sort_by(~-.x$erro_cv) %>% 
  map_chr(~.x$modelo)