library(purrr)

# map mais complicados ---------------------------------------------------

mtcars %>% #conjunto de dados padrao do R
  split_by(cyl) %>% #faz um group_by forçado
  map(~ lm(mpg ~ wt, data = .)) %>% #mapeia cada pedaço do data.frame em um modelo linear
  map(summary) %>% #mapeia os modelos em sumários
  map_dbl("r.squared") #mapeia os sumários em R^2

# map2 --------------------------------------------------------------------

gera_formulas <- function(variaveis, n_vars, response_var, exclude_vars) {
  
  variaveis <- unlist(variaveis)
  
  if(!(response_var %in% variaveis)){
    stop("Response_var não pertence a variaveis.")
  }
  
  if(any(!(exclude_vars %in% variaveis))){
    stop("Todas as exclude_vars devem pertencer a variaveis")
  }
  
  variaveis <- variaveis[!(variaveis %in% c(response_var,exclude_vars))]
  
  combn(variaveis, n_vars, FUN = function(vars) {
    if(length(vars) == 0) vars <- "1"
    
    explicativas <- paste0(vars, collapse = " + ")
    formula <- paste0("Survived ~ ", explicativas)
    return(formula)
  })
}

modelos <- gera_formulas(names(titanic), 3, response_var = 'Survived',
                         exclude_vars = c("Name","Cabin","Ticket","PassengerId"))

map2(list(names(titanic)), #primeiro argumento
     c(1:5), #segundo argumento
     gera_formulas, #função que será aplicada
     response_var = "Survived", #parâmetros adicionais
     exclude_vars = c("Name","Cabin","Ticket","PassengerId","Embarked") # parâmetros adicionais
) %>% 
  unlist()



# map_if e map_at ---------------------------------------------------------

exemplo <- list(a = list(aplica = T, valor = 11),
     b = list(aplica = F, valor = 10),
     c = list(aplica = F, valor = 3),
     d = list(aplica = T, valor = 4))

predicado <- function(lista){
  lista$aplica
}

troca_sinal <- function(lista){
  list(aplica = lista$aplica,
       valor = -lista$valor)
}

map_if(exemplo, predicado, troca_sinal)
map_at(exemplo, c(1,4), troca_sinal)
