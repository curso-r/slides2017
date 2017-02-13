library(purrr)

# map mais complicados ---------------------------------------------------
# map2 --------------------------------------------------------------------

gerar_formulas <- function(variaveis, n_vars, response_var, exclude_vars) {
  
  variaveis <- unlist(variaveis)
  if(!(response_var %in% variaveis)) {
    stop("Response_var não pertence a variaveis.")
  }
  
  if(any(!(exclude_vars %in% variaveis))) {
    stop("Todas as exclude_vars devem pertencer a variaveis")
  }
  
  variaveis <- variaveis[!(variaveis %in% c(response_var,exclude_vars))]
  
  combn(variaveis, n_vars, FUN = function(vars) {
    if(length(vars) == 0) vars <- "1"
    explicativas <- paste0(vars, collapse = " + ")
    formula <- paste0("Survived ~ ", explicativas)
    return(as.character(formula))
  })
}

modelos <- gerar_formulas(
  variaveis = names(titanic), 
  n_vars = 3, 
  response_var = 'Survived',
  exclude_vars = c("Name", "Cabin", "Ticket", "PassengerId")
)

titanic %>% 
  names() %>% 
  list() %>% 
  map2(
    c(1:5),                # segundo argumento
    gerar_formulas,        # função que será aplicada
    response_var = "Survived", # parâmetros adicionais
    exclude_vars = c("Name","Cabin","Ticket",
                      "PassengerId","Embarked") # parâmetros adicionais
) %>% 
  unlist()


# Exercício: aplicar um filtro em mtcars usando a seguinte função
multiplicar_mpg_cyl <- function(a, b) {
  prod(a, b) < 100
}
# dica: usar map2

# map_if e map_at ---------------------------------------------------------

exemplo <- list(
  a = list(aplica = TRUE, valor = 11),
  b = list(aplica = FALSE, valor = 10),
  c = list(aplica = FALSE, valor = 3),
  d = list(aplica = TRUE, valor = 4)
)

predicado <- function(lista) {
  lista$aplica
}

trocar_sinal <- function(lista) {
  list(aplica = lista$aplica, valor = -lista$valor)
}

map_if(exemplo, predicado, trocar_sinal)
map_at(exemplo, c(2, 3), trocar_sinal)

