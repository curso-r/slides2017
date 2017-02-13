library(purrr)

# Substituindo "for" ------------------------------------------------------

titanic <- readr::read_csv('https://github.com/curso-r/pu.modelos/raw/master/data/titanic-train.csv')
titanic$Survived <- as.factor(titanic$Survived)

for(i in 1:ncol(titanic)) {
  nome <- names(titanic[i])
  tipo <- class(titanic[[i]])
  n_na <- sum(is.na(titanic[[i]]))
  print(paste0("A coluna ", nome," é do tipo ", 
               tipo," e tem ", n_na," NA's."))
}

# agora sem for
# a função map funciona exatamente como o modelo de funcional apresentado no slide

resumir_coluna <- function(coluna){
  tipo <- class(coluna)
  n_na <- sum(is.na(coluna))
  paste0("Esta coluna é do tipo ", 
         tipo," e tem ", n_na," NA's.")
}

map(titanic, resumir_coluna)

# pode ficar mais sofisticado dependendo do tipo de saída desejado
map_chr(titanic, resumir_coluna)

# outras maneiras
## funciona para todos os funcionais
map_chr(titanic, function(x) resumir_coluna(x))

## jeito especial do purrr funcionar
map_chr(titanic, ~resumir_coluna(.x))

# Exercício: estudar todos os tipos de map_*
# Criar um exemplo não vetorizável, em que seja necessário aplicar map.
