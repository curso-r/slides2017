library(purrr)

# predicados --------------------------------------------------------------

exemplo <- list(a = list(aplica = TRUE, valor = 11),
                b = list(aplica = FALSE, valor = 10),
                c = list(aplica = FALSE, valor = 3),
                d = list(aplica = TRUE, valor = 4))

# retorna um subset dos verbos

keep(exemplo, ~.x$aplica)
discard(exemplo, ~.x$aplica)

# retorna TRUE ou FALSE
every(exemplo, ~ .x$aplica)
some(exemplo, ~ .x$aplica)
