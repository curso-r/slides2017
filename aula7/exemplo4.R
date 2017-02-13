library(purrr)

# predicados --------------------------------------------------------------

exemplo <- list(a = list(aplica = T, valor = 11),
                b = list(aplica = F, valor = 10),
                c = list(aplica = F, valor = 3),
                d = list(aplica = T, valor = 4))

keep(exemplo, ~ .x$aplica)
discard(exemplo, ~ .x$aplica)
every(exemplo, ~ .x$aplica)
some(exemplo, ~ .x$aplica)
