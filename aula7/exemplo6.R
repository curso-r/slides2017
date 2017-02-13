library(purrr)

dormir_secs <- function(secs = 1L) {
  Sys.sleep(secs)
  return(sprintf('dormi %d segundo(s)', secs))
}

# ------------------------------------------------------------------
## Progress bar

vetor <- rep(1, 4)
prog <- dplyr::progress_estimated(length(vetor))

dormidas <- map(vetor, ~{
  print(prog$tick())
  dormir_secs(.x)
})

# Exercício: reescrever com funções anônimas normais
# Exercício: reescrever com uma função apenas

# ------------------------------------------------------------------
## Paralelo com plyr, parallel e doParallel

# preparar clusters
n_cores <- parallel::detectCores()
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

## diferença entre paralelo e nao paralelo
system.time({
  dormidas2 <- plyr::llply(vetor, dormir_secs, .parallel = TRUE)
})

system.time({
  dormidas3 <- plyr::llply(vetor, dormir_secs, .parallel = FALSE)
})

# identical(dormidas, dormidas2)
# identical(dormidas, dormidas3)

# parar clusters (limpa sujeira)
parallel::stopCluster(cl)

# ------------------------------------------------------------------
## Failwith

vetor <- c('acerto', 'erro', 'acerto')

errar <- function(x) {
  if (x == 'erro') stop('errei') else 'acertei'
}

f <- dplyr::failwith('putz, errei!', errar)

map_chr(vetor, errar)
map_chr(vetor, f)

# ------------------------------------------------------------------
## Estude:

purrr::accumulate
purrr::transpose

