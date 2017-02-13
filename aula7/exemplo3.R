library(ggplot2)
library(dplyr)

# transformação -----------------------------------------------------------

plotar_mpg_wt <- function(x) {
  x %>% 
    ggplot(aes(x = wt, y = mpg)) +
    geom_point() + 
    theme_bw()
}

salvar_plot <- function(linha, nomes) {
  ggsave(plot = linha$graficos[[1]], filename = linha$nome)
}

nomes_dos_arquivos <- paste0("aula7/ex3_out/", 1:3, '.png')

df_graficos <- mtcars %>% 
  group_by(cyl) %>% 
  by_slice(plotar_mpg_wt, .to = "graficos") %>% 
  mutate(nome = nomes_dos_arquivos)

dir.create('aula7/ex3_out', showWarnings = FALSE)

df_graficos %>%
  by_row(salvar_plot, .to = 'resultado')

# Exercício: fazer isso com purrr::walk
