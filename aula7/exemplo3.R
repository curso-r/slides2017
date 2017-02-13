library(ggplot2)

# transformação -----------------------------------------------------------

plota_mpg_wt <- function(x){
  x %>% 
    ggplot(aes(x = wt, y = mpg)) +
    geom_point() + 
    theme_bw()
}
  
  salva_plot <- function(linha, nomes){
    ggsave(plot =   linha$graficos[[1]], filename = linha$nome)
    return("plotou")
  }
  
  nomes_dos_arquivos <- paste0("aula7/ex3_out/",1:3,'.png')
  
  df_graficos <- mtcars %>% 
    group_by(cyl) %>% 
    by_slice(plota_mpg_wt, .to = "graficos") %>% 
    mutate(nome = nomes_dos_arquivos)
  
  dir.create('aula7/ex3_out')
  
  df_graficos %>%
    by_row(salva_plot, .to = 'resultado')
  
  
  
