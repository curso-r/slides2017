library(knitr)
library(magrittr)

dir.create("content/")
ex_dir <- normalizePath("content/")

dir.create(ex_dir)

rmds <- list.files(pattern = "\\.Rmd", recursive = T, full.names = T)

htmls <- gsub("\\.Rmd","\\.html",rmds) %>% 
  basename %>% 
  paste0(ex_dir,"/", .)

for (arq in list.files(pattern = "\\.Rmd$", recursive = T, full.names = T)){
  #' Estou usando um for porque gerar o markdown é um side effect
  rmarkdown::render(arq, intermediates_dir = ex_dir)
  pkgs <- names(sessionInfo()$otherPkgs)
  pkgs <- paste('package:', pkgs, sep = "")
  if (pkgs[1] != 'package:') {
    try({
      invisible(lapply(pkgs, detach, character.only = TRUE, unload = FALSE))
    })
  }
}

file.copy(list.files(pattern = "\\.html$", recursive = T, full.names = T), htmls)
