library(readr)
library(openxlsx)
library(gdata)

# Abre planilhas em csv que usem como separado decimal o caractér "."
legendas_pt_1 <- readr::read_csv(file = "inputs/legendas_pt_1.csv")

# Abre planilhas em csv que usem como separado decimal o caractér ","
legendas_pt_2 <- readr::read_csv2(file = "inputs/legendas_pt_2.csv")

# Abre planilhas em xlsx que
comentarios_pt_1 <- openxlsx::read.xlsx(file = "inputs/comentarios_pt_1.xlsx")

# Abre planilhas em xls
#comentarios_pt_2 <- gdata::read.xls(file = "inputs/comentarios_pt_2.xlsx")

#empilha as linhas de um data.frame que tem as mesmas colunas na mesma ordem
legendas <- rbind(legendas_pt_1, legendas_pt_2)

#comentarios <- rbind(comentarios_pt_1, comentarios_pt_2)