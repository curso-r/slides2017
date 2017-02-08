# Pacotes -----------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)

# Ler os dados ------------------------------------------------------------
download.file('https://github.com/curso-r/pu.modelos/blob/master/data/BodyFat.xls?raw=true', 'aula5/bodyfat.xls')
bodyfat <- read_excel('aula5/bodyfat.xls')


# Análise -----------------------------------------------------------------
ggplot(bodyfat, aes(x = WEIGHT, y = BODYFAT)) + geom_point()

ajuste <- lm(BODYFAT ~ WEIGHT, data = bodyfat)
summary(ajuste)
str(ajuste, max.level = 1)
bodyfat$predito <- predict(ajuste, newdata = bodyfat)
mse <- mean((bodyfat$BODYFAT - bodyfat$predito)^2)
erro_usando_media <- mean((bodyfat$BODYFAT - mean(bodyfat$BODYFAT))^2)


