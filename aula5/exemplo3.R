# Pacotes -----------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Gerar dados -------------------------------------------------------------
set.seed(7)
dados <- data_frame(
  x = runif(10),
  y = 2*x + rnorm(10, 0, 0.1)
)

ggplot(dados, aes(x = x, y = y)) + geom_point()



modelo <- lm(y ~ x, data = dados)
summary(modelo)

modelo2 <- lm(y ~ poly(x, 9), data = dados)
summary(modelo2)

erro_modelo1 <- mean((dados$y - predict(modelo, newdata = dados))^2)
erro_modelo2 <- mean((dados$y - predict(modelo2, newdata = dados))^2)
erro_modelo1 %>% round(3)
erro_modelo2 %>% round(3)

ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), se = FALSE, method = 'lm')

# gerando mais dados
dados2 <- data_frame(
  x = runif(100),
  y = 2*x + rnorm(100, 0, 0.1)
)
erro_modelo1 <- mean((dados2$y - predict(modelo, newdata = dados2))^2)
erro_modelo2 <- mean((dados2$y - predict(modelo2, newdata = dados2))^2)
erro_modelo1 %>% round(3)
erro_modelo2 %>% round(3)

ggplot(dados2, aes(x = x, y = y)) + geom_point() +
  geom_smooth(data = dados, formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(data = dados, formula = y ~ poly(x, 9), se = FALSE, method = 'lm')
