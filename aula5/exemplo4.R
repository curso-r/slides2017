# Pacotes -----------------------------------------------------------------
library(readr)

# Ler os dados ------------------------------------------------------------
titanic <- read_csv('https://github.com/curso-r/pu.modelos/raw/master/data/titanic-train.csv')
titanic$Survived <- as.factor(titanic$Survived)

id_treino <- sample(1:nrow(titanic), size = 0.7*nrow(titanic))
treino <- titanic[id_treino,]
teste <- titanic[-id_treino,]

ajuste <- glm(Survived ~ Sex + Age + Fare, data = treino, family = 'binomial')
summary(ajuste)

# medir o erro nas duas bases

# aumentar o modelo até começar a ficar pior

