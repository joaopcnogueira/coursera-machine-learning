# Estudo de caso 01 ---- 

# Carregando bibliotecas ----
library(readxl)
library(funModeling)
library(dplyr)
library(Metrics)

df <- read_excel("data/credito.xlsx", sheet = 1)
View(df)
glimpse(df)


# Treinando o modelo: parte 01 ----
train_df <- df %>% 
    select(-Id)

modelo <- glm(Inadimplencia ~ ., 
              family = binomial(link = "logit"),
              data = train_df)
modelo
summary(modelo)

score <- predict(modelo, train_df, type = "response")
pred <- ifelse(score > 0.5, 1, 0)

accuracy(train_df$Inadimplencia, pred)

# matriz de confusão
table(train_df$Inadimplencia, pred)


# Treinando o modelo: parte 02 ----

# Balancear a base de dados - 50% e 50%

# filtrar Inadimplencia = 1
base1 <- train_df %>% 
    filter(Inadimplencia == 1)

# Selecionar aleatoriamente n observações dos 0
n <- nrow(base1)

# semente para obter sempre a mesma amostra
set.seed(123) 

# filtrar Inadimplencia = 0
base0 <- train_df %>% 
    filter(Inadimplencia == 0)


idx <- sort(sample(nrow(base0), n))
amostra_0 <- base0[idx,] # coloca as linhas selecionadas na base amostra_0

# Junta as duas bases
base_balanceada = rbind(base1, amostra_0)

table(base_balanceada$Inadimplencia) #Tabela de frequencias 
View(base_balanceada)

modelo <- glm(Inadimplencia ~ Sexo+Cargo+Residencia+Idade+Valor+Prazo, 
              family = binomial(link = "logit"),
              data = base_balanceada)

score <- predict(modelo, base_balanceada, type = "response")
pred <- ifelse(score > 0.5, 1, 0)

accuracy(base_balanceada$Inadimplencia, pred)

table(base_balanceada$Inadimplencia, pred)

# testando na base desbalanceada
score <- predict(modelo, train_df, type = "response")
pred <- ifelse(score > 0.5, 1, 0)

accuracy(train_df$Inadimplencia, pred)

table(train_df$Inadimplencia, pred)

