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
# Aplicando scale nas variáveis numéricas
train_df$Idade_scaled <- scale(train_df$Idade)
train_df$Valor_scaled <- scale(train_df$Valor)
train_df$Prazo_scaled <- scale(train_df$Prazo)

modelo <- glm(Inadimplencia ~ Sexo+Cargo+Residencia+Idade_scaled+Valor_scaled+Prazo_scaled, 
              family = binomial(link = "logit"),
              data = train_df)
modelo
summary(modelo)

score <- predict(modelo, train_df, type = "response")
pred <- ifelse(score > 0.5, 1, 0)

accuracy(train_df$Inadimplencia, pred)

# matriz de confusão
table(train_df$Inadimplencia, pred)

# Treinando o modelo: parte 03 ----
# Balanceando a base de dados
train_df <- train_df %>% 
    select(-contains("scaled"))

# Balancear a base de dados - 50% e 50%

# Coloca os 1 em uma base de dados
base1 <- subset(train_df, train_df$Inadimplencia == 1)

#Selecionar, aleatoriamente, n observações dos 0
n <- nrow(base1)
set.seed(123) #semente para obter sempre a mesma amostra
base0 <- subset(train_df, train_df$Inadimplencia == 0) #filtrar os 0

dt = sort(sample(nrow(base0), n)) #amostra
#coloca no vetor dt uma amostra ordenada(amostra(de 5192 (nrow),amostra 1271))

amostra_0 <- base0[dt,] #coloca as linhas selecionadas na base Amostra_0

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

