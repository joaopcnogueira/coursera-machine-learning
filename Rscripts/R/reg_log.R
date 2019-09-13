# 01 Estudo de Caso: Seguro ----

# Carregando pacotes
library(funModeling)

options(scipen=999) # Retira a notação científica
base <- readxl::read_excel("data/sinistro.xlsx", sheet = 1) 
names(base) 


# Análise Descritiva da Base de Dados ----

# Variável Sexo
table(base$Sexo) 
freq(base, "Sexo")
cross_plot(data = base, input = "Sexo", target = "Sinistro")


# Variável Cidade
table(base$Cidade)
freq(base, "Cidade")
cross_plot(data = base, input = "Cidade", target = "Sinistro")

# Variável Idade
summary(base$Idade)
boxplot(base$Idade)
hist(base$Idade)
plotar(base, input = "Idade", target = "Sinistro", plot_type = "boxplot")
 
# Variável Sinistro
base$Sinistro <- as.factor(base$Sinistro)
table(base$Sinistro)
prop.table(table(base$Sinistro))
freq(base, "Sinistro")


# Modelo de Regressão Logística ----

# cria o modelo usando GLM (general linear model) 
# família binomial - duas possibilidades - 0 ou 1
# link = logit utiliza a função logística

modelo <- glm(Sinistro ~ Sexo + Idade + Cidade,
              family = binomial(link='logit'),
              data = base)

summary(modelo)

base$score <- predict(modelo, base, type = "response")
base$pred <- ifelse(base$score > 0.5, 1, 0)

Metrics::accuracy(base$Sinistro, base$pred)
Metrics::auc(base$Sinistro, base$score)


# 02 Estudo de Caso: Consórcio ----
base <- readxl::read_excel("data/consorcio.xls",sheet=1)
names(base)


# Modelo de Regressão Logística ----
base$Y <- as.factor(base$Y)

modelo <- glm(Y ~ DI + Financiamento + Poupança + Salário + CC,
              family=binomial(link='logit'),
              data=base)
summary(modelo)

score <- predict(modelo, base, type = "response")
pred <- ifelse(score > 0.5, 1, 0)

# matriz de confusão
table(base$Y, pred)
Metrics::accuracy(base$Y, pred)
Metrics::auc(base$Y, score)


# Método Backward para estimar o modelo final ----
# retirar Financiamento e CC
modelo <- glm(Y ~ DI + Poupança + Salário,
              family=binomial(link='logit'),
              data=base)
summary(modelo)


score = predict(modelo, base, type = "response")
pred <- ifelse(score > 0.5, 1, 0)

# matriz de confusão
table(base$Y, pred)
Metrics::accuracy(base$Y, pred)
Metrics::auc(base$Y, score)


# 03 Backward with the step function ----
base <- readxl::read_excel("data/consorcio.xls",sheet=1)
base$Y <- as.factor(base$Y)

modelo <- glm(Y ~ DI + Financiamento + Poupança + Salário + CC,
              family=binomial(link='logit'),
              data=base)
summary(modelo)

# Step function ----
modelo_selecionado <- step(modelo, direction = "backward")
summary(modelo_selecionado)


# Salvando o modelo ----
modelo_selecionado %>% saveRDS("modelsaved2.rds")
modelo_selecionado %>% readr::write_rds("modelsaved1.rds")

