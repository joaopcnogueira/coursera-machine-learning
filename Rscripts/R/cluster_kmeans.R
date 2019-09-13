library(tidyverse)   # Manipulação de dados
library(cluster)     # Algoritmos de cluster
library(factoextra)  # Algoritmos de cluster e visualização
library(dendextend)  # Comparar Dendogramas

# carregando a base
df <- USArrests

# removendo valores faltantes
df <- na.omit(df)

# padronizando os dados: mean = 0 e std = 1
df <- scale(df)
head(df)

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
k2$centers
k2$cluster

fviz_cluster(k2, data = df)

df %>% 
  as_tibble() %>% 
  mutate(cluster = k2$cluster, 
         state = row.names(USArrests)) %>% 
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)
# Gráficos para comparação
g2 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
g3 <- fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")
g4 <- fviz_cluster(k4, geom = "point", data = df) + ggtitle("k = 4")
g5 <- fviz_cluster(k5, geom = "point", data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(g2, g3, g4, g5, nrow = 2)

set.seed(123)
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}
k_values <- 1:15
wss_values <- map_dbl(k_values, wss)
plot(k_values, wss_values, type = "b", pch = 19,
     xlab = "Number of clusters K", 
     ylab = "Soma dos quadrados totais dentro dos cluster")

set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

# Extraindo os resultados
set.seed(123)
final <- kmeans(df, 4, nstart= 25)
final

fviz_cluster(final, data = df)

USArrests %>% 
  mutate(Cluster = final$cluster) %>% 
  group_by(Cluster) %>% 
  summarise_all("mean")
