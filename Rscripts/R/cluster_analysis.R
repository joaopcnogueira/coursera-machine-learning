# Loading data ----
# criar banco de dados
mcdonalds <- read.csv("data/mcdonalds.csv",
                        sep = ";",
                        header = T,
                        dec = ",",
                        row.names = 1)

View(mcdonalds)
# standard scaler
mcdonalds_padronizado <- scale(mcdonalds)
View(mcdonalds_padronizado)

# Cluster Hierárquico ----
library(cluster)
library(dplyr)

# calcular a matriz de distância utilizando a distância euclidiana
distancia <- dist(mcdonalds_padronizado,
                  method = "euclidean")

# calcular o cluster: métodos disponíveis: "average", "single", "complete" e "ward.D"
cluster_hierarquico <- hclust(distancia,
                              method = "ward.D")
# dendrograma
plot(cluster_hierarquico,
     cex = 0.6,
     hang = -1)


# Cluster K-means ----
set.seed(5)

library(cluster)
library(factoextra)
library(gridExtra)

# rodar o modelo
mcdonalds_k2 <- kmeans(mcdonalds_padronizado,
                       centers = 2, 
                       nstart = 25, 
                       iter.max = 100)

# visualizar os clusters
fviz_cluster(mcdonalds_k2, data = mcdonalds_padronizado, main = "Cluster Kmeans")

# criar clusters
mcdonalds_k3 <- kmeans(mcdonalds_padronizado, centers = 3, nstart = 25)
mcdonalds_k4 <- kmeans(mcdonalds_padronizado, centers = 4, nstart = 25)
mcdonalds_k5 <- kmeans(mcdonalds_padronizado, centers = 5, nstart = 25)

# criar gráficos
G1 <- fviz_cluster(mcdonalds_k2, geom = "point", data = mcdonalds_padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds_k3, geom = "point",  data = mcdonalds_padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds_k4, geom = "point",  data = mcdonalds_padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds_k5, geom = "point",  data = mcdonalds_padronizado) + ggtitle("k = 5")

# imprimir gráficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

# Validar modelo ----
set.seed(5)

# kmeans
fviz_nbclust(mcdonalds_padronizado, kmeans, method = "wss")
fviz_nbclust(mcdonalds_padronizado, kmeans, method = "silhouette")

# cluster hierárquico
fviz_nbclust(mcdonalds_padronizado, FUN = hcut, method = "wss")
fviz_nbclust(mcdonalds_padronizado, FUN = hcut, method = "silhouette")
