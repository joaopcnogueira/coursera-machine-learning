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

apply(df, 2, mean) # means = 0
apply(df, 2, sd)   # std = 1

# Matriz de dissimilaridade
d <- dist(df, method = "euclidean")
# Cluster hierárquico utilizando "Complete Linkage"
hc1 <- hclust(d, method = "complete")
# Dendograma
plot(hc1, cex = 0.6, hang = -1)

# Cluster utilizando agnes
hc2 <- agnes(df, method = "complete")
# Coeficiente aglomerativo
hc2$ac

# Avaliação de métodos hierárquicos que produzem estruturas de clusters mais fortes
m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")
# função para computar o coeficiente
ac <- function(x) {
  agnes(df, method = x)$ac
}
map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendograma utilizando agnes")


# Cluster hierárquico divisivo
hc4 <- diana(df)
# Coeficiente divisivo
hc4$dc
# plotar dendograma
pltree(hc4, cex = 0.6, hang = -1, main = "Dendograma do método diana")


# Método Ward
hc5 <- hclust(d, method = "ward.D2")
plot(hc5, cex = 0.6, hang = -1)
# Cortar a árvore em 4 clusters 
sub_grp <- cutree(hc5, k = 4)
# número de obs em cada cluster
table(sub_grp)

USArrests %>% 
  mutate(cluster = sub_grp) %>% 
  head()

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

# Compute distance matrix
res_dist <- dist(df, method = "euclidean")
# Compute 2 hierarquical clusterings
hc1 <- hclust(res_dist, method = "complete")
hc2 <- hclust(res_dist, method = "ward.D2")
# Create two dendrograms
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2,
 highlight_distinct_edges = FALSE, # Turn-off dashed lines
 common_subtrees_color_lines = FALSE, # Turn-off line colors
 common_subtrees_color_branches = TRUE, # Color common branches 
 main = paste("Emaranhado =", round(entanglement(dend_list), 2))
 )


# Método do cotovelo
fviz_nbclust(df, FUN = hcut, method = "wss")

# Average silhoutte
fviz_nbclust(df, FUN = hcut, method = "silhouette")
