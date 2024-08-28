#### Base de dados trabalho ####

# Link base de dados: https://archive.ics.uci.edu/dataset/476/buddymove+data+set #
# User interest information extracted from user reviews published in holidayiq.com about various types of point of interests in South India #
# Colunas (7): (User.Id/Sports/Religious/Nature/Theatre/Shopping/Picnic) #

library(tidyverse)
library(factoextra)
packageVersion("factoextra")
library(cluster)
library(FactoMineR)
library(gridExtra)


getwd()
setwd("C:/Users/rodri/OneDrive/Área de Trabalho/RodrigoBragança_Análise de clusters [23E3_2]")
Base_dados = read.csv("Base de dados Utilizada_ buddymove_holidayiq.csv")
View(Base_dados)
summary(Base_dados)

# Pré tratamento da base de dados - Tirando a 1º coluna "User.id" e colocando ela como rótulo de linha #

Dados_semChar = Base_dados[,-1]
row.names(Dados_semChar) = Base_dados[,1]


#### Transformando em Scale (standardize) ####
# Obs.Professor -> "Scale = queremos que a média = 0 / desvio padrão = 1" #

Dados_scaled = scale(Dados_semChar)
View(Dados_scaled)
summary(Dados_scaled)


#### Selecting the number of K ####

fviz_nbclust(Dados_scaled, kmeans, method = "wss") # Resultado entre (3k), (4k) ou 5K? #
fviz_nbclust(Dados_scaled, kmeans, method = "silhouette")
fviz_nbclust(Dados_scaled, kmeans, method = "gap_stat")
Dados_kmeans = kmeans(Dados_scaled, 5)

# Outras formas de verificar o número ideal de cluster para utilizar no Kmeans #
# fviz_nbclust(Dados_scaled, kmeans, method = "gap_stat") #
# fviz_nbclust(Dados_scaled, kmeans, method = "silhouette") #


##### Kmeans Clustering #####
set.seed(3)
kmean_2_out = kmeans(Dados_scaled, centers = 2, iter.max = 1000)
kmean_3_out = kmeans(Dados_scaled, centers = 3, iter.max = 1000)
kmean_4_out = kmeans(Dados_scaled, centers = 4, iter.max = 1000)
kmean_5_out = kmeans(Dados_scaled, centers = 5, iter.max = 1000)
print(kmean_2_out)
kmeans_clusters = kmean_out$cluster
rownames(Dados_scaled) = Base_dados$user.Id
fviz_cluster(list(data=Dados_scaled, cluster = kmeans_clusters))


# Comparando mapa com diferente número de clusters -  "library(gridExtra)" #

p2 = fviz_cluster(kmean_2_out,geom = 'point', data = Dados_scaled) + ggtitle("k=2")
p3 = fviz_cluster(kmean_3_out,geom = 'point', data = Dados_scaled) + ggtitle("k=3")
p4 = fviz_cluster(kmean_4_out,geom = 'point', data = Dados_scaled) + ggtitle("k=4")
p5 = fviz_cluster(kmean_5_out,geom = 'point', data = Dados_scaled) + ggtitle("k=5")



grid.arrange(p2, p3, p4, p5) # À partir do "grid" apresentado ao lado, observamos que o k=5 não é interessante para a nossa base de dados, pois conseguimos observar que 2 cluster se sobrepõem #





# PAM clustering

pam_out = pam(Dados_scaled, k = 3, diss = TRUE)
pam_clusters = pam_fit$clustering



#### Clusterização Hierarquica ####

dist_euclid = dist(Dados_scaled, method = 'euclidean')
Matriz_dist_euclid = as.matrix(dist_euclid)
view(Matriz_dist_euclid)

# Agrupamento hclust #
set.seed(3)
Group_cluster = hclust(dist_euclid, method = 'ward.D')
fviz_dend(Group_cluster, k=4)
hclust_cluster = cutree(Group_cluster, k = 4)
plot(Dados_scaled, col = hclust_cluster)


# Gerando o clusterplot #

fviz_cluster(list(data= Dados_scaled, cluster = hclust_cluster))

# Exemplo dado em aula para incluir palheta de cores. "palette = c("#2E9FDF", "#00AFBB", "#E7B800")ellipse.type = "convex", repel = TRUE, show.clust.cent = TRUE ggtheme = theme_minimal())" #






