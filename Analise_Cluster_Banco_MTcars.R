################### ANALISE DE CLUSTER #########################################################
# Curso de Análise de Cluster no R 4.1 
# Aluno: Mário Diego Rocha Valente
# Sistema de Informação/ UFPA
##########################################################################################################




###########################################################################################################
############### Análise de Cluster/Agrupamento ############################################################
# Métodos Hierarquicos
# 1) Cluster Aglomerativo
# i)  hclust()
# ii) agnes()

# 2) Cluster Divisivo
# i)  diana()
# ii) mona()

# 2) Cluster com Particionamento
# i) kmeans()       : k-médias                       : pacote (stats)
# ii) pam()         : Partitioning Around Medoids    : pacote (cluster)
# iii) clara()      : Clustering Large Applications  : pacote (factoextra)
###############################################################################################


################ Medidas de Distâncias ##################################################################
# 1) Distancia Euclidiana      : method = "euclidean"
# 2) Distancia de Manhattan    : method = "manhattan" 
# 3) Distancia                 : method = "minkowski" 
# 4) Distancia                 : method = "canberra" 
# 5) Distancia                 : method = "binary"
# 6) Distancia                 : method = "maximum" 

############### Medidas Baseado em Correalacao #############################################
# 1) Correlacao de Pearson     : method = "pearson"
# 2) Correlacao de Spearman    : method = "spearman"
# 3) Correlacao de Kendall     : method = "kendall"
#########################################################################################################


############## Metodos de Ligação #######################################################################
# Single link (nearest neighbour)    : method = "single"
# Complete link (furthest neighbour) : method = "complete"
# Group average link                 : method = "average"
# Weighted average link              : method = "mcquitty"
# Centroid                           : method = "centroid"
# Incremental sum of squares         : method = "ward.D" "ward.D2"
# Median                             : method = "median"
##########################################################################################################

############ Metodos + utilizados na pratica ############################################################
# ward     : minimiza a variancia intra-clusters; 
# complete : define a distancia entre 2 clusters como o valor maximo de todos os pareamentos entre os elementos

#########################################################################################################


############## Método K-means ############################################################################
# O método de clusterização K-means classifica os objetos dentro de múltiplos grupos,
# de forma que a variação intra-cluster seja minimizada pela soma dos quadrados 
# das distâncias Euclidianas entre os itens e seus centroides.
#########################################################################################################

############## Usar a funcao kmeans ####################################################################
# 1) x        : base de dados
# 2) centers  : numero de clusters
# 3) iter.max : numero de interações, padrao e 10
# 4) nstart   : numerio inicial de particoes, superior a 1 

kmeans(x, centers, iter.max=10, nstart=1)
########################################################################################################


############### Instalacao de Pacotes #################################################################

install.packages(c("devtools", "tidverse", "ggplot2", "factoextra", "cluster", "survminer", "ggcorrplot"))
##########################################################################################################

############### Carregamento de Pacotes #################################################################
library(devtools)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(cluster)
library(survminer)
library(ggcorrplot)
library(NbClust)
library(RColorBrewer)
library(dendextend)
library(corrplot)
library(gplots)
library(pheatmap)
library(d3heatmap)
library(ComplexHeatmap)
library(circlize)
library(pkgs)
library(fpc)              # clustering validation statistics

library(clValid)
library(pvclust)
library(parallel)
library(fcm)
library(mclust)
library(dbscan)
#######################################################################################################

#########################################################################################################
############### Exemplo com Banco Nativo do R4.0, Mtcars ################################################
# Vamos carregar a tradicional base de dados nativa do RStudio mtcars, que traz informações 
# sobre 32 modelos de automóveis, sendo as respectivas variáveis que os descrevem:

############### Variáveis do Mtcars #####################################################################
# 1) mpg: milhas por galão;
# 2) cyl: número de cilindros;
# 3) disp: número que representa o volume total no motor como um fator de circunferência do cilindro, profundidade e número total de cilindros;
# 4) hp: potência;
# 5) drat: relação do eixo traseiro;
# 6) wt: peso (1.000 lbs);
# 7) qsec: tempo de 1/4 de milha;
# 8) vs: motor (0 = em forma de V; 1 = linha reta);
# 9) am: transmissão (0 = automático; 1 = manual);
# 10) gear: número de marchas na transmissão (3-4 automático; 4-5 manual);
# 11) carb: número de carburadores;
#########################################################################################################

############### Carregamento dos Dados #################################################################
data("mtcars")
dados_carros=scale(mtcars)
head(dados_carros, n=3)
#######################################################################################################


################ Calculo das Distância ###################################################################
################ Matriz de Distancias Euclidiana #####################################
distancia <- dist(dados_carros, method = "euclidean") 
###################################################################################


############### Matriz de Dissimilaridade #########################################
as.matrix(distancia)

############## Somente as 3 primeiras linhas ######################################
as.matrix(distancia)[1:3,1:3]

############# Estrutura da matriz #################################################
str(distancia)
###################################################################################

############# Matriz de Distancia de Correalacao de Pearson #######################
distancia_correlacao <- get_dist(dados_carros, method = "pearson")

############# Matriz de Dissimilaridade ###########################################
as.matrix(distancia_correlacao)

############# Somente as 3 primeiras linhas #######################################
as.matrix(distancia_correlacao)[1:3,1:3]
###################################################################################

############### Visualizar a Matriz de Distancia ##################################
fviz_dist(distancia)
fviz_dist(distancia_correlacao)
###################################################################################

############## Clusterização k-means ###################################################################

############## Número Otimo de clusters 
fviz_nbclust(distancia, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

############# Gerar o cluster k-medias
set.seed(123)
km.res = kmeans(distancia, 4, nstart=25)
print(km.res)

# características de cada aglomeração
aggregate(mtcars, by=list(cluster=km.res$cluster), mean)


# Informação por Modelos de Carros
mtcars2 = cbind(mtcars, cluster=km.res$cluster)
head(mtcars2)


# Nº de cluster p/ cada observacao
km.res$cluster
head(km.res$cluster, 4)

# Tamanho do Cluster
km.res$size


# Clusters Means
km.res$centers


# Similaridade dos Cluster/Forma Vizual

fviz_cluster(km.res, data = mtcars2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal())

######################################################################################

############### Dendograma ############################################################

# Distancia
res.dist <- dist(distancia, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]
res.hc <- hclust(d = res.dist, method = "ward.D2")

# Dendograma
fviz_dend(res.hc, cex = 0.85)
# cex: tamanho da legenda

#######################################################################################

############## Correlação Cofrenetica ##################################################################

# Correlacao Cofrenetica
res.coph <- cophenetic(res.hc)
res.coph
plot(res.coph)

# Correlacao : Distancia Cofrenetica x Distancia Euclidiana
cor(res.dist, res.coph)


# Numero de Itens cada cluster
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)

table(grp)
rownames(df)[grp == 1]


# Definir o corte e cores nos cluster
fviz_dend(res.hc, k = 4, cex = 0.5, k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
color_labels_by_k = TRUE, rect = TRUE)


# Scatter Plot dos Clusters
fviz_cluster(list(data = distancia, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())
########################################################################################################



############## HeatMap #################################################################################
x <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start=0, end=.3)
cc <- rainbow(ncol(x), start=0, end=.3)
hv <- heatmap(x, col = cm.colors(256), scale="column",
              RowSideColors = rc, ColSideColors = cc, margin=c(5,10),
              xlab = "Características", ylab= "Modelos de Carros",
              main = "Mapa de Calor")

heatmap(distancia, scale = "none")




col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(distancia, scale = "none", col = col,
        RowSideColors = rep(c("blue", "pink"), each = 16),
        ColSideColors = c(rep("purple", 5), rep("orange", 6)))

########################################################################################################


############### Grafico de Siluetta ###################################################################

# Silhouette plot
fviz_silhouette(km.res, palette = "jco",
                ggtheme = theme_classic())


# Silhouette information
silinfo <- km.res$silinfo
names(silinfo)

# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)

# Average silhouette width of each cluster
silinfo$clus.avg.widths

# The total average (mean of all individual silhouette widths)
silinfo$avg.width




#######################################################################################################






