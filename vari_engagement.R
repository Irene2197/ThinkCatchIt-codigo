Output2 <- read.csv(file = 'Output2.csv', header = TRUE, sep = ";")

Output2<-df.merged4

#Normalización de las Variables

library(dplyr)
library(ggplot2)
library(corrplot)
library(gmodels)
library(scales)

Output2$CTR<-as.numeric(gsub(",",".",Output2$CTR))
Output2$avgTimeOnPage<-as.numeric(gsub(",",".",Output2$avgTimeOnPage))
Output2$pageviews<-as.numeric(Output2$pageviews)

result <- Output2 %>% select(CTR, pageviews, avgTimeOnPage)

Output_estandar <-  as.data.frame(scale(result, center = TRUE, scale = TRUE))

Output_estandar$CTR.rescaled <- rescale(Output_estandar$CTR)
Output_estandar$pageviews.rescaled <- rescale(Output_estandar$pageviews)
Output_estandar$avgTimeOnPage.rescaled <- rescale(Output_estandar$avgTimeOnPage)

hist(Output_estandar$pageviews.rescaled, breaks=10, xlab="Pageviews", main="")
hist(Output_estandar$CTR.rescaled, breaks=10, xlab="CTR", main="")
hist(Output_estandar$avgTimeOnPage.rescaled, breaks=10, xlab="AvgTimeOnPage", main="")

out_final <- Output_estandar %>% select(CTR.rescaled, pageviews.rescaled, avgTimeOnPage.rescaled)

summary(out_final)


#Media Ponderada
pesos<-c(0.2,0.4,0.4)
out_final <- out_final %>% 
  mutate( engagement_score = ((CTR.rescaled*0.2) + (pageviews.rescaled*0.4) + (avgTimeOnPage.rescaled*0.4)) / sum(pesos) )


#Clusterización buena
library(tidyverse)
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggmap)
library(factoextra)
library(cluster)
################################ K-MEANS CLUSTERING ################################ 

# Número de clusters
centers = 3

# Número de configuraciones iniciales
nstarts = 25

k2 <- kmeans(out_final, centers = centers, nstart = nstarts)
str(k2)

# OUTPUT:
k2$cluster         # vector de enteros que indica el cluster de cada línea del dataset (from 1:k) 
k2$centers         # matriz de centroides
k2$totss           # la suma total de los cuadrados
k2$withinss        #vector de suma de cuadrados dentro del cluster, uno por cluster
k2$tot.withinss    #Suma de cuadrados total dentro del cluster
k2$betweenss       #La suma de cuadrados entre clusters
k2$size            #el número de puntos en cada cluster


# VISUALIZACIÓN EN 2 DIMENSIONES: 
# SI hay más de 2 dimensiones calculará PCA (Análisis de componentes principales) para reducirlo a las 2 componentes
# principales que explican la mayoría de la varianza

fviz_cluster(k2, data = out_final)

### COMPARAR VARIAS CONFIGURACIONES

k3 <- kmeans(out_final, centers = 3, nstart = nstarts)
k4 <- kmeans(out_final, centers = 4, nstart = nstarts)
k5 <- kmeans(out_final, centers = 5, nstart = nstarts)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df_scaled) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)


### B

set.seed(123)
fviz_nbclust(df_scaled, kmeans, method = "silhouette")
fviz_nbclust(df_scaled, kmeans, method = "wss")

# acepta los siguientes métodos: "silhouette", "wss", "gap_stat"

###### SELECCIÓN SOLUCIÓN ÓPTIMA

clu_sel <- 2

k <- kmeans(df_scaled, centers = clu_sel, nstart = nstarts)
fviz_cluster(k, data = df_scaled)

### RESULTADOS

final <-  df_final
final$cluster <- as.factor(k$cluster)  

k$size 

#####  VISUALIZAR ESTACIONES Y CLUSTERS GEOLOCALIZADOS

qmplot(LONGITUD, LATITUD, data = final, maptype = "toner-lite", color = cluster,
       size = I(6), alpha = I(.6), legend = "topleft", shape = I(15) )


#####  ANALIZAR LOS RESULTADOS
# calcular la media diaria por estación para identificar qué es cluster 1(alta emisión), 2 (baja emisión)
final$Mean <- rowMeans(final[,8:31])



################################ CLUSTERING JERÁRQUICO ################################ 

# Matriz de distancias
d <- dist(df_scaled, method = "euclidean")

# USING COMPLETE LNKAGE
hc1 <- hclust(d, method = "complete" )

# Visualización del arbol del cluster
plot(hc1, hang = -1,cex=0.7,labels = rownames(df_scaled), main= "Cluster Estaciones Calidad Aire")

# Visualización de los grupos formados por el cluster

k = 2
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 12, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=2")

k = 3
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 9, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=3")

k = 4
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=4")



### RESULTADOS

# Creación de k grupos
clusters <- cutree(hc1, k=2)

final$cluster_JERQ <- as.factor(clusters)  


#####  VISUALIZAR ESTACIONES Y CLUSTERS GEOLOCALIZADOS

qmplot(LONGITUD, LATITUD, data = final, maptype = "toner-lite", color = cluster_JERQ,
       size = I(6), alpha = I(.6), legend = "topleft", shape = I(15) )



#Clusterización (david)

R = kmeans(out_final, 4)
summary(R)

out_final2<-out_final %>% filter(!is.na(CTR.rescaled)) %>% filter(!is.na(avgTimeOnPage.rescaled))  %>% filter(!is.na(pageviews.rescaled))

R = kmeans(arasi2, 3)
summary(R)


R$centers

#para realizar el codo
cors<-cor(out_final[,1:3])
pfa.eigen<-eigen(cors)
NumVars<-dim(out_final[,1:3])[2]
plot(1:NumVars, pfa.eigen$values, xlab="Clusters", ylab="Criterio del codo")
lines(1:NumVars, pfa.eigen$values)


fviz_nbclust(x = out_final[,2:4], FUNcluster = kmeans, method = "wss", k.max = 7, 
             diss = get_dist(out_final[,1:3], method = "euclidean"), nstart = 50)

x<-out_final[,2:4]
calcular_totwithinss <- function(n_clusters, x, iter.max=70, nstart=50){
  # Esta función aplica el algoritmo kmeans y devuelve la suma total de
  # cuadrados internos.
  cluster_kmeans <- kmeans(centers = n_clusters, x = x, iter.max = iter.max,
                           nstart = nstart)
  return(cluster_kmeans$tot.withinss)
}

# Se aplica esta función con para diferentes valores de k
total_withinss <- map_dbl(.x = 1:7,
                          .f = calcular_totwithinss,
                          datos = x)
total_withinss


# Matriz de distancias
d <- dist(x, method = "euclidean")

# USING COMPLETE LNKAGE
hc1 <- hclust(d, method = "complete" )

# Visualización del arbol del cluster
plot(hc1, hang = -1,cex=0.7,labels = rownames(df_scaled), main= "Cluster Estaciones Calidad Aire")

# Visualización de los grupos formados por el cluster

k = 2
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 12, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=2")

k = 3
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 9, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=3")

k = 4
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=4")



### RESULTADOS

# Creación de k grupos
clusters <- cutree(hc1, k=2)

final$cluster_JERQ <- as.factor(clusters)  


#####  VISUALIZAR ESTACIONES Y CLUSTERS GEOLOCALIZADOS

qmplot(LONGITUD, LATITUD, data = final, maptype = "toner-lite", color = cluster_JERQ,
       size = I(6), alpha = I(.6), legend = "topleft", shape = I(15) )
