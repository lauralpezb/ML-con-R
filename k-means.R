rm(list=ls()) #Limpiar memoria

install.packages("tidyverse")
library(tidyverse) #manipulacion de datos


install.packages("cluster")
library(cluster) #Algoritmo de cluster


install.packages("factoextra")
library(factoextra) #algoritmo de cluster y visualizacion

install.packages("gridExtra")
library(gridExtra) #Visualizacion

datos <- USArrests
head(datos, 8)
#murder: muertes por cada 100.000 habitantes
#Assault: arrestos or asalto por cada 100.000 habitantes
#UrbanPop: porcentaje de poblacion urbana
#Rape: Numero de arrestos por violacion por cada 100.000 habitantes

#Pretratar datos
datos <- na.omit(datos) #Eliminar datos nulos

datos <- scale(datos) #escalar datos para eliminar sesgos
head(datos, 8)

distancia <- get_dist(datos)

fviz_dist(distancia, gradient = list(low = "blue", high = "red"))

#definir modelo
k2 <- kmeans(datos, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = datos)

datos%>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests))%>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster),
             label = state)) + geom_text()

#Evaluar modelos
k3 <- kmeans(datos, centers = 3, nstart = 25)
k4 <- kmeans(datos, centers = 4, nstart = 25)
k5 <- kmeans(datos, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = datos) + ggtitle("K = 2")
p2 <- fviz_cluster(k3, geom = "point", data = datos) + ggtitle("K = 3")
p3 <- fviz_cluster(k4, geom = "point", data = datos) + ggtitle("K = 4")
p4 <- fviz_cluster(k5, geom = "point", data = datos) + ggtitle("K = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#optimizar el modelo
#1) Elbow Method
#Minimiza la suma de las variaciones al cuadrado de las distancias dentro de los clusters
#preparar datos
set.seed(1234)
wss <- function(k) {
  kmeans(datos, k, nstart = 10)$tot.withinss
}

num_clusters <- 1:15

wss_values <- map_dbl(num_clusters, wss)

plot(num_clusters, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab = "Número de clusters (X)",
     ylab = "Suma total de cuadrados dentro de los grupos")

fviz_nbclust(datos,kmeans,method = "wss")

#2) Average Silhouette Method
#calcular el ga de cada elemento dentro del grupo
avg_sil <- function(k){
  km.res <- kmeans(datos, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(datos))
  mean(ss[, 3])
}

k.values <- 2:15

avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Average Silhouette")

fviz_nbclust(datos, kmeans,method = "silhouette")

#3) Gap statistic method
gap_stat <- clusGap(datos, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

final <- kmeans(datos, 4, nstart = 25)

print(final)

fviz_cluster(final, data = datos)

#puntos medios
USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")








