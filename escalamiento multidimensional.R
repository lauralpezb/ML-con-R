rm(list=ls()) #Limpiar memoria

install.packages("igraph")
library(igraph) #conjunto de datos

datos <- matrix(c(0, 1, 2, 4, 5, 1, 0, 2.5, 4, 5, 2, 2.5, 0, 4, 5, 4,
                  4, 4, 0, 1.2, 5, 5, 5, 1.2, 0), nrow = 5)
rownames(datos) <- colnames(datos) <- c("Whisky","Ron","Licor","Vodka","Ginebra")

datos

modelos <- cmdscale(datos)

modelos

distancia <- dist(modelos)
distancia

#Evaluar modelo
#Diagrama de dispersion
plot(modelos, type = "n", xlab = "Coord. 1", ylab = "Coord. 2")
text(modelos[,1], modelos[,2], labels = rownames(modelos), cex = 0.8)

valores <- cmdscale(distancia, eig = T) #las dosprimeras coordenadas principales

valores$eig #valores propios calculados

valores$GOF #determinar ajuste

#Visualizacion de grafo
grafo <- graph.full(nrow(datos)) # definir tamaño
V(grafo)$label <- colnames(datos) #nombre a los vertices
layout <- layout.mds(grafo, dist = as.matrix(distancia)) # definir tamaño de aristas

plot(grafo, layout = layout, vertex.size = 4)
