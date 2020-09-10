rm(list=ls()) #Limpiar memoria

install.packages("imager")
library(imager) #visualizar imagenes


install.packages("grid")
library(grid) #Visualizar graficos


install.packages("cowplot")
library(cowplot) #visualizacion multiples graficos

install.packages("ggplot2")
library(ggplot2) #Visualizacion

#directorio
setwd("D:/OneDrive/Trabajo_IEBSchool/Modulo de Machine Learning/Ejemplos en R/Ejemplos de reducción de dimensionalidad")

data("iris")
head(iris,5)

flor <- load.image("flor.jpg")
plot(flor)

datos <- iris[-5] #Se elimina el tipo de flor
summary(datos)

p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()
p3 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + geom_point()
p4 <- ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, color=Species)) + geom_point()
plot_grid(p1, p2, p3, p4, labels = "AUTO")

modelo <- prcomp(datos)
modelo

summary(modelo)
#Standard deviation  Valores de la varianza para cada componente
#Proportion of Variance Porcentaje de varianza por cada factor
#Cumulative Proportion Porcentaje de varianza acumulada para cada factor
plot(modelo)

#Evaluar resultados
colores <- as.character(iris$Specie)
colores[colores=="setosa"] <- "red"
colores[colores=="virginica"] <- "black"
colores[colores=="versicolor"] <- "blue"

pairs(modelo$x,col=colores)

#PC1 y PC2
plot(modelo$rotation,pch='')
abline(h=0, v=0, col="blue")
text(modelo$rotation,labels = rownames(modelo$rotation))

#PC1 y PC3
plot(modelo$rotation[,1],modelo$rotation[,3],pch='.')
abline(h=0, v=0, col="blue")
text(modelo$rotation[,1],modelo$rotation[,3],labels = rownames(modelo$rotation))







