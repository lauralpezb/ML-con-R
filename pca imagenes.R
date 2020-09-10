
rm(list=ls()) #Limpiar memoria

install.packages("jpeg", dependencies = TRUE)
library("jpeg") #visualizar imagenes JPG


install.packages("imager")
library(imager) #Visualizar graficos

setwd("C:/Users/usuario/Documents/R")

imagen <- load.image("la_noche_estrellada.jpg")
plot(imagen)

#Preparar datos y definir el modelo
#Separar imagen en sus tres componentes RGB aplicando a cada una PCA
imagen.r <- prcomp(imagen[,,1], center = FALSE)
imagen.g <- prcomp(imagen[,,2], center = FALSE)
imagen.b <- prcomp(imagen[,,3], center = FALSE)

rgb.pca <- list(imagen.r, imagen.g, imagen.b)

#Definir componentes de compresion
componentes.select <- c(10,15,30,100,200,270,280,300)
componentes.select.char <- c("010","015","030","100","200","270","280","300")

#EVALUACION RESULTADOS
#carpeta para guardar imagenes
indice <- 1
for (i in componentes.select) {
  pca.img <- sapply(rgb.pca, function(j){
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  
  writeJPEG(pca.img, paste('imagenes/comprimida_con_', componentes.select.char[indice],'_componentes.jpg', sep = ''))
  indice <- indice + 1
}





