
rm(list=ls()) #Limpiar memoria

install.packages("psych")
library("psych") #conjunto de datos

#datos
data("bfi")
head(bfi, 7)

#Preparacion de datos. Datos de la encuesta
datos <- bfi[1:25]
head(datos,7)

#valores nulos
sapply(datos, function(x) sum(is.na(x)))

#Eliminar nulos
delete.na <- function(df, n=0) { df[rowSums(is.na(df)) <=n,]}

datos <- delete.na(datos)

#definir modelo
scree.plot(datos, type = 'R')

modelo <- factanal(datos, 6, scores = c("regression"), rotation = "none")
print(modelo, digits = 2, cutoff = .6, sort=TRUE)

#Vectores y factores caracteristicos
head(modelo$scores)

#muestra visual
load <- modelo$loadings[,1:2]
plot(load,type="n")
text(load, labels = names(datos),cex=.7)


