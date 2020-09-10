rm(list=ls()) #Limpiar memoria


install.packages("factoextra")
library(factoextra) #Visualización de analisis exploratorio multivariante


install.packages("class")
library(class) #Gestion de clases


install.packages("caTools")
library(caTools) #Libreria para partir el conjunto de datos

#Cargar datos
data("iris")

#preparar datos
set.seed(1234)

split <- sample.split(iris, SplitRatio = 0.75)
training <- subset(iris,split == TRUE)
test <- subset(iris,split == FALSE)

#Calcular la matriz de distancias
distancia <- dist(scale(training[,-5]))

#Definir modelo
hc_datos <- hclust(distancia, method = "ward.D2")

#Visualizar el resultado en forma de dendograma
fviz_dend(hc_datos,color_labels_by_k = TRUE,
          ggtheme = theme_dark())

fviz_dend(hc_datos,k = 3, cex = 0.5,
          k_colors = c("red","green","blue"),
          color_labels_by_k = TRUE,
          ggtheme = theme_dark())

#Calcular los grupos del conjunto de entrenamiento
grupos <- cutree(hc_datos, k = 3)
table(grupos)

#Ejecutar el algoritmo de vecinos mas cercanos
clusterKNN <- knn(train = training[,-5], 
                  test = test[,-5],
                  k = 1, cl = grupos)
#K numero de vecinos considerados
#cl factor de clasificaciones verdaderas del conjunto de entrenamiento
#Calsificacion del conjunto test
clusterKNN

#Evaluar modelo. Calcular el analisis de componentes principales
pca_train <- data.frame(prcomp(training[,-5],scale. = T)$x[,1:2],
                        cluster = as.factor(grupos), factor = "train")

pca_test <- data.frame(prcomp(test[,-5],scale. = T)$x[,1:2],
                       cluster = as.factor(clusterKNN), factor = "test")

#unir ambos analisis en una variable
pca <- as.data.frame(rbind(pca_train, pca_test))

#visualizar los resultados
ggplot(pca, aes(x = PC1, y = PC2, color = cluster, size = 1, alpha = factor)) +
  geom_point(shape = 19) + theme_bw()






