rm(list = ls()) #Limpiar memoria

#Instalar librerias
install.packages("dplyr") # Instalar de manipulación de dataframes "dplyr"
library(dplyr)

install.packages("Hmisc") #Descripción de datos
library(Hmisc)

install.package("tidyverse")#Manipulacion de datos
library("tidyverse")

install.packages("caTools")#Partir el conjunto de datos
library(caTools)

install.packages("e1071")#Modelo naiveBayes y svm
library(e1071)

install.packages("rpart")#Modelo Tree
library(rpart)

install.packages("rpart.plot") #Grafica de Arbol de decisión
library(rpart.plot)

install.packages("randomForest") #Modelo Random Forest
library(randomForest)

#Cargar datos:
taxiFlota<-read.csv("http://www.diegocalvo.es/wp-content/uploads/2019/11/taxiFlota.csv",sep = ";",header=TRUE)
head(taxiFlota,5) #Visualizar las 5 primeras filas de cada columna del dataset

#Preparación de datos:
#Selección de columnas:
datos <- taxiFlota[c('Fecha.Matriculación','Combustible','Clasificación.medioambiental')]

#Eliminar filas con datos faltantes:
sapply(datos, function(x) sum(is.na(x)))
delete.na <- function(df, n=0) { df[rowSums(is.na(df)) <=n,]}
datos <- delete.na(datos)

#Cambio de columna Fecha.Matriculación para almacenar unicamente el año y convertirlas en factor
datos$Fecha.Matriculación <- as.factor(format(as.Date(datos$Fecha.Matriculación, format="%d/%m/%Y"),"%Y"))
datos$Combustible <- as.factor(datos$Combustible)
datos$Clasificación.medioambiental <- as.factor(datos$Clasificación.medioambiental)
head(datos,5)

#Exploración de datos:
glimpse(datos) #Visualizar los datos
table(datos$Clasificación.medioambiental) #Contar cada combinación de la variable a predecir
describe(datos) #Descripción de las columnas del dataset
#La variable a predecir clasificación.medioambiental tiene 4 valores: B,C,ECO,0

#Se inicia una semilla
set.seed(1234)

#Preparación de datos para aplicar los algoritmos de clasificación
split <- sample.split(datos$Clasificación.medioambiental, SplitRatio = 0.75)
training <- subset(datos,split == TRUE) #Datos de entrenamiento
test <- subset(datos,split == FALSE) #Datos de prueba

#Explorar datos de entrada a los modelos:
summary(training)
nrow(training)
nrow(test)

#SVM (Support Vector Machine)
  attach(training)
  modelo_svm <- svm(training$Clasificación.medioambiental ~ .,
               data = training,
               type = 'C-classification',
               kernel = 'radial')
  
  summary(modelo_svm)

  #predecir
  prediccion <- predict(modelo_svm, newdata = test)

  #Matriz de confusión
  confusionMatrix_svm <- table(test$Clasificación.medioambiental, prediccion)
  confusionMatrix_svm
  
  #Porcentaje de acierto
  correctos_svm <- sum(diag(confusionMatrix_svm))/nrow(test)*100

#Ãrboles de Decisión
  modelo_tree <- rpart(Clasificación.medioambiental ~ .,
                 data = training)
  
  summary(modelo_tree)
  
  rpart.plot(modelo_tree)
  
  #predecir
  prediccion <- predict(modelo_tree, newdata = test, type = "class")

  #Matriz de confusiÃ³n
  confusionMatrix_tree <- table(test$Clasificación.medioambiental, prediccion)
  confusionMatrix_tree
  
  #Porcentaje de acierto
  correctos_tree <- sum(diag(confusionMatrix_tree))/nrow(test)*100

#Bosques aleatorios (Random Forest) 
  modelo_rf <- randomForest(Clasificación.medioambiental ~ .,
                        data = training,
                        ntree = 50)
  
  summary(modelo_rf)
  
  #predecir
  prediccion <- predict(modelo_rf, newdata = test, type = "class")
  
  #Matriz de confusión
  confusionMatrix_rf <- table(test$Clasificación.medioambiental, prediccion)
  confusionMatrix_rf
  
  
  #Porcentaje de acierto
  correctos_rf <- sum(diag(confusionMatrix_rf))/nrow(test)*100

#Cuantificador Bayesiano Ingenuo (Naive Bayes)
  modelo_nb <- naiveBayes(Clasificación.medioambiental ~ .,
                      data = training,
                      laplace = 0) #suavizado de Laplace
  
  summary(modelo_nb)
  
  #predecir
  prediccion <- predict(modelo_nb, newdata = test, type = "class")
  
  #Matriz de confusión
  confusionMatrix_nb <- table(test$Clasificación.medioambiental, prediccion)
  confusionMatrix_nb
  
  #Porcentaje de acierto
  correctos_nb <- sum(diag(confusionMatrix_nb))/nrow(test)*100

#Mejor algoritmo
modelos <- c('Naive Bayes','Random Forest','Arbol de decisión','SVM')
correctos <- c(correctos_nb,correctos_rf,correctos_tree,correctos_svm)
evaluar <- data.frame (cbind(modelos,correctos))
cat("El mejor algoritmo para clasificación medioambiental es",evaluar[evaluar$correctos == max(evaluar$correctos),][1,1], "con un porcentaje de acierto de ", max(evaluar$correctos))
confusionMatrix_rf #Matriz de confusiÃ³n de Random Forest
table(test$Clasificación.medioambiental) #Contar cada combinación de la variable que se predijo

#En conclusión, la predicción del modelo Random Forest es la más acertada comparada con el set de datos de prueba
