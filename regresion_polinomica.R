rm(list=ls()) #Limpiar memoria

#Librerias
install.packages("tidyverse")
library(tidyverse) #Visualización

install.packages("MASS")
library(MASS)

install.packages("caTools")
library(caTools)

install.packages("ggplot2")
library(ggplot2)

#datos
data("Boston")
head(Boston,7)

#Preparar datos
set.seed(1234)

split <- sample.split(Boston$medv, SplitRatio = 0.75)
training <- subset(Boston,split == TRUE)
test <- subset(Boston,split == FALSE)

#Explorar datos:
summary(training)
nrow(training)
nrow(test)

attach(training)

ggplot(training, aes(lstat, medv)) +
  stat_smooth() +
  geom_point(colour="red") + 
  theme_minimal() +
  xlab("Población en situación desfavorable") +
  ylab("Valor medio de viviendas en \ $1000s.")

model <- lm(lstat~poly(medv,2))
model

summary(model) #*** Vaiable altamente significativa

#y=53.22x2 - 113.35x +  12.67
# Residual standard error: 3.932 (RSE) error cometido por el modelo

#predecir
prediccion <- predict(model, newdata = test)
prediccion

#Evaluar modelo, analizando los residuos
residuos <- rstandard(model)
par(mfrow=c(1,3))
hist(residuos)
boxplot(residuos, main="Diagrama de cajas")
qqnorm(residuos)
qqline(residuos, col = "red")

#Varianza constante - la varianza de los errores es constante
par(mfrom=c(1,1))
plot(fitted.values(model),rstandard(model), xlab="valores ajustados", ylab="Residuos estandarizados")
abline(h=0, col = "red")