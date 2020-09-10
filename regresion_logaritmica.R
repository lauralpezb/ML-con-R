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
data("Animals")
head(Animals,7)

#Preparar datos
set.seed(1234)

split <- sample.split(Animals$brain, SplitRatio = 0.75)
training <- subset(Animals,split == TRUE)
test <- subset(Animals,split == FALSE)

#Explorar datos:
summary(training)
nrow(training)
nrow(test)

attach(training)

ggplot(training, aes(body, brain)) +
  stat_smooth(method = "glm") +
  geom_point(colour="red") + 
  theme_minimal() +
  xlab("Peso corporal en kg") +
  ylab("Peso corporal en g")

#Regresión logistica
ggplot(training, aes(log1p(brain), log1p(body))) +
  stat_smooth(method = "glm") +
  geom_point(colour="red") + 
  theme_minimal() +
  xlab("Peso corporal en kg") +
  ylab("Peso corporal en g") + 
  scale_x_log10() +
  scale_y_log10()


model <- lm(log1p(brain)~log1p(body),data=training)
model

summary(model) #*** Vaiable altamente significativa

#y=0.6783 log (x) + 1.9049

#predecir
prediccion <- predict(model, newdata = log1p(test))
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

