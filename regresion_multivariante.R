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
data("mtcars")
head(mtcars,7)

#Preparar datos
set.seed(1234)

split <- sample.split(mtcars$mpg, SplitRatio = 0.75)
training <- subset(mtcars,split == TRUE)
test <- subset(mtcars,split == FALSE)

#Explorar datos:
summary(training)
nrow(training)
nrow(test)

attach(training)

ggplot(training, aes(mpg, hp+wt)) +
  stat_smooth(method = "lm") +
  geom_point(colour="red") + 
  xlab("millas / (US) galón") +
  ylab("Potencia + Peso") +
  theme_minimal()

model <- lm(mpg~hp+wt)
model

summary(model) #*** Vaiable altamente significativa

#mpg millas por galon = 36.70319 - 0.03261 (hp)potencia - 3.67617 (wt)peso
#E R-squared es del 0.7826 78%, las variables explican bien el consumo
# A mayor peso y potencia menor millas por galon
#RSE (Residual standard error: 2.908)

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

#Ejercicio
mycar=data.frame(hp=170, wt=2.5)
mympg=predict.lm(model,mycar)
mympg

