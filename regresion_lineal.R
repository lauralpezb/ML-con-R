rm(list=ls()) #Limpiar memoria

#Librerias
install.packages("ggplot2")
library(ggplot2)

install.packages("MASS")
library(MASS)

install.packages("caTools")
library(caTools)

#datos
data(package="MASS")
data("road")
head(road,7)

set.seed(1234) #Fijar una semilla para que los datos salgan igual

split <- sample.split(road$deaths, SplitRatio = 0.75)
training <- subset(road,split == TRUE)
test <- subset(road,split == FALSE)

#Explorar datos:
summary(training)
nrow(training)
nrow(test)

attach(training)

ggplot(training, aes(deaths, drivers)) +
  stat_smooth(method = "lm") +
  geom_point(colour="blue") + 
  theme_minimal() +
  xlab("Número de conductores (10,000)") +
  ylab("Muertes")

#Definir modelo
model <- lm(deaths~drivers)
model

# y = 3.832 x = 237.957

summary(model) 

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

