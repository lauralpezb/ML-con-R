rm(list=ls()) #Limpiar memoria

install.packages("caTools")
library(caTools)

install.packages("randomForest")
library(randomForest)

install.packages("party")
library(party)

install.packages("ROCR")
library(ROCR)

#Datos
satelite <- read.csv("http://www.diegocalvo.es/wp-content/uploads/2019/11/satelliteData.csv")
head(satelite,7) #1=Agua, 2=Tierra

#Preparación de datos
colnames(satelite)[3] <- "TipoSuperficie"

satelite$TipoSuperficie <- factor(satelite$TipoSuperficie, levels = c("1","2"),labels = c("Agua","Tierra"))

set.seed(1234)

split <- sample.split(satelite$TipoSuperficie, SplitRatio = 0.75)
training <- subset(satelite,split == TRUE)
test <- subset(satelite,split == FALSE)

#Explorar datos:
summary(training)
nrow(training)
nrow(test)

attach(training)

model <- randomForest(TipoSuperficie ~ .,
               data = training,
               ntree = 50)

summary(model)

#predecir
prediccion <- predict(model, newdata = test, type = "class")
prediccion

#Matriz de confusión
confusionMatrix <- table(test$TipoSuperficie, prediccion)
confusionMatrix


#Porcentaje de acierto
(correctos <- sum(diag(confusionMatrix))/nrow(test)*100)

#ROC
roc <- prediction(as.numeric(prediccion), as.numeric(test$TipoSuperficie))
roc <- performance(roc, "tpr", "fpr")
plot(roc)