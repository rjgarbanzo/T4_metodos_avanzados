# 5. [20 puntos] Esta pregunta utiliza los datos sobre muerte del corazon en Sudafrica (SAheart.csv).
# La variable que queremos predecir es chd que es un indicador de muerte coronaria basado en
# algunas variables predictivas (factores de riesgo) como son el fumado, la obesidad, las bebidas
# alcoholicas, entre otras. Las variables son:


# a) Usando Bosques Aleatorios (train.randomForest(...) del paquete traineR) para la tabla
# SAheart.csv con el 80% de los datos para la tabla aprendizaje y un 20% para la
# tabla testing determine la mejor Probabilidad de Corte, de forma tal que se prediga de
# la mejor manera posible la categorıa Si de la variable chd, pero sin desmejorar de manera
# significativa la precision global.

library(traineR)
library(caret)
datos <- read.csv("SAheart.csv",sep = ";",header=T)
str(datos)


muestra <- sample(1:nrow(datos),floor(nrow(datos)*0.20))
ttesting <- datos[muestra,]
taprendizaje <- datos[-muestra,]


modelo <- train.randomForest(formula = chd~., data = taprendizaje, importance = T)
prediccion <- predict(modelo, ttesting, type = "prob")
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc=MC)



Clase <- ttesting[,10]
Score <- prediccion$prediction[,2]
for(Corte in seq(1, 0, by = -0.05)) {
  Prediccion <- ifelse(Score >= Corte, "Si", "No")
  MC <- table(Clase, Pred = factor(Prediccion, levels = c("No", "Si")))
  cat("\nCorte usado para la Probabilidad = ")
  cat(Corte)
  cat("\n")
  print(general.indexes(mc=MC))
  cat("\n========================================")
}


# El mejor corte es usando la probabilidad de 0.4, ya que se obtiene un 76% de si y una precision global de 72%







# Punto B 
# Repita el ejercicio anterior usando XGBoosting. ¿Cambio la probabilidad de corte? Explique.

library(traineR)
library(caret)
datos <- read.csv("SAheart.csv",sep = ";",header=T)
str(datos)

muestra <- sample(1:nrow(datos),floor(nrow(datos)*0.20))
ttesting <- datos[muestra,]
taprendizaje <- datos[-muestra,]

modelo <- train.xgboost(formula = chd ~ .,data = taprendizaje, nrounds = 15,verbose = F)
prediccion <- predict(modelo, ttesting, type = "prob")


Clase <- ttesting[,10]
Score <- prediccion$prediction[,2]
for(Corte in seq(1, 0, by = -0.05)) {
  Prediccion <- ifelse(Score >= Corte, "Si", "No")
  MC <- table(Clase, Pred = factor(Prediccion, levels = c("No", "Si")))
  cat("\nCorte usado para la Probabilidad = ")
  cat(Corte)
  cat("\n")
  print(general.indexes(mc=MC))
  cat("\n========================================")
}

# El mejor corte es usando la probabilidad de 0.15, ya que se obtiene un 73% de si y una precision global de 68%, 
#el cual se encuentra por arriba de las otras probabilidades
