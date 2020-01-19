#Ejercicio 3
# 3. [20 puntos] Para esta pregunta usaremos nuevamente los datos Datos Churn.csv. Usando el
# paquete traineR realice lo siguiente:

install.packages("traineR")

library(caret)
library(traineR)
# 
# El objetivo de este ejercicio es calibrar el metodo de kknn para esta Tabla de Datos. Aquı
# interesa predecir en la variable DejaBanco que indica si el cliente deja sus negocios con
# el banco (1) o no (0). Para esto genere 5 Validaciones Cruzadas con 10 grupos calibrando
# el modelo de acuerdo con todos los tipos de algoritmos que permite train.kknn en
# el parametro kernel, estos algoritmos son: rectangular, triangular, epanechnikov,
# biweight, triweight, cos, inv, gaussian y optimal. Para medir la calidad de metodo
# sume la cantidad de 1’s detectados en los diferentes grupos. Luego grafique las 5 iteraciones
# para todos algoritmos en el mismo grafico. ¿Se puede determinar con claridad cual
# algoritmo es el mejor? Para generar los modelos predictivos use el paquete traineR.


datos <- read.csv("Datos_Churn.csv",header=TRUE, sep=",", dec=".")
datos <- datos[, -c(1,4,2,7)]
datos <- na.omit(datos)

datos$TarjetaCredito <- factor(datos$TarjetaCredito, ordered = T)
datos$Activo <- factor(datos$Activo, ordered = T)
datos$DejaBanco <- factor(datos$DejaBanco, ordered = T)
str(datos)

numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- 2
cantidad.grupos <- 2


deteccion.si.rectangular <- c()
deteccion.si.triangular <- c()
deteccion.si.epanechnikov <- c()
deteccion.si.biweight <- c()
deteccion.si.triweight <- c()
deteccion.si.cos <- c()
deteccion.si.inv <- c()
deteccion.si.gaussian <- c()
deteccion.si.optimal <- c()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos) 
  si.rectangular <- 0
  si.triangular <- 0
  si.epanechnikov <- 0
  si.biweight <- 0
  si.triweight <- 0
  si.cos <- 0
  si.inv <- 0
  si.gaussian <- 0
  si.optimal <- 0
  
  
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "rectangular")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.rectangular <- si.rectangular + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "triangular")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.triangular <- si.triangular + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "epanechnikov")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.epanechnikov <- si.epanechnikov + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "biweight")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.biweight <- si.biweight + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "triweight")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.triweight <- si.triweight + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "cos")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.cos <- si.cos + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "inv")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.inv <- si.inv + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "gaussian")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.gaussian <- si.gaussian + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.knn(formula = DejaBanco~., data = taprendizaje, kmax = 3, kernel = "optimal")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.optimal <- si.optimal + MC[2, 2] # Detección DejaBanco
  }
  
  
  deteccion.si.rectangular[i] <- si.rectangular
  deteccion.si.triangular[i] <- si.triangular
  deteccion.si.epanechnikov[i] <- si.epanechnikov
  deteccion.si.biweight[i] <- si.biweight
  deteccion.si.triweight[i] <- si.triweight
  deteccion.si.cos[i] <- si.cos
  deteccion.si.inv[i] <- si.inv
  deteccion.si.gaussian[i] <- si.gaussian
  deteccion.si.optimal[i] <- si.optimal
  

}


resultados <- data.frame("rectangular" = deteccion.si.rectangular,
                         "triangular" = deteccion.si.triangular,
                         "epanechnikov" = deteccion.si.epanechnikov,
                         "biweight" = deteccion.si.biweight,
                         "triweight" = deteccion.si.triweight,
                         "cos" = deteccion.si.optimal,
                         "inv" = deteccion.si.cos,
                         "gaussian" = deteccion.si.inv,
                         "optimal" = deteccion.si.gaussian)

par(oma=c(0, 0, 0, 5)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Detección Deja Banco", 
        xlab = "Número de iteración",
        ylab = "Cantidad de Deja Banco",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda








# e) Repita el ejercicio anterior, pero esta vez en lugar de sumar la cantidad de 1’s, promedie los
# errores globales cometidos en los diferentes grupos (folds). Luego grafique las 5 iteraciones
# para los tres algoritmos en el mismo grafico. ¿Se puede determinar con claridad cual
# algoritmo es el mejor?

numero.filas <- nrow(datos)
cantidad.validacion.cruzada <-5
cantidad.grupos <- 10

deteccion.error.discrete <- c()
deteccion.error.real <- c()
deteccion.error.gentle <- c()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos) 
  error.discrete <- 0
  error.real <- 0
  error.gentle <- 0
  
  
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10 , type = 'discrete')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    error.discrete <- error.discrete + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10, type = 'real')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    error.real <- error.real + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10, type = 'gentle' )
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    error.gentle <- error.gentle + MC[2, 2] # Detección DejaBanco
    
  }
  
  deteccion.error.discrete[i] <- error.discrete
  deteccion.error.real[i] <- error.real
  deteccion.error.gentle[i] <- error.gentle
  
}


resultados <- data.frame("discrete"     = deteccion.si.discrete,
                         "real"     = deteccion.si.real,
                         "gentle" = deteccion.si.gentle) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparacion Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA, cex = 0.8,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda