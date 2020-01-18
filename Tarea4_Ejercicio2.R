#Ejercicio 2
# A 
# Cargue la tabla de datos Datos Churn.csv en R y ejecute un str(...), summary(...)
# y un dim(...), verifique la correcta lectura de los datos, recuerde que IdCliente es el
# identificador de fila.

datos <- read.csv("Datos_Churn.csv",header=TRUE, sep=",", dec=".")
str(datos)
summary(datos)
dim(datos)


#b) Elimine las variables de valor ´unico, es decir, las variables Apellido y IdCuenta
datos <- datos[, -c(1,4,2,7)]
datos <- na.omit(datos)
str(datos)


# C) Asegurese re-codificar las variables que sean cualitativas y que esten codificadas en el
# archivo con numeros; lo cual causa que R interprete incorrectamente el tipo de la variable,
# es decir, las variables TarjetaCredito, Activo y DejaBanco.

datos$TarjetaCredito <- factor(datos$TarjetaCredito, ordered = T)
datos$Activo <- factor(datos$Activo, ordered = T)
datos$DejaBanco <- factor(datos$DejaBanco, ordered = T)
str(datos)


# D) El objetivo de este ejercicio es calibrar el metodo de ADA para esta Tabla de Datos. Aqui
# interesa predecir en la variable DejaBanco que indica si el cliente deja sus negocios con
# el banco (1) o no (0). Para esto genere 5 Validaciones Cruzadas con 10 grupos calibrando
# el modelo de acuerdo con los tres tipos de algoritmos que permite, discrete, real
# y gentle. Para medir la calidad de metodo sume la cantidad de 1’s detectados en los
# diferentes grupos. Luego grafique las 5 iteraciones para los tres algoritmos en el mismo
# grafico. ¿Se puede determinar con claridad cual algoritmo es el mejor? Para generar los
# modelos predictivos use el paquete traineR.


summary(datos)
numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- 5
cantidad.grupos <- 10

deteccion.si.discrete <- c()
deteccion.si.real <- c()
deteccion.si.gentle <- c()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos) 
  si.discrete <- 0
  si.real <- 0
  si.gentle <- 0
  
  
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10 , type = 'discrete')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.discrete <- si.discrete + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10, type = 'real')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.real <- si.real + MC[2, 2] # Detección DejaBanco
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10, type = 'gentle' )
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    si.gentle <- si.gentle + MC[2, 2] # Detección DejaBanco
    
  }
  
  deteccion.si.discrete[i] <- si.discrete
  deteccion.si.real[i] <- si.real
  deteccion.si.gentle[i] <- si.gentle
  
}


resultados <- data.frame("discrete"     = deteccion.si.discrete,
                         "real"     = deteccion.si.real,
                         "gentle" = deteccion.si.gentle) # Preparamos los datos

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









# e) Repita el ejercicio anterior, pero esta vez en lugar de sumar la cantidad de 1’s, promedie los
# errores globales cometidos en los diferentes grupos (folds). Luego grafique las 5 iteraciones
# para los tres algoritmos en el mismo grafico. ¿Se puede determinar con claridad cual
# algoritmo es el mejor?

#calculo de indices
indices.general <- function(MC) {
  precision.global <- sum(diag(MC))/sum(MC)
  error.global <- 1 - precision.global
  precision.categoria <- diag(MC)/rowSums(MC)
  res <- list(matriz.confusion = MC, precision.global = precision.global, error.global = error.global,
              precision.categoria = precision.categoria)
  names(res) <- c("Matriz de Confusión", "Precisión Global", "Error Global",
                  "Precisión por categoría")
  return(res)
}


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
    Error.Global <- indices.general(MC)
    error.discrete <- error.discrete + Error.Global$`Error Global` # Detección DejaBanco
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10, type = 'real')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    Error.Global <- indices.general(MC)
    error.real <- error.real + Error.Global$`Error Global` # Detección DejaBanco
    
    modelo <- train.ada(formula = DejaBanco ~ ., data = taprendizaje, iter = 10, type = 'gentle' )
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    Error.Global <- indices.general(MC)
    error.gentle <- error.gentle + Error.Global$`Error Global` # Detección DejaBanco
    
  }
  
  error.discrete <- error.discrete/10
  error.real <- error.real/10
  error.gentle <- error.gentle/10
  
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






