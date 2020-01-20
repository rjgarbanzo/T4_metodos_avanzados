#Ejercicio 4
# Esta pregunta tambi´en utiliza los datos Datos Churn.csv. Usando el paquete
# traineR realice lo siguiente:

# a) El objetivo de este ejercicio es comparar todos los metodos predictivos vistos en el curso con
# esta tabla de datos. Aquı interesa predecir en la variable DejaBanco que indica si el cliente
# deja sus negocios con el banco (1) o no (0), para esto genere 5 Validaciones Cruzadas con
# 10 grupos para los metodos SVM, KNN, Arboles, Bosques, Potenciacion, eXtreme
# Gradient Boosting, Bayes y Redes Neuronales (con los paquetes vistos en clase), para
# KNN y Potenciacion use los parametros obtenidos en las calibraciones realizadas en los
# ejercicios anteriores. Luego grafique las 5 iteraciones para todos los metodos en el mismo
# grafico. ¿Se puede determinar con claridad cual metodos es el mejor?


library(caret)
library(traineR)

datos <- read.csv("Datos_Churn.csv", header=TRUE, sep=",", dec=".")
datos <- datos[, -c(1,4,2,7)]
datos <- na.omit(datos)

datos$TarjetaCredito <- factor(datos$TarjetaCredito, ordered = T)
datos$Activo <- factor(datos$Activo, ordered = T)
datos$DejaBanco <- factor(datos$DejaBanco, ordered = T)
str(datos)
summary(datos)

numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- 2
cantidad.grupos <- 3

deteccion.no.svm <- c()
deteccion.no.knn <- c()
deteccion.no.bayes <- c()
deteccion.no.arbol <- c()
deteccion.no.bosque <- c()
deteccion.no.potenciacion <- c()
deteccion.no.red <- c()
deteccion.no.xgboost <- c()


# Validación cruzada 5 veces
for (i in 1:cantidad.validacion.cruzada) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos) # Crea los 10 grupos
  no.svm <- 0
  no.knn <- 0
  no.bayes <- 0
  no.arbol <- 0
  no.bosque <- 0
  no.potenciacion <- 0
  no.red <- 0
  no.xg  <- 0

  
  # Este ciclo es el que hace validación cruzada con 10 grupos
  for (k in 1:cantidad.grupos) {
    muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    ttraining <- datos[-muestra, ]
    
    modelo <- train.svm(DejaBanco ~ ., data = ttraining, kernel = "linear", probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.svm <- no.svm + MC[2, 2] # Detección de los No Pagadores
    
    modelo <- train.knn(DejaBanco ~ ., data = ttraining, kmax = 5, kernel = "cos")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.knn <- no.knn + MC[2, 2] # Detección de los No Pagadores
    
    modelo <- train.bayes(DejaBanco ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.bayes <- no.bayes + MC[2, 2] # Detección de los No Pagadores
    
    modelo = train.rpart(DejaBanco ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.arbol <- no.arbol + MC[2, 2] # Detección de los No Pagadores
    
    modelo <- train.randomForest(DejaBanco ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.bosque <- no.bosque + MC[2, 2] # Detección de los No Pagadores
    
    modelo <- train.ada(DejaBanco ~ ., data = ttraining, iter = 10, nu = 1, type = "gentle")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.potenciacion <- no.potenciacion + MC[2, 2] # Detección de los No Pagadores
    
    modelo <- train.nnet(DejaBanco ~ ., data = ttraining, size = 20, MaxNWts = 5000, rang = 0.01, 
                         decay = 5e-4, maxit = 20, trace = TRUE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.red <- no.red + MC[2, 2] # Detección de los No Pagadores
    
    modelo <- train.xgboost(DejaBanco ~ ., data = ttraining, nrounds = 10,
                            print_every_n = 10, maximize = F , eval_metric = "error")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.xg <- no.xg + MC[2, 2] # Detección de los No Pagadores
    

  }
  
  deteccion.no.svm[i] <- no.svm
  deteccion.no.knn[i] <- no.knn
  deteccion.no.bayes[i] <- no.bayes
  deteccion.no.arbol[i] <- no.arbol
  deteccion.no.bosque[i] <- no.bosque
  deteccion.no.potenciacion[i] <- no.potenciacion
  deteccion.no.red[i] <- no.red
  deteccion.no.xgboost[i] <- no.xg

}

resultados <- data.frame("svm" = deteccion.no.svm,
                         "k_vecinos" = deteccion.no.knn,
                         "bayes" = deteccion.no.bayes,
                         "arboles" = deteccion.no.arbol,
                         "bosques" = deteccion.no.bosque,
                         "potenciacion" = deteccion.no.potenciacion,
                         "redes_nnet" = deteccion.no.red,
                         "xgboost" = deteccion.no.xgboost)

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda


matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Detección del SI DejaBanco", 
        xlab = "Número de iteración",
        ylab = "Cantidad de NO pagadores detectados",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda




#Punto B
# Repita el ejercicio anterior, pero en lugar de sumar la cantidad de 1’s, promedie los errores
# globales cometidos en los diferentes grupos (folds). Luego grafique las 5 iteraciones para
# todos los metodos vistos en el curso en el mismo grafico. ¿Se puede determinar con claridad
# cual algoritmo es el mejor?

#datos <- read.csv("Datos_Churn.csv", header=TRUE, sep=",", dec=".")
datos <- read.csv("Datos_Churn.csv", header=TRUE, sep=",", dec=".")
datos <- datos[, -c(1,4,2,7)]
datos <- na.omit(datos)

datos$TarjetaCredito <- factor(datos$TarjetaCredito, ordered = T)
datos$Activo <- factor(datos$Activo, ordered = T)
datos$DejaBanco <- factor(datos$DejaBanco, ordered = T)
str(datos)
summary(datos)

numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- 5
cantidad.grupos <- 5

deteccion.error.svm <- c()
deteccion.error.knn <- c()
deteccion.error.bayes <- c()
deteccion.error.arbol <- c()
deteccion.error.bosque <- c()
deteccion.error.potenciacion <- c()
deteccion.error.red <- c()
deteccion.error.xgboost <- c()


# Validación cruzada 5 veces
for (i in 1:cantidad.validacion.cruzada) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos) # Crea los 10 grupos
  error.svm <- 0
  error.knn <- 0
  error.bayes <- 0
  error.arbol <- 0
  error.bosque <- 0
  error.potenciacion <- 0
  error.red <- 0
  error.xg  <- 0
  error.red.neu <- 0
  error.glm <- 0
  
  # Este ciclo es el que hace validación cruzada con 10 grupos
  for (k in 1:cantidad.grupos) {
    muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    ttraining <- datos[-muestra, ]
    
    modelo <- train.svm(DejaBanco ~ ., data = ttraining, kernel = "linear", probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.svm<-error.svm+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.knn(DejaBanco ~ ., data = ttraining, kmax = 5, kernel = "cos")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.knn <- error.knn+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.bayes(DejaBanco ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.bayes <- error.bayes+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo = train.rpart(DejaBanco ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.arbol <- error.arbol+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.randomForest(DejaBanco ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.bosque <- error.bosque+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.ada(DejaBanco ~ ., data = ttraining, iter = 10, nu = 1, type = "gentle")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.potenciacion <- error.potenciacion+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.nnet(DejaBanco ~ ., data = ttraining, size = 10, MaxNWts = 5000, rang = 0.01, 
                         decay = 5e-4, maxit = 20, trace = TRUE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.red <- error.red+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.xgboost(DejaBanco ~ ., data = ttraining, nrounds = 10,
                            print_every_n = 10, maximize = F , eval_metric = "error")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.xg <- error.xg+(1-(sum(diag(MC)))/sum(MC))*100
    
    
  }
  deteccion.error.svm[i] <- error.svm/cantidad.grupos
  deteccion.error.knn[i] <- error.knn/cantidad.grupos
  deteccion.error.bayes[i] <- error.bayes/cantidad.grupos
  deteccion.error.arbol[i] <- error.arbol/cantidad.grupos
  deteccion.error.bosque[i] <- error.bosque/cantidad.grupos
  deteccion.error.potenciacion[i] <- error.potenciacion/cantidad.grupos
  deteccion.error.red[i] <- error.red/cantidad.grupos
  deteccion.error.xgboost[i] <- error.xg/cantidad.grupos

}

resultados <- data.frame("svm" = deteccion.error.svm,
                         "k_vecinos" = deteccion.error.knn,
                         "bayes" = deteccion.error.bayes,
                         "arboles" = deteccion.error.arbol,
                         "bosques" = deteccion.error.bosque,
                         "potenciacion" = deteccion.error.potenciacion,
                         "redes_nnet" = deteccion.error.red,
                         "xgboost" = deteccion.error.xgboost)

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda
matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA, cex = 0.8,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda



#Punto C
#La comparación de error y el si deja banco permite observar que los métodos de Potenciacion y árboles
#son los mejores para el caso Data Churn




#Punto D
# Encuentre el mejor modelo usando el paquete caret. Para esto compare los metodos SVM,
# KNN, Arboles, Bosques, Potenciacion, eXtreme Gradient Boosting, Bayes y
# Regresion Logıstica. Para esto genere muestras de entrenamiento y testing usando
# la funcion createDataPartition(...), luego genere los modelos usando las funciones
# trainControl(...) y train(...) con validacion cruzada de 10 grupos y usando como criterio
# (metrica de calidad) la precision global (Accuracy). Usando la funcion
# indices.general(...) determine cual es el mejor metodo.

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


library(caret)

datos <- read.csv("Datos_Churn.csv", header=TRUE, sep=",", dec=".")
datos <- datos[, -c(1,4,2,7)]
datos <- na.omit(datos)

datos$TarjetaCredito <- factor(datos$TarjetaCredito, ordered = T)
datos$Activo <- factor(datos$Activo, ordered = T)
datos$DejaBanco <- factor(datos$DejaBanco, ordered = T)
str(datos)
summary(datos)

muestra <- createDataPartition(datos$DejaBanco, p = 0.7, list = F)
# 70% para entrenamiento
tabla.datos.vc <- datos[muestra,]
# 30% para prueba
tabla.validacion <- datos[-muestra,]


# SVM Kernel Radial = “svmRadial”
# k-nearest neighbors = “kknn”
# decision tree = “rpart”
# AdaBoost = “adaboost”
# eXtreme Gradient Boosting = “xgbTree”
# Naive Bayes = “naive_bayes”
# Generalized Linear Model = “glm”

# Validación cruzada con 10 folds
control <- trainControl(method = "cv", number = 2)

svmRadial.model <- train(DejaBanco~., data = tabla.datos.vc, method = "svmRadial",trControl = control, metric = "Accuracy", verbose = FALSE)
kknn.model <- train(DejaBanco~., data = tabla.datos.vc, method = "kknn",trControl = control, metric = "Accuracy", verbose = FALSE)
rpart.model <- train(DejaBanco~., data = tabla.datos.vc, method = "rpart",trControl = control, metric = "Accuracy")
adaboost.model <- train(DejaBanco~., data = tabla.datos.vc, method = "adaboost",trControl = control, metric = "Accuracy", verbose = FALSE)
boosting.model <- train(DejaBanco~., data = tabla.datos.vc, method = "xgbTree",trControl = control, metric = "Accuracy", verbose = FALSE)
naive_bayes.model <- train(DejaBanco~., data = tabla.datos.vc, method = "naive_bayes",trControl = control, metric = "Accuracy", verbose = FALSE)
glm.model <- train(DejaBanco~., data = tabla.datos.vc, method = "glm",trControl = control, metric = "Accuracy")


prediccion.svmRadial <- predict(svmRadial.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.svmRadial)
svmRadial.ind <- indices.general(MC)
svmRadial.ind$`Precisión Global`

prediccion.kknn <- predict(kknn.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.kknn)
kknn.ind <- indices.general(MC)
kknn.ind$`Precisión Global`

prediccion.rpart <- predict(rpart.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.rpart)
rpart.ind <- indices.general(MC)
rpart.ind$`Precisión Global`

prediccion.adaboost <- predict(adaboost.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.adaboost)
adaboost.ind <- indices.general(MC)
adaboost.ind$`Precisión Global`

prediccion.xgbTree <- predict(boosting.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.xgbTree)
boosting.ind <- indices.general(MC)
boosting.ind$`Precisión Global`

prediccion.naive_bayes <- predict(naive_bayes.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.naive_bayes)
naive_bayes.ind <- indices.general(MC)
naive_bayes.ind$`Precisión Global`

prediccion.glm <- predict(glm.model, tabla.validacion[,-9])
MC <- table(tabla.validacion$DejaBanco,prediccion.glm)
glm.ind <- indices.general(MC)
glm.ind$`Precisión Global`


# Según la precision gloabal el mejor método obtenido es el de boosting con una precision global del 86%
















