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
datos <- datos[, -c(1,2,7)]
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










