#Eje 1

# Punto A
# Variando k desde 1 hasta 20, para estos datos determine usando el Codo de Jambu el
# mejor valor para k en el m´etodo k−medias. Utilice nstart=5 y iter.max = 2000, ¿que
# significan estos parametros? ¿Por qu´e es importante usar estos valores?

datos <- read.csv("DJTable.csv",header=TRUE, sep=";", dec=".", row.names=1)

#Declaracion
InerciaIC.Hartigan<-rep(0,20)
InerciaIC.Lloyd<-rep(0,20)
InerciaIC.Forgy<-rep(0,20)
InerciaIC.MacQueen<-rep(0,20)

#Variacion K
for(k in 1:20) {
  grupos<-kmeans(datos,k,iter.max=2000,nstart=5,algorithm = "Hartigan-Wong")
  InerciaIC.Hartigan[k]<-grupos$tot.withinss
  grupos<-kmeans(datos,k,iter.max=2000,nstart=5,algorithm = "Lloyd")
  InerciaIC.Lloyd[k]<-grupos$tot.withinss
  grupos<-kmeans(datos,k,iter.max=2000,nstart=5,algorithm = "Forgy")
  InerciaIC.Forgy[k]<-grupos$tot.withinss
  grupos<-kmeans(datos,k,iter.max=2000,nstart=5,algorithm = "MacQueen")
  InerciaIC.MacQueen[k]<-grupos$tot.withinss
}


plot(InerciaIC.Hartigan,col="blue",type="b")
points(InerciaIC.Lloyd,col="red",type="b")
points(InerciaIC.Forgy,col="green",type="b")
points(InerciaIC.MacQueen,col="magenta",type="b")
legend("topright",legend = c("Hartigan","Lloyd","Forgy","MacQueen"), 
       col = c("blue", "red","green","magenta"), lty = 1, lwd = 1)

# R/
# Según el gráfico los algortimos se estabilizan en K=4
# con estos parametros se obtendran 5 puntos de inicio y 2000 repeticiones de asginacion de cluster
# Es importante asignar un nstart bajo ya que de lo contrario se obtienen clusters vacios 


# Punto B

# Usando k = 3, nstart=5 y iter.max = 2000 en el metodo k−medias determine cual
# de los algoritmos “Hartigan-Wong”, “Lloyd”, “Forgy” y “MacQueen”funciona mejor para
# estos datos en el sentido de que minimizan la inercia intra–clases.


#Declaracion
Hartigan<-0
Lloyd<-0
Forgy<-0
MacQueen<-0
# Re calculo de inercias
for(i in 1:50) {
  grupos<-kmeans(datos,3,iter.max=2000,nstart=5,algorithm = "Hartigan-Wong")
  Hartigan<-Hartigan+grupos$tot.withinss
  grupos<-kmeans(datos,3,iter.max=2000,nstart=5,algorithm = "Lloyd")
  Lloyd<-Lloyd+grupos$tot.withinss
  grupos<-kmeans(datos,3,iter.max=2000,nstart=5,algorithm = "Forgy")
  Forgy<-Forgy+grupos$tot.withinss
  grupos<-kmeans(datos,3,iter.max=2000,nstart=5,algorithm = "MacQueen")
  MacQueen<-MacQueen+grupos$tot.withinss
}  
Hartigan/50
Lloyd/50
Forgy/50
MacQueen/50


# MacQueen presentan la menor inercia intra clases.





# Punto C

# Repita el ejercicio anterior usando k = 3, nstart=1 y iter.max = 20. ¿Que se observa?
# ¿A que se debe la diferencia? ¿Cual resultado es mas creıble el del punto b) o el del punto c)?

#Declaracion
Hartigan<-0
Lloyd<-0
Forgy<-0
MacQueen<-0
# Re calculo de inercias
for(i in 1:50) {
  grupos<-kmeans(datos,3,iter.max=20,nstart=1,algorithm = "Hartigan-Wong")
  Hartigan<-Hartigan+grupos$tot.withinss
  grupos<-kmeans(datos,3,iter.max=20,nstart=1,algorithm = "Lloyd")
  Lloyd<-Lloyd+grupos$tot.withinss
  grupos<-kmeans(datos,3,iter.max=20,nstart=1,algorithm = "Forgy")
  Forgy<-Forgy+grupos$tot.withinss
  grupos<-kmeans(datos,3,iter.max=20,nstart=1,algorithm = "MacQueen")
  MacQueen<-MacQueen+grupos$tot.withinss
}  
Hartigan/50
Lloyd/50
Forgy/50
MacQueen/50


# Se da una disminucion de la inercia intra clase al existir mas componentes dentro de cada cluster
# Tomando en cuenta que apartir de K=4 se estabiliza el tot.withinss sería mas confiable el punto B