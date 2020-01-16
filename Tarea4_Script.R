#Eje 1
#A

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



