###Class 06 - Spatial Statistics 1###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


# Spatial randomness

install.packages('plot.matrix')
library(plot.matrix)
library(classInt)

a<-rnorm(100,0,1) ### cree una variable aleatoria.
A<-matrix(a,10,10) ### La puse en una matriz.
b_A<-classIntervals(a,n = 5,style = 'jenks') ### genero los intervalos por el que quiero que se plotee.
plot(A, breaks = b_A$brks, key=NULL) ### ploteo la variable.

b<-a
B<-matrix(c(sample(b,15),sort(sample(b,25)),sample(b,15),sort(sample(b,15)),sample(b,10)),10,10)
#B<-matrix(sort(b),10,10)
b_B<-classIntervals(b,n = 5,style = 'jenks')
plot(B, breaks = b_B$brks, key=NULL)

### La diferencia con la anterior es que esta se ve un poco más concentrada, en los extremos están las partes más
### oscuras.


#Covid
library(data.table)

archivos<-dir(path = "Class_06/producto2/") ### la función dir es para que me diga que hay en esa carpeta.
COVID<-fread(input =paste0("Class_06/producto2/",archivos[1])) ### genero un primer obejto que me lea el archivo 1, casos confirmados para el 30 de marzo. 
names(COVID)[6]<-paste0("Confirmados_",substr(archivos[1],start = 1,stop = 10))

for(i in 2:length(archivos)){
  aa<-fread(input =paste0("Class_06/producto2/",archivos[i])) ### genere un objeto aa que sea la lectura de i, que irá de 2 hasta 9
  aa<-aa[,.(`Codigo comuna`,`Casos Confirmados`)] ### lo leerá y lo guardará en aa.
  names(aa)[2]<-paste0("Confirmados_",substr(archivos[i],start = 1,stop = 10))
  COVID<-merge(COVID,aa,by="Codigo comuna",all.x=T,sort=F)
}
View(COVID) ### loop que me da los casos confirmados de cada región por fecha respectiva. UTIL.

COVID[is.na(`Confirmados_2020-03-30`),`Confirmados_2020-03-30`:=0] ### que me reemplace los na por un 0.

library(ggplot2)
ggplot(COVID,aes(x=`Confirmados_2020-03-30`,y=`Confirmados_2020-04-17`))+geom_point()+geom_smooth(method = lm)

ggplot(COVID,aes(x=`Confirmados_2020-04-10`,y=`Confirmados_2020-04-17`))+geom_point()+geom_smooth(method = lm)

ggplot(COVID,aes(x=`Confirmados_2020-04-15`,y=`Confirmados_2020-04-17`))+geom_point()+geom_smooth(method = lm)


#---- Intro 2 Spatial Autocorrelation  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

comunas_rm<-mapa_comunas[mapa_comunas$codigo_region==13,]

comunas_rm<-merge(x = comunas_rm,y = COVID[`Codigo region`==13,],by.x="codigo_comuna",by.y="Codigo comuna",all.x=TRUE,sort=F)

comunas_rm<-as_Spatial(comunas_rm)

install.packages("spdep")
library(spdep)

nbs<-poly2nb(comunas_rm,queen = T)

w_rm<-nb2listw(nbs,style = "W")

plot(comunas_rm)
plot(nbs,coordinates(comunas_rm),add=T,col='blue',pch=".")

sl<-lag.listw(w_rm,comunas_rm$Confirmados_2020.04.17)

plot(comunas_rm$Confirmados_2020.04.17,sl)


# Optional Reading
# https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
