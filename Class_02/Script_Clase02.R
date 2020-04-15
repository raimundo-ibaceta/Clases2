##########################################
## Class 02: Review and  Data Management
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez
##########################################

#---- Part 1: Review  -------------------

#Estas son las cosas que me gustaría que les queden bien claras

### 1. Sintaxis básica

# Creación de Objetos

x<-NULL
y<-c(TRUE,FALSE) ### Elementos logicos, True y false
as.numeric(y) ### aquí al True le puse 1 y al False le puse 0.

A<-1
years<-2010:2020 ### 
year<- seq(2010,2020,by = 0.5)
tiktoc<-c("Que", "linda", "te ves", "limpiando", "Esperancita",4)

paste("Hola","Mundo",sep=" ")

paste(tiktoc,collapse = " ")

obj2<- as.numeric(c(1,2,3,4,"Esperancita"))
is.na(obj2) ### Si es que hay un na. en objeto 2


numeros_en_texto<-c("1","2","3")
as.numeric(numeros_en_texto)

m1<-matrix(1:4,2,2)
m1%*%t(m1)
diag(m1)
solve(m1)


a1<-array(1:12,dim = c(2,2,4)) ### un array tiene filas, columnas y profundidad.

d1<-data.frame(m1)
data("quakes") # promise, promesa de que usaré esos datos
d1<-data.frame(quakes)

ls()

l1<-list(Perrito=A,years,tiktoc,m1)
A<-3L

# Manipulación de Objetos
ls()

A<-1L

class(A)
typeof(A) ### los elementos del objeto de que tipo son (entero, float, etc...)

length(years)
dim(m1)

object.size(d1)

names(d1)
head(d1)
tail(d1)

rm(A)

#Bonus: como se borra todo?
rm(list=ls())

# Indexación uso de los []

length(years)
years[11]

dim(m1)
m1[1,2] ### cuando indexo una matriz me da el elemento de la fila 1 y la columna 2.

dim(a1)
class(a1)
a1[2,1,3] ### me dará el elemento ubicado en la fila 2 con la columna 1 de la 3era dimensión del array.

l1[2]
l1[[2]][1:2] ### es una lista, me dará el segundo elemento, que es la secuencia year, 

l1[[2]][3:5]

l1$Perrito

d1[1,]### me dará la primera fila con los valores de todas las columnas.
d1[,1] ### me dará los valores que se encuentran en la columna 1 de todas las filas
d1[,'lat'] ### me dará todos los valores que se encuentran en latitud.
d1$mag[seq(1,16,2)] ### me dará los valores que se encuentran en mag, del 1 al 16 pero de 2 en 2.
d1$lat[1:4] ### me dará los primeros 4 valores de de lat.
d1$lat ### me dará todos los valores de lat.


d1[,'lat']
d1[1:4,c('lat','long')] ### me dará los valores del 1 al 4 de las series de lat y long.

d1$mag>5 ### me muestra con un true or false los valores que son mayores que 5
table(d1$mag>5) ### me cuenta los true y los false sacados anteriormente.
d1[d1$mag>6,'stations'] ### me muestra las estaciones en cuya maginitud fue mayor que 6.
d1[d1$mag>6,]
d1$mag>5
table(d1$mag>5)
d1[d1$mag>5,]

d1$dummy_5up<-as.numeric(d1$mag>5)
table(d1$dummy_5up)
head(d1)

# Distinguir entre funciones, objetos, números y sintaxis básica
# Funciones: palabra + () con argumentos separados por commas
# Objetos: palabras a la izquierda del signo <- 


#---- Part 2: Loops  -------------------

A<-2 ### estoy reasignando el valor del objeto A.

if(A==1){ ### la función if implica que, si A es igual a 1.
  print("A es un objeto con un elemento numérico 1") ### imprime lo que está entre ""
} else { ### en caso que el objeto no sea igual a 1
  print("A no es igual a 1, pero no se preocupe que lo hacemos")### imprime lo que está entre ""
  A<-1L ### si se cumple el "else", se reasigna el valor del objeto A.
}

A<-1 ### reasigno el valor del objeto A a 1.
class(A) ### le estoy pidiendo que me de la clase del objeto A.
typeof(A) ### estoy pidiendo que indique de que tipo son los elementos en el objeto A, en este caso será "double" ya que es un decimal y no un entero.

dim(A) ### le estoy pidiendo que me de la dimension de A, en este caso es un objeto de 1 dimesión por lo que será Null.
length(A)### al ser de 1 dimensión, si puedo sacar el largo en este caso, será 1.

# For loop

for(i in 1:5){ ### para un "i" que recorra del 1 hasta el 5.
  print(paste("Me le declaro a la ", i)) ### imprime "me declaro a la", el valor respectivo de i.
  Sys.sleep(2)### le estoy pidiendo que antes de seguir leyendo hacia abajo espere 2 segundos.
  print("no mejor no... fail!")### imprime "no mejor no.... fail!" posterior a la espera de 2 segundos.
  Sys.sleep(1) ### espere un segundo y que vuelva al inicio del ciclo for.
}

i<-1 ### siendo el objeto i igual a 1
eps<-50/(i^2)### siendo el objeto eps la operación de 50 dividido en el valor respectivo de i al cuadrado.
while(eps>0.001){ ### mientras eps sea mayor que 0.001.
  eps<-50/(i^2)### que al eps se le aplique esta formula.
  print(paste("eps value es still..", eps)) ### imprime "eps value..., el valor que tome eps.
  i<-i+1 ### psoteriormente al obejto i sumale 1.
}

#---- Part 3: Data Management ----
# Tres formas de trabajar con datos

### 1. R-Base 
#http://github.com/rstudio/cheatsheets/raw/master/base-r.pdf

quakes[quakes$mag>6,"mag"] ### en este caso se me pide los datos dentro de quakes que sean mayores que 6, dará como resultado 6.1 y 6.4

by(data = quakes$mag,INDICES = quakes$stations,FUN = mean)###la función by, me dice que para las magnitudes que se encuentren en stations (para todas las estaciones), me saque el promedio.
tapply(X = quakes$mag,INDEX = quakes$stations, FUN = mean)### parecido a lo anterior, solo que lo entrega de otra manera, entrega el promedio por cada vector.

### 2. tydiverse 
#https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
library(tidyverse)
#Cómo se instala el paquete si no lo tengo? Tank!!! ayudaaaa!
install.packages("tidyverse")### primero se debe instalar el paquete tydiverse.

quakes %>% ### operador py, estoy concatenando acciones que son jerárquicas.
  filter(mag>6) %>% ###filtro al objeto "quakes" para que la magnitud sea mayor que 6.
  select(mag) ### y lo vuelvo a filtrar para que me seleccione la magnitud.

quakes %>% ### comienzo del objeto "quakes"
  group_by(stations) %>% ### le pido que de los valores que se encuentren en estación
  summarise(mean(mag)) ### me calcule el promedio de las magnitudes por cada estación.


### 3. data.table (recommended in this course)
library(data.table)
#https://github.com/rstudio/cheatsheets/raw/master/datatable.pdf
install.packages("data.table")

quakes<-data.table(quakes)### convierto el objeto quakes a data.table.


quakes[quakes$mag>6,'mag'] ### le pido que me busque en quakes las mag>6 (TREU OR FALSE), para que despues me de cuales cumplen esa condición.

quakes[mag>6,mag] ### mas simple que lo anterior, llamo a la variable mag>6, luego me creará una lista con las magnitudes mayores que 6.

quakes[,mean(mag),by=.(stations)] ### pido el promedio de la magnitud por estación.

### Reading data from a file

library(readxl) ### la función library se encarga de cargar y adjuntar paquetes adicionales.


casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE) ### al objeto casos, se le está convirtiendo a un data.table, en cuya función se le pide leer un excel.

casos<-casos[Región=="Metropolitana",] ### estoy pidiendo los casos de la data anteriror que se encuentren en la región metropolitana.



library(ggplot2) ### estoy cargando y adjuntando el paquete ggplot2.

ggplot(casos[order(Edad,decreasing = T)],)+geom_bar(stat = 'identity' ,aes(x=`Centro de salud`, y=Edad/Edad, group=Sexo, fill=Edad)) + coord_flip()+ facet_wrap(~Sexo) 

casos[Sexo=="Fememino",Sexo:="Femenino"] ### esoy pidiendo la ubicación de los casos femenino y masculino.

ggplot(casos[order(Edad,decreasing = T),])+geom_bar(stat = 'identity',aes(x=`Centro de salud` ,y=Edad/Edad,fill=Edad)) + coord_flip()+ facet_wrap(~Sexo) +labs(title = "Casos Confirmados por Sexo y Establecimiento",subtitle = "Región Metropolitana - 2020-03-17",caption = "Fuente: https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/")

