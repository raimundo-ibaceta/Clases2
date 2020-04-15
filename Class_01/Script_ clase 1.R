# Introducción a la Programación con R
# Autor: Esteban López Ochoa
# Institución: Universidad Adolfo Ibáñez

#------ Parte 1: Explorando R ------

print('Hola mundo!, Mamá mírame!, estoy programando en R,...') ### función print que se encarga de imprimir en console lo que está entre corchetes redondos

1+1 ### sumando dos números enteros.

pi ### valor predeterminado en R, por lo que me dará 3,1416

demo('graphics') ### Función demo: Muestra las distintas variedades en que R puede hacer gráficos.

install.packages("leaflet") ### esta función me permite instalar los elementos de lealflet en R.
library(leaflet) ### la función library me permite obtener todos los datos en lealflet
leaflet::leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-71.530294, lat=-33.019305,  popup="Aquí Estamos!")

#------ Parte 2: Creando un objeto ------

dosmasdos <- 2 + 2 ### estoy creando un objeto de nombre dosmasdos que me sume los enteros 2 y 2.
dos_mas_dos <- 2 + 2 ### estoy creando un objeto que me sume los enteros dos y dos
dos.mas.dos <- 2 + 2 ### estoy creando un objeto que me sume los enteros 2 y 2.
sumita <- 3+4 ### estoy creando un obejto que me entregue la suma de 3 más 4

#------ Parte 2: Funciones, manipulando un objeto ------

dosmasdos ### estoy pidiendo que me de el resultado del objeto creado anteriormente 

dosmasdos*dosmasdos ### estoy pidiendo que multiplique al objeto dosmasdos por si mismo.


#¿Qué clase de objeto es 'dosmasdos'?

class(dosmasdos) ### la función class me entregará el tipo de objeto que es dosmasdos

#¿Qué puedo hacer con el objeto 'dosmasdos'?
sum(dos_mas_dos,dosmasdos,3) ### con la función sum, se están sumando elementos y objetos creados.

sum(c(dos_mas_dos,dosmasdos,3)) ### mediante la función sum, en esta línea se están sumando elementos concatenando objetos.

#------ Parte 4: Tipos de Objetos ------

a<-1 ### se está definiendo al obejto "a" como un -1.

b<-"Muchachita muchachita la peineta..." ### se está definiendo al obejto "b" como una str.

l1<-list(a,b) ## se está generando una lista con los obejtos a y b ya creados, mediante la función list.

m1<-matrix(0,2,2) ### se está generando una matriz compuesta por ceros de 2 filas y 2 columnas, mediante la función matrix

sq1<-seq(1,10,1) ### se está generando una secuencia con numeros del 1 al 10, de uno en uno mediante una función seq

sq2<-LETTERS[sq1] ### se está generando un secuencia con las primeras 10 letras de abecedario.

df1<- data.frame(sq1,sq1) ### se está creando un objeto (df1) mediante la función data.frame que compone al elemento sq1.

caja<-array(data = 0,dim = c(2,2,3)) ### creo el objeto caja mediante la función array, lo cual me devuelve en este caso 3 matrices con dimension de 2x2.



#------ Parte 5: Indexación de Objetos ------

A<-c(1836457,2) ### creando un objeto A con dos elementos.

A ### llamando al obejto "A" creado previamente.
A[1] ###llamando al primer elemento del objeto "A".
A[2] ### llamando al segundo elemenro del obejto "A".
A[-1] ### llamando al último elemento del obejto "A"
A[1:2] ### llamando a los elementos posicionados del 1 al 2 del objeto A.

notas<-rnorm(100,5,1.8) ### generando un objeto "notas" mediante una función rnorm de 100 elementos, que tenga media 5 y varianza de 1.8.

notas[1:5] ### llamando a los elementos posicionados del 1 al 5 del objeto notas.
notas>4 ### "preguntando" cuales son  las notas mayores a 4 (True or False).

notas[notas>4] ### pidiendo que deje las notas solo mayores a 4.

#------ Parte 5: Manipulación de Objetos ------

class(notas) ### la función class me dará el tipo de variable son las notas.
class(m1) ###que tipo de variable es el objeto m1.

length(notas) ### la cantidad de elementos que se encuentran en el objeto notas.
length(m1) ### la cantidad de elementos que se encuentran dentro del obejto(matriz) m1.

dim(m1) ### le pido, mediante la función dim, la dimensión de la matriz m1, en este caso es de 2x2.
dim(df1) ### le pido, mediante la función dim, la dimension de la data creada anteriormente.
dim(caja) ### le pido, mediante la función dim, la dimensión de la caja creada anteriormente.

names(df1) ### le pido, mediante la función names, los nombres de los obejtos en df1.

rm(m1) ### la función rm, se encargará de remover los elementos que se encuentren en el obejto m1.

ls() ### mediante la función ls, le pido que me muestre todos los obejtos creados en environment.



#------ Parte 6: Paquetes ------
#https://www.youtube.com/watch?v=6AOpomu9V6Q

install.packages("leaflet") # instalar 

library(leaflet) # cargar

#usar
leaflet::leaflet() %>% ### función que importa a R y ver distintos mapas dinámicos.
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-71.530294, lat=-33.019305,  popup="Aquí Estamos!") ### la función addmarkers me permite incertar coordenadas para que me muestre un punto en el mapa en específico.


library(foreign) ### la función library me permite cargar aquellos datos que se encuentran en foreing.  


#------ Parte 6: Ayuda ------

?sum ### pido ayuda a R, para saber que significa la función sum. 
help('sum') ### pido ayuda a R, para saber que significa sum.

help(package='foreign') ### pido ayuda a R, mediante la función help, sobre las paginas de ayuda.

??regression ### los dos signos implican que estoy pidiendo ayuda respecto al funcionamiento de R.

#------ Parte 7: Practica ------

install.packages("swirl") # instalar 

library(swirl) # cargar

