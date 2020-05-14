###Class 04 - Visualization & Intro to Mapping ###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)
library(data.table)



casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)
a <- read
names(casos)
casos<-casos[Región=="Metropolitana",] ### defino casos como los de la región metropolitana.

saveRDS(casos,"Class_03/casosRM.rds")

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8')

writexl::write_xlsx(casos,path = "Class_03/CasosenExcel.xlsx")

library(foreign)

#write.dta



casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casosRM[,table(Sexo)] ### table me cuenta los casos masculinos y femeninos, recordar!!
casosRM[Sexo=="Fememino",Sexo:="Femenino"]

casosRM[`Centro de salud`=="Clínica Alemana",`Centro de salud`:="Clinica Alemana"]
casosRM[,.N,by=.(`Centro de salud`)]

# Creating (factor) variables

class(casosRM$Sexo)

casosRM[,Sexo:=factor(Sexo)]

head(casosRM$Sexo)
head(as.numeric(casosRM$Sexo))

table(casosRM$Sexo)
table(casosRM$`Centro de salud`)
casosRM[,.N,by=.(Sexo)]
casosRM[,.N,by=.(Sexo,`Centro de salud`)] ### me ordena en una lista los casos femeninos y masculinos por centro de salud.

#Collapsing by Centro de Salud 

names(casosRM)
obj1<-casosRM[,.N,by=.(`Centro de salud`)] ### quiero una lista con la cantidad de casos por centro de salud.


obj1[,sum(N,na.rm = T)] ### Suma de todos los casos en la región metropolitana.

obj1[,porc:=N/sum(N,na.rm = T)] ### agrego una columna con el procentaje en cada centro de salud, esto se llamaba LAG.

obj1

# collapsing (colapsar) by average age


A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)] ### promedio de edad de contagiados por cada centro de salud d

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)] ### casos totales por cada centro de salud.

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)] ### Casos confirmados solo de mujeres por centro de salud.

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)] ### Casos confirmados de hombres por centro de salud.

dim(A)
dim(B)
dim(C)
dim(D)


#merging data sets


AB<-merge(A,B,by = "Centro de salud",all = T,sort = F)### Mezclo ambas datas creadas, las de promedio de edad y cantidad de contagiados.


ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F) ### mezcla total de mujeres, hombres y promedio de edad.

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro] ### agrego una columna que me dará el procentaje de mujeres contagiadas.
ABCD[,porc_hombres:= Total_Centro_Hombres/Total_centro]

# reshaping

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)]

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud') ### reestructuración de la base de datos.

#---- Part 2: Visualization  -------------------

#Scatter plot
#Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`) ### grafico de dipsersion con eje x casos feminimos y eje y casos masculinos.
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5) ### le agrego un texto en que se muestra al centro de salud en ves de los puntos, para poder "ver" cual tiene más casos en el grafico.

#ggplot2
library(ggplot2)

ggplot(data=E,mapping = aes(x = AvAge, y = `Casos confirmados`)) + geom_point() ### gráfico típico, primero pongo la data, después los ejes y el tipo de gráfico que quiero.
names(E)
ggplot(data = E,mapping = aes(x = AvAge,y=`Casos confirmados`)) + geom_point()


ggplot(data = G,mapping = aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point() ### gráfico con los casos confirmados femenino y masculino.

p1<- ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)

p1

ggplot(data = E,mapping = aes(x=AvAge,y=`Casos confirmados`))+geom_point()+facet_wrap(~Sexo)+geom_smooth(method = 'lm',se=F) + geom_smooth(method = 'loess',col='red',se=F)


#plotly
install.packages('plotly')
library(plotly)
ggplotly(p1) ### modo más interactivo, puedo poner el mouse encima y ver los números directamente.

#histograms

ggplot(casos,aes(x=Edad))+geom_histogram() ### lo unico que cambia es el geom_histogram.
ggplot(E,aes(x=AvAge))+geom_histogram()

# Kernel Densities

ggplot(E,aes(x=AvAge))+geom_density()
ggplot(E,aes(x=AvAge,group=Sexo))+geom_density()
ggplot(E,aes(x=AvAge,group=Sexo,colour=Sexo))+geom_density() ### los diferencio por color.
ggplot(E,aes(x=AvAge,group=Sexo,colour=Sexo))+geom_density()+facet_wrap(~Sexo)### los veo separados


#looking at the whole country
casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

ggplot(casos,aes(x=Edad,group=Sexo,fill=Sexo))+geom_histogram()+facet_wrap(~factor(Región))

#como sacamos el "fememino"?


ggplot(casos,aes(x=Edad,group=Sexo,fill=Sexo))+geom_histogram()


#high charter
# http://jkunst.com/highcharter/index.html

#https://chilecracia.org 

#---- Part 3: Intro to Mapping (Shapefile) -------------------
install.packages("chilemapas")
install.packages("rgdal")
library(rgdal)
library(sp)
library(chilemapas)
library(data.table)



# 3.1 Shapefiles as in the `sp` package
help(package='sp')
View(ogrDrivers())

comunas_rm<-readOGR("Class_04/ComunasR13/COMUNA_C17.shp")
class(comunas_rm)

comunas_rm@proj4string ### aca estoy pidiendo la proyección, medición que se hace

comunas_rm@proj4string

View(comunas_rm@data) ### veo las comunas de la región metropolitana.
plot(comunas_rm) ### se plotean las comunas de santiago.

coordinates(comunas_rm) ### función que permite ver las coordenadas latitud y longitud, son los centroides.

centroids_rm<-SpatialPoints(coordinates(comunas_rm),proj4string = comunas_rm@proj4string)
plot(comunas_rm)
plot(centroids_rm,add=T,col='red',lty=1,pch=21,cex=0.1)
lines(coordinates(comunas_rm),col='blue')


str(comunas_rm@data)

# 3.2 Shapefiles as in the `sf` package

help(package = "chilemapas") ### IMPORTANTE, con este help puedo viualizar que es lo que realmente quiero plotear.

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)### Creo un data table de lo que quiero del paquete.
head(zonas_censo) ### edad y población.
table(zonas_censo$edad) ### encuentro la categoría que quiero.


poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]
head(poblacion_adulto_mayor_zonas) ### divido o más bien, filtro la data table creada de acuerdo a lo que me interesa, en este caso adultos mayores.



zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",] ### filtro por la región de valpo.

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)### le meto aquí el geocódigo.

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109","05103"),] ### filtro solo las comunas que quiero de valparaíso.

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


library(ggplot2)
ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)

# creating a fake spatial distribution of adult population in space
zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")
