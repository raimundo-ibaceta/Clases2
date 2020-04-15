###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)
library(data.table)


casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE) ### cargue los datos y los transformé en data.table

names(casos) ### nombres de las variables del data.table casos COVID.
casos_region_metropolitana <- casos[Región=="Metropolitana",]### que me de todos los datos de la región metropolitana.
casos[,table(Región)] ### me da cuantos casos hay por región


casos_region_metropolitana[,table(Sexo)] ### me da cuantos casos hay por sexo en la región metropolitana.

casos[,table(`Centro de salud`)]


saveRDS(casos,"Class_03/casosRM.rds")

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8')

writexl::write_xlsx(casos,path = "Class_03/CasosenExcel.xlsx") ### si es que quiero escribir un excel

library(foreign)

write.dta



casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casos_region_metropolitana[,table(Sexo)]
casos_region_metropolitana[Sexo=="Fememino",Sexo:="Femenino"]

casos_region_metropolitana[`Centro de salud`=="Clínica Alemana",`Centro de salud`:="Clinica Alemana"]
casos_region_metropolitana[,.N,by=.(`Centro de salud`)] ### casos de acuerdo al centro de salud en la rm.

# Creating (factor) variables

class(casos_region_metropolitana$Sexo)

casos_region_metropolitana[,Sexo:=factor(Sexo)] ### reemplazpo sexo con factor de sexo

head(casos_region_metropolitana$Sexo) ### los niveles que hay son fememino y masculino.
head(as.numeric(casos_region_metropolitana$Sexo))

levels(casos_region_metropolitana$Sexo)

table(casos_region_metropolitana$Sexo)
casos_region_metropolitana[,.N,by=.(Sexo)]
casos_region_metropolitana[,.N,by=.(Sexo,`Centro de salud`)]### me entregará una lista de acuerdo a los casos de covid por sexo y centro de salud.

#Collapsing by Centro de Salud 

names(casos_region_metropolitana)
obj1<-casos_region_metropolitana[,.N,by=.(`Centro de salud`)]### lista de los casos de la región metropolitana.
obj1



obj1[,sum(N,na.rm = T)] ### suma de los casos que hay en la region metropolitana.

obj1[,porc:=N/sum(N,na.rm = T)] ## me está sacando el porcentaje de la suma anterior por centro de salud

obj1

# collapsing (colapsar) by average age


A<-casos_region_metropolitana[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)]### media de edad por centro de salud

B<-casos_region_metropolitana[,.(Total_centro=.N),by=.(`Centro de salud`)]

C<-casos_region_metropolitana[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)]

D<-casos_region_metropolitana[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)]

dim(A)
dim(B)
dim(C)
dim(D)


#merging data sets


AB<-merge(A,B,by = "Centro de salud",all = T,sort = F)
AB



ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)

ABC


ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)

ABCD

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro] ### DE ABCD quiero que me calcule tmbn el procentaje de mujeres.

ABCD

ABCD[, porc_hombres:=Total_Centro_Hombres/Total_centro] ### Acá le agregue el % de los hombres.

ABCD


# reshaping

E<-casos_region_metropolitana[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)]

E

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud')

G

#---- Part 2: Visualization  -------------------

#Scatter plot
  #Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`)
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5)

#ggplot2
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)
p1

#plotly
library(plotly)
ggplotly(p1)

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",]

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),]

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


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
