# Tarea 2: Lab 02 Data Management en R

#instalar y cargar el paquete "car"
install.packages("car")
help(package="car")
library(car)
library(data.table)

#cargar los datos preinstalados "Chile"
data("Chile")
?Chile
Chile
plebicito<-Chile

plot(Chile)

# 0. ¿De qué clase es el objeto Chile?
class(Chile) ### en este caso el objeto chile es un data.frame.


# 1. ¿De qué clase es cada variable?
class(Chile$region)
class(Chile$population)
class(Chile$sex)
class(Chile$education)
class(Chile$statusquo)
class(Chile$income)
class(Chile$vote)


# 2. Cree una nueva variable que se llame income1000, que sea igual al ingreso en miles de pesos

Chile$income1000 <- Chile$income/1000





# 3. Cuente cuántas personas votaron por el si, y cuántas por el no

Chile$vote
Chile$vote=="Y"
table(Chile$vote=="Y") ### se que 868 personas votaron por el Si.

Chile$vote=="N"
table(Chile$vote=="N") ### se que 889 personas votaron por el No.



# 4. Cree un nuevo objeto tipo data.table en base al objeto Chile, que se llame Chile2
library(data.table)
install.packages(data.table)

Chile2 <- data.table(Chile) ### debiese ser así, pero por alguna razón no me carga.




# 5. Borre la variable statusquo
Chile
ChileNuevo <- Chile [ , -c(7)] ### forma que busqué yo por internet

Chile
Chile2[, statusquo:=NULL] ### := se reemplazan elementos dentro de una variable, entonces la columna statusquo será Nula.
Chile2




# 7. Cree una nueva variable de ingreso per cápita
  # 7.1. Reemplace los NAs de income por ceros

is.na(Chile2$income)
Chile2[is.na(income),]
Chile2[is.na(income), income:= 0]

# 8. Cree una variable que tenga un 1 si age>65 y 0 en el caso contrario

str(Chile)
Chile2[Chile2$age>65, viejitos := 1]
Chile2[age>65,]$viejitos<-1



# 9. Cree un nuevo objeto que muestre los valores promedio de ingreso y edad por region y sexo
#     Nota: Si tiene observaciones con NA, por qué es esto? Cómo lo arreglamos?

Chile2[,.(promedio_ingreso=mean(income,na.rm = T), promedio_edad = mean(age,na.rm = T)) by =.(region,sex)]


#10. loops: Solo para propósitos demostrativos, no entra en la tarea!

for(i in 1:10){
  print(paste("Estoy contando de 1 en 1 al 10, y voy en el", i))
  Sys.sleep(3)
}


regiones<-names(table(Chile$region))
sexo<-names(table(Chile$sex))

Chile2<-data.table(Chile)
for(r in 1:length(regiones)){
  for(s in 1:length(sexo)){
    prom.inc<-Chile2[region==regiones[r] & sex==sexo[s],mean(income,na.rm = T)]
    prom.age<-Chile2[region==regiones[r] & sex==sexo[s],mean(age,na.rm = T)]
    plot(Chile2[region==regiones[r] & sex==sexo[s],.(age,income)])
    title(main = paste("Region", regiones[r], "y Sexo", sexo[s]),sub = paste("Ingreso Promedio:", prom.inc, "| Edad Promedio:", prom.age))
    abline(h = prom.inc, col='red')
    abline(v = prom.age, col='blue')
    Sys.sleep(2)
  }
}