# Nombre: Iv?n Mart?n y Alvaro Manjarr?s

# Con los siguientes dos comandos limpiamos el entorno de trabajo y la consola
rm(list = ls())
cat("\014")

# Otro punto es establecer el working directory, con este comando hacemos que 
# éste sea igual a aquél en el que se encuentra el script
# Si es tu 1ª vez con R, tendrás que instalar el paquete rstudioapim
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Comprobamos que está correcto
#getwd()

#Librer?as
library(frbs)
library(ggplot2)
#---------------------------------------------------------------------------

#leer los csv's

weather = read.csv("csv/Tiempo Madrid.csv")
#weather$id = seq.int(nrow(weather))
#weather = weather[weather$id > 4621,]
#write.csv(weather, file = "csv/Tiempo Madrid.csv")

presionBaja = c(2,986, 1003, 1010, NA) #Aquí se ha decidido empezar por 0, ya que es el mínimo valor, luego que a partir de 1.25 empieze a dejar de ser una aplicación con rating "bajo", para que así, poco antes de la mitad deje de ser una aplicación con rating "bajo"
presionMedia = c(4, 1007, 1012, 1015 , 1019)#Aquí empezamos por 1.75 para que haya más margen hasta el 2.5 (que sería la mitad exacta), luego a partir de 3.25 consideramos que empieza a dejar de ser una app "normal", hasta llegar a 4
presionAlta = c(3,1017, 1028, 1043, NA)#Por último, consideramos que una app empieza a ser el rating "alto" a partir de 3.5, hasta 4.5, donde de ahí en adelante (hasta 5), se puede considerar una App con rating "alto"

tempBaja = c(2, -3 , 5 , 12 , NA) #Aquí empezamos por 9, ya que es el mínimo, empezamos a reducir su pertenencia a partir de 16, y consideramos que dejan de ser "pocos" los dispositivos soportados a partur de 25
tempMedia = c(4,10, 15, 18, 22) #Aquí empezamos a considerar como "algunos" las apps con valor de 23, hasta 35, donde empeiezan ya a ser miembros de "pleno derecho" al conjunto (se hace así ya que hay grandes saltos entre dispositivos conectados). Por último, se considera que a partir de 37, empiezan a estar menos en el medio, hasta llegar a 40 
tempAlta = c(3,20, 26, 31, NA)

humedadBaja = c(2, 16 , 24 , 35 , NA)
humedadMedia = c(4,33, 45, 62, 70)
humedadAlta = c(3,65, 78, 99, NA)

varinp.mf = cbind(tempBaja,tempMedia,tempAlta,presionBaja,presionMedia,presionAlta)

num.fvalinput = matrix(c(3,3), nrow=1)


varinput2 = c("Poca", "Algo", "Mucha")
varinput1 = c("Baja", "Media", "Alta")
names.varinput = c(varinput1, varinput2)

range.data = matrix(c( -3, 31, 986, 1043, 0, 100), nrow = 2)

type.defuz = "COG"
type.tnorm = "MIN"
type.snorm = "MAX"
type.implication.func = "MIN"
type.model = "MAMDANI" #Indicamos el tipo de modelo que vamos a usar

name = "Probabilidad de lluvia" #Le damos nombre

newdata = read.csv("csv/Tiempo Madrid.csv")[,c(4,13)]

colnames.var = c("Temperatura", "Presion", "Resultado")

probBaja = c(2,0, 20, 40, NA)#Consideramos que una app es "mala" si tiene entre 0 y 20 puntos, a partir de la cual pensamos que deja de ser tam mala, hasta llegar a la valoración de 40
probMedia = c(4,30, 45, 55, 70) #Consideramos que una app empieza a ser "regular" a partir de los 30 puntos, hasta llegar a los 45 puntos; y empieza a dejar de ser "regular" a partir de los 55, hasta llegar a los 70 puntos
probAlta = c(3,65, 85, 100, NA)

num.fvaloutput = matrix(c(3), nrow = 1)

varoutput1 = c("Baja", "Media", "Alta") #Indicamos sus nombres

varout.mf = cbind(probBaja,probMedia,probAlta)

rule = matrix( c("Baja", "and", "Poca","->", "Alta",
                 "Baja", "and", "Algo","->", "Media",
                 "Baja", "and", "Mucha","->", "Media",
                 
                 "Media", "and", "Poca","->", "Media",
                 "Media", "and", "Algo","->", "Media",
                 "Media", "and", "Mucha","->", "Baja",
                 
                 "Alta", "and", "Poca","->", "Media",
                 "Alta", "and", "Algo","->", "Baja",
                 "Alta", "and", "Mucha","->", "Baja"
                 ), nrow = 9, byrow = TRUE)

sistema = frbs.gen(range.data, num.fvalinput, names.varinput,
                   num.fvaloutput, varout.mf, varoutput1, rule,
                   varinp.mf, type.model, type.defuz, type.tnorm,
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)

plotMF(sistema)#También, por simple hecho didáctico, mostramos cómo quedan nuestros conjuntos de entrada y el de salida

res = predict(sistema, newdata)$predicted.val 

newdata$probabilidad = res
