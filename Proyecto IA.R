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

#elimina las filas cuyo año sea menor que 2010 debido a que ese año cambió el sistema de puntuación
#y el resultado no sería homogéneo
weather$id = seq.int(nrow(weather))
weather = weather[weather$id > "4622",]


