# Nombre: Iv?n Mart?n y Alvaro Manjarr?s

# Con los siguientes dos comandos limpiamos el entorno de trabajo y la consola
rm(list = ls())
cat("\014")

# Otro punto es establecer el working directory, con este comando hacemos que 
# éste sea igual a aquél en el que se encuentra el script
# Si es tu 1ª vez con R, tendrás que instalar el paquete rstudioapim
path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Comprobamos que está correcto
#getwd()

#Librer?as
library(frbs)
library(ggplot2)
#---------------------------------------------------------------------------

#leer los csv's
drivers = read.csv("csv/drivers.csv")
driverStandings = read.csv("csv/driverStandings.csv")
races = read.csv("csv/races.csv")
results = read.csv("csv/results.csv")
circuits = read.csv("csv/circuits.csv")

#combinar csv's
driveStands = merge(drivers, driverStandings, by = "driverId")
driverResults = merge(driveStands, results, by = "driverId")

#Borramos las columnas que no nos hagan falta
driverResults <- driverResults[, !(colnames(driverResults) %in% c("fastestLapTime","fastestLapSpeed", "fastestLap", "time", "constructorId", "points.x", "position.x", "positionText.x"))]
