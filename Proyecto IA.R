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
drivers = read.csv("csv/drivers.csv")
driverStandings = read.csv("csv/driverStandings.csv")
races = read.csv("csv/races.csv")
results = read.csv("csv/results.csv")

drivers = drivers[, !(colnames(drivers) %in% c("number","code", "forename", "surname", "dob", "nationality", "url"))]
driverStandings = driverStandings[, !(colnames(driverStandings) %in% c("positionText","driverStandingsId"))]
races = races[, !(colnames(races) %in% c("round","date", "time", "url"))]
results = results[, !(colnames(results) %in% c("round","date", "time", "url"))]

#combinar csv's
driveStands = merge(drivers, driverStandings, by = "driverId")
driverResults = merge(driveStands, races, by = "raceId")

#elimina las filas cuyo año sea menor que 2010 debido a que ese año cambió el sistema de puntuación
#y el resultado no sería homogéneo
driverResults = driverResults[driverResults$year > "2009",]


