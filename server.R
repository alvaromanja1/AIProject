# Nombre: Iván Martín y Alvaro Manjarrés

# Con los siguientes dos comandos limpiamos el entorno de trabajo y la consola
rm(list = ls())
cat("\014")

# Otro punto es establecer el working directory, con este comando hacemos que 
# éste sea igual a aquél en el que se encuentra el script
# Si es tu 1ª vez con R, tendrás que instalar el paquete rstudioapim
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Comprobamos que está correcto
#getwd()

#Instalar librerias
#install.packages("rlist")
#install.packages("SmarterPoland")

#Librerías
library(frbs)
library(ggplot2)
library(scales)
library(SmarterPoland)
library(htmltools)
library(rlist)
library(shiny)
library(darksky)
library(weathermetrics)

file = 'Apikey.txt'
APIKEY = readChar(file, file.info(file)$size)

#---------------------------------------------------------------------------

#leer los csv's  
weather = read.csv("csv/Weather Bilbao.csv",check.names=FALSE)
#colnames(weather)[1] <- "id2"
#weather$id2 = NULL
#weather$id = NULL
#weather$Id = NULL
#weather$Pressure = round(weather$Pressure)
#weather$id = seq.int(nrow(weather))
#weather = weather[weather$id > 4621,]
#write.csv(weather, file = "csv/Weather Bilbao.csv")
#weather <- weather[, -25]

presionBaja = c(2,972, 1002, 1011, NA) #Aquí se ha decidido empezar por 0, ya que es el mínimo valor, luego que a partir de 1.25 empieze a dejar de ser una aplicación con rating "bajo", para que así, poco antes de la mitad deje de ser una aplicación con rating "bajo"
presionMedia = c(4, 1009, 1014, 1018 , 1020)#Aquí empezamos por 1.75 para que haya más margen hasta el 2.5 (que sería la mitad exacta), luego a partir de 3.25 consideramos que empieza a dejar de ser una app "normal", hasta llegar a 4
presionAlta = c(3,1018, 1026, 1037, NA)#Por último, consideramos que una app empieza a ser el rating "alto" a partir de 3.5, hasta 4.5, donde de ahí en adelante (hasta 5), se puede considerar una App con rating "alto"

tempBaja = c(2, -3 , 8 , 14 , NA) #Aquí empezamos por 9, ya que es el mínimo, empezamos a reducir su pertenencia a partir de 16, y consideramos que dejan de ser "pocos" los dispositivos soportados a partur de 25
tempMedia = c(4,12, 16, 20, 24) #Aquí empezamos a considerar como "algunos" las apps con valor de 23, hasta 35, donde empeiezan ya a ser miembros de "pleno derecho" al conjunto (se hace así ya que hay grandes saltos entre dispositivos conectados). Por último, se considera que a partir de 37, empiezan a estar menos en el medio, hasta llegar a 40 
tempAlta = c(3,20, 27, 39, NA)

humedadBaja = c(2, 21 , 40 , 52 , NA)
humedadMedia = c(4,47, 61, 71, 80)
humedadAlta = c(3,77, 93, 100, NA)

varinp.mf = cbind(tempBaja,tempMedia,tempAlta,humedadBaja,humedadMedia,humedadAlta,presionBaja,presionMedia,presionAlta)

num.fvalinput = matrix(c(3,3,3), nrow=1)


varinput1 = c("Baja", "Media", "Alta")
varinput2 = c("Reducida","Intermedia","Grande")
varinput3 = c("Poca", "Algo", "Mucha")

names.varinput = c(varinput1, varinput2,varinput3)

range.data = matrix(c( -3, 39, 21, 100, 972, 1037, 0, 100), nrow = 2)
#Temp minima historia bilbao: -6, Max: 41
type.defuz = "COG"
type.tnorm = "MIN"
type.snorm = "MAX"
type.implication.func = "MIN"
type.model = "MAMDANI" #Indicamos el tipo de modelo que vamos a usar

name = "Probabilidad de lluvia" #Le damos nombre

newdata = read.csv("csv/Weather Bilbao.csv")[,c("Temperature", "Humidity","Pressure")]

colnames.var = c("Temperatura", "Humedad","Presion", "Resultado")

probBaja = c(2,0, 20, 40, NA)#Consideramos que una app es "mala" si tiene entre 0 y 20 puntos, a partir de la cual pensamos que deja de ser tam mala, hasta llegar a la valoración de 40
probMedia = c(4,30, 45, 55, 70) #Consideramos que una app empieza a ser "regular" a partir de los 30 puntos, hasta llegar a los 45 puntos; y empieza a dejar de ser "regular" a partir de los 55, hasta llegar a los 70 puntos
probAlta = c(3,65, 85, 100, NA)

num.fvaloutput = matrix(c(3), nrow = 1)

varoutput1 = c("Baja", "Media", "Alta") #Indicamos sus nombres

varout.mf = cbind(probBaja,probMedia,probAlta)
#Temperatura: -> Baja, Media, Alta
#Humedad: -> Reducida, Intermedia, Grande
#Humedad: -> Poca, Algo, Mucha
rule = matrix( c("Baja", "and", "Grande" , "and" , "Poca","->", "Alta",
                 "Baja", "and","Grande" , "and" , "Algo","->", "Alta",
                 "Baja", "and","Grande" , "and" , "Mucha","->", "Alta",
                 
                 "Baja", "and","Intermedia" , "and" , "Poca","->", "Alta",
                 "Baja", "and","Intermedia" , "and" , "Algo","->", "Alta",
                 "Baja", "and","Intermedia", "and"  , "Mucha","->", "Media",
                 
                 "Baja", "and","Reducida" , "and" , "Poca","->", "Alta",
                 "Baja", "and","Reducida" , "and" , "Algo","->", "Media",
                 "Baja", "and","Reducida", "and"  , "Mucha","->", "Media",
                 
                 "Media", "and","Grande" , "and" , "Poca","->", "Alta",
                 "Media", "and","Grande" , "and" , "Algo","->", "Alta",
                 "Media", "and","Grande" , "and" , "Mucha","->", "Media",
                 
                 "Media", "and","Intermedia", "and"  , "Poca","->", "Alta",
                 "Media", "and","Intermedia" , "and" , "Algo","->", "Media",
                 "Media", "and","Intermedia" , "and" , "Mucha","->", "Media",
                 
                 "Media", "and","Reducida"  , "and", "Poca","->", "Media",
                 "Media", "and","Reducida" , "and" , "Algo","->", "Media",
                 "Media", "and","Reducida" , "and" , "Mucha","->", "Baja",
                 
                 "Alta", "and","Grande", "and"  , "Poca","->", "Media",
                 "Alta", "and","Grande", "and"  , "Algo","->", "Media",
                 "Alta", "and","Grande" , "and" , "Mucha","->", "Baja",
                 
                 "Alta", "and","Intermedia" , "and" , "Poca","->", "Media",
                 "Alta", "and","Intermedia" , "and" , "Algo","->", "Baja",
                 "Alta", "and","Intermedia" , "and" , "Mucha","->", "Baja",
                 
                 "Alta", "and","Reducida" , "and" , "Poca","->", "Baja",
                 "Alta", "and","Reducida" , "and" , "Algo","->", "Baja",
                 "Alta", "and","Reducida", "and"  , "Mucha","->", "Baja"
), nrow = 27, byrow = TRUE)

sistema = frbs.gen(range.data, num.fvalinput, names.varinput,
                   num.fvaloutput, varout.mf, varoutput1, rule,
                   varinp.mf, type.model, type.defuz, type.tnorm,
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)

windows()
plotMF(sistema)#También, por simple hecho didáctico, mostramos cómo quedan nuestros conjuntos de entrada y el de salida

res = predict(sistema, newdata)$predicted.val 

newdata$probabilidad = res

newdata$id = seq.int(nrow(weather))

result = merge(weather, newdata, by.x = 'Id', by.y = 'id')

result = result[,c(1,2,3,4,5,6,7,8,12)]


#Servidor de shiny mediante el cual pasaremos la info a ui.r y lo visualizamos
#A partir de aquí, lo de la predicción actual
shinyServer(function(input, output) {
  names(unlist(APIKEY))
  location = 'Bilbao'
  
  #Coordenadas para saber el tiempo en bilbao
  long = -2.934991
  lat = 43.263005 
  #establecemos las fechas de mañana y pasado para realizar las predicciones
  tomorrowDate = paste(Sys.Date()+1, "12:00:00-0400", sep="T")
  theDayAfterTomorrowDate = paste(Sys.Date()+2, "12:00:00-0400", sep="T")
  
  output$location <- renderText({
    location 
  })
  
  forecast = getWeatherForecast(APIKEY, city=location)
  
  realTemp = forecast[[1]]
  
  realTemp$icon = NULL
  realTemp$apparentTemperature = NULL
  realTemp$temperature = NULL
  realTemp$dewPoint = NULL
  realTemp$windSpeed = NULL
  realTemp$windBearing = NULL
  realTemp$cloudCover = NULL
  realTemp$uvIndex = NULL
  realTemp$visibility = NULL
  realTemp$ozone = NULL
  realTemp$apparentTemperatureCelsius = NULL
  
  realTemp2 = realTemp[,c("temperatureCelsius","humidity","pressure")]
  
  realTemp2$humidity = realTemp$humidity * 100
  
  res = predict(sistema, realTemp2)$predicted.val 
  
  output$image1 <- renderImage({
    if(res >=0 && res < 25){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (res >=25 && res < 45){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (res >=45 && res < 65){
      return(list(
        src = "images/nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=65 && res <= 100){
      return(list(
        src = "images/lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }
  }, deleteFile = FALSE)
  
  realTemp2$probabilidad = res
  
  
  #A partir de aquí sacamos la predicción de los siguientes días. Con ello lo volcaremos a la interfaz
  
  # Dia 2
  dailyForecast = get_forecast_for(lat,long, tomorrowDate)
  day2 = dailyForecast[[2]]
  
  tomorrowTemp = day2[,c("temperatureMin", "temperatureMax", "humidity","pressure")]
  
  tomorrowTemp$humidity = day2$humidity * 100
  
  tomorrowTemp$temperatureMin = fahrenheit.to.celsius(day2$temperatureMin)
  tomorrowTemp$temperatureMax = fahrenheit.to.celsius(day2$temperatureMax)
  
  tomorrowTemp$temperature = (tomorrowTemp$temperatureMin + tomorrowTemp$temperatureMax) / 2
  
  tomorrowTemp = tomorrowTemp[,c("temperature", "humidity","pressure")]
  
  Tomorrowres = predict(sistema, tomorrowTemp)$predicted.val 
  
  tomorrowTemp$probabilidad = Tomorrowres
  
  output$image2 <- renderImage({
    if(Tomorrowres >=0 && Tomorrowres < 25){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (Tomorrowres >=25 && Tomorrowres < 45){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (Tomorrowres >=45 && Tomorrowres < 65){
      return(list(
        src = "images/nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(Tomorrowres >=65 && Tomorrowres <= 100){
      return(list(
        src = "images/lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }
  }, deleteFile = FALSE)
  
  #Dia 3 -----------------------------
  
  dailyForecast = get_forecast_for(lat,long, theDayAfterTomorrowDate)
  day3 = dailyForecast[[2]]
  
  day3Temp = day3[,c("temperatureMin", "temperatureMax", "humidity","pressure")]
  
  day3Temp$humidity = day3$humidity * 100
  
  
  day3Temp$temperatureMin = fahrenheit.to.celsius(day3$temperatureMin)
  day3Temp$temperatureMax = fahrenheit.to.celsius(day3$temperatureMax)
  
  day3Temp$temperature = (day3Temp$temperatureMin + day3Temp$temperatureMax) / 2
  
  day3Temp = day3Temp[,c("temperature", "humidity","pressure")]
  
  day3res = predict(sistema, day3Temp)$predicted.val 
  
  day3Temp$probabilidad = day3res
  
  output$image3 <- renderImage({
    if(day3res >=0 && day3res < 25){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (day3res >=25 && day3res < 45){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (day3res >=45 && day3res < 65){
      return(list(
        src = "images/nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(day3res >=65 && day3res <= 100){
      return(list(
        src = "images/lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }
  }, deleteFile = FALSE)
  
})
