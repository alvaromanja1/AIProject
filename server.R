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
#weather <- subset(weather, weather$Id <= 15361) 
#colnames(weather)[1] <- "id2"
#weather$id2 = NULL
#weather$Id = NULL
#weather$Id = NULL
#weather$Pressure = round(weather$Pressure)
#weather$id = seq.int(nrow(weather))
#weather = weather[weather$id > 4621,]
#write.csv(weather, file = "csv/Weather Bilbao.csv")
#weather <- weather[, -25]

presionBaja = c(2,972, 1002, 1011, NA) #Aquí se ha decidido empezar por 0, ya que es el mínimo valor, luego que a partir de 1.25 empieze a dejar de ser una aplicación con rating "bajo", para que así, poco antes de la mitad deje de ser una aplicación con rating "bajo"
presionMedia = c(4, 1009, 1014, 1018 , 1020)#Aquí empezamos por 1.75 para que haya más margen hasta el 2.5 (que sería la mitad exacta), luego a partir de 3.25 consideramos que empieza a dejar de ser una app "normal", hasta llegar a 4
presionAlta = c(3,1018, 1026, 1037, NA)#Por último, consideramos que una app empieza a ser el rating "alto" a partir de 3.5, hasta 4.5, donde de ahí en adelante (hasta 5), se puede considerar una App con rating "alto"
presionAny = c(4,972,972,1037,1037)

tempBaja = c(2, -3 , 8 , 14 , NA) #Aquí empezamos por 9, ya que es el mínimo, empezamos a reducir su pertenencia a partir de 16, y consideramos que dejan de ser "pocos" los dispositivos soportados a partur de 25
tempMedia = c(4,10, 14, 18, 24) #Aquí empezamos a considerar como "algunos" las apps con valor de 23, hasta 35, donde empeiezan ya a ser miembros de "pleno derecho" al conjunto (se hace así ya que hay grandes saltos entre dispositivos conectados). Por último, se considera que a partir de 37, empiezan a estar menos en el medio, hasta llegar a 40 
tempAlta = c(3,20, 27, 39, NA)
tempAny = c(4,-3,-3,39,39)

humedadBaja = c(2, 21 , 40 , 52 , NA)
humedadMedia = c(4,47, 61, 71, 80)
humedadAlta = c(3,77, 93, 100, NA)
humedadAny = c(4,21,21,100,100)

varinp.mf = cbind(tempBaja,tempMedia,tempAlta,tempAny,humedadBaja,humedadMedia,humedadAlta,humedadAny,presionBaja,presionMedia,presionAlta,presionAny)

num.fvalinput = matrix(c(4,4,4), nrow=1)


varinput1 = c("Baja", "Media", "Alta","Any")
varinput2 = c("Reducida","Intermedia","Grande","Any")
varinput3 = c("Poca", "Algo", "Mucha","Any")

names.varinput = c(varinput1, varinput2,varinput3)

range.data = matrix(c( -3, 39, 21, 100, 972, 1037, 0, 100), nrow = 2)
#Temp minima historia bilbao: -6, Max: 41
type.defuz = "COG"
type.tnorm = "MIN"
type.snorm = "MAX"
type.implication.func = "MIN"
type.model = "MAMDANI" #Indicamos el tipo de modelo que vamos a usar

name = "Probabilidad de lluvia" #Le damos nombre

newdata = weather[,c("Temperature", "Humidity","Pressure")]

colnames.var = c("Temperatura", "Humedad","Presion", "Resultado")

probBaja = c(2,0, 20, 40, NA)#Consideramos que una app es "mala" si tiene entre 0 y 20 puntos, a partir de la cual pensamos que deja de ser tam mala, hasta llegar a la valoración de 40
probMedia = c(4,30, 45, 55, 70) #Consideramos que una app empieza a ser "regular" a partir de los 30 puntos, hasta llegar a los 45 puntos; y empieza a dejar de ser "regular" a partir de los 55, hasta llegar a los 70 puntos
probAlta = c(3,65, 85, 100, NA)

num.fvaloutput = matrix(c(3), nrow = 1)

varoutput1 = c("Baja", "Media", "Alta") #Indicamos sus nombres

varout.mf = cbind(probBaja,probMedia,probAlta)
#Temperatura: -> Baja, Media, Alta
#Humedad: -> Reducida, Intermedia, Grande
#presion: -> Poca, Algo, Mucha

rule = matrix( c("Baja", "and", "Any" , "and" , "Any","->", "Alta",
                 "Baja", "and","Grande" , "and" , "Mucha","->", "Media",
                 "Baja", "and","Intermedia", "and"  , "Mucha","->", "Media",
                 "Baja", "and","Reducida" , "and" , "Algo","->", "Media",
                 "Baja", "and","Reducida", "and"  , "Mucha","->", "Media",
                 
                 "Media", "and","Any" , "and" , "Any","->", "Media",
                 "Media", "and","Grande" , "and" , "Mucha","->", "Baja",
                 "Media", "and","Intermedia" , "and" , "Mucha","->", "Baja",
                 "Media", "and","Intermedia" , "and" , "Algo","->", "Baja",
                 "Media", "and","Reducida" , "and" , "Any","->", "Baja",
                 
                 
                 "Alta", "and","Any", "and"  , "Any","->", "Baja",
                 "Alta", "and","Grande", "and"  , "Poca","->", "Media",
                 "Alta", "and","Intermedia" , "and" , "Poca","->", "Media"
                 
                 
               
), nrow = 13, byrow = TRUE)

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

result = result[,c(1,2,3,4,5,6,12)]


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
  # Obtenemos del dataset la hora actual para dibujar una luna en caso de que sea de noche
  currentTime = realTemp$time;
  # Hacemos un substring para guardar en currentTime la hora actual
  currentTime = substr(currentTime, 12, 19)

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
  # Redondeamos el resultado de la prediccion, temperatura y humedad
  res = round(res,digits=0)
  realTemp2$humidity = round(realTemp2$humidity,digits=0)
  realTemp2$temperatureCelsius = round(realTemp2$temperatureCelsius,digits=0)
  
  #Indicamos los textos que queremos volcar a la interfaz
  Currentprobability = paste("Precipitations:",res, sep=" ")
  Currentprobability = paste(Currentprobability,"%", sep="")
  
  CurrentTemperature = paste("Temperature:",realTemp2$temperatureCelsius, sep=" ")
  CurrentTemperature = paste(CurrentTemperature,"º", sep="")
  
  CurrentHumidity = paste("Humidity:",realTemp2$humidity, sep=" ")
  CurrentHumidity = paste(CurrentHumidity,"%", sep="")
  
  output$res <- renderText({
    Currentprobability
  })
  output$currentTemp <- renderText({
    CurrentTemperature
  })
  output$currentHum <- renderText({
    CurrentHumidity
  })
  
  output$image1 <- renderImage({
    if(res >=0 && res < 25 && currentTime < "18:30:00"){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (res >=25 && res < 45 && currentTime < "18:30:00"){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (res >=45 && res < 65 && currentTime < "18:30:00"){
      return(list(
        src = "images/nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=65 && res <= 100 && currentTime < "18:30:00"){
      return(list(
        src = "images/lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(realTemp2$temperatureCelsius < 0){
      return(list(
        src = "images/nieve.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=0 && res < 25 && currentTime > "18:30:00"){ 
      return(list(
        src = "images/luna.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (res >=25 && res < 45 && currentTime > "18:30:00"){
      return(list(
        src = "images/luna_nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (res >=45 && res < 65 && currentTime > "18:30:00"){
      return(list(
        src = "images/noche_lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=65 && res <= 100 && currentTime > "18:30:00"){
      return(list(
        src = "images/noche_truenos.png",
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

  Tomorrowres = round(Tomorrowres,digits=0)
  tomorrowTemp$humidity = round(tomorrowTemp$humidity,digits=0)
  tomorrowTemp$temperature = round(tomorrowTemp$temperature,digits=0)
  
  #Indicamos los textos que queremos volcar a la interfaz

  Tomorrowprobability = paste("Precipitations:",Tomorrowres, sep=" ")
  Tomorrowprobability = paste(Tomorrowprobability,"%", sep="")
  
  TomorrowTemperature = paste("Temperature:",tomorrowTemp$temperature, sep=" ")
  TomorrowTemperature = paste(TomorrowTemperature,"º", sep="")
  
  TomorrowHumidity = paste("Humidity:",tomorrowTemp$humidity, sep=" ")
  TomorrowHumidity = paste(TomorrowHumidity,"%", sep="")
  
  output$tomorrowRes <- renderText({
    Tomorrowprobability
  })
  output$tomorrowTemp <- renderText({
    TomorrowTemperature
  })
  output$tomorrowHum <- renderText({
    TomorrowHumidity
  })
  
  output$tomorrowres <- renderText({
    Tomorrowres 
  })
  
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
      
    }else if(tomorrowTemp$temperature < 0){
      return(list(
        src = "images/nieve.png",
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
  
  #DayAfterTomorrowprobabilityText = paste(day3res, "% probability of precipitations.", sep=" ")
  
  day3res = round(day3res,digits=0)
  day3Temp$humidity = round(day3Temp$humidity,digits=0)
  day3Temp$temperature = round(day3Temp$temperature,digits=0)
  
  #Indicamos los textos que queremos volcar a la interfaz
  
  day3probability = paste("Precipitations:",day3res, sep=" ")
  day3probability = paste(day3probability,"%", sep="")
  
  day3Temperature = paste("Temperature:",day3Temp$temperature, sep=" ")
  day3Temperature = paste(day3Temperature,"º", sep="")
  
  day3Humidity = paste("Humidity:",day3Temp$humidity, sep=" ")
  day3Humidity = paste(day3Humidity,"%", sep="")
  
  output$day3Res <- renderText({
    day3probability
  })
  output$day3Temp <- renderText({
    day3Temperature
  })
  output$day3Hum <- renderText({
    day3Humidity
  })
  
  output$day3res <- renderText({
    day3res 
  })
  
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
      
    }else if(day3Temp$temperature < -2){
      return(list(
        src = "images/nieve.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }
  }, deleteFile = FALSE)
})
