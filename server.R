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
weather = read.csv("csv/Weather Madrid.csv",check.names=FALSE)
#write.csv(weather, file = "csv/Weather Madrid.csv")

presionBaja = c(2,980, 1005, 1012, NA) #Aquí se ha decidido empezar por 0, ya que es el mínimo valor, luego que a partir de 1.25 empieze a dejar de ser una aplicación con rating "bajo", para que así, poco antes de la mitad deje de ser una aplicación con rating "bajo"
presionMedia = c(4, 1008, 1014, 1018 , 1021)#Aquí empezamos por 1.75 para que haya más margen hasta el 2.5 (que sería la mitad exacta), luego a partir de 3.25 consideramos que empieza a dejar de ser una app "normal", hasta llegar a 4
presionAlta = c(3,1018, 1030, 1047, NA)#Por último, consideramos que una app empieza a ser el rating "alto" a partir de 3.5, hasta 4.5, donde de ahí en adelante (hasta 5), se puede considerar una App con rating "alto"
presionAny = c(4,980,980,1047,1047)

tempBaja = c(2, -6 , 5 , 12 , NA) #Aquí empezamos por 9, ya que es el mínimo, empezamos a reducir su pertenencia a partir de 16, y consideramos que dejan de ser "pocos" los dispositivos soportados a partur de 25
tempMedia = c(4,10, 15, 18, 22) #Aquí empezamos a considerar como "algunos" las apps con valor de 23, hasta 35, donde empeiezan ya a ser miembros de "pleno derecho" al conjunto (se hace así ya que hay grandes saltos entre dispositivos conectados). Por último, se considera que a partir de 37, empiezan a estar menos en el medio, hasta llegar a 40 
tempAlta = c(3,20, 26, 39, NA)
tempAny = c(4,-6,-6,39,39)

humedadBaja = c(2, 16 , 35 , 45 , NA)
humedadMedia = c(4,35, 54, 70, 78)
humedadAlta = c(3,75, 88, 100, NA)
humedadAny = c(4,16,16,100,100)

varinp.mf = cbind(tempBaja,tempMedia,tempAlta,tempAny,humedadBaja,humedadMedia,humedadAlta,humedadAny,presionBaja,presionMedia,presionAlta,presionAny)

num.fvalinput = matrix(c(4,4,4), nrow=1)


varinput1 = c("Baja", "Media", "Alta","Any")
varinput2 = c("Reducida","Intermedia","Grande","Any")
varinput3 = c("Poca", "Algo", "Mucha","Any")

names.varinput = c(varinput1, varinput2,varinput3)

range.data = matrix(c( -6, 39, 16, 100, 980, 1047, 0, 100), nrow = 2)
#Temp minima historia bilbao: -6, Max: 41
type.defuz = "COG"
type.tnorm = "MIN"
type.snorm = "MAX"
type.implication.func = "MIN"
type.model = "MAMDANI" #Indicamos el tipo de modelo que vamos a usar

name = "Probabilidad de lluvia" #Le damos nombre

newdata = weather[,c("Mean.TemperatureC", "Mean.Humidity","Mean.Sea.Level.PressurehPa")]

colnames.var = c("Temperatura", "Humedad","Presion", "Resultado")

probBaja = c(2,0, 20, 40, NA)#Consideramos que una app es "mala" si tiene entre 0 y 20 puntos, a partir de la cual pensamos que deja de ser tam mala, hasta llegar a la valoración de 40
probMedia = c(4,30, 40, 50, 65) #Consideramos que una app empieza a ser "regular" a partir de los 30 puntos, hasta llegar a los 45 puntos; y empieza a dejar de ser "regular" a partir de los 55, hasta llegar a los 70 puntos
probAlta = c(3,60, 85, 100, NA)

num.fvaloutput = matrix(c(3), nrow = 1)

varoutput1 = c("Baja", "Media", "Alta") #Indicamos sus nombres

varout.mf = cbind(probBaja,probMedia,probAlta)
#Temperatura: -> Baja, Media, Alta
#Humedad: -> Reducida, Intermedia, Grande
#presion: -> Poca, Algo, Mucha

rule = matrix( c("Baja", "and", "Grande" , "and" , "Any","->", "Alta",
                 
                 "Baja", "and","Intermedia" , "and" , "Poca","->", "Alta",
                 "Baja", "and","Intermedia" , "and" , "Algo","->", "Media",
                 "Baja", "and","Intermedia", "and"  , "Mucha","->", "Media",
                 
                 "Baja", "and","Reducida" , "and" , "Any","->", "Media",
                 
                 "Media", "and","Grande" , "and" , "Poca","->", "Alta",
                 "Media", "and","Grande" , "and" , "Algo","->", "Alta",
                 "Media", "and","Grande" , "and" , "Mucha","->", "Media",
                 
                 "Media", "and","Intermedia", "and"  , "Poca","->", "Alta",
                 "Media", "and","Intermedia" , "and" , "Algo","->", "Media",
                 "Media", "and","Intermedia" , "and" , "Mucha","->", "Media",
                 
                 "Media", "and","Reducida"  , "and", "Any","->", "Media",
                 
                 "Alta", "and","Grande", "and"  , "Poca","->", "Media",
                 "Alta", "and","Grande", "and"  , "Algo","->", "Media",
                 "Alta", "and","Grande" , "and" , "Mucha","->", "Baja",
                 
                 "Alta", "and","Intermedia" , "and" , "Poca","->", "Media",
                 "Alta", "and","Intermedia" , "and" , "Algo","->", "Baja",
                 "Alta", "and","Intermedia" , "and" , "Mucha","->", "Baja",
                 
                 "Alta", "and","Reducida" , "and" , "Any","->", "Baja"
                 
), nrow = 19, byrow = TRUE)

sistema = frbs.gen(range.data, num.fvalinput, names.varinput,
                   num.fvaloutput, varout.mf, varoutput1, rule,
                   varinp.mf, type.model, type.defuz, type.tnorm,
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)

#windows()
#plotMF(sistema)#También, por simple hecho didáctico, mostramos cómo quedan nuestros conjuntos de entrada y el de salida

res = predict(sistema, newdata)$predicted.val 

newdata$probabilidad = res

newdata$id = seq.int(nrow(weather))

result = merge(weather, newdata, by.x = 'Id', by.y = 'id')

result = result[,c(1,2,3,4,5,6,7,8,12)]

# Obtenemos el portentaje de acierto 
accuracy = function(result){
  right = 0
  rain = 0
  event = ""
  prob = 0
  for(i in 1:nrow(result)){
    rain = result[i,7]
    event = result[i,8]
    prob = result[i,9]
    if(rain > 0 || event != ""){
      if(prob >= 50){ 
        right = right + 1
      }
      
    }else{
      if(prob < 50){ 
        right = right + 1
      }
      
    }
  }
  precision = (right / nrow(result) ) * 100
  return(round(precision,2))
}

accuracyOfPrediction = paste("Accuracy:", accuracy(result), sep=" ")
accuracyOfPrediction2 = paste(accuracyOfPrediction,"%", sep=" ")
#Servidor de shiny mediante el cual pasaremos la info a ui.r y lo visualizamos
#A partir de aquí, lo de la predicción actual
shinyServer(function(input, output) {
  names(unlist(APIKEY))
  location = 'Madrid'
  
  #Coordenadas para saber el tiempo en bilbao
  long = -3.6827461
  lat = 40.4893538 
  #establecemos las fechas de mañana y pasado para realizar las predicciones
  tomorrowDate = paste(Sys.Date()+1, "12:00:00-0400", sep="T")
  theDayAfterTomorrowDate = paste(Sys.Date()+2, "12:00:00-0400", sep="T")
  
  
  output$location <- renderText({
    location 
  })
  
  output$accuracy <- renderText({
    accuracyOfPrediction2 
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
  
  realTemp2$humidity = realTemp$humidity  * 100
  
  temp = realTemp2$temperatureCelsius
  
  humidity = realTemp2$humidity 
  
  pressure = realTemp2$pressure
  
  res = predict(sistema, realTemp2)$predicted.val 
  rule = function(temp, humidity, pressure, prob){
    presRange = c(980,1010,1020,1047)
    positionPres = min(which(presRange >= pressure))
    vector <- vector(mode="character", length = 3)
    if(positionPres == 2){
      vector[3] = "Poca"
    }else if(positionPres == 3){
      vector[3] = "Algo"
    }else{
      vector[3] = "Mucha"
    }
    
    tempRange = c(-6,11,21,39)
    positiontemp = min(which(tempRange >= temp))
    if(positiontemp == 2){
      vector[1] = "Baja"
    }else if(positiontemp == 3){
      vector[1] = "Media"
    }else{
      vector[1] = "Alta"
    }
    
    humRange = c(16,40,77,100)
    positionHum = min(which(humRange >= humidity))
    if(positionHum == 2){
      vector[2] = "Reducida"
    }else if(positionHum == 3){
      vector[2] = "Intermedia"
    }else{
      vector[2] = "Grande"
    }
    
    if(vector[1] == "Baja"){
      if(vector[2] == "Grande" || vector[2] == "Reducida") vector[3] = "Any"
    }else if(vector[1] == "Media"){
      if(vector[2] == "Reducida") vector[3] = "Any"
    }else{
      if(vector[2] == "Reducida") vector[3] = "Any"
    }
    
    probRange = c(0,40, 70, 100)#Consideramos que una app es "mala" si tiene entre 0 y 20 puntos, a partir de la cual pensamos que deja de ser tam mala, hasta llegar a la valoración de 40
    positionPro = min(which(probRange > prob))
    if(positionPro == 2){
      vector[4] = "Baja"
    }else if(positionPro == 3){
      vector[4] = "Media"
    }else{
      vector[4] = "Alta"
    }
    
    return(vector)
  }
  
  rulesToday = rule(temp,humidity,pressure, res[1])
  rulesToday
  tipoTempUI = paste("Temperatura:",rulesToday[1], sep=" ")
  tipoHumUI = paste("Humedad:", rulesToday[2], sep=" ")
  tipoPressUI = paste("Presión:", rulesToday[3], sep=" ")
  tipoResultUI = paste("Resultado probabilidad:", rulesToday[4], sep=" ")
  
  output$tipoTemp <- renderText({
    tipoTempUI
  })
  output$tipoHum <- renderText({
    tipoHumUI
  })
  output$tipoPress <- renderText({
    tipoPressUI
  })
  output$tipoResult <- renderText({
    tipoResultUI
  })
  
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
    if(res >=0 && res < 35 && currentTime < "18:00:00"){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (res >=35 && res <= 45 && currentTime < "18:00:00"){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (res >45 && res < 65 && currentTime < "18:00:00"){
      return(list(
        src = "images/nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=65 && res <= 100 && currentTime < "18:00:00"){
      return(list(
        src = "images/lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(realTemp2$temperatureCelsius < -2){
      return(list(
        src = "images/nieve.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=0 && res < 35 && currentTime > "18:00:00"){ 
      return(list(
        src = "images/luna.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (res >=35 && res <= 45 && currentTime > "18:00:00"){
      return(list(
        src = "images/luna_nubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (res > 45 && res < 65 && currentTime > "18:00:00"){
      return(list(
        src = "images/noche_lluvia.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if(res >=65 && res <= 100 && currentTime > "18:00:00"){
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
  
  tomorrowTemperature = tomorrowTemp$temperature
  
  tomorrowHumidity = tomorrowTemp$humidity
  
  tomorrowPressure = tomorrowTemp$pressure
  
  Tomorrowres = predict(sistema, tomorrowTemp)$predicted.val
  
  rulesTomorrow = rule(tomorrowTemperature,tomorrowHumidity,tomorrowPressure,Tomorrowres[1])
  
  tipoTempTomorUI = paste("Temperatura:", rulesTomorrow[1], sep=" ")
  tipoHumTomorUI = paste("Humedad:", rulesTomorrow[2], sep=" ")
  tipoPressTomorUI = paste("Presión:", rulesTomorrow[3], sep=" ")
  tipoResulTomortUI = paste("Resultado probabilidad:", rulesTomorrow[4], sep=" ")
  
  output$tipoTempTomor <- renderText({
    tipoTempTomorUI
  })
  output$tipoHumTomor <- renderText({
    tipoHumTomorUI
  })
  output$tipoPressTomor <- renderText({
    tipoPressTomorUI
  })
  output$tipoResultTomor <- renderText({
    tipoResulTomortUI
  })
  
  
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
    if(Tomorrowres >=0 && Tomorrowres < 35){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (Tomorrowres >=35 && Tomorrowres <= 45){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (Tomorrowres > 45 && Tomorrowres < 65){
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
      
    }else if(tomorrowTemp$temperature < -2){
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
  
  day3Temperature = day3Temp$temperature
  
  day3Humidity = day3Temp$humidity
  
  day3pressure = day3Temp$pressure
  
  day3res = predict(sistema, day3Temp)$predicted.val 
  
  rules3Days = rule(day3Temperature,day3Humidity,day3pressure,day3res[1])
  
  tipoTemp3DayUI = paste("Temperatura:", rules3Days[1], sep=" ")
  tipoHum3DayUI = paste("Humedad:", rules3Days[2], sep=" ")
  tipoPress3DayUI = paste("Presión:", rules3Days[3], sep=" ")
  tipoResul3DayUI = paste("Resultado probabilidad:", rules3Days[4], sep=" ")
  
  output$tipoTemp3Day <- renderText({
    tipoTemp3DayUI
  })
  output$tipoHum3Day <- renderText({
    tipoHum3DayUI
  })
  output$tipoPress3Day <- renderText({
    tipoPress3DayUI
  })
  output$tipoResult3Day <- renderText({
    tipoResul3DayUI
  })
  
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
    if(day3res >=0 && day3res < 35){ 
      return(list(
        src = "images/sol.png",
        contentType = "image/png",
        alt = "Face"
      ))
    }else if (day3res >=35 && day3res <= 45){
      return(list(
        src = "images/solNubes.png",
        contentType = "image/png",
        alt = "Face"
      ))
      
    }else if (day3res > 45 && day3res < 65){
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
