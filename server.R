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

#Definimos los conjuntos difusos en base a tres columnas, presión, temperatura y humedad. 
presionBaja = c(2,980, 1005, 1012, NA) #Etiqueta de izquierdas. Aquí se ha decidido empezar por 980, ya que es el valor mínimo existente en el dataset del tiempo en madrid
presionMedia = c(4, 1008, 1014, 1018 , 1021) #Etiqueta central. Consideramos una presión media a aquella que está comprendida entre los valores 1008 y 1021.
presionAlta = c(3,1018, 1030, 1047, NA) #Etiqueta derechas. Por último, consideramos que la presión medioambiental empieza a ser "alta" a partir de 1018 hasta 1047, valor máximo existente en el dataset.
presionAny = c(4,980,980,1047,1047) #Definimos una etiqueta general de manera que a la hora de definir las reglas, podeamos globalizarlas con Any

tempBaja = c(2, -6 , 5 , 12 , NA) #Etiqueta de izquierdas para la temperatura. Aquí se ha decidido empezar por -6, ya que es el valor mínimo existente en el dataset del tiempo en madrid
tempMedia = c(4,10, 15, 18, 22) #Etiqueta central. Consideramos una temperatura media a aquella que está comprendida entre los valores 10 y 22.
tempAlta = c(3,20, 26, 39, NA) #Etiqueta derechas. Por último, consideramos que la temperatura medioambiental empieza a ser "alta" a partir de 20 hasta 39, valor máximo existente en el dataset.
tempAny = c(4,-6,-6,39,39) #Definimos una etiqueta general de manera que a la hora de definir las reglas, podeamos globalizarlas con Any

humedadBaja = c(2, 16 , 35 , 45 , NA) #Etiqueta de izquierdas para la humedad Aquí se ha decidido empezar por 16, ya que es el valor mínimo existente en el dataset del tiempo en madrid
humedadMedia = c(4,35, 54, 70, 78) #Etiqueta central. Consideramos una humedad media a aquella que está comprendida entre los valores 35 y 78.
humedadAlta = c(3,75, 88, 100, NA) #Etiqueta derechas. Por último, consideramos que la humedad medioambiental empieza a ser "alta" a partir de 75 hasta 100, valor máximo existente en el dataset.
humedadAny = c(4,16,16,100,100) #Definimos una etiqueta general de manera que a la hora de definir las reglas, podeamos globalizarlas con Any

# Pegamos todas las etiquetas previamente definidas con la funcion cbind por columnas y asignamos a una variable
varinp.mf = cbind(tempBaja,tempMedia,tempAlta,tempAny,humedadBaja,humedadMedia,humedadAlta,humedadAny,presionBaja,presionMedia,presionAlta,presionAny)

# Definimos una matriz con el número de etiquetas de cada entrada, en este caso 4 por cada una de ellas
num.fvalinput = matrix(c(4,4,4), nrow=1)

# Damos nombres a cada una de las etiquetas en un vector.
varinput1 = c("Baja", "Media", "Alta","Any")
varinput2 = c("Reducida","Intermedia","Grande","Any")
varinput3 = c("Poca", "Algo", "Mucha","Any")

# Las juntamos en un vector llamado names.varinput, el cual contendrá todos los nombres para cada una de las etiquetas existentes
names.varinput = c(varinput1, varinput2,varinput3)

# Definimos una matriz en la que colocamos los rangos posibles de los conjuntos de presión, temperatura y humedad
# la ultima de ellas hará referencia a los valores en los que puede estar la solucion final, probabilidad de lluvia entre 0 y 100. 
range.data = matrix(c( -6, 39, 16, 100, 980, 1047, 0, 100), nrow = 2)
#Temp minima historia bilbao: -6, Max: 41
type.defuz = "COG"
type.tnorm = "MIN"
type.snorm = "MAX"
type.implication.func = "MIN"
type.model = "MAMDANI" #Indicamos el tipo de modelo que vamos a usar

# Le damos un nombre para el ejercicio, probabilidad de lluvia
name = "Probabilidad de lluvia"
# Creamos una nueva variable new data que contiene unicamente las columnas con las que haremos la lógica
newdata = weather[,c("Mean.TemperatureC", "Mean.Humidity","Mean.Sea.Level.PressurehPa")]
# De la misma manera que antes, creamos los nombres de las etiquetas
colnames.var = c("Temperatura", "Humedad","Presion", "Resultado")

# Definimos el conjunto difuso para la probabilidad resultante
probBaja = c(2,0, 20, 40, NA) # Etiqueta de izquierdas. Consideramos que una probabilidad de lluvia baja es aquella que está comprendida entre los valores 0% y 40%.
probMedia = c(4,30, 40, 50, 65) # Etiqueta central. Consideramos que una probabilidad de lluvia media es aquella que está comprendida entre los valores 30 % y 65%. A partir de 50%, la probabilidad empieza a dejar de ser media.
probAlta = c(3,60, 85, 100, NA) # Etiqueta de derechas. Consideramos que la probabilidad empieza a ser alta desde 60% a 85%, donde de ahí en adelante hasta 100% (porcentaje máximo) se puede considerar como probabilidad alta

# Definimos los conjuntos de salida junto con sus etiquetas correspondientes. Lo resultante será la probabilidad de lluvia que se dió en cada uno de los días del dataset en Madrid
num.fvaloutput = matrix(c(3), nrow = 1)

varoutput1 = c("Baja", "Media", "Alta") #Indicamos sus nombres

varout.mf = cbind(probBaja,probMedia,probAlta)

#Temperatura: -> Baja, Media, Alta
#Humedad: -> Reducida, Intermedia, Grande
#presion: -> Poca, Algo, Mucha

# Definimos una matriz con las reglas que creemos que causan que la probabilidad de lluvia sea baja, media o alta. Para ello nos hemos basado en diversa información residente en Internet y viendo como cada uno de estos parámetros afecta a las condiciones medioambientales
rule = matrix( c("Baja", "and", "Grande" , "and" , "Any","->", "Alta",
                 
                 "Baja", "and","Intermedia" , "and" , "Poca","->", "Alta",
                 "Baja", "and","Intermedia" , "and" , "Algo","->", "Media",
                 "Baja", "and","Intermedia", "and"  , "Mucha","->", "Media",
                 
                 #Si la temperatura es baja, la humedad es reducida y la presión es cualquiera, poca, algo o mucha, entonces la probabilidad de lluvia será media
                 "Baja", "and","Reducida" , "and" , "Any","->", "Media",
                 
                 "Media", "and","Grande" , "and" , "Poca","->", "Alta",
                 "Media", "and","Grande" , "and" , "Algo","->", "Alta",
                 "Media", "and","Grande" , "and" , "Mucha","->", "Media",
                 
                 "Media", "and","Intermedia", "and"  , "Poca","->", "Alta",
                 "Media", "and","Intermedia" , "and" , "Algo","->", "Media",
                 "Media", "and","Intermedia" , "and" , "Mucha","->", "Media",
                 
                 #Si la temperatura es media, la humedad es reducida y la presión es cualquiera, poca, algo o mucha, entonces la probabilidad de lluvia será media
                 "Media", "and","Reducida"  , "and", "Any","->", "Media",
                 
                 "Alta", "and","Grande", "and"  , "Poca","->", "Media",
                 "Alta", "and","Grande", "and"  , "Algo","->", "Media",
                 "Alta", "and","Grande" , "and" , "Mucha","->", "Baja",
                 
                 "Alta", "and","Intermedia" , "and" , "Poca","->", "Media",
                 "Alta", "and","Intermedia" , "and" , "Algo","->", "Baja",
                 "Alta", "and","Intermedia" , "and" , "Mucha","->", "Baja",
                 
                 #Si la temperatura es alta, la humedad es reducida y la presión es cualquiera, poca, algo o mucha, entonces la probabilidad de lluvia será baja
                 "Alta", "and","Reducida" , "and" , "Any","->", "Baja"
                 
), nrow = 19, byrow = TRUE)

# Utilizamos la función frbs.gen() para crear el sistema difuso
sistema = frbs.gen(range.data, num.fvalinput, names.varinput,
                   num.fvaloutput, varout.mf, varoutput1, rule,
                   varinp.mf, type.model, type.defuz, type.tnorm,
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)

#windows()
#plotMF(sistema)#También, por simple hecho didáctico, mostramos cómo quedan nuestros conjuntos de entrada y el de salida

# Utilizamos la función predict para ponerlo a prueba sobre los datos y predecir la probabilidad de lluvia en cada uno de los días del dataset
res = predict(sistema, newdata)$predicted.val 

# En el vector new data introducimos una nueva columna con el resultado de la predicción
newdata$probabilidad = res

# Generamos una columna id en el vector new data para mergearlo con la tabla weather recogida al principio. De esta manera, será más facil unirlas 
newdata$id = seq.int(nrow(weather))

# Unimos ambas tablas por su id, de esta manera, tenemos una tabla result donde tendremos la información parseada del dataset junto con la probabilidad de elluvia para cada uno de los días
result = merge(weather, newdata, by.x = 'Id', by.y = 'id')

result = result[,c(1,2,3,4,5,6,7,8,12)]

# Obtenemos el portentaje de aciertode nuestra predicción
# Si el valor de la columna rain que nos proporciona el dataset es mayor que cero, es decir, ha llovido o el evento que dispone el dataset no es nulo, entonces si la probabilidad de lluvia es mayor que 50%, damos por buena nuestra predicción
# Por otro lado, si el valor de rain proporcionado por el dataset no es mayor que cero, es decir ,no ha llovido y el evento es nulo, entonces si nuestra predicción es menos que 50, damos por válida la predicción
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
  # Calculamos la precisión dividiendo los aciertos entre el número de filas totales del dataset y multiplicandolo por 100 para obtener el porcentaje
  precision = (right / nrow(result) ) * 100
  return(round(precision,2))
}

#Creamos las variables que serán pasadas a la parte cliente para su visulización, en este caso, queremos mostrar una etiqueta de esta manera: Accuracy: 80%
accuracyOfPrediction = paste("Accuracy:", accuracy(result), sep=" ")
accuracyOfPrediction2 = paste(accuracyOfPrediction,"%", sep=" ")
# Hasta aquí la predicción de la probabilidad de lluvia del dataset de Madrid

# ----------------------------------------------------------------------------------

#Servidor de shiny mediante el cual pasaremos la info a ui.r y lo visualizamos
#A partir de aquí, lo de la predicción actual
shinyServer(function(input, output) {
  # Obtenemos la apiKey
  names(unlist(APIKEY))
  # Definimos la localización donde aplicaremos la predicción del tiempo futuro
  location = 'Madrid'
  
  #Coordenadas para saber el tiempo en madrid, las tendremos que pasar más adelante
  long = -3.6827461
  lat = 40.4893538 
  #establecemos las fechas de mañana y pasado para realizar las predicciones
  tomorrowDate = paste(Sys.Date()+1, "12:00:00-0400", sep="T")
  theDayAfterTomorrowDate = paste(Sys.Date()+2, "12:00:00-0400", sep="T")
  
  # Creamos una variable para usarla en la UI con el valor location
  output$location <- renderText({
    location 
  })
  # Creamos una variable para usarla en la UI con el valor del acierto en nuestra predicción
  output$accuracy <- renderText({
    accuracyOfPrediction2 
  })
  
  # Llamamos a la API correspondiente para obtener los valores del tiempo en este momento en Madrid y lo guardamos en la variable forecast
  forecast = getWeatherForecast(APIKEY, city=location)
  
  realTemp = forecast[[1]]
  # Obtenemos del dataset la hora actual para dibujar una luna en caso de que sea de noche
  currentTime = realTemp$time;
  # Hacemos un substring para guardar en currentTime la hora actual
  currentTime = substr(currentTime, 12, 19)
  
  #Borramos las columnas que no vayamos a utilizar, nos quedamos con la presión, temperatura y humedad
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
  
  # Creamos una nueva matriz únicamente con los valores de las tres columnas que vayamos a necesitar
  realTemp2 = realTemp[,c("temperatureCelsius","humidity","pressure")]
  
  # El valor de la humedad que retorna la API para el instante actual se encuentra en valor decimal, por lo que será necesario multiplicarlo por 100
  realTemp2$humidity = realTemp$humidity  * 100

  # Guardamos en una variable temp la temperatura actual de madrid
  temp = realTemp2$temperatureCelsius
  
  # Guardamos en una variable humidity la humedad actual de madrid
  humidity = realTemp2$humidity 
  
  # Guardamos en una variable pressure la presion actual de madrid
  pressure = realTemp2$pressure
  
  # Llamamos a la función predict definida anteriormente para predecir la probabilidad de lluvia en este mismo instante
  res = predict(sistema, realTemp2)$predicted.val 
  
  # La función rule nos obtendrá las etiquetas correspondientes para temperatura, humedad y presión para los valores que nos devuelve la API en este mismo instante. 
  # De la misma manera, obtenemos el nombre de la etiqueta resultante aplicado en las reglas que hemos definido en pasos anteriores.
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
    
    probRange = c(0,40, 70, 100) # Consideramos el rango de probabilidad para obtener su etiqueta resultante
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
  
  # Guardamos las 4 etiquetas que hemos obtenido gracias a la función anterior en una variable rulesToday
  rulesToday = rule(temp,humidity,pressure, res[1])
  
  # Crearemos unas variables que contendrán los nombres de las etiquetas que hayan sido utilizadas y las mostramos en la interfaz
  tipoTempUI = paste("Temperatura:",rulesToday[1], sep=" ")
  tipoHumUI = paste("Humedad:", rulesToday[2], sep=" ")
  tipoPressUI = paste("Presión:", rulesToday[3], sep=" ")
  tipoResultUI = paste("Resultado probabilidad:", rulesToday[4], sep=" ")
  
  # Referenciamos las variables anteriores con las variables que se emplearán en la UI
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
  
  # Redondeamos el resultado de la prediccion, temperatura y humedad y los guardamos en distintas variables
  res = round(res,digits=0)
  realTemp2$humidity = round(realTemp2$humidity,digits=0)
  realTemp2$temperatureCelsius = round(realTemp2$temperatureCelsius,digits=0)
  
  #Indicamos los textos que queremos volcar a la interfaz junto con los valores de la temperatura, humedad,  presión y resultado
  Currentprobability = paste("Precipitations:",res, sep=" ")
  Currentprobability = paste(Currentprobability,"%", sep="")
  
  CurrentTemperature = paste("Temperature:",realTemp2$temperatureCelsius, sep=" ")
  CurrentTemperature = paste(CurrentTemperature,"º", sep="")
  
  CurrentHumidity = paste("Humidity:",realTemp2$humidity, sep=" ")
  CurrentHumidity = paste(CurrentHumidity,"%", sep="")
  
  # Hacemos el render para poder usar dichas variables en la parte cliente
  output$res <- renderText({
    Currentprobability
  })
  output$currentTemp <- renderText({
    CurrentTemperature
  })
  output$currentHum <- renderText({
    CurrentHumidity
  })
  
  # En este paso vamos a elegir las imagenes que queremos mostrar dependiendo del valor de la probabilidad obtenida y el horario ya que si son más de las 18:00, consideramos que es de noche y los iconos serán distintos.
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
  # Hacemos la llamada a la API para obtener la predicción de lluvia en el día de mañana. Para ello es necesario pasarle la latitud y longitud que habiamos definido previamente y la localización donde se aplicará dicha predicción 
  dailyForecast = get_forecast_for(lat,long, tomorrowDate)
  day2 = dailyForecast[[2]]
  
  tomorrowTemp = day2[,c("temperatureMin", "temperatureMax", "humidity","pressure")]
  
  # Como en la parte superior hemos realizado, la humedad está en decimal y la tenemos que multiplicar por 100
  tomorrowTemp$humidity = day2$humidity * 100
  
  # La temperatura que obtenemos está en fahrenheit y la tenemos que pasar a celsius
  tomorrowTemp$temperatureMin = fahrenheit.to.celsius(day2$temperatureMin)
  tomorrowTemp$temperatureMax = fahrenheit.to.celsius(day2$temperatureMax)
  
  # El resto de procedimientos son los mismos que los realizados en el paso anterior para la current prediction 
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
