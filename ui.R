# Nombre: Iván Martín y Alvaro Manjarrés

#En esta clase pintaremos la interfaz, los iconos que se mostrarán dependiendo del tiempo que vaya a hacer
#http://rstudio.github.io/shiny/tutorial/#hello-shiny

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

#Librerías utilizadas para la creación de la interfaz con Shiny
#library(frbs)
#library(ggplot2)
#library(scales)
#library(SmarterPoland)
#library(htmltools)
#library(rlist)
library(shiny)
library(shinydashboard)
#---------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  #incluimos el css donde aplicamos los estilos de la página: colores, estilos, etc
  includeCSS("styles.css"),
  #Creamos una barra en la parte izquierda de nuestra interfaz donde incluimos el 
  #apartado Weather forecast el cual hace referencia a lo que nos compete este proyecto
  sidebarMenu(
    menuItem("Weather Forecast", tabName = "today", icon = icon("dashboard"),badgeLabel = "new", badgeColor = "green"))
  
)

#Creamos el cuerpo de la interfaz. Colocamos las etiquetas, variables e imágenes que 
#se mostrarán por pantalla
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "today",
            h1(textOutput("location")),
            h5(textOutput("accuracy")),
            h2("Current forecast"),
            h5(textOutput("res")),
            h5(textOutput("currentTemp")),
            h5(textOutput("currentHum")),
            br(),
            h4("Etiquetas resultantes: "),
            h5(textOutput("tipoPress")),
            h5(textOutput("tipoTemp")),
            h5(textOutput("tipoHum")),
            h5(textOutput("tipoResult")),
            #imagen del tiempo actual
            imageOutput("image1"),
            #Día 2
            h2("Tomorrow's forecast"),
            h5(textOutput("tomorrowRes")),
            h5(textOutput("tomorrowTemp")),
            h5(textOutput("tomorrowHum")),
            br(),
            h4("Etiquetas resultantes: "),
            h5(textOutput("tipoPressTomor")),
            h5(textOutput("tipoTempTomor")),
            h5(textOutput("tipoHumTomor")),
            h5(textOutput("tipoResultTomor")),
            #imagen de la predicción del tiempo de mañana
            imageOutput("image2"),
            #Día 3
            h2("The day after tomorrow forecast"),
            h5(textOutput("day3Res")),
            h5(textOutput("day3Temp")),
            h5(textOutput("day3Hum")),
            br(),
            h4("Etiquetas resultantes: "),
            h5(textOutput("tipoPress3Day")),
            h5(textOutput("tipoTemp3Day")),
            h5(textOutput("tipoHum3Day")),
            h5(textOutput("tipoResult3Day")),
            #imagen de la prediccion del tiempo de pasado mañana
            imageOutput("image3")
            #)
            #))
    )
  )
)

# El dashboard page es donde colocamos los distintos elementos que componen la UI: 
# la cabecera de la app, el cuerpo y la barra desplegable lateral.
dashboardPage(
  dashboardHeader(title = "Weather forecast App"),
  sidebar,
  body
)

