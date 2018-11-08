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

#Librerías
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
  
  includeCSS("styles.css"),
  
  sidebarMenu(
    menuItem("Weather Forecast", tabName = "today", icon = icon("dashboard"),badgeLabel = "new", badgeColor = "green"))
  
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "today",
            h1(textOutput("location")),
            h3("Today's forecast"),
            imageOutput("image1"),
            h3("Tomorrow's forecast"),
            imageOutput("image2"),
            h3("The day after tomorrow forecast"),
            imageOutput("image3")
            #)
            #))
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Weather forecast App"),
  sidebar,
  body
)


