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
#---------------------------------------------------------------------------

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Weather forecast"),
  # Sidebar with a slider input for number of observations
  #sidebarPanel(
   # sliderInput("obs", 
    #            "Number of observations:", 
     #           min = 1,
    #            max = 1000, 
   #             value = 500)
  #),
  
  # Show a plot of the generated distribution
  #mainPanel(
  #  plotOutput("distPlot")
  #)
  
))
