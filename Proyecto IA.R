# Nombre: Iván Martín y Alvaro Manjarrés

# Con los siguientes dos comandos limpiamos el entorno de trabajo y la consola
rm(list = ls())
cat("\014")

# Otro punto es establecer el working directory, con este comando hacemos que 
# Ã©ste sea igual a aquÃ©l en el que se encuentra el script
# Si es tu 1Âª vez con R, tendrÃ¡s que instalar el paquete rstudioapi
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Comprobamos que estÃ¡ correcto
#getwd()


#---------------------------------------------------------------------------



