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

#Comentar después de instalar los paquetes necesarios
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#cran <- getOption("repos")
#cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
#options(repos = cran)
#install.packages("mxnet")

#---------------------------------------------------------------------------

#Definimos el directorio donde tendremos los datasets de imagenes
image_dir <- "C:/Users/usuario/Desktop/DEUSTO/MASTER/IA_AVANZADA/PROYECTO/train"

#instalar paquete
#source("https://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#library(EBImage)

#Cargar foto de bici y comprobar que se visualiza correctamente
example_bike_image <- readImage(file.path(image_dir, "bike.jpg"))
display(example_bike_image)


width <- 28
height <- 28
install.packages("pbapply")
extract_feature <- function(dir_path, width, height, is_bike = TRUE, add_label = TRUE) {
  img_size <- width*height
  ## List images in path
  images_names <- list.files(dir_path)
  if (add_label) {
    ## Select only cats or dogs images
    images_names <- images_names[grepl(ifelse(is_bike, "bike", "notBike"), images_names)]
    ## Set label, cat = 0, dog = 1
    label <- ifelse(is_bike, 0, 1)
  }
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale
    grayimg <- channel(img_resized, "gray")
    ## Get the image as a matrix
    img_matrix <- grayimg@.Data
    ## Coerce to a vector
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    ## Add label
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}

bike_data <- extract_feature(dir_path = image_dir, width = width, height = height)
