## 1.- Instalando los paquetes requeridos

install.packages("reticulate")
-
library(reticulate)
library(dplyr)
library(lubridate)
library(glue)
library (satin)

##NOTA: Sólo en caso de presentarse algún durante la instalación de miniconda en R, debes ejecutar el siguiente par de lineas de código
reticulate::miniconda_uninstall()
reticulate::install_miniconda(force = TRUE)

## 2.- Creando el entorno virtual de conda en R sin entrar a la terminal

conda_create(envname = "CopernicusMarine", python_version = "3.12")

# Posteriormente deberás crear el entorno virtual que se conectará a CopernicusMarine con Python versión 3.12
reticulate::conda_create(envname = "CopernicusMarine", python_version = "3.12")

# Instalando la librería 'copernicusmarine' dentro del entorno que hemos creado
reticulate::conda_install(envname = "CopernicusMarine", packages = "copernicusmarine", pip = TRUE)

# Activando el entorno de conda para verificar que se hayan importado los datos de la Temperatura Superficial de Mar (TSM) de Copernicus para la región ecuatuatorial del proyecto
use_condaenv("CopernicusMarine", required = TRUE)


# Importando el módulo de Python a R
cmt <- import("copernicusmarine")

# 3.-Realizar la autentificación en Coperninus Marine Service
## Configura estas variables de entorno con tus credenciales antes de ejecutar el script.
usuario_copernicus <- Sys.getenv("COPERNICUS_USERNAME")
password_copernicus <- Sys.getenv("COPERNICUS_PASSWORD")

if (!nzchar(usuario_copernicus) || !nzchar(password_copernicus)) {
  stop("Define COPERNICUS_USERNAME y COPERNICUS_PASSWORD en las variables de entorno antes de continuar.")
}

# 4.-Verifica que las variables de entorno estén configuradas antes de continuar

cmt$login(usuario_copernicus, password_copernicus)

# 5.-Definición de parámetros para el anális de la variable de interés

dataset_id <- "cmems_mod_glo_phy_my_0.083deg_P1D-m"

##Nota: La Variable oceanográfica "thetao" corresponde a la temperatura potencial del mar en Copernicus
variables <- c("TSM","TSM_uncertainty", "flags")
variables <- list("thetao")

# 6.- Creación de la carpeta de salida 
  output_dir <- file.path("./data/_Copernicus") 
  
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 7.-Definición de límites geográficos y temporales según el proyecto durante el periodo de 1993 al 2020 
# (Coordenadas ajustadas al área de estudio entro de Copernicus)

min_lon <- -91.0
max_lon <- -78.0
min_lat <- 1.0
max_lat <- 25
minimum_depth <- 0.1
maximim_depth <-1
fecha_inicio <- "1993-01-01"
fecha_fin <- "2020-12-31"

## 8.- ##Ejecución automatizada del Subset para la descarga del archivo que contiene los datos (Bucle de descarga) 

library(glue)

fname <- "./data/_Copernicus/thetao_cmems_mod_glo_phy_my_0.083deg_P1D-m_1795042169825.nc"

if (file.exists(fname)) {
  file.remove(fname)
}

cmt$subset(
  dataset_id = dataset_id,
  variables = list("thetao"),
  minimum_longitude = -91,
  maximum_longitude = -78,
  minimum_latitude = 1,
  maximum_latitude = 25,
  minimum_depth = 0,
  maximum_depth = 1,
  start_datetime = fecha_inicio,
  end_datetime = fecha_fin,
  output_filename = fname
)

## 9.- Cargando el paquete satin y las librerias necesarias para calcular el promedio 
## de los datos oceánicos según un período de tiempo seleccionado en el proyecto (1993-2020)

library(satin)
library(terra)
library(ncdf4)

# 10.- Explorando el raster de temperatura superficial del mar (TSM) durante el periodo de estudio
sst <- rast(
  "./data/_Copernicus/thetao_cmems_mod_glo_phy_my_0.083deg_P1D-m_1795042169825.nc")

sst


# Cerrar la conexión con el archivo NetCDF creado usando el paquete 
nc_close(nc)


# Inspeccionado los datos de la variable oceánica en el área de estudio
nc <- nc_open("./data/_Copernicus/thetao_cmems_mod_glo_phy_my_0.083deg_P1D-m_1795042169825.nc")
print(nc)

# 11.- Verificando la profundidad exacta de los datos de temperatura generados
depth <- ncdf4::ncvar_get(nc, "depth")
depth

# 12.- Explorando las fechas disponibles para realizar un análisis preliminar de la temperatura superficial del área de estudio a través del tiempo (1993-2020)

time(sst)

head(time(sst))

tail(time(sst))

