# Descarga de variables ambientales de Copernicus (Elka J. Garcia Rada)


##  Cargar paquetes ----
library(reticulate)
library(dplyr)
library(lubridate)
library(glue)

## Nombre de la base de datos ----
# buscar en CMEMS: https://data.marine.copernicus.eu/products
dataset_id <- "cmems_obs-oc_glo_bgc-plankton_my_l3-olci-300m_P1D" 


## 1. Activar entorno virtual CopernicusMarine ----
 use_condaenv("C:/Users/Hector/AppData/Local/r-miniconda/envs/CopernicusMarine", 
              required = TRUE)
 cmt <- import("copernicusmarine")

 cmt$login("", "")  # login y pwd de Copernicus

## 2. Variables  ----
 #variables <- c("thetao", "so", "mlotst","zos","uo","vo","bottomT")
 variables <- c("CHL","CHL_uncertainty", "flags")


## 3. Carpeta de salida ----
output_dir <- file.path("D:/DataExt/_Copernicus")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


## 4. Bucle de descarga ----
for (var in variables) {
  fname <- glue("{output_dir}/{var}_plankton_my_l3-olci-300m_P1D.nc")
  cat(glue("\n>>> Descargando {var} de {fecha_inicio} a {fecha_fin} en: {fname}\n"))
  try({
    cmt$subset(
      dataset_id = dataset_id,
      variables = list(var),
      minimum_longitude = -111,
      maximum_longitude = -109,
      minimum_latitude = 23,
      maximum_latitude = 25,
      start_datetime = "2025-07-01",
      end_datetime = "2025-07-31",
      output_filename = fname
    )
    cat(glue("Descarga completa para {var}: {fname}\n"))
  }, silent = FALSE)
}

library(satin)
setwd("D:/DataExt/_Copernicus")

chl <- read.cmems("CHL_plankton_my_l3-olci-300m_P1D.nc")
