# install.packages("readxl")
library(readxl)
getwd()

# Analysis de datos GBIF --------------------------------------------------

# importar base de datos gbif
datos_gbif <- read_excel(
  "data/GBIF_Colon_Coco_2026_TableToExcel original descargada de Gbif.xlsx")

head(datos_gbif)
str(datos_gbif)
dim(datos_gbif)
summary(datos_gbif)
ncol(datos_gbif)

# analizar cuantas columnas vacias existen
colSums(is.na(datos_gbif))

columnas_vacias <- names(datos_gbif)[colSums(is.na(datos_gbif)) == nrow(datos_gbif)]
columnas_vacias
length(columnas_vacias)

# eliminar las columnas totalmente vacias
datos_gbif_clean <- datos_gbif[
  , colSums(!is.na(datos_gbif)) > 0
]

length(datos_gbif_clean)
head(datos_gbif_clean)

# Seleccionar las variables de interés
datos_gbif_final <- datos_gbif_clean[
  , c("OBJECTID", "eventDate", "year", "month",
      "day","decimalLatitude", "decimalLongitude",
      "scientificName")
]

# Visualizar los primeros registros
head(datos_gbif_final)

# Verificar la estructura
str(datos_gbif_final)

library(dplyr)

datos_gbif_final <- datos_gbif_final %>%
  rename(
    id = OBJECTID,
    latitud = decimalLatitude,
    longitud = decimalLongitude
  )

# Exportar la base depurada
write.csv(
  datos_gbif_final,
  "datos_gbif_final.csv",
  row.names = FALSE
)

# crear nueva eventDate a partir de year, month y day ---------------------
library(lubridate)

datos_gbif_final <- datos_gbif_final %>%
  mutate(
    eventDate_new = make_date(
      year = as.integer(year),
      month = as.integer(month),
      day = as.integer(day)
    )
  )
# verificar las fechas
head(datos_gbif_final %>%
       select(eventDate, year, month, day, eventDate_new), 15)

# Número de fechas faltantes
sum(is.na(datos_gbif_final$eventDate_new))

# Porcentaje de fechas faltantes
mean(is.na(datos_gbif_final$eventDate_new)) * 100

# Contar registros sin año, mes o día
datos_gbif_final %>%
  summarise(
    sin_year = sum(is.na(year)),
    sin_month = sum(is.na(month)),
    sin_day = sum(is.na(day))
  )

# Identificar filas donde ambas variables son NA
filas_na <- datos_gbif_final %>%
  filter(is.na(eventDate_new) & is.na(eventDate))

# Visualizar registros
#View(filas_na)

# Contar registros
nrow(filas_na)

#filtrar las filas con NA 
datos_gbif_final <- datos_gbif_final %>%
  filter(!(is.na(eventDate_new) & is.na(eventDate)))

nrow(datos_gbif_final)

datos_gbif_analisis <- datos_gbif_final
str(datos_gbif_analisis)

# Seleccionar las variables de interés
datos_gbif_analisis <- datos_gbif_analisis[
  , c("id", "eventDate_new", "longitud", "latitud",
      "scientificName")
]
#cambio de nombre a la variable date
datos_gbif_analisis <- datos_gbif_analisis %>%
  rename(
    eventDate = eventDate_new
  )
str(datos_gbif_analisis)

#guardar la nueva base de datos
write.csv(
  datos_gbif_analisis,
  "datos_gbif_analisis.csv",
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------------------------------
# analysis por años

resumen_anual_mensual <- datos_gbif_final %>%
  group_by(year, month) %>%
  summarise(
    numero_registros = n(),
    .groups = "drop"
  ) %>%
  arrange(year, month)

resumen_anual_mensual


# -------------------------------------------------------------------------
# numero de meses con 10 datos como minimo

resumen_anual_mensual %>%
  filter(numero_registros > 10) %>%
  arrange(year, month)


# -------------------------------------------------------------------------

ggplot(datos_gbif_final, aes(x = year)) +
  geom_histogram(
    binwidth = 5,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución histórica de registros GBIF",
    x = "Año",
    y = "Frecuencia de registros"
  ) +
  theme_minimal()


# -------------------------------------------------------------------------
# Distribucion geografica de los datos GBIF -------------------------------
# -------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Obtener mapa mundial
mundo <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Filtrar registros con coordenadas válidas
datos_mapa <- datos_gbif_final %>%
  filter(
    !is.na(longitud),
    !is.na(latitud),
    between(longitud, -180, 180),
    between(latitud, -90, 90)
  )

# Crear mapa
ggplot() +
  
  # Fondo oceánico
  theme_minimal() +
  
  # Continentes e islas
  geom_sf(
    data = mundo,
    fill = "gray90",
    color = "gray55",
    linewidth = 0.25
  ) +
  
  # Registros GBIF
  geom_point(
    data = datos_mapa,
    aes(x = longitud, y = latitud),
    color = "#D73027",
    alpha = 0.45,
    size = 1.2
  ) +
  
  # Extensión geográfica
  coord_sf(
    xlim = c(-100, -88),
    ylim = c(-3, 9),
    expand = FALSE
  ) +
  
  labs(
    title = "Distribución geográfica de registros GBIF",
    subtitle = "Registros de ocurrencia en la región de Galápagos",
    x = "Longitud (°)",
    y = "Latitud (°)",
    caption = paste("Total de registros:", nrow(datos_mapa))
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_line(
      color = "gray85",
      linewidth = 0.3
    )
  )

# -------------------------------------------------------------------------
