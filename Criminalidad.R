#Criminalidad en el trasnporte publico en Bogotá
#Esteban Labastidas-Juan Dueñas
#Ultima fecha de modificación:24/11/2023
library(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, RColorBrewer, cowplot, ggpubr,
               ggspatial, rnaturalearth, rnaturalearthdata,dplyr,readxl,stringr,dplyr,cowplot,stringr,ggplot2,fpc,haven,Hmisc)
ruta<- "C:\\Users\\57314\\Documents\\Investigacion\\Criminalidad en transporte publico\\1107  SISTEMA DELICTIVO TRANSPORTE PUBLICO (1).xlsx"
datos<-read_excel(ruta,skip=10)
datos<-datos[-c(71430:71433),]
datos<- datos %>%
  separate(`BARRIOS HECHO`, into = c("nombre_barrio", "numero_barrio"), sep = " E-",remove = FALSE)
table(datos$nombre_barrio)

#barrios --------------------------------------------------------------------------------------------
ruta2<- "C:\\Users\\57314\\Downloads\\estaciones-de-transmilenio\\estaciones-de-transmilenio.shp"
barrios<-st_read(ruta2)
barrios$numero_barrio<-barrios$fid
# Convertir la columna 'numero_barrio' en 'datos' a tipo de dato character
barrios$numero_barrio <- as.character(barrios$numero_barrio)

# Unir los conjuntos de datos por la columna 'numero_barrio'
datos_unidos <- left_join(datos, barrios, by = "numero_barrio")
datos_unidos<-datos_unidos[,-2]
# Agrega una columna para el año
datos_unidos <- datos_unidos %>%
  mutate(anio = format(FECHA_HECHO, "%Y"))

# Agrupa por estación y cuenta el número de delitos
delitos_por_estacion <- datos_unidos %>%
  group_by(numero_barrio) %>%
  summarise(n_delitos = n())

# Delitos por año y estación manteniendo todas las demás variables
delitos_por_estacion_anio <- datos_unidos %>%
  group_by(anio, numero_barrio) %>%
  summarise(n_delitos = n())

estaciones_ano<- left_join(delitos_por_estacion_anio, barrios, by = "numero_barrio")

#Uniendo los datos a estaciones ----------------------------------------------------
file.choose()
rutaestaciones<-"C:\\Users\\57314\\Downloads\\Estaciones_Troncales_de_TRANSMILENIO.csv"
estaciones<-read.csv(rutaestaciones)





#Creando mapas----------------------------------------------------------------------
rutash<-"C:\\Users\\57314\\Downloads\\barriolegalizado\\BarrioLegalizado.shp"
datosbogota<-st_read(rutash)
install.packages("leaflet")
library(leaflet)


# Crea el mapa de Bogotá
mapa_bogota <- leaflet() %>%
  addTiles()

# Agrega las estaciones al mapa
mapa_bogota <- mapa_bogota %>%
  addCircleMarkers(
    data = datosbogota,
    lat = ~Shape_Area,   # Reemplaza con el nombre correcto de la columna de shape_area en 'bogota_sf'
    lng = ~Shape_Leng,   # Reemplaza con el nombre correcto de la columna de shape_leng en 'bogota_sf'
    color = "red",
    fillOpacity = 1,
    radius = 8
  )

# Agrega el mapa de calor de los delitos
mapa_bogota <- mapa_bogota %>%
  addHeatmapOptions(
    data = estaciones_ano,
    lat = ~coord_y,   # Reemplaza con el nombre correcto de la columna de coordenadas Y en 'delitos_por_estacion_anio'
    lng = ~coord_x,   # Reemplaza con el nombre correcto de la columna de coordenadas X en 'delitos_por_estacion_anio'
    intensity = ~n_delitos,  # Reemplaza con el nombre correcto de la columna de intensidad en 'delitos_por_estacion_anio'
    radius = 20
  )

# Muestra el mapa de Bogotá con estaciones y mapa de calor
mapa_bogota







directorio <- getwd()

# Guardar como CSV en el directorio actual
write.csv(estaciones_ano, file.path(directorio, "estaciones_ano.csv"), row.names = FALSE)



#datos
library(writexl)
library(haven)
write_xlsx(datos_unidos)
ruta_dta <- file.path(getwd(), "datos_unidos.dta")
write_dta(datos_unidos, ruta_dta)

