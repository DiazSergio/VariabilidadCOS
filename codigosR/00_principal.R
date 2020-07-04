### Cargar librerias a utiizar
library(jsonlite)
library(aqp)
library(ithir)
library(sp)
library(lattice)
library(cluster)
library(sharpshootR)
library(sf)
library(gstat)
library(raster)
library(rgdal)
library(tmap)

# Cambiar directorio de trabajo, donde se encuentran los datos para trabajar
setwd( "D:/Datos")

#cargar y leer los archivos a utilizar
aoi <- shapefile("areaEstudioUTM.shp") # Se carga la zona de estudio, shapefile en UTM
json <- "perfilesSISLAC.json"          # Perfiles en formato JSON
json <- fromJSON(json, flatten = TRUE)$features

### OBJETIVO ESPECIFICO 1
# validacion de perfiles con error y su exclusion del archivo JSON
errores <- f0101_validarErrores(json)
json <- f0102_removerErrores(json,errores)
# Validacion de inconsistencias corregibles en los horizontes
inconsistencias <- f0103_validarInconsistencias(json)
# Corregir inconsistencias y armar objeto SoilProfileCOllection
spc <- f0105_crearSPC(json, inconsistencias)


### OBJETIVO ESPECIFICO 2
spc2 <- f0201_sliceAdaptado(spc)


### OBJETIVO ESPECIFICO 3
spc1 <- f0301_sliceAQP(spc)             # Segmentacion del spc para COS
agregacion <- f0302_agregacionAQP(spc1) # O spc2, para los valores ajustados
f0303_plotearAgregacion(agregacion)     # 


### OBJETIVO ESPECIFICO 4
interpolacion <- f0401_interpolacionIDW(spc1, aoi) # O spc2, para los valores ajustados
