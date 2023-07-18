## Procedimiento para recortar las capas de Chelsa a la CRU
# Enrique Martínez Meyer
# 10 de marzo de 2023

paquetes <- c("devtools", # herramientas para desarrolladores
                "raster","leaflet","wicket", # Analisis espacial
                "rgdal","rasterVis","crosstalk",# Analisis espacial
                "maptools","sp","sf","elevatr", # Analisis espacial
                "GISTools","tmap", "spatstat","spdep", # Analisis espacial
                "tmaptools","gstat", # Analisis espacial
                "spocc","dismo","ENMeval","biomod2", # Distribucion de especies
                "ggplot2","rgl","car","plotly", # Graficacion
                "dplyr","stringr","zoo", # manejo de datos
                "readxl","rio", # Lectura de datos
                "popbio", # Demografia
                "matrixStas","Matrix","RSpectra", # Operaciones con matrices
                "animation", # Para hacer animaciones
                "purrr", # Programacion funcional
                "crosstalk", # comunicar mapas, tablas y graficos interactivos
                "rvest",
                "tidyverse") # Paquetes para ciencia de datos
# (purrr,ggplot2, dplyr,readr...)



install.packages(paquetes,
                 repos = "https://cloud.r-project.org/")

library(knitr)
library(rgl)
library(leaflet)
library(maptools)
library(magrittr)
library(plotly)
library(rgeos)
library(sf)
library(tmap)
library(tmaptools)
library(ntbox)
library(raster)
library(rasterVis)
library(rgdal)
library(sp)
library(tabularaster)
library(tidyverse)
library(terra)


# Crear el mapa de la CRU a partir del shp
CRU_LL <- rgdal::readOGR(dsn = "Lim_CRU/", layer = "CRU_LL")

#### Temperatura Media-----------------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa por variable y crear una lista: Temperatura Media Anual (TAS)
path_TAS <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/tas",
                          pattern = "*.tif",full.names = TRUE)

TAS_stk <- raster::stack(path_TAS) # Crear un stack de las capas

# Recortar el stack de capas TAS a la CRU
CRU_TAS <- raster::mask(crop(TAS_stk, CRU_LL), CRU_LL)
plot(CRU_TAS[[1]])
CRU_TAS_Cels <- round((CRU_TAS/10)-273.15, 2)
plot(CRU_TAS_Cels[[1]])
CRU_TAS_stk <- stack(CRU_TAS_Cels) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_capasID <- as_tibble(names(CRU_TAS_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_capasID, "/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_TMd_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_CRU_stk <- as_tibble(names(CRU_TAS_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_TAS_tbbl <- na.omit(tabularaster::as_tibble(CRU_TAS_stk, xy=TRUE, dim=TRUE,
                                                cell=TRUE, value=TRUE))
CRU_TMd_Completa <- CRU_TAS_tbbl %>%
  left_join(capas_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, TMd =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_TMd <- dplyr::select(CRU_TMd_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_TMd, "/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_TMd.csv")



#### Archivo de coordenadas  ---------------------------------------------------------------------------------------------------------------------

# Generar el csv del índice de coordenadas. Este archivo se genera con TMd pero sirve para todas las variables 
CRU_Coordenadas <- CRU_TMd_Completa %>%
  filter(Anho == 1980, Mes == 1) %>%
  select(CellID, Long, Lat)
write_csv(CRU_Coordenadas, "/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Coordenadas.csv")



#### Temperatura Mínima-----------------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa por variable y crear una lista: Temperatura Mínima (TMn)
path_TMn <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/tasmin",
                       pattern = "*.tif",full.names = TRUE)

TMn_stk <- raster::stack(path_TMn) # Crear un stack de las capas

# Recortar el stack de capas TMn a la CRU
CRU_TMn <- raster::mask(crop(TMn_stk, CRU_LL), CRU_LL)
plot(CRU_TMn[[1]])
CRU_TMn_Cels <- round((CRU_TMn/10)-273.15, 2)
plot(CRU_TMn_Cels[[1]])
CRU_TMn_stk <- stack(CRU_TMn_Cels) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_TMn_capasID <- as_tibble(names(CRU_TMn_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_TMn_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_TMn_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_TMn_CRU_stk <- as_tibble(names(CRU_TMn_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_TMn_tbbl <- na.omit(tabularaster::as_tibble(CRU_TMn_stk, xy=TRUE, dim=TRUE,
                                                cell=TRUE, value=TRUE))
CRU_TMn_Completa <- CRU_TMn_tbbl %>%
  left_join(capas_TMn_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, TMn =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_TMn <- dplyr::select(CRU_TMn_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_TMn, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_TMn.csv")



#### Temperatura Máxima-----------------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa por variable y crear una lista: Temperatura Mínima (TMx)
path_TMx <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/tasmax",
                       pattern = "*.tif",full.names = TRUE)

TMx_stk <- raster::stack(path_TMx) # Crear un stack de las capas

# Recortar el stack de capas TMx a la CRU
CRU_TMx <- raster::mask(crop(TMx_stk, CRU_LL), CRU_LL)
plot(CRU_TMx[[1]])
CRU_TMx_Cels <- round((CRU_TMx/10)-273.15, 2)
plot(CRU_TMx_Cels[[1]])
CRU_TMx_stk <- stack(CRU_TMx_Cels) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_TMx_capasID <- as_tibble(names(CRU_TMx_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_TMx_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_TMx_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_TMx_CRU_stk <- as_tibble(names(CRU_TMx_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_TMx_tbbl <- na.omit(tabularaster::as_tibble(CRU_TMx_stk, xy=TRUE, dim=TRUE,
                                                cell=TRUE, value=TRUE))
CRU_TMx_Completa <- CRU_TMx_tbbl %>%
  left_join(capas_TMx_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, TMx =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_TMx <- dplyr::select(CRU_TMx_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_TMx, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_TMx.csv")



##### Precipitación-----------------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa para precipitación y crear una lista: Precipitación (Prec)
path_Prec <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/pr",
                       pattern = "*.tif",full.names = TRUE)

Prec_stk <- raster::stack(path_Prec) # Crear un stack de las capas

# Recortar el stack de capas Prec a la CRU
CRU_Prec <- raster::mask(crop(Prec_stk, CRU_LL), CRU_LL)
plot(CRU_Prec[[1]])
CRU_Prec_stk <- stack(CRU_Prec) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_Prec_capasID <- as_tibble(names(CRU_Prec_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_Prec_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Prec_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_Prec_CRU_stk <- as_tibble(names(CRU_Prec_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_Prec_tbbl <- na.omit(tabularaster::as_tibble(CRU_Prec_stk, xy=TRUE, dim=TRUE,
                                                cell=TRUE, value=TRUE))

CRU_Prec_Completa <- CRU_Prec_tbbl %>%
  left_join(capas_Prec_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, Prec =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_Prec <- dplyr::select(CRU_Prec_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_Prec, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_Prec.csv")



##### Evapotranspiración potencial de Penman-Montheit-----------------------------------------------------------------------

# Llamar a las capas raster de Chelsa para evapotranspiración y crear una lista: Evapotranspiración (PET)
path_PET <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/pet",
                        pattern = "*.tif",full.names = TRUE)

PET_stk <- raster::stack(path_PET) # Crear un stack de las capas

# Recortar el stack de capas PET a la CRU
CRU_PET <- raster::mask(crop(PET_stk, CRU_LL), CRU_LL)
plot(CRU_PET[[1]])
CRU_PET_stk <- stack(CRU_PET) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_PET_capasID <- as_tibble(names(CRU_PET_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_PET_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_PET_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_PET_CRU_stk <- as_tibble(names(CRU_PET_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_PET_tbbl <- na.omit(tabularaster::as_tibble(CRU_PET_stk, xy=TRUE, dim=TRUE,
                                                 cell=TRUE, value=TRUE))

CRU_PET_Completa <- CRU_PET_tbbl %>%
  left_join(capas_PET_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, PET =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_PET <- dplyr::select(CRU_PET_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_PET, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_PET.csv")



##### Humedad relativa-----------------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa para humedad y crear una lista: Humerdad Relativa cerca de la Superficie (Hur)
path_Hur <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/hurs",
                        pattern = "*.tif",full.names = TRUE)

Hur_stk <- raster::stack(path_Hur) # Crear un stack de las capas

# Recortar el stack de capas Hur a la CRU
CRU_Hur <- raster::mask(crop(Hur_stk, CRU_LL), CRU_LL)
plot(CRU_Hur[[1]])
CRU_Hur_stk <- stack(CRU_Hur) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_Hur_capasID <- as_tibble(names(CRU_Hur_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_Hur_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Hur_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_Hur_CRU_stk <- as_tibble(names(CRU_Hur_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_Hur_tbbl <- na.omit(tabularaster::as_tibble(CRU_Hur_stk, xy=TRUE, dim=TRUE,
                                                 cell=TRUE, value=TRUE))

CRU_Hur_Completa <- CRU_Hur_tbbl %>%
  left_join(capas_Hur_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, Hur =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_Hur <- dplyr::select(CRU_Hur_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_Hur, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_Hur.csv")



##### Índice de humedad climática--------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa para el Índice de Humedad Climática (CMI) y crear una lista. Este índice es
# la diferencia entre la cantidad de precipitación y la evapotranspiración potencial
path_CMI <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/cmi",
                        pattern = "*.tif",full.names = TRUE)

CMI_stk <- raster::stack(path_CMI) # Crear un stack de las capas

# Recortar el stack de capas CMI a la CRU
CRU_CMI <- raster::mask(crop(CMI_stk, CRU_LL), CRU_LL)
plot(CRU_CMI[[1]])
CRU_CMI_stk <- stack(CRU_CMI) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_CMI_capasID <- as_tibble(names(CRU_CMI_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_CMI_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_CMI_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_CMI_CRU_stk <- as_tibble(names(CRU_CMI_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_CMI_tbbl <- na.omit(tabularaster::as_tibble(CRU_CMI_stk, xy=TRUE, dim=TRUE,
                                                 cell=TRUE, value=TRUE))

CRU_CMI_Completa <- CRU_CMI_tbbl %>%
  left_join(capas_CMI_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, CMI =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_CMI <- dplyr::select(CRU_CMI_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_CMI, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_CMI.csv")



##### Déficit de presión de vapor--------------------------------------------------------------------------------------------

# Llamar a las capas raster de Chelsa para el Déficit de presión de vapor (DPV) y crear una lista. Este índice es la diferencia
# entre la cantidad de humedad efectiva en el aire y la cantidad máxima de humedad que el aire puede contener a cierta temperatura
path_DPV <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/vpd",
                       pattern = "*.tif",full.names = TRUE)

DPV_stk <- raster::stack(path_DPV) # Crear un stack de las capas

# Recortar el stack de capas DPV a la CRU
CRU_DPV <- raster::mask(crop(DPV_stk, CRU_LL), CRU_LL)
plot(CRU_DPV[[1]])
CRU_DPV_stk <- stack(CRU_DPV) # Se tiene que convertir el brick en stack para los siguientes procesos

# Generar el csv del índice de capas del stack
CRU_DPV_capasID <- as_tibble(names(CRU_DPV_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_DPV_capasID, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_DPV_capasID.csv")

# Crear una tabla separando año y mes del nombre de las capas
capas_DPV_CRU_stk <- as_tibble(names(CRU_DPV_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value) %>%
  separate(Capa, c(NA, NA, "Mes", "Anho", NA), sep = "_", convert = T)

# Convertir el stack en un tibble y unir todos los campos
CRU_DPV_tbbl <- na.omit(tabularaster::as_tibble(CRU_DPV_stk, xy=TRUE, dim=TRUE,
                                                cell=TRUE, value=TRUE))

CRU_DPV_Completa <- CRU_DPV_tbbl %>%
  left_join(capas_DPV_CRU_stk, by = "dimindex") %>%
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, Anho, Mes, DPV =
             cellvalue)

# Seleccionar solo los campos de año, mes y temperatura, exportarlo a csv
CRU_SerT_DPV <- dplyr::select(CRU_DPV_Completa, -CapaID, -Long, -Lat)
write_csv(CRU_SerT_DPV, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_DPV.csv")



##### Elevación--------------------------------------------------------------------------------------------

# Llamar al DEM de la CRU a 90m y convertirlo en raster
CRU_DEM <-raster("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mexico/CRU/CRU_elev90m.tif")
plot(CRU_DEM)

# Llamar al archivo de coordenadas y convertirlo en objeto espacial para pegarle los valores de elevación
CRU_Coord <- read.csv("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Coordenadas.csv")

CRU_Coord_sf <- st_as_sf(CRU_Coord, coords = c("Long", "Lat"), crs = 4326)
plot(CRU_Coord_sf)
st_write(CRU_Coord_sf, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Coordenadas.shp")

CRU_elev_sp <- extract(CRU_DEM, CRU_Coord_sf, method = 'simple', cellnumbers = F, df = T, sp = T)
CRU_elev <- CRU_elev_sp %>% 
  as_tibble() %>%
  relocate(CellID, Elevacion = CRU_elev90m) %>%
  subset(select = -c(3, 4))

write_csv(CRU_elev, "/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Elev.csv")


