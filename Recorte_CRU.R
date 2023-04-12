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



####--------------------------------------------------------------------------------------------------------------------------

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

