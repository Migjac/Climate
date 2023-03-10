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

# Llamar a las capas raster de chelsa por variable y crear una lista: Temperatura Media Anual (TAS)
path_TAS <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/tas",
                          pattern = "*.tif",full.names = TRUE)

TAS_stk <- raster::stack(path_TAS) # Crear un stack de las capas

# Crea el mapa de la CRU a partir del shp
CRU_LL <- rgdal::readOGR("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Climate/Lim_CRU", "CRU_LL")

# Recorta el stack de capas TAS a la CRU
CRU_TAS <- raster::mask(crop(TAS_stk, CRU_LL), CRU_LL)
plot(CRU_TAS[[1]])
CRU_TAS_stk <- stack(CRU_TAS)
stackSave(CRU_TAS_stk, "CRU_TAS_stk")

