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

# Llamar a las capas raster de Chelsa por variable y crear una lista: Temperatura Media Anual (TAS)
path_TAS <- list.files("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/tas",
                          pattern = "*.tif",full.names = TRUE)

TAS_stk <- raster::stack(path_TAS) # Crear un stack de las capas

# Crear el mapa de la CRU a partir del shp
CRU_LL <- rgdal::readOGR("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Climate/Lim_CRU", "CRU_LL")

# Recortar el stack de capas TAS a la CRU y guardarlo
CRU_TAS <- raster::mask(crop(TAS_stk, CRU_LL), CRU_LL)
plot(CRU_TAS[[1]])
CRU_TAS_stk <- stack(CRU_TAS)
stackSave(CRU_TAS_stk, "CRU_TAS_stk")


# Convertir el stack en un tibble y exportarlo como csv
CRU_TAS_tbbl <- na.omit(tabularaster::as_tibble(CRU_TAS_stk, xy=TRUE, dim=TRUE, cell=TRUE, value=TRUE))
CRU_TAS_tbbl %>% 
  relocate(CellID = cellindex, CapaID = dimindex, Long = x, Lat = y, TMd = cellvalue) %>% 
  write.csv("/Users/enriquemartinez/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_TMd.csv")
  


####Create a rute to ggdrive EXAMPLE

install.packages("googledrive") ##Install package to connect drive and rstudio
library(googledrive)

####Getting permision to access mydrive 

mydrive<-drive_find(n_max=10) ##Create an access to my ten more recent files
mydrive #showing my recen files informatio and getting the id of my "data.frame"
id<-"1QZ0yq-KTIVvJz1gkAYF4_y2lEK7zy_-U"  #using the ID of DF
hatched<-read.csv(paste0("https://docs.google.com/uc?id=", id, "&export=download")) #reading my DF in R (depending on the size-the time) 

sd <- shared_drive_get("My drive/PAPIIT_2022/")
drive_upload("~/Library/Mobile Documents/com~apple~CloudDocs/CCGS/Proyectos/PAPIIT/Climate_Analisis/Climate/proof_hatch.csv")

