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
path_TAS <- list.files("/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Geodatos/Mundo/Chelsa_v21_month/tas",
                          pattern = "*.tif",full.names = TRUE)

TAS_stk <- raster::stack(path_TAS) # Crear un stack de las capas

# Crear el mapa de la CRU a partir del shp
CRU_LL <- rgdal::readOGR("/Users/enriquemm/Documents/GitHub/Climate/Lim_CRU")

# Recortar el stack de capas TAS a la CRU
CRU_TAS <- raster::mask(crop(TAS_stk, CRU_LL), CRU_LL)
plot(CRU_TAS[[1]])
CRU_TAS_Cels <- round((CRU_TAS/10)-273.15, 2)
plot(CRU_TAS_Cels[[1]])
CRU_TAS_stk <- stack(CRU_TAS_Cels)

# Generar el csv del índice de capas del stack
CRU_capasID <- as_tibble(names(CRU_TAS_stk)) %>%
  mutate(dimindex = row_number()) %>%
  relocate(dimindex, Capa = value)
write_csv(CRU_capasID, "/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_capasID.csv")

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
write_csv(CRU_SerT_TMd, "/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_SerT_Tmd.csv")

# Generar el csv del índice de coordenadas
CRU_Coordenadas <- CRU_TMd_Completa %>%
  filter(Anho == 1980, Mes == 1) %>%
  select(CellID, Long, Lat)
write_csv(CRU_Coordenadas, "/Users/enriquemm/Library/CloudStorage/GoogleDrive-emm@st.ib.unam.mx/Mi unidad/Proyectos/PAPIIT2022_CC_CRU/Analisis/Clima/Archivos_grandes/CRU_Coordenadas.csv")







####Create a rute to ggdrive EXAMPLE
install.packages("googledrive") ##Install package to connect drive and rstudio
library(googledrive)
#Accessing to CRU_TMd.csv file in shared folder in EMM drive
drive_ls("Analisis/Clima/Archivos_grandes") #giving permission to the folder "Archivos_grandes"
id_tmd<-"1w4Aw4X5cv7XQIwZKMGcz6qBKq1kKlCJA" #using the "<drv_id>" of CRU_TMd.csv
tas_tmd<-read.csv(paste0("https://docs.google.com/uc?id=", id_tmd, "&export=download"), header =
                    FALSE, quote = "", sep = '\t') #reading the file in my computer
x<-as_id("https://drive.google.com/file/d/1w4Aw4X5cv7XQIwZKMGcz6qBKq1kKlCJA/view?
usp=share_link")
tas_tmd<-drive_read_raw(x)
str(tas_tmd)
#For upload heavy databases
##drive_upload("~/Library/Mobile
Documents/com~apple~CloudDocs/CCGS/Proyectos/PAPIIT/Climate_Analisis/Climate/proof_hatch.csv")

####Create a rute to ggdrive EXAMPLE

install.packages("googledrive") ##Install package to connect drive and rstudio
library(googledrive)

#Accessing to CRU_TMd.csv file in shared folder in EMM drive
drive_ls("Analisis/Clima/Archivos_grandes") ##giving permission to the folder "Archivos_grandes"
id_tmd<-"1w4Aw4X5cv7XQIwZKMGcz6qBKq1kKlCJA" #using the "<drv_id>" of CRU_TMd.csv
tas_tmd<-read.csv(paste0("https://docs.google.com/uc?id=", id_tmd, "&export=download"), header = FALSE, quote = "", sep = '\t') #reading the file in my computer

x<-as_id("https://drive.google.com/file/d/1w4Aw4X5cv7XQIwZKMGcz6qBKq1kKlCJA/view?usp=share_link")
tas_tmd<-drive_read_string(x, type="csv")
tas_tmd %>%
  drive_read_string() %>%
  read.csv(text = .)


temp <- tempfile(fileext = ".zip")
download.file("https://drive.google.com/file/d/1w4Aw4X5cv7XQIwZKMGcz6qBKq1kKlCJA/view?usp=share_link",
              temp)
out <- unzip(temp, exdir = tempdir())
tas <- read.csv(out[14], sep = ";")
str(tas)

temp <- tempfile(fileext = ".zip")
dl <- drive_download(
  as_id("1w4Aw4X5cv7XQIwZKMGcz6qBKq1kKlCJA"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
bank <- read.csv(out[14], sep = ";")
str(bank)

#For upload heavy databases
##drive_upload("~/Library/Mobile Documents/com~apple~CloudDocs/CCGS/Proyectos/PAPIIT/Climate_Analisis/Climate/proof_hatch.csv")
