library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(lubridate)

# Leer el csv de TMd
CRU_Tmd <- read_csv("/Users/enriquemm/Documents/Copenhague/CRU/Archivos_grandes/CRU_SerT_Tmd.csv",
                    col_names = T, cols(CellID = "i", Anho = "i", Mes = "f", TMd = "d"))
CRU_Tmd_toda <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/CCGS/Proyectos/PAPIIT/Análisis/CRU_Tmd_toda.csv" )#Miguel
CRU_Tmd<-CRU_Tmd_toda

# Generar un tibble con los promedios anuales y graficarlos
TMd_anual <- CRU_Tmd %>%
  group_by(Anho) %>%
  summarise(TMd_anual = mean(TMd)) 

ggplot(TMd_anual, aes(x = Anho, y = TMd_anual)) +
  geom_point() +
  geom_smooth(se = T, method = "lm") +
  labs(x = "Año", y = "Temperatura media")


# Generar un tibble con los promedios mensuales por años y graficarlos
TMd_mensual <- CRU_Tmd %>%
  group_by(Mes, Anho) %>%
  summarise(TMd_mensual = mean(TMd)) 

ggplot(TMd_mensual, aes(x = Anho, y = TMd_mensual)) +
  geom_smooth(aes(color = Mes), se = T, method = "lm") +
  labs(x = "Año", y = "Temperatura media")

ggplot(TMd_mensual, aes(x = Anho, y = TMd_mensual)) +
  geom_point() +
  geom_smooth(color = "red", method = "loess") +
  facet_wrap(~ Mes) +
  labs(x = "Año", y = "Temperatura media")

ggplot (TMd_mensual, aes(x = Anho, y = TMd_mensual)) +
  geom_boxplot(aes(group = Anho)) +
  labs(x = "Año", y = "Temperatura media")

# Plot the mean temperature over time
ggplot(TMd_mensual, aes(x = Anho, y = TMd_mensual)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mean Temperature by Decade (1980-2018)",
       x = "Año", y = "Mean Temperature (°C)")

yr <- as.Date(as.character(CRU_Tmd$Anho), format = "%Y")
y <- year(yr)
CRU_Tmd["year"]<-y

year <- CRU_Tmd %>%
  mutate(year = as.Date(ISOdate(year, 1, 1)) , #first, convert the year into a date
         year = format(year,'%Y'))    


#Spatial analysis
# Convert the data to a spatial object (assuming the data contains latitude and longitude columns)
CRU_Tmd_sf <- st_as_sf(CRU_Tmd, coords = c("Long", "Lat"))

# Plot the temperature data on a map
tm_shape(CRU_Tmd_sf) +
  tm_dots(col = "TMd", palette = "Blues",
          title = "Temperature (°C)", size = 0.1) +
  tm_scale_bar()

ggplot(CRU_Tmd, aes(x = year, y = TMd)) +
  geom_line(aes(group = 1, color = factor(TMd)))  +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")

