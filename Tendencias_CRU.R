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


# Compute the 90th percentile of temperatures
temp_90 <- quantile(CRU_Tmd$TMd, 0.9)

# Compute the number of extreme days each year
TMd_extreme <- CRU_Tmd %>%
  group_by(Anho) %>%
  summarize(num_extreme = sum(TMd >= temp_90)) 

# Plot the number of extreme days by year
p1<-ggplot(TMd_extreme, aes(x = Anho, y = num_extreme, color=num_extreme)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Events of Extreme Temperature (1980-2018)",
       x = "Year", y = "Number of Cells with extreme temperature")
p1+scale_color_gradient(low="blue", high="red")

#Spatial analysis

# Plot the temperature data on a map
tm_shape(CRU_Tmd_sf) +
  tm_dots(col = "TMd", palette = "Blues",
          title = "Temperature (°C)", size = 0.1) +
  tm_scale_bar()


