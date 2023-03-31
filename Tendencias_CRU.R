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

#####HEAD
# Generar un tibble para análisis por pixel
TMd_pixel_mes <- CRU_Tmd %>%
  group_by(CellID, Mes) %>%
  summarise(TMd_dif_inifin = last(TMd) - first(TMd), TMd_rango = max(TMd)-min(TMd))
TMd_pixel_mes$Mes<-as.factor(TMd_pixel_mes$Mes)

ggplot (TMd_pixel_mes, aes(x = Mes, y = TMd_dif_inifin)) +
  geom_boxplot(aes(group = Mes)) +
  labs(x = "Mes", y = "Temperatura media")

ggplot (TMd_pixel_mes, aes(x = Mes, y = TMd_rango)) +
  geom_boxplot(aes(group = Mes)) +
  labs(x = "Mes", y = "Temperatura media")

# Tibble a nivel de pixel por año
TMd_pixel_año <- CRU_Tmd %>%
  group_by(CellID, Anho) %>%
  summarise(TMd_año = mean(TMd))
#=======

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
  labs(title = "Cells with Extreme Temperature (1980-2018)",
       x = "Year", y = "Number of Cells with extreme temperature")
p1+scale_color_gradient(low="blue", high="red")
#>>>>>>> de165caf3567d4334caf9da05e4614debc8d2c19

##Visualizing
ggplot(data = CRU_Tmd, aes(x = Anho, y = TMd)) +
  geom_line(aes(color = as.factor(Mes))) +
  facet_grid(rows = vars(Lat), cols = vars(Long)) +
  labs(title = "Temperature Data by Coordinates and Time",
       x = "Year", y = "Temperature (°C)", color = "Mes")

# Analyze relationship
correlation_matrix <- cor(CRU_Tmd[,c("Lat", "Long", "TMd")])
print(correlation_matrix)


