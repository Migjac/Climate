library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(lubridate)
library(multcomp)

# Leer el csv de TMd
CRU_Tmd <- read_csv("/Users/enriquemm/Documents/Copenhague/CRU/Archivos_grandes/CRU_SerT_TMd.csv",
                    col_names = T, cols(CellID = "i", Anho = "i", Mes = "f", TMd = "d"))
CRU_Tmd_toda <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/CCGS/Proyectos/PAPIIT/Análisis/CRU_TMd_toda.csv" )#Miguel
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

###Analyze regions
##Load regions
CRU_coord_region<-CRU_coord_region[,-c(4,5)]
CRU_Tmd_region<-merge(x=CRU_Tmd,y=CRU_coord_region, by="CellID", all.x=TRUE)
CRU_Tmd_region<-CRU_Tmd_region[,-c(9,10)]

# Extract the region, year, and temperature from the data
temp_region <- CRU_Tmd_region %>%
  select(Nombre, Anho, TMd)

# Compute the mean temperature for each region and year
temp_region_mean <- temp_region %>%
  group_by(Nombre, Anho) %>%
  summarize(mean_temp = mean(TMd))

# Compute the average temperature for each region over the entire period
temp_region_avg <- temp_region_mean %>%
  group_by(Nombre) %>%
  summarize(avg_temp = mean(mean_temp))

# Compute the temperature change for each region over the period
temp_region_change <- temp_region_avg %>%
  mutate(temp_change = (avg_temp - avg_temp[1]) / avg_temp[1] * 100)

# Create a bar plot of the temperature change for each region
p<-ggplot(temp_region_change, aes(x = reorder(Nombre, temp_change), y = temp_change)) +
  geom_bar(stat = "identity", fill = "#1F78B4") +
  labs(x = "Region", y = "Temperature Change (%)",
       title = "Temperature Change by Region (1980-2018)") +
  theme_bw() +
  coord_flip()

# Create a line plot of temperature change over time for each region
p1<-ggplot(temp_region_mean, aes(x = Anho, y = mean_temp, color = Nombre)) +
  geom_line() +
  scale_x_continuous(limits = c(1980, 2018), breaks = seq(1980, 2018, by = 4)) +
  labs(x = "Year", y = "Temperature (°C)",
       title = "Temperature Change over Time by Region (1980-2018)") +
  theme_bw()

# Add a linear regression line to each region's plot
p1+geom_smooth(method = "lm", se = TRUE, aes(group = Nombre, colour = "Mean Temperature (lm)"))

# Get the slopes of the linear regression lines
slopes <- coef(summary(lm(mean_temp ~  Nombre, data = temp_region_mean)))

# Fit linear regression models for each region
model_region1 <- lm(mean_temp ~ Anho, data = filter(temp_region_mean, Nombre == "Cuenca Baja"))
model_region2 <- lm(mean_temp ~ Anho, data = filter(temp_region_mean, Nombre == "Cuenca Media"))
model_region3 <- lm(mean_temp ~ Anho, data = filter(temp_region_mean, Nombre == "Cuenca Alta"))

# Test the hypothesis that the slopes are equal across all regions
anova(model_region1, model_region2, model_region3)

# Perform pairwise comparisons using Tukey's HSD method
temp_region_mean$Anho<-as.factor(temp_region_mean$Anho)
temp_region_mean$Nombre<-as.factor(temp_region_mean$Nombre)

tukey_results <- TukeyHSD(aov(mean_temp ~ Nombre + Anho, data = temp_region_mean))

# Obtain p-values for the pairwise comparisons
glht_results <- glht(aov(mean_temp ~ Nombre + Anho, data = temp_region_mean),
                     linfct = mcp(Nombre = "Tukey"))

# Print the results
summary(glht_results)

