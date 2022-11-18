##
## Distribución de población de Bolivia, mapa joyplot
##
## Adaptado de script original por @helenmakesmaps
## Ref: https://www.helenmakesmaps.com/post/how-to-joy-plot
##

# Cargar paquetes
library(ggplot2)
library(ggridges)

# Cargar datos CSV
transects <- read.csv("dat/tab_muestreo_ptos_elevac.csv")

# Cargar los nombres de los campos de datos
head(transects)

# Renombrar campos
names(transects)[1] <- "ID"
names(transects)[2] <- "Elevation"
names(transects)[3] <- "Lon"
names(transects)[4] <- "Lat"
head(transects)

# Un joyplot simple
joy <- ggplot(transects,
              aes(x = Lon, y = Lat, group = Lat, height = Elevation))+
  geom_density_ridges(stat = "identity")
joy


# Ahora un joyplot avanzado
joy <- ggplot(transects,
              aes(x = Lon, y = Lat, group = Lat, height = Elevation))+
  geom_density_ridges(stat = "identity",
                      scale = 200,
                      fill = "black",
                      color = "white")+
  # Personalizar el tema, quitando elementos innecesarios
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank())
joy
