##
## Vista aérea minimalista de una ciudad usando datos de OpenStreetMap
## Apoyado con R City Views (https://github.com/koenderks/rcityviews)
## un paquete R y una aplicación web Shiny. 
##

# Librería
remotes::install_github("koenderks/rcityviews", dependencies = TRUE)

library(rcityviews)

# Buscar un nombre de ciudad en la base de datos (Montero, SC, Bolivia)
list_cities(match = "Montero")

# Ver la ciudad
p <- cityview(name = "Montero", crop = "circle")

# Guardar la imagen resultante
ggplot2::ggsave(filename = "montero.png", plot = p, height = 500, width = 500, units = "mm", dpi = 300)
