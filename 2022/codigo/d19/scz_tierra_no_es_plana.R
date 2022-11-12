##
## Representación de la ciudad de Santa Cruz, SC, Bolivia como centro del globo
##
## Versión adaptada del código original por Ansgar Wolsing (@_ansgar) 
## Código original: https://raw.githubusercontent.com/bydata/30DayMapChallenge-2021/main/R/day28-earth-not-flat.R

library(osmdata)

pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "rnaturalearth")
shape_cgn <- st_read("geodat/scz_limites_2019.shp")
st_crs(shape_cgn) <- "EPSG:4326"
st_centroid(shape_cgn)

world <- ne_countries(returnclass = "sf")


p <- ggplot(world) +
  geom_sf(fill = "cyan", col = NA, alpha = 0.75, size = 0.05) +
  coord_sf(crs = "+proj=laea +y_0=0 +lon_0=-63.182126 +lat_0=-17.783330 +ellps=WGS84 +no_defs") +
  labs(title = "<span style='color:cyan'>SANTA CRUZ,BOLIVIA</span>",
       caption = "Datos: colaboradores **OpenStreetMap** | Visualización: **Eric Armijo**") +
  cowplot::theme_map() +
  theme(plot.background = element_rect(color = NA, fill = "grey9"),
        text = element_text(family = "Montserrat", color = "grey80"),
        plot.title = element_markdown(family = "Bangers", face = "plain",
                                      color = "white", size = 36, hjust = 0.5),
        plot.caption = element_markdown(size = 7, hjust = 0.5)
  )
ggsave(here("salida", "tierra_no_es_plana.png"), plot = p, dpi = 300,
       width = 6, height = 6)



p + geom_point(data = shape_cgn,
               aes(x = st_coordinates(st_centroid(geometry))[, "X"],
                   y = st_coordinates(st_centroid(geometry))[, "Y"]),
               size = 1, col = "white") +
  ggforce::geom_mark_circle(
    data = shape_cgn,
    aes(label = "Santa Cruz",
        fill = "Santa Cruz",
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = "white", col = "white", size = 0.2,
    expand = unit(3, "mm"),
    con.colour = "white", con.cap = unit(1, "mm"),
    label.family = "Montserrat", label.fontsize = 8,
    label.colour = "grey16",
    label.margin = margin(1.5, 1.5, 1.5, 1.5, "mm"),
    label.fontface = "bold", label.buffer = unit(1, "mm"))
ggsave(here("salida", "tierra_no_es_plana_con_scz.png"), dpi = 300,
       width = 6, height = 6)

