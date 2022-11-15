##
## Distancia a centros de salud (3er nivel) en Bolivia
##
## Código adaptado de script original: https://github.com/HudsonJamie/tidy_tuesday/blob/main/2021/week_46/afrilearndata.R
## por Jamie Hudson

# cargar librerías ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(rgdal)
library(osmdata)
library(sf)
library(ggfx)
library(showtext)
library(viridis)
font_add_google("Old Standard TT", "oldtt")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# cargar conjunto de datos ------------------------------------------------------------

# cargar archivo hex de https://cartogrid.vercel.app/
bolivia_hex <- readOGR(dsn = "geodat/Bolivia_hex_10km.geojson")
bolivia_hex_sf <- as(bolivia_hex, 'sf')

# preparar y procesar datos ------------------------------------------------------------

bolivia_bb <- getbb("Bolivia", featuretype = "country",
                  format_out = "sf_polygon")

bolivia_hospital <- read.csv(file = 'geodat/est_salud_2016_nivel3_geom.csv')

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

bolivia_hospital_sf <- st_as_sf(bolivia_hospital,
                       coords = c("LONG", "LAT"),
                       crs = projcrs)

bolivia_hospital_join = bolivia_hospital_sf %>% 
  st_union()

sf::sf_use_s2(FALSE)

distance <-  st_distance(bolivia_hex_sf, bolivia_hospital_join) %>% 
  units::set_units("km")

bolivia_wf <- bolivia_hex %>% 
  st_as_sf() %>% 
  mutate(
    dist = distance) %>% 
  drop_na()

# graficar ------------------------------------------------------------

ggplot() +
  with_outer_glow(with_inner_glow(
    geom_sf(
      data = bolivia_wf,
      aes(fill = as.numeric(dist)),
      color = "NA", alpha = 0.95
    ),
    colour = "white",
    sigma = 2
  ),
  colour = "black",
  sigma = 10,
  expand = 4) +
  coord_sf(xlim = c(st_bbox(bolivia_bb)[1], st_bbox(bolivia_bb)[3]),
           ylim = c(st_bbox(bolivia_bb)[2], st_bbox(bolivia_bb)[4])) +
  scale_fill_gradientn(colours = magma(10),
                       labels = c("0km", "100km", "200km", "300km"),
                       guide = guide_colourbar(
                         title.position = "top",
                         direction = "horizontal",
                         barwidth = 17,
                         barheight = 0.5,
                         frame.colour = "black"
                       )) +
  geom_point(data = bolivia_hospital, 
             aes(x = LONG, y = LAT), 
             colour = "white", size = 0.3,
             alpha = 0.3) +
  labs(title = "Distancia a Centros de Salud",
       fill = "Distancia a Centros de Salud de 3er nivel en Bolivia (puntos blancos)",
       caption = "@rcrmj Eric Armijo | fuente = {GeoBolivia 2016}") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
        plot.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
        plot.title = element_text(hjust = 0.5, family = "oldtt",
                                  colour = "black",
                                  size = 20,
                                  margin = margin(20, 0, -5, 0)),
        plot.caption = element_text(family = "oldtt",
                                    size = 4,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "oldtt", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(paste0("bolivia_hospitales_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 5,
       height = 5)
