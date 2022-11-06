## Calcular el tiempo que toma caminar desde el centro de una ciudad
## Ciudad de Santa Cruz, SC, Bolivia
## Adaptado del script original desarrollado por Rafael H.M. Pereira (@UrbanDemog)
## fuente: https://www.urbandemographics.org/post/mapping-walking-time-osm-r5r/
## Modificado por Eric Armijo (@rcrmj)

options(java.parameters = "-Xmx4G")

library(r5r)
library(sf)
library(ggplot2)
library(here)
library(osmextract)
library(data.table)
library(magrittr)


# crear subdirectorio "data" e "img"
dir.create(here::here("data"))
dir.create(here::here("img"))


# obtener límites del municipio
city <- 'Santa Cruz'
city_boundary <- st_read("geodat/scz_lim_munic.shp")

# definir centro de ciudad de Santa Cruz, SC, Bolivia
city_center_df <- data.frame(id='center', lon=-63.182126, lat=-17.783330)
city_center_sf <- sfheaders::sfc_point(obj = city_center_df, x='lon', y='lat')
st_crs(city_center_sf) <- 4326

# definir zona de influencia de análisis como 3 Km
buff <- st_buffer(city_center_sf, dist = 3000)

# ajustar src
city_boundary <- st_transform(city_boundary, 4326)
city_center_sf <- st_transform(city_center_sf, 4326)
buff_bb <- st_bbox(buff)


# obtener datos OSM
plaza24sep = sf::st_sfc(sf::st_point(c(-63.182126, -17.783330)), crs = 4326)
osmextract::oe_download(provider = 'openstreetmap_fr',
                        file_url = osmextract::oe_match(plaza24sep)[[1]],
                        download_directory = here::here("data"), force_download = T)

# construir red de enrutamiento
r5r_core <- r5r::setup_r5(data_path = here::here("data"), verbose = FALSE)

# obtener red de calle como sf
street_network <- r5r::street_network_to_sf(r5r_core)

# quitar red fuera de la zona de influencia
edges_buff <- street_network$edges[buff, ] %>% st_intersection(., buff)
vertices_buff <- street_network$vertices[buff, ] %>% st_intersection(., buff)
city_boundary_buff <- st_intersection(city_boundary, buff)
plot(city_boundary_buff)

# añadir id a vértices
vertices_buff$id <- vertices_buff$index


# calcular tiempos de viaje al centro de la ciudad 
tt <- r5r::travel_time_matrix(r5r_core,
                              origins = vertices_buff,
                              destinations = city_center_df,
                              mode = 'walk')


# añadir info de tiempos de viaje a la red de calles
tt$fromId <- as.numeric(tt$fromId)
setDT(edges_buff)[tt, on=c('from_vertex'='fromId'), travel_time := i.travel_time]
edges_buff <- st_sf(edges_buff)

# graficar y guardar la figura
# figura
temp <- ggplot() + 
  geom_sf(data=buff, fill='lightblue', color=NA)  +
  geom_sf(data=city_boundary_buff, fill='white', size=.3)  +
  geom_sf(data=buff, fill='gray90', color=NA, alpha=.5)  +
  geom_sf(data=edges_buff, aes(color=travel_time, size=length))  +
  geom_sf(data=city_center_sf, color='red', size=.5) +
  scale_size_continuous(range = c(0.1, .8), guide='none') +
  scale_color_viridis_c(direction = -1) +
  labs(title='Tiempo que toma caminar desde el centro de la ciudad',
       subtitle = 'Santa Cruz, SC, Bolivia',
       caption = 'Datos: OpenStreetMap | Herramienta: R + paquete r5r',
       color='Minutos') +
  coord_sf(xlim = c(buff_bb[[1]], buff_bb[[3]]), 
           ylim = c(buff_bb[[2]], buff_bb[[4]]), expand = FALSE) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color='white'),
        plot.title = element_text(color = "gray20"),
        plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(color = "gray"))

# guardar figura
ggsave(temp, file=here::here("img", "scz_camin.png"), 
       dpi=300, width = 14, height = 14, units = 'cm')
