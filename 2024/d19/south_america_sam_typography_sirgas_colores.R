library(sf)
library(tidyverse)
library(ggplot2)
library(stringr)
library(RColorBrewer)

# Crear el subdirectorio "salida" si no existe
output_dir <- "C:/Users/yoviajo/Documents/LAB/datvis/salida"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Cargar la capa "sam_admin" desde el GeoPackage
geopackage_path <- "C:/Users/yoviajo/Documents/LAB/geodat/naturalearth/sam_paises.gpkg"
layer_name <- "sam_admin"
sam_shp <- st_read(geopackage_path, layer = layer_name)

# Verificar que la Guayana Francesa esté presente
if (!any(sam_shp$ADMIN == "France")) {
  stop("Guayana Francesa no está incluida en los datos. Verifica la fuente del GeoPackage.")
}

# Reproyectar a SIRGAS 2000 (EPSG:4674)
sam_shp <- st_transform(sam_shp, crs = "EPSG:4674")

# Calcular los centroides de las geometrías
sam_shp <- sam_shp |> 
  mutate(centroid = st_centroid(geom))

# Excluir islas remotas (Galápagos e Isla de Pascua)
sam_shp <- sam_shp |> 
  filter(!(ADMIN == "Ecuador" & st_coordinates(centroid)[, 1] < -80 & st_coordinates(centroid)[, 2] < 0)) |> 
  filter(!(ADMIN == "Chile" & st_coordinates(centroid)[, 1] < -100))

# Crear una grilla para la visualización con letras
bbox <- st_bbox(sam_shp)
aspect_ratio <- (bbox["xmax"] - bbox["xmin"]) / (bbox["ymax"] - bbox["ymin"])
n_cells <- 100
grid <- st_make_grid(sam_shp, n = c(n_cells, round(n_cells / aspect_ratio)), what = "centers") |> 
  st_as_sf() |> 
  st_filter(sam_shp, .predicate = st_intersects)

# Unir la grilla con las formas de los países
grid_countries <- grid |> 
  st_join(sam_shp, join = st_intersects)

# Extraer coordenadas y distribuir letras en la grilla con orientación horizontal
grid_countries_letters <- grid_countries |> 
  group_split(NAME_ES, .keep = TRUE) |> 
  map(function(x) {
    coords <- st_coordinates(x)
    x <- x |> 
      mutate(
        coord_x = coords[, 1],
        coord_y = coords[, 2]
      ) |> 
      arrange(coord_y, coord_x) |> 
      mutate(
        row = row_number(),
        row_rest = (row_number() - 1) %% str_length(NAME_ES) + 1,
        foo = NAME_ES,
        letter = unlist(str_split(foo, pattern = ""))[row_rest]
      )
    return(x)
  }) |> 
  bind_rows()

# Generar colores únicos para los países utilizando RColorBrewer
unique_countries <- unique(grid_countries_letters$NAME_ES)
n_countries <- length(unique_countries)

# Elegir una paleta categórica adecuada
color_palette <- brewer.pal(min(n_countries, 12), "Set3")
if (n_countries > 12) {
  color_palette <- colorRampPalette(brewer.pal(12, "Set3"))(n_countries)
}
names(color_palette) <- unique_countries

# Asignar colores específicos
color_palette["Brasil"] <- "white"         # Brasil en blanco
color_palette["Argentina"] <- "skyblue"   # Argentina en celeste
color_palette["Perú"] <- "skyblue"        # Perú en celeste (igual que Argentina)
color_palette["Ecuador"] <- "orange"      # Ecuador en naranja
color_palette["Bolivia"] <- "orange"      # Bolivia en naranja

# Graficar letras distribuidas en la grilla
ggplot(grid_countries_letters) + 
  geom_sf_text(
    aes(
      label = toupper(letter),
      color = NAME_ES
    ), 
    size = 1.5,
    family = "Source Sans Pro", fontface = "bold", show.legend = FALSE
  ) +
  annotate(
    "text",
    x = bbox["xmin"] + (bbox["xmax"] - bbox["xmin"]) * 0.2,
    y = bbox["ymax"] + (bbox["ymax"] - bbox["ymin"]) * 0.05,  # Reducido para acercar el título
    label = "Sudamérica\nen letras",
    family = "Source Sans Pro", fontface = "bold", size = 10, lineheight = 0.9, hjust = 0, color = "white"
  ) +
  scale_color_manual(values = color_palette) +  
  labs(
    caption = "Fuente: Natural Earth. Visualización: Código original de Ansgar Wolsing, adaptado para Sudamérica"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "#000000", fill = "#000000"),
    panel.background = element_rect(fill = "black", color = NA),
    plot.caption = element_text(size = 7, hjust = 0.5, margin = margin(t = 10), color = "white"),
    plot.margin = margin(50, 4, 20, 4)
  )

# Guardar el gráfico en el subdirectorio "salida"
ggsave(file.path(output_dir, "sudamerica_tipografia_ajustado.png"), width = 6, height = 8.5)

# Guardar la capa actualizada (opcional)
output_shapefile <- file.path(output_dir, "sudamerica_incluyendo_guayana_francesa.shp")
st_write(sam_shp, output_shapefile, delete_layer = TRUE)
