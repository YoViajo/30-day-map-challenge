## Red de vuelos domésticos de Bolivia
## Autor: Eric Armijo (@rcrmj)

# INICIALIZACIÓN
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())


# LIBRERÍAS
library(dplyr)
library(maps)
library(geosphere)

# DATOS
# leer CSV
aero <- read.csv(file = 'dat/bol_airports_ed.csv')
vuel <- read.csv(file = 'dat/bol_routes_ed_domestico_ed.csv')
head(aero)
head(vuel)

# CREAR MAPA DE CONEXIÓN DE VUELOS
# crear mapa base
map("world", regions=c("bolivia"), fill=T, col="grey8", bg="grey15", ylim=c(-23,-9), xlim=c(-70,-57), resolution = 0)
# sobreponer aeropuertos
points(aero$Longitude,aero$Latitude, pch=3, cex=0.1, col="chocolate1")


# lista de aeropuertos origen en Bolivia
df_aero_orig <- vuel[!duplicated(vuel$source_airport), ]
lis_aero_orig <- as.list(df_aero_orig[ , c("source_airport")])

# ADICIONAR CONEXIONES DE TODOS LOS AEROPUERTOS DE ORIGEN
for (k in 1:length(lis_aero_orig)) {
  #print(lis_aero_orig[[k]])
  inic <- filter(aero, IATA==lis_aero_orig[[k]]) #df separado para aeropuerto origen
  vuel_orig <- filter(vuel, source_airport==lis_aero_orig[[k]]) #identificar vuelos con aeropuerto de interés de origen
  vuel_orig <- vuel_orig[!duplicated(vuel_orig$destination_airport), ] #eliminar filas duplicadas
  
  #adicionar lat y lon para aeropuertos destino
  for (j in (1:dim(vuel_orig)[1])) {
    vuel_orig$dest_lat[j] <- aero$Latitude[aero$IATA == vuel_orig$destination_airport[j]]
    vuel_orig$dest_lon[j] <- aero$Longitude[aero$IATA == vuel_orig$destination_airport[j]]
  }
  
  #adicionar líneas de vuelo
  for (i in (1:dim(vuel_orig)[1])) {
    inter <- gcIntermediate(c(inic$Longitude[1], inic$Latitude[1]), c(vuel_orig$dest_lon[i], vuel_orig$dest_lat[i]), n=200)
    lines(inter, lwd=0.5, col="turquoise2")
  }
}


  




