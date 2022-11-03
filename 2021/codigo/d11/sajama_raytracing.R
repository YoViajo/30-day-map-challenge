# Demostración de uso del paquete rayvista p/ representar el nevado Sajama, Bolivia
# Ref: https://github.com/h-a-graham/rayvista

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd("/home/yoviajo/Documentos/lab/datvis/99/")

library(rayshader) 
library(rayvista)

.lat <- -18.1167247
.long <- -68.8909149

Sajama <- plot_3d_vista(lat=.lat, long=.long, zscale=5, zoom=0.5,
                          overlay_detail=14, theta=-65, windowsize =1200, 
                          phi=25)

render_label(heightmap= Sajama, text='Sajama: 6542 m', lat = .lat,
             long=.long, extent = attr(Sajama, 'extent'),
             clear_previous = T)

#Sajama <- plot_3d_vista(lat = .lat, long = .long, phi=30)

#render_label(heightmap= Sajama, text='Sajama: 6542 m', lat = .lat,
#             long=.long, extent = attr(Sajama, 'extent'),altitude=600,
#             clear_previous = T, zscale = 2)

render_compass()

render_highquality(lightdirection = 220, clear=TRUE)

#render_snapshot(clear=TRUE)
