install.packages('plotKML')

library(plotKML)

r <- raster('rL3')

kml(r, colour_scale = SAGA_pal[[1]]) 

raster::KML(r, file = 'test.kmz', color_scale = SAGA_pal[[1]], overwrite = T)


list.files()
