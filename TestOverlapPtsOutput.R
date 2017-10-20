
setwd("~/Dropbox/parks_for_parasites/")

# LIBRARIES
require(raster)
require(rgdal)
require(raster)
require(maptools)
require(sp)
require(rgeos)
require(rdrop2)
require(parallel)
require(doSNOW)
require(iterators)
require(foreach)


all.output <- list.files("output/")

point.files <- all.output[grep("overlap", all.output)]

for (f in point.files) {
  
  pts <- readRDS(paste0("output/", f))
  
  outfile <- paste0("output/shapefiles/", f, ".shp")
  layer.name <- f
  
  writeOGR(pts, dsn = outfile, layer = layer.name, driver = "ESRI Shapefile", 
           overwrite_layer = T)
}
