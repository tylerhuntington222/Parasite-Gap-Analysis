library(rgdal)
library(maptools)
library(raster)
library(rgeos)

setwd("~/Dropbox/parks_for_parasites/")

coords <- read.csv('data/Carlson_et_al_2017/rawdata.csv')
coords <- coords[,-1]
coords

names <- data.frame(matrix(0,458,7))

names(names)[1] <- 'names'
names$names <- unique(coords$ScientificName)

cont.list <- c('Africa','Antarctica','Australia','Eurasia','North America','South America')

names(names)[2:7] <- cont.list

######## Hey guys! This is where you're gonna put in the minimum number of points you want per continent.

min <- 5

newcoords <- coords[0,]

cont <- readOGR(dsn="data/Carlson_et_al_2017/contnew.shp", layer="contnew")

for (name in unique(coords$ScientificName)) {
  
  coords.sp <- coords[coords$ScientificName==name,]
  
  for (i in 1:6) {
  
    continent <- cont.list[i]
    extent <- cont[cont$continent %in% list(continent),]
    points.sp <- SpatialPoints(coords.sp[,4:5])
    points.sp@proj4string <- extent@proj4string
    
    # subset points for those in current continent
    points.sp <- points.sp[extent,]
    names[names$names==name,i+1] <- nrow(points.sp@coords)
    
  }
  
  print(name)
  
}

######## 'names' has the number of points in each continent now.

predictorsorig <- getData('worldclim', var='bio', res=10, download = TRUE)

splist <- unique(coords$ScientificName)

setwd('./ActualMaps')

pass <- 1
for (p in splist) {
  
  predictors <- predictorsorig[[1]]
  
  coords.sp <- coords[coords$ScientificName==p,]
    
  cont.sp <-  cont.list[as.numeric(names[names$names==p,2:7]>min)*c(1:6)]
  extent <- cont[cont$continent %in% cont.sp,]
  mask <- mask(predictors, extent)
  
  print(pass)
  pass <- pass+1
}

  # CURRENT
  
  c <- raster::stack()
  
  for (t in splist) {
    r <- raster(paste(as.character(t),"current.grd"))
    r <- mask(r,mask)
    c <- stack(c, r)
  }
  
  
  # FUTURE
  scenarios <- c('ac45','ac85',
                 'bc26','bc45','bc60','bc85',
                 'cc26','cc45','cc60','cc85',
                 'hd26','hd45','hd60','hd85',
                 'he26','he45','he60','he85')
  
  for (s in scenarios) {
  
    
    f <- raster::stack()
    
    for (t in splist) {
      r <- raster(paste(paste(paste(as.character(t),"future"),s, sep=''),'.grd', sep=''))
      r <- mask(r,mask)
      f <- stack(f, r)
    }
  }
  print(p)
}
