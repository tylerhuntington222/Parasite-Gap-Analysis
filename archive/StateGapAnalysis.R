

# NOTES:

# Steps for state level analysis

1. crop grid of species distribution to extent of state
2. convert para raster to points
3. calculate areas of cells 
4. attach cell area vals to raster points
5. convert df of points data to spatial points   
6. intersect para raster points with wdpa
7. calculate total area of points that result from intersection




# LIBRARIES
require(raster)
require(rgdal)
require(raster)
require(maptools)
require(sp)
require(rgeos)

setwd("~/Dropbox/parks_for_parasites/")


# load WDPA data 
filename = "WDPA_USA_subset"
wdpa.dsn = paste0("data/WDPA_Sep2017/", filename,".shp")
wdpa.layer = filename
wdpa.data <- readOGR(dsn = wdpa.dsn, layer = wdpa.layer, 
                       stringsAsFactors = F)

# load US state boundaries data
us.states <- readOGR(dsn = "data/US_state_boundaries/cb_2016_us_state_20m.shp",
                     layer = "cb_2016_us_state_20m")
# re-project states to WDPA layer's projection
us.states <- spTransform(us.states, crs(wdpa.data))


# susbet for boundary of particular state
state = "Arkansas"
state.bound <- us.states[us.states$NAME == state, ]

# export state WDPA layer
out.layer <- paste0(state, "_bound")
out.dsn <- paste0("data/US_state_boundaries/", out.layer, ".shp")
writeOGR(state.bound, out.dsn, out.layer, driver = "ESRI Shapefile")

# attribute subset for protected areas in US

# spatial subset for protected areas in <state>
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}
wdpa.state <- wdpa.data
wdpa.state <- wdpa.state[state.bound,]
zones_clipped <- gClip(wdpa.state, extent(state.bound))

test.raster <- rasterize(wdpa.data, pearl)

# export state WDPA layer
out.layer <- paste0(state, "_state_WDPA")
out.dsn <- paste0("data/WDPA/", out.layer, ".shp")
writeOGR(out.dsn, out.layer, driver = "ESRI Shapefile")
    

# load parasite distribution data for sample species accross climate scenarios
pearl.sp1 <- raster("data/PEARL/allocorrigia_filiformis/ALLOCORRIGIA FILIFORMIS current.gri")

pearl.sp2 <- raster("data/PEARL/amblyomma_americanum/AMBLYOMMA AMERICANUM current.gri")

# convert parasite distributions to bounding polygons

# iterating over para species and scenarios, calculate overlap between dist and PAs

for (para in para.species) {
  for (s in scenarios) {
    
    # interset para dist for this climate scenario with PA network
    
    # calculate total area of overlap
    
    # calculate error involved in estimation (TODO: figure out how to do this??)
    
  }
}

test.crop <- crop(pearl.sp1, extent(state.bound))

# reduce the cell size by 80% and double the number of rows and columns.      
resampleFactor <- 0.2  
inputRaster <- test.crop     
inCols <- ncol(inputRaster)
inRows <- nrow(inputRaster)

# create resampling raster template
resampledRaster <- raster(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor))
extent(resampledRaster) <- extent(inputRaster)

# resample parasite distribution to higher resolution raster
resampledRaster <- resample(inputRaster,resampledRaster,datatype="INT1U",
                            method='ngb',filename="testOutResamp.tif",
                            overwrite=TRUE)


para.ras.pts <- rasterToPoints(resampledRaster)
para.ras.pts <- data.frame(para.ras.pts)
para.ras.pts <- para.ras.pts[para.ras.pts$layer == 1,]

# spatialize parasite distribution raster points 
para.ras.pts.sp <- SpatialPoints(para.ras.pts[,1:2], crs(wdpa.data))

overlap.pts <- para.ras.pts.sp[wdpa.data,]

tn.wdpa <- wdpa.data[wdpa.data$SUB_LOC == "US-TN",]

