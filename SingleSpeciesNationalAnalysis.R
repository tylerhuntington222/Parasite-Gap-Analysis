

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
require(rdrop2)

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

# subset for contiguous 48 states
contig.us <- us.states[!(us.states$NAME %in% 
                           c("Alaska", "Hawaii", "Puerto Rico")),]


# load parasite distribution data for sample species accross climate scenarios
pearl.sp1 <- raster("data/PEARL/allocorrigia_filiformis/ALLOCORRIGIA FILIFORMIS current.gri")

pearl.sp2 <- raster("data/PEARL/amblyomma_americanum/AMBLYOMMA AMERICANUM current.gri")

# crop parasite distribution to contiguous US
para.dist.us <- crop(pearl.sp2, extent(contig.us))

# reduce the cell size by 90% and double the number of rows and columns.      
resampleFactor <- 0.1 
inputRaster <- para.dist.us     
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

# calculate areas of raster cells
cell.areas <- area(resampledRaster)

# make raster stack with presence/absence and cell area layers
para.dist.stack <- stack(resampledRaster, cell.areas)

# convert para dist raster to points
para.ras.pts <- rasterToPoints(para.dist.stack)
para.ras.pts <- data.frame(para.ras.pts)

# subset for points representing raster cells where para presence indicated
para.ras.pts <- para.ras.pts[para.ras.pts$layer.1 == 1,]

# spatialize parasite distribution raster points 
para.ras.pts.sp <- SpatialPointsDataFrame(coords = para.ras.pts[,1:2], 
                                          data = para.ras.pts[,1:4],
                                          proj4string = crs(wdpa.data))

# rename attribute fields
names(para.ras.pts.sp) <- c("LONG", "LAT", "PRESENCE", "AREA")

# find points in para dist that fall within PA system
overlap.pts <- para.ras.pts.sp[wdpa.data,]

# export overlap pts for para species under current climate scenario
outfile.path <- paste0("output/", p, "_", scenario)
saveRDS(overlap.pts, outfile.path)

# calculate total area of overlap points (in square km)
overlap.area <- sum(overlap.pts$AREA)

# write overlap area to output df

# loads a dataframe 
source("GetParasitesInUSA.R")

# iterate over parasite species
for (p in para.species) {
  for (s in scenarios) {
    
    # interset para dist for this climate scenario with PA network
    
    # calculate total area of overlap
    
    # calculate error involved in estimation (TODO: figure out how to do this??)
    
  }
}


install.packages("rdrop2")
require(rdrop2)

drop_acc()
d