# RasterizeWDPA.R

# create a raster layer of the WDPA polygon layer


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

package.list <- c("raster", "rgdal", "raster", "maptools", "sp", 
                  "rgeos", "rdrop2", "parallel", "doSNOW", "foreach")


# load WDPA data 
filename = "WDPA_contig_US"
wdpa.dsn = paste0("data/WDPA_Sep2017/", filename,".shp")
wdpa.layer = filename
wdpa.data <- readOGR(dsn = wdpa.dsn, layer = wdpa.layer, 
                     stringsAsFactors = F)

# load US state tabular data
us.state.key <- read.csv("data/state_table.csv")

# generate list of contiguous US state abbreviations to subset WDPA data
contig.states <- us.state.key$abbreviation
contig.states <- contig.states[!(contig.states %in% c("AK", "DC", "HI"))]
contig.states <- paste0("US-", contig.states)

# subset WDPA protected areas to contiguous US only
contig.us.wdpa <- wdpa.data[wdpa.data$SUB_LOC %in% contig.states,]

# load parasite distribution data for sample species accross climate scenarios
para.ras.orig <- raster("data/PEARL/allocorrigia_filiformis/ALLOCORRIGIA FILIFORMIS current.gri")

# crop para dist raster to contig US
para.ras.cropped <- crop(para.ras.orig, contig.us.wdpa)

# increase resolution to 1km x 1km 
resampleFactor <- 0.0608
inputRaster <- para.ras.cropped     
inCols <- ncol(inputRaster)
inRows <- nrow(inputRaster)

# create resampling raster template
resampledRaster <- raster(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor))
extent(resampledRaster) <- extent(inputRaster)

# resample parasite distribution to higher resolution raster
resampledRaster <- resample(inputRaster, resampledRaster,
                            method='ngb',
                            filename=paste0("test_resample.tif"),
                            overwrite=TRUE)

areas <- area(resampledRaster)
print(mean(values(areas)))
hist(areas)

# generate template for rasterizing US WDPA polygon layer
us.ras.template <- raster(resampledRaster)

# create dummy field for rasterizing WDPA layer with uniform PA area vals
contig.us.wdpa$PA <- 1

# rasterize WDPA layer for contiguous US
# and export to /data directory
wdpa.ras <- rasterize(contig.us.wdpa, us.ras.template, field = "PA",
                      filename = "data/WDPA_Sep2017/WDPA_raster_contig_US.tif",
                      overwrite = T)



para.stack <- stack(resampledRaster, area(resampledRaster))
para.stack$layer.1[is.na(para.stack$layer.1)] <- 0

# get summed area of cells where parasite is present
sum(values(para.stack$layer.2)[values(para.stack$layer.1)==1])

# overlay para dist with WDPA raster layer
over <- overlay(wdpa.ras, resampledRaster, 
                fun=function(r2, r1){return(r1-r2)})










