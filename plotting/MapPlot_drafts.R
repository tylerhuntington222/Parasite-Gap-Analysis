# RasterGapAnalysis.R

# raster overlay approach to calculating gaps

# # local machines
setwd("~/Dropbox/parks_for_parasites/")

# # virtual machines
# setwd("~/para/")


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
require(MASS)



# load WPDA raster
wdpa.ras <- raster("data/WDPA_Sep2017/WDPA_raster_contig_US.tif")

us.bound <- readOGR("data/US_state_boundaries/contig_US_boundary.shp",
                    layer = "contig_US_boundary")
plot(us.bound, col = "gray", 
     xlab = NA,
     ylab = NA)
plot(wdpa.ras, col = "black", add = T, legend = F)



# resample parasite rater to higher resolution ~ 1km x 1km 
resampleFactor <- 50
inCols <- ncol(wdpa.ras)
inRows <- nrow(wdpa.ras)

# create resampling raster template
wdpa.resampled <- raster(ncol=(inCols / resampleFactor), 
                             nrow=(inRows / resampleFactor))
extent(wdpa.resampled) <- extent(wdpa.ras)

# resample parasite distribution to higher resolution for PA overlay
resample.file <- paste0("output/", "test_wdpa_resample", "_", ".tif")

para.ras.resampled <- resample(wdpa.ras, wdpa.resampled,
                               method='ngb',
                               filename=resample.file,
                               overwrite=TRUE)

plot(para.ras.resampled)

# save raster as pts
pts <- rasterToPoints(wdpa.ras)


pts.sp <- SpatialPointsDataFrame(pts, pts.df, proj4string = crs(wdpa.ras))

writeOGR(pts.sp, dsn = "wdpa.points.shp", layer = "wdpa.points", driver = "ESRI Shapefile")

# convert to data frame
pts.df <- data.frame(pts)


dens <- kde2d(pts.df$x, pts.df$y, n=50, h = 100)  #overrode default bandwidth
filled.contour(dens)


# GGPLOT HEATMAP
p <- ggplot(pts.df, aes(x = x, y = y)) + 
geom_point() + 
stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")))









