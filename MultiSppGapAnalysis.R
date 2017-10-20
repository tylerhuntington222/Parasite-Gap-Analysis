
setwd("~/Dropbox/parks_for_parasites/rem")

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



# authentication step - requires action in browser after executing line
drop_auth(rdstoken = "data/dropbox_token.RDS")

# init scenarios vector
scenarios <- c("current",
               "futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")

# load list of parasites in contiguouos US
contig.us.paras <- readRDS("output/contig_US_parasites.RDS")
contig.us.paras <- as.character(contig.us.paras)

# reverse order of list for processing in reverse-alphabetical order
#contig.us.paras <- rev(contig.us.paras)

# init dataframe for storing results
results.df <- data.frame(SPECIES = character(), 
                         SCENARIO = character(),
                         TOTAL_DIST_AREA = character(),
                         PA_OVERLAP_AREA = character())


todo.runs <- readRDS("output/todo_run_list.RDS")

# # iterate over US parasites
# for (p in contig.us.paras) {

# parallel comp
# init cluster
no.cores <- detectCores()-1
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)


combined.results <- foreach(p = contig.us.paras,.combine = "rbind",
                            .packages = package.list, 
                            .export = ls()) %dopar% {
                              
                              # iterate over climate scenarios
                              for (s in scenarios) {
                                
                                #try({
                                
                                # # load libraries
                                # require(raster)
                                # require(rgdal)
                                # require(raster)
                                # require(maptools)
                                # require(sp)
                                # require(rgeos)
                                # require(rdrop2)
                                # require(parallel)
                                # require(doSNOW)
                                # require(iterators)
                                # require(foreach)
                                # 
                                # init one-row df for storing this parasite-scenario combo results
                                
                                # authorize dropbox connection
                                drop_auth(rdstoken = "data/dropbox_token.RDS")
                                
                                results.row <- data.frame(SPECIES = p, 
                                                          SCENARIO = s,
                                                          TOTAL_DIST_AREA = (NA),
                                                          PA_OVERLAP_AREA = (NA))
                                results.row$SCENARIO <- as.character(results.row$SCENARIO)
                                results.row$TOTAL_DIST_AREA <- as.numeric(results.row$TOTAL_DIST_AREA)
                                results.row$PA_OVERLAP_AREA <- as.numeric(results.row$PA_OVERLAP_AREA)
                                
                                para <- gsub(" ", "_", p)
                                this.run <- paste0(para, "_", s, "_", "overlap.RDS")
                                #if (this.run %in% todo.runs) {
                                if (1==1) {
                                  
                                  # download species-scenario .grd file
                                  dir <- "/gapanalysisswarthmore/actualmaps/"
                                  filename <- paste0(p, " ", s, ".grd")
                                  path <- paste0(dir, filename)
                                  new.grd.filename <- gsub(" ", "_", filename)
                                  rdrop2::drop_download(path, local_path = paste0("temp_bin/", new.grd.filename), overwrite = T)
                                  
                                  # download species-scenario .gri file
                                  filename <- paste0(p, " ", s, ".gri")
                                  path <- paste0(dir, p, " ", s, ".gri") 
                                  new.gri.filename <- gsub(" ", "_", filename)
                                  rdrop2::drop_download(path, local_path = paste0("temp_bin/", new.gri.filename), overwrite = T)
                                  
                                  # make raster layer from parasite distribution grid
                                  para.ras <- raster(paste0("temp_bin/", new.gri.filename))
                                  
                                  # crop parasite distribution to contiguous US
                                  para.dist.us <- crop(para.ras, extent(contig.us))
                                  
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
                                  resampledRaster <- resample(inputRaster, resampledRaster,
                                                              method='ngb',
                                                              filename=paste0("temp_bin/", para, "_", s, "_resampled.tif"),
                                                              overwrite=TRUE)
                                  
                                  # calculate areas of raster cells
                                  cell.areas <- area(resampledRaster)
                                  
                                  # make raster stack with presence/absence and cell area layers
                                  para.dist.stack <- stack(resampledRaster, cell.areas)
                                  
                                  # convert para dist raster to points
                                  para.ras.pts.mx <- rasterToPoints(para.dist.stack)
                                  
                                  # spatialize parasite distribution raster points 
                                  para.ras.pts.sp <- SpatialPointsDataFrame(coords = para.ras.pts.mx[,1:2], 
                                                                            data = data.frame(para.ras.pts.mx[,1:4]),
                                                                            proj4string = crs(wdpa.data))
                                  
                                  # subset for points representing raster cells where para presence indicated
                                  para.ras.pts.sp <- para.ras.pts.sp[which(para.ras.pts.sp$layer.1 == 1),]
                                  
                                  # rename attribute fields
                                  names(para.ras.pts.sp) <- c("LONG", "LAT", "PRESENCE", "AREA")
                                  
                                  # calculate total area of parasite distribution
                                  para.dist.area <- sum(para.ras.pts.sp$AREA)
                                  results.row[1, "TOTAL_DIST_AREA"] <- para.dist.area
                                  
                                  # find points in para dist that fall within PA system
                                  overlap.pts <- para.ras.pts.sp[wdpa.data,]
                                  
                                  # calculate total area of overlap points (in square km)
                                  overlap.area <- sum(overlap.pts$AREA)
                                  results.row[1, "PA_OVERLAP_AREA"] <- overlap.area
                                  
                                  # add this parasite-scenario results to results df
                                  results.df <- rbind(results.df, results.row)
                                  
                                  # create parasite-scenario output object
                                  # export overlap pts for para species under current climate scenario
                                  outfile.path <- paste0("output/overlap_pts/", p, "_", s, "_output.RDS")
                                  outfile.path <- gsub(" ", "_", outfile.path)
                                  para.output <- list(results.row, overlap.pts)
                                  saveRDS(para.output, outfile.path)
                                  
                                  # remove .gri/.grd files from temp_bin
                                  system(paste0("rm temp_bin/", new.gri.filename))
                                  system(paste0("rm temp_bin/", new.grd.filename))
                                  # }) # end try expression
                                }
                              }
                              results.row
                            }

# cancel parallel backend
stopCluster(cl)

# export results
saveRDS(combined.results, "output/contig_US_all_model_runs.RDS")






