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

package.list <- c("raster", "rgdal", "raster", "maptools", "sp", 
                  "rgeos", "rdrop2", "parallel", "doSNOW", "foreach")




# load WPDA raster
wdpa.ras <- raster("data/WDPA_Sep2017/WDPA_raster_contig_US.tif")

# load list of parasites in contiguouos US
contig.us.paras <- readRDS("output/contig_US_parasites.RDS")
contig.us.paras <- as.character(contig.us.paras)
is.na(values(wdpa.ras)) <- 0

# reverse order of list for processing in reverse-alphabetical order
contig.us.paras <- rev(contig.us.paras)

# init scenarios vector
scenarios <- c("current",
               "futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")

# parallel comp
# init cluster
no.cores <- detectCores()-1
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)


combined.results <- 
  foreach(p = contig.us.paras,.combine = "rbind",
                            .packages = package.list, 
                            .export = ls()) %dopar% {
  for (s in scenarios) {
    # authorize dropbox connection
    drop_auth(rdstoken = "data/dropbox_token.RDS")
    
    # remove spaces from parasite species name
    para.spec <- gsub(" ", "_", p)

    results.row <- data.frame(SPECIES = para.spec, 
                              SCENARIO = s,
                              TOTAL_DIST_AREA = (NA),
                              PA_OVERLAP_AREA = (NA))
    results.row$SCENARIO <- as.character(results.row$SCENARIO)
    results.row$TOTAL_DIST_AREA <- as.numeric(results.row$TOTAL_DIST_AREA)
    results.row$PA_OVERLAP_AREA <- as.numeric(results.row$PA_OVERLAP_AREA)

    
    # download species-scenario .grd file
    dir <- "/gapanalysisswarthmore/actualmaps/"
    filename <- paste0(p, " ", s, ".grd")
    path <- paste0(dir, filename)
    new.grd.filename <- gsub(" ", "_", filename)
    rdrop2::drop_download(path, 
                          local_path = paste0("temp_bin/", new.grd.filename), 
                          overwrite = T)
    
    # download species-scenario .gri file
    filename <- paste0(p, " ", s, ".gri")
    path <- paste0(dir, p, " ", s, ".gri") 
    new.gri.filename <- gsub(" ", "_", filename)
    rdrop2::drop_download(path,
                          local_path = paste0("temp_bin/", new.gri.filename), 
                          overwrite = T)
    
    # make raster layer from parasite distribution grid
    para.ras <- raster(paste0("temp_bin/", new.gri.filename))
    
    # crop parasite distribution to contiguous US
    para.ras <- crop(para.ras, extent(wdpa.ras))
    
    # unproject to lat long coords
    projectRaster(para.ras, crs = CRS(proj4string(wdpa.ras)))
    
    # get total area of parasite distribution before resampling and overlaying
    para.orig.stack <- stack(para.ras, area(para.ras))
    para.orig.stack$layer.1[is.na(para.orig.stack$layer.1)] <- 0
    para.dist.area <- sum(values(para.orig.stack$layer.2)
                          [values(para.orig.stack$layer.1)==1])
    results.row$TOTAL_DIST_AREA <- para.dist.area
    
    # resample parasite rater to higher resolution ~ 1km x 1km 
    resampleFactor <- 0.0608
    inCols <- ncol(para.ras)
    inRows <- nrow(para.ras)
    
    # create resampling raster template
    para.ras.resampled <- raster(ncol=(inCols / resampleFactor), 
                              nrow=(inRows / resampleFactor))
    extent(para.ras.resampled) <- extent(para.ras)
    
    # resample parasite distribution to higher resolution for PA overlay
    resample.file <- paste0("temp_bin/", para.spec, "_", s, ".tif")
    para.ras.resampled <- resample(para.ras, para.ras.resampled,
                                   method='ngb',
                                   filename=resample.file,
                                   overwrite=TRUE)
    
    # overlay parasite distribution raster with wdpa layer
    overlay.filename <- paste0("output/raster/overlays/", para.spec, "_", s, 
                               "_overlay.tif")
    
    para.wdpa.overlay <- overlay(wdpa.ras, para.ras.resampled, 
                                 fun=function(r1, r2){return(r2+r1)},
                                 filename = overlay.filename,
                                 overwrite = T)
    
  
    
    # get total area of parasite distribution overlap with PAs
    para.stack <- stack(para.wdpa.overlay, area(para.wdpa.overlay))
    names(para.stack) <- c("PA_OVERLAP", "CELL_AREA")
    para.stack$PA_OVERLAP[is.na(para.stack$PA_OVERLAP)] <- 0
    para.over.area <- sum(values(para.stack$CELL_AREA)
                          [values(para.stack$PA_OVERLAP)==2])
    results.row$PA_OVERLAP_AREA <- para.over.area
    
    saveRDS(results.row, 
            paste0("output/raster/", para.spec, "_", s, "_", "results_row.RDS"))
    
    # delete intermediate files on disk
    # remove .gri/.grd files from temp_bin
    system(paste0("rm temp_bin/", new.gri.filename))
    system(paste0("rm temp_bin/", new.grd.filename))
    system(paste0("rm ", resample.file))
    
  }
  results.row
                            }

# export combined results table
saveRDS(combined.results, "output/raster_all_para_runs.RDS")

# cancel parallel backend
stopCluster(cl)


# # TESTING
# test.ras <- raster("output/raster/overlays/XENOPSYLLA_CHEOPIS_futureac45_overlay.tif")
# 
# test.res <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_futureac45_results_row.RDS")
# 
# test.res2 <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_current_results_row.RDS")
    
    
    
    
    
    
