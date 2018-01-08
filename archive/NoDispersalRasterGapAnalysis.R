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
scenarios <- c("current", "futureac45", "futureac85", 
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
  foreach(p = contig.us.paras, .combine = "rbind",
          .packages = package.list, 
          .export = ls()) %dopar% {
            
            # # get the current SDM for this parasite 
            # dir <- "/gapanalysisswarthmore/actualmaps/"
            # filename <- paste0(p, " ", "current", ".grd")
            # path <- paste0(dir, filename)
            # new.grd.filename <- gsub(" ", "_", filename)
            # rdrop2::drop_download(path, 
            #                       local_path = paste0("temp_bin/", new.grd.filename), 
            #                       overwrite = T)
            # 
            # # download species-scenario .gri file
            # filename <- paste0(p, " ", "current", ".gri")
            # path <- paste0(dir, p, " ", s, ".gri") 
            # new.gri.filename <- gsub(" ", "_", filename)
            # rdrop2::drop_download(path,
            #                       local_path = paste0("temp_bin/", new.gri.filename), 
            #                       overwrite = T)
            # 
            # # make raster layer from parasite distribution grid
            # para.cur.ras <- raster(paste0("temp_bin/", new.gri.filename))
            # 
            # # crop parasite distribution to contiguous US
            # para.cur.ras <- crop(para.cur.ras, extent(wdpa.ras))
            # 
            # # unproject to lat long coords
            # projectRaster(para.cur.ras, crs = CRS(proj4string(wdpa.ras)))
            # 
            
            
            # run zero percent dispersal scenario
            
            # remove spaces from parasite species name
            para.spec <- gsub(" ", "_", p)
            
            
            # load current distribution raster for this species
            cur.file <- paste0("output/raster/overlays/", para.spec, "_current_overlay_FD.tif")
            cur <- raster(cur.file)
            
            # authorize dropbox connection
            drop_auth(rdstoken = "data/dropbox_token.RDS")
            
            # download current SDM for this parasite
            dir <- "/gapanalysisswarthmore/actualmaps/"
            filename <- paste0(p, " ", "current", ".grd")
            path <- paste0(dir, filename)
            new.grd.filename <- gsub(" ", "_", filename)
            rdrop2::drop_download(path, 
                                  local_path = paste0("temp_bin/", new.grd.filename), 
                                  overwrite = T)
            
            # download species-scenario .gri file
            filename <- paste0(p, " ", "current", ".gri")
            path <- paste0(dir, p, " ", "current", ".gri") 
            new.gri.filename <- gsub(" ", "_", filename)
            rdrop2::drop_download(path,
                                  local_path = paste0("temp_bin/", new.gri.filename), 
                                  overwrite = T)
            
            # make raster layer from parasite distribution grid
            para.cur.ras <- raster(paste0("temp_bin/", new.gri.filename))
            
            # crop parasite distribution to contiguous US
            para.cur.ras <- crop(para.cur.ras, extent(wdpa.ras))
            
            # unproject to lat long coords
            projectRaster(para.cur.ras, crs = CRS(proj4string(wdpa.ras)))
            
            
            futures <- c("futureac45", "futureac85", 
                         "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
                         "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
                         "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
                         "futurehe26", "futurehe45", "futurehe60", "futurehe85")
            
            
            for (f in futures) {
              
              # init results.row to store this species results
              results.row <- data.frame(SPECIES = para.spec, 
                                        SCENARIO = f,
                                        TOTAL_DIST_AREA = (NA),
                                        PA_OVERLAP_AREA = (NA))
              results.row$SCENARIO <- as.character(results.row$SCENARIO)
              results.row$TOTAL_DIST_AREA <- as.numeric(results.row$TOTAL_DIST_AREA)
              results.row$PA_OVERLAP_AREA <- as.numeric(results.row$PA_OVERLAP_AREA)
              
              
              # download species-scenario .grd file
              dir <- "/gapanalysisswarthmore/actualmaps/"
              filename <- paste0(p, " ", f, ".grd")
              path <- paste0(dir, filename)
              new.grd.filename <- gsub(" ", "_", filename)
              rdrop2::drop_download(path, 
                                    local_path = paste0("temp_bin/", new.grd.filename), 
                                    overwrite = T)
              
              # download species-scenario .gri file
              filename <- paste0(p, " ", f, ".gri")
              path <- paste0(dir, p, " ", f, ".gri") 
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
              
              # # determine overlap between projected range and current SDM
              # para.ras <- mask(para.cur.ras, para.ras, maskvalue = 0, updatevalue = 0)
              
              # mask future dist by current dist bc to assume zero dispersal
              overlay.filename <- paste0("output/raster/overlays/", para.spec, "_", f, 
                                         "_temp_overlay.tif")
              
              over <- overlay(para.ras, para.cur.ras, 
                              fun=function(r1, r2){return(r2+r1)},
                              filename = overlay.filename,
                              overwrite = T)
              # Now, cells with value 2 in 'over' represent future distribution assuming zero dispersal
              
              # get total area of parasite distribution before resampling and overlaying
              para.orig.stack <- stack(over, area(over))
              names(para.orig.stack) <- c("in_range", "area")
              para.orig.stack$in_range[is.na(para.orig.stack$in_range)] <- 0
              
              para.dist.area <- sum(values(para.orig.stack$area)
                                    [values(para.orig.stack$in_range)==2])
              
              # store total distribution area for this scenario
              results.row$TOTAL_DIST_AREA <- para.dist.area

              
                
              # CALC PERCENT OVERLAP FOR THIS FUTURE SCENARIO UNDER ZERO PERCENT DISPERSAL
              
              # load future raster calculated for 100% dispersal
              future <- raster(paste0("output/raster/overlays/", para.spec, "_", 
                                      f, "_overlay_FD.tif"))
              
              overlay.filename <- paste0("output/raster/overlays/", para.spec, "_", f, 
                                         "_overlay_ZD.tif")
              
              zero.dis <- overlay(cur, future, fun=function(r1, r2){return(r2+r1)},
                                  filename = overlay.filename,
                                  overwrite = T)
              
              
              zero.dis[zero.dis == 2 | zero.dis == 3] <- 0
              zero.dis[zero.dis == 4] <- 2
              
              # get total area of parasite distribution overlap with PAs
              para.stack <- stack(zero.dis, area(zero.dis))
              names(para.stack) <- c("PA_OVERLAP", "CELL_AREA")
              para.stack$PA_OVERLAP[is.na(para.stack$PA_OVERLAP)] <- 0
              para.over.area <- sum(values(para.stack$CELL_AREA)
                                    [values(para.stack$PA_OVERLAP)==2])
              results.row$PA_OVERLAP_AREA <- para.over.area
              
              saveRDS(results.row, 
                      paste0("output/raster/", 
                             para.spec, "_", f, "_", "results_row_ZD.RDS"))
            }
            results.row
            
            
          }

# export combined results table
#saveRDS(combined.results, "output/raster_all_para_runs.RDS")

# cancel parallel backend
stopCluster(cl)


# # TESTING
# test.ras <- raster("output/raster/overlays/XENOPSYLLA_CHEOPIS_futureac45_overlay.tif")
# 
# test.res <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_futureac45_results_row.RDS")
# 
# test.res2 <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_current_results_row.RDS")






