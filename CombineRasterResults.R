# CombineRasterResults.R

# Stitch together raster overlay results for each parasite-scenario run into 
# singe results dataframe.


# set working directory

# local machines
# setwd("~/Dropbox/parks_for_parasites/")

# virtual machines
setwd("~/para/")


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

# 100% DISPERSAL RESULTS
# load list of parasite result row files
res.files <- list.files("output/raster")
res.files <- res.files[grep("results_row_FD.RDS", res.files)]


# init scenarios vector
scenarios <- c("current",
               "futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")

# init pas counter 
pass <- 1

for (f in res.files) {
  para.res.file <-paste0("output/raster/", f)
  para.row <- readRDS(para.res.file)
  
  if(para.row$SCENARIO == "current") {
    para.row$DISPERSAL <- "current"
  } else {
    para.row$DISPERSAL <- "100"
  }
  
  if (pass == 1) {
    combined.results <- para.row
    
  } else {
    combined.results <- rbind(combined.results, para.row)
    
  }
  pass <- pass + 1
}


# ADD ZERO PERCENT DISPERSAL RESULTS TO COMBINED RESULTS DF
# load list of parasite result row files
res.files <- list.files("output/raster")
res.files <- res.files[grep("results_row_ZD.RDS", res.files)]


# init scenarios vector
scenarios <- c("futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")


for (f in res.files) {
  para.res.file <-paste0("output/raster/", f)
  para.row <- readRDS(para.res.file)
  para.row$DISPERSAL <- "0"
  
  # bind to comibined df
  combined.results <- rbind(combined.results, para.row)
}


# export results
saveRDS(combined.results, "output/all_raster_overlay_results.RDS")






    
# TESTING:
    
    
    
res1 <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_futurehe26_results_row_ZD.RDS")
res2 <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_futurehe45_results_row_ZD.RDS")
res3 <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_futurehe60_results_row_ZD.RDS")
res4 <- readRDS("output/raster/ZYGOCOTYLE_LUNATA_futurehe85_results_row_ZD.RDS")


print(res1)
print(res2)
print(res3)
print(res4)
    
    
    