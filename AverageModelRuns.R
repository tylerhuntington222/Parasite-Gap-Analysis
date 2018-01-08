# AverageModelRuns.R

setwd("~/Dropbox/parks_for_parasites")

# libaries
require(plyr)
require(dplyr)

# 100% DISPERSAL 
all.runs.df <- readRDS("output/all_raster_overlay_results.RDS")

# create field for RCP code of each model
all.runs.df$RCP <- substr(all.runs.df$SCENARIO, 9, 10)
all.runs.df$RCP[all.runs.df$RCP==""] <- 0
all.runs.df$RCP <- as.factor(all.runs.df$RCP)


# init climate scenarios 
scenarios <- c("current",
               "futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")


# collapse all run results by RCP of climate model, averaging area fields
model.avs.df <- ddply(all.runs.df, 
                      .variables = c("SPECIES", "RCP", "DISPERSAL"), summarize,
                      TOTAL_DIST_AREA = mean(TOTAL_DIST_AREA),
                      PA_OVERLAP_AREA = mean(PA_OVERLAP_AREA))


saveRDS(model.avs.df, "output/averaged_gap_analysis_results_FDZD.RDS")



