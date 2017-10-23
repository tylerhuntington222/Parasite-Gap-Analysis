setwd("~/Dropbox/parks_for_parasites/output")

x <- readRDS("all_raster_overlay_results.rds")
x
head(x)

# edit data frame to have only 1 row per species

# First calculate column that takes PA_OVERLAP_AREA/TOTAL_DIST_AREA

# calculate the % total that falls under current protection and then for
# each climate scenario calculate % total that overlaps with park areas

# For each climate scenario create a column for percent change from 
# current scenario. Percent change = 1 - POcurrent/POscenario