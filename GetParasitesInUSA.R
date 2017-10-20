
# GetParasitesInUSA.R
#
# A script to determine which parasites in the Carlson et al. 2017 study
# were reported captured/sighted at least three times in the conitguous US
#
# Tyler Huntington, 2017


# set working directory
setwd("~/Dropbox/parks_for_parasites/")

# LIBRARIES
require(raster)
require(rgdal)
require(raster)
require(maptools)
require(sp)
require(rgeos)
require(rdrop2)
require(plyr)
require(dplyr)

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
captures.df <- read.csv("data/Carlson_et_al_2017/table_s12.csv")

# create spatial points data from captures.df
coords.df <- captures.df[, c("Longitude", "Latitude")]
coords.df$Latitude <- as.numeric(coords.df$Latitude)
coords.df$Longitude <- as.numeric(coords.df$Longitude)
captures.sp <- SpatialPointsDataFrame(coords = coords.df, 
                                      data = captures.df,
                                      proj4string = crs(contig.us))

# spatial subset captures for those in contiguous US
us.captures <- captures.sp[contig.us,]

# extract dataframe for only contiguos US parasites
us.para.df <- us.captures@data

# create counter col for tallying captures by species
us.para.df$Counter <- 1

# collapse by species
collapse.df <- ddply(us.para.df, .(ScientificName), summarize, 
                     Count = sum(Counter))

# subset for species with at least three observations in contiguous US
collapse.df.subset <- collapse.df[collapse.df$Count >= 3,]

# get name list for US parasites
us.para.names <- collapse.df.subset$ScientificName

# export df with species
saveRDS(us.para.names, "output/contig_US_parasites.RDS")




