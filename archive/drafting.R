
# LIBRARIES
require(raster)
require(rgdal)


setwd("~/Dropbox/parks_for_parasites/")


# load global WDPA data

# load US state boundaries data
us.states <- readOGR(dsn = "data/US_state_boundaries/cb_2016_us_state_20m.shp",
                     layer = "cb_2016_us_state_20m")

# attribute subset for protected areas in US

# spatial subset for protected areas in <state>



# load parasite distribution data for sample species accross climate scenarios
pearl <- raster("data/PEARL/AMBLYOMMA CUNEATUM current.gri")


# convert parasite distributions to bounding polygons

# iterating over para species and scenarios, calculate overlap between dist and PAs

for (para in para.species) {
  for (s in scenarios) {
    
    # interset para dist for this climate scenario with PA network
    
    # calculate total area of overlap
    
    # calculate error involved in estimation (TODO: figure out how to do this??)
    
  }
}