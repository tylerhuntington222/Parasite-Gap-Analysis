
setwd("~/Dropbox/parks_for_parasites")

require(ggplot)
require(ggplot2)

### LOAD DATA
res <- read.csv("results/FDZD_final_gap_analysis_results.csv")

# Make new column called NPA_RANGE that is the total non-protected area
res$NPA_RANGE <- 0
res$NPA_RANGE <- (res$TOTAL_DIST_AREA.0 - res$PA_OVERLAP_AREA.0)

# Current protection as function of initial range size
jpeg(file = "figures/p1.1_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")
plot.data <- res[res$DISPERSAL == "100",]
p1 <- ggplot(aes(y = NPA_RANGE , x = TOTAL_DIST_AREA.0, color = CLADE), 
             data = plot.data) + geom_point() +
  labs(y = "Total Current Non-Protected Area (km sq.)", 
       x = "Current Range Size (km sq.)")  +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p1

dev.off()


