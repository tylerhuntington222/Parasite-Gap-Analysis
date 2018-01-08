
#-----------------------------------------------------------------------------#
# analysisSandbox.R
# Tyler Huntington, 2017

# PURPOSE
# Experimental analysis and plotting script for exploring 
# gap analysis output data.

# BIO 135 Parasite Ecology and Conservation
# Project: US Parasites and Parks Gap Analysis Study
#-----------------------------------------------------------------------------#

setwd("~/Dropbox/parks_for_parasites/")

require(ggplot)
require(ggplot2)

### LOAD DATA
res <- read.csv("results/gap_analysis_full_results.csv")

### MANIP DATA FOR PLOTTING
res <- res[,(! names(res) %in%c("X"))]

# change name of AVG_CHANGE col
names(res)[names(res) == "AVG_CHANGE"] <- "AVG_CHANGE_PROP_PROT"

total.dist.cols <- grep("TOTAL_DIST_AREA", names(res))
total.dist.cols <- total.dist.cols[2:length(total.dist.cols)]

PA.dist.cols <- grep("PA_OVERLAP_AREA", names(res))
PA.dist.cols <- PA.dist.cols[2:length(PA.dist.cols)]

res$AVG_TOTAL_DIST_2070 <- rowMeans(res[,c(total.dist.cols)])
res$AVG_PA_DIST_2070 <- rowMeans(res[,c(PA.dist.cols)])

res$TOTAL_AREA_CHANGE = res$AVG_TOTAL_DIST_2070  - res$TOTAL_DIST_AREA.0
res$PA_AREA_CHANGE = res$AVG_PA_DIST_2070  - res$PA_OVERLAP_AREA.0
res$NONPA_AREA_CHANGE = res$TOTAL_AREA_CHANGE - res$PA_AREA_CHANGE

# calculate col for percent range change that occurred outsite protected areas
res$PERC_DIST_CHANGE_NPA <- res$NONPA_AREA_CHANGE/ res$TOTAL_AREA_CHANGE
res$PERC_DIST_CHANGE_PA <- res$PA_AREA_CHANGE/ res$TOTAL_AREA_CHANGE

# determine which species gained or lost range from current to 2070
res$PA_RANGE_GAIN <- res$PA_AREA_CHANGE > 0
res$PA_RANGE_LOSS <- res$PA_AREA_CHANGE < 0

# determine which species gained or lost non protectedrange from current to 2070
res$NPA_RANGE_GAIN <- res$NONPA_AREA_CHANGE > 0
res$NPA_RANGE_LOSS <- res$NONPA_AREA_CHANGE < 0

# 
res$NPA_DOMINATED_CHANGE <- res$PERC_DIST_CHANGE_PA < res$PCT_AREA_PROTECTED.0
res$PA_DOMINATED_CHANGE <- res$PERC_DIST_CHANGE_NPA > res$PCT_AREA_PROTECTED.0


# group species into process categories
# Categorization Scheme:
# 1. Lose PA and lose NPA
# 2. Lose PA and gain NPA
# 3. Gain PA and lose NPA
# 4. Gain PA and gain NPA
res$DRIVER_CAT <- NA

for (i in 1:nrow(res)) {
  if (res$PA_RANGE_LOSS[i] & res$NPA_RANGE_LOSS[i]) {
    res$DRIVER_CAT[i] <- 1
  }
  
  if (res$PA_RANGE_LOSS[i] & res$NPA_RANGE_GAIN[i]) {
    res$DRIVER_CAT[i] <- 2
  }
  
  if (res$PA_RANGE_GAIN[i] & res$NPA_RANGE_LOSS[i]) {
    res$DRIVER_CAT[i] <- 3
  }
  
  if (res$PA_RANGE_GAIN[i] & res$NPA_RANGE_GAIN[i]) {
    res$DRIVER_CAT[i] <- 4
  }
}

res$DRIVER_CAT <- as.factor(res$DRIVER_CAT)

# create column for distiguishing between species that experienced increases
# in protected area from current to 2070
res$PA_GAINED <- res$PA_AREA_CHANGE > 0

# create column for percent change in protected area for each species
# from current to 2070
res$PERC_CHANGE_GROSS_PA <- (res$AVG_PA_DIST_2070-res$PA_OVERLAP_AREA.0)/
  res$PA_OVERLAP_AREA.0

res$PERC_CHANGE_TOT_RANGE <- (res$TOTAL_AREA_CHANGE/res$TOTAL_DIST_AREA.0)

# change dispersal var to factor
res$DISPERSAL <- as.factor(res$DISPERSAL)

# add col for expected loss of 



######################### PLOTS ######################### 


###### CURRENT LANDSCAPE OF PROTECTION 
# Q: Do clades vary by current R_PA?

p <- ggplot(aes(y = PA_OVERLAP_AREA.0, x = CLADE, fill = CLADE), 
            data = res) + 
  geom_boxplot() + 
  labs(x = "Clade" )+
  labs(y = "Current Range in PAs (km sq.)") 
p


# Q: Do clades vary by current PRP?
jpeg(file = "figures/currentPRC_v_clade_boxplot.jpg", res = 400, width = 9, height = 6,
     units = "in")
p <- ggplot(aes(y = PCT_AREA_PROTECTED.0, x = CLADE, fill = CLADE), 
            data = res) + 
  geom_boxplot() + 
  labs(x = "Clade" )+
  labs(y = "Current Percentage of Range in PAs (%)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

p
dev.off()


# make violin plot of percent change in protection by driving process 
# Categorization Scheme:
# 1. Range gain and most gain inside of PAs
# 2. Range gain and most gain outside of PAs
# 3. Range loss and most loss inside of PAs
# 4. Range loss and most loss outside of PAs
plot.data <- res[res$DISPERSAL == "100",]
p <- ggplot(aes(y = PERC_CHANGE_GROSS_PA, x = DRIVER_CAT, fill = DISPERSAL), 
            data = plot.data) + geom_violin()+
  labs(x = "Driving Process") +
  labs(y = "Percent Change in Protected Range Size by 2070")
p




# Q: Do clades vary by percent change in protection from current to 2070?
p <- ggplot(aes(y = PERC_CHANGE_GROSS_PA, x = CLADE, fill = DISPERSAL), 
            data = res) + 
  geom_boxplot()+ 
  labs(x = "Clade")+
  labs(y = "Percent Change in Protected Range Size by 2070") + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0))
p

# make violin plot of percent change in protection by driving process 
# Categorization Scheme:
# 1. Range gain and most gain inside of PAs
# 2. Range gain and most gain outside of PAs
# 3. Range loss and most loss inside of PAs
# 4. Range loss and most loss outside of PAs
plot.data <- res[res$DISPERSAL == "100",]
p <- ggplot(aes(y = PERC_CHANGE_GROSS_PA, x = DRIVER_CAT, fill = DISPERSAL), 
            data = plot.data) + geom_violin()+
  labs(x = "Driving Process") +
  labs(y = "Percent Change in Protected Range Size by 2070")
p


#------------------------------------------------------------------------------#
# Q: Do clades vary by percent change in ratio of protected range to total range 
 
  labs(x = "Clade")+
  labs(y = "% Change in ratio of Protected:Total Range by 2070")
p

# make violin plot of percent change in protection by driving process 
# Categorization Scheme:
# 1. Protected Range gain and most gain inside of PAs
# 2. Protected Range gain and most gain outside of PAs
# 3. Protected Range loss and most loss inside of PAs
# 4. Protected Range loss and most loss outside of PAs

plot.data <- res[res$DISPERSAL == "100",]
p <- ggplot(aes(y = AVG_CHANGE_PROP_PROT, x = DRIVER_CAT, fill = DISPERSAL), 
            data = plot.data) + geom_violin()+
  labs(x = "Driving Process") +
  labs(y = "% Change in ratio of Protected:Total Range by 2070")
p

#------------------------------------------------------------------------------#

# Q: Do species with larger current ranges have larger ranges in 2070
###
# plot regression of current range size vs 2070 range size
p <- ggplot(aes(y = AVG_TOTAL_DIST_2070 , x = TOTAL_DIST_AREA.0, col = DISPERSAL), 
            data = res) + geom_point()+
  labs(x = "Current Range Area (km sq.)", y = "2070 Range Area (km sq.)")
p
###
# A: Yes, strong positive relationship between current and future range size.

#------------------------------------------------------------------------------#
# Q: Is there a relationship between current range size and amount of range 
# gained or lost by 2070?

###
# plot regression of current range size vs change in range by 2070 
p <- ggplot(aes(y = TOTAL_AREA_CHANGE , x = TOTAL_DIST_AREA.0, col = DISPERSAL), 
            data = res) + geom_point() + 
  labs(x = "Current Range Area (km sq.)", 
       y = "Change in Range Area by 2070 (km sq.)")
p
###
# A: Species that have larger ranges currently tend to lose more native habitat
# by 2070 than species with small current ranges.

#------------------------------------------------------------------------------#

# Q: Do species with larger current ranges have lower protected:total range 
# ratios than species with larger ranges?
###
# plot current range size vs. current protected:total range ratio
plot.data <- res[res$DISPERSAL == "100",]
p <- ggplot(aes(y = PCT_AREA_PROTECTED.0 , x = TOTAL_DIST_AREA.0), 
            data = plot.data) + geom_point() +
  labs(y = "% Current Range in Protected Areas", 
         x = "Current Range Size (km sq.)")
p
###
# A: Species with smallest current ranges have the largest protected:total
# range ratios. What is going on at large current ranges though??
#------------------------------------------------------------------------------#


# Q: What is the relationship between species' change in gross PA occupation and 
# the change in their PA:Total range ratio
###
# plot current range size vs. current protected:total range ratio

p <- ggplot(aes(y = AVG_CHANGE_PROP_PROT, x = PERC_CHANGE_GROSS_PA, col = DISPERSAL), 
            data = res) + geom_point() + 
  labs(y = "% Change in ratio of Protected:Total Range by 2070", 
       x = "% Change in Protected Range by 2070")
p
###
# A: Species with smallest current ranges have the largest protected:total
# range ratios. 
#------------------------------------------------------------------------------#


# Q: Under 0% dispersal, what is the relationship between magnitude of range 
# loss and change in protected:total range

###
# plot current range size vs. current protected:total range ratio
plot.data <- res[res$DISPERSAL == "0",]
p <- ggplot(aes(y = AVG_CHANGE_PROP_PROT, x = PERC_CHANGE_TOT_RANGE ), 
            data = res) + geom_point()
p + labs(y = "% Change in ratio of Protected:Total Range by 2070", 
         x = "% Change in Total Range by 2070")
###
# A: Species that lose the most total range by 2070 also experience
# the largest gains in proportion of their range that is protected.
# Species that lose relatively less range experience smaller gains in proportion
# of their range that falls in protected areas. 
#------------------------------------------------------------------------------#



# Q: Under 0% dispersal, what is the relationship between the actual and expected
# lost of protected area measured as the proportion of total range loss that 
# occurred in protected areas? Note, expected proportion is based on current fraction
# of range that falls within protected areas.

###
# plot current range size vs. current protected:total range ratio
plot.data <- res[res$DISPERSAL == "0",]
p <- ggplot(aes(y = PERC_DIST_CHANGE_PA, x = PCT_AREA_PROTECTED.0,
                col = PERC_CHANGE_TOT_RANGE), 
            data = plot.data) + geom_point() +
  labs(x = "Expected Ratio of Protected:Total Range Loss", 
       y = "Actual Ratio of Protected:Total Range Loss") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, col = 'purple')
p

###
# A: Model these residuals in some way to determine why species fall above or 
# below the diagonal?
#------------------------------------------------------------------------------#



# Q: Under 100% dispersal, what is the relationship between the actual and expected
# lost of protected area for those species that lose range overall? 
# Note, expected proportion is based on current fraction
# of range that falls within protected areas.

###
# plot current range size vs. current protected:total range ratio
plot.data <- res[res$DISPERSAL == "100" & res$RANGE_LOSS,]
p <- ggplot(aes(y = PERC_DIST_CHANGE_PA, x = PCT_AREA_PROTECTED.0,
                col = PERC_CHANGE_TOT_RANGE), 
            data = plot.data) + geom_point() +
  labs(x = "Expected Ratio of Protected:Total Range Loss", 
       y = "Actual Ratio of Protected:Total Range Loss") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, col = 'red')
p

###
# A: Model these residuals in some way to determine why species fall above or 
# below the diagonal?
#------------------------------------------------------------------------------#



# Q: Under 100% dispersal, what is the relationship between the actual and expected
# gain of protected area for those species that gain range overall? 
# Note, expected proportion is based on current fraction
# of range that falls within protected areas.

###
# plot current range size vs. current protected:total range ratio
plot.data <- res[res$DISPERSAL == "100" & res$RANGE_GAIN,]
p <- ggplot(aes(y = PERC_DIST_CHANGE_PA, x = PCT_AREA_PROTECTED.0,
                col = PERC_CHANGE_TOT_RANGE), 
            data = plot.data) + geom_point() +
  labs(x = "Expected Ratio of Protected:Total Range Gain", 
       y = "Actual Ratio of Protected:Total Range Gain") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, col = 'red')
p

###
# A: Model these residuals in some way to determine why species fall above or 
# below the diagonal?
#------------------------------------------------------------------------------#



###### FINAL PLOTS


### Added 12/12/2017
### Current protection as a function of all these together 
### (range, number of hosts, transmission cycle)

### Current protection = res$PCT_AREA_PROTECTED.0 = y

# Save res into new data frame called figdf
figdf <- data.frame(res)

# First add column for number of hosts in figdf
figdf$N_HOSTS <- 0

# Read in data frame with column for number of hosts
hostDF <- read.csv("output/GeneralismDF.csv")





#------------------------------------------------------------------------------#

# Q: Do species with larger current ranges have larger ranges in 2070
###
# plot regression of current range size vs 2070 range size
jpeg(file = "figures/p0_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")
p0 <- ggplot(aes(y = AVG_TOTAL_DIST_2070 , x = TOTAL_DIST_AREA.0, col = DISPERSAL), 
            data = res) + geom_point()+
  labs(x = "Current Range Area (km sq.)", y = "2070 Range Area (km sq.)")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p0
dev.off()

###
# A: Yes, strong positive relationship between current and future range size.

#------------------------------------------------------------------------------#

# Current protection as function of initial range size
jpeg(file = "figures/p1_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")
plot.data <- res[res$DISPERSAL == "100",]
p1 <- ggplot(aes(y = 100*PCT_AREA_PROTECTED.0 , x = TOTAL_DIST_AREA.0, color = CLADE), 
             data = plot.data) + geom_point() +
  labs(y = "% Current Range in Protected Areas", 
       x = "Current Range Size (km sq.)")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p1

dev.off()

# Current protection as a function of number of hosts
jpeg(file = "figures/p2.0_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")
p2 <- ggplot(aes(y = 100*PCT_AREA_PROTECTED.0 , x = N_HOSTS, color = CLADE), 
            data = hostDF) + geom_point() +
  labs(y = "% Current Range in Protected Areas", 
       x = "Number of Definitive Hosts") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p2
dev.off()

# If you have more hosts do you have a larger overall range?
jpeg(file = "figures/p2.1_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")
p2_1 <- ggplot(aes(y = TOTAL_DIST_AREA.0 , x = N_HOSTS, color = CLADE), 
             data = hostDF) + geom_point() +
  labs(y = "Total Current Distribution Area", 
       x = "Number of Definitive Hosts") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) 
  
p2_1
dev.off()


# Need dataframe with direct and indirect classifications to make third figure
# for current protection
# merge lifecycle data to figdf
lifecycles.df <- read.csv("data/us_endo_paras_lifecycle.csv")
lifecycles.df <- lifecycles.df[, c("SCI_NAME", "LIFECYCLE")]
names(lifecycles.df) <- c("SPECIES", "LIFE_CYCLE")
lifecycles.df$LIFE_CYCLE <- as.character(lifecycles.df$LIFE_CYCLE)
lifecycles.df$LIFE_CYCLE <- toupper(lifecycles.df$LIFE_CYCLE)
nons <- which(!(lifecycles.df$LIFE_CYCLE %in% c("DIRECT", "INDIRECT")))
lifecycles.df$LIFE_CYCLE[nons] <- NA
res <- merge(res, lifecycles.df, all.x = T)
#lcycle$LIFE_CYCLE <- as.factor(lcycle$LIFE_CYCLE)

# Boxplots of current protection by transmission (direct or indirect)
plot.data <- res[!is.na(lifecycles.df$LIFE_CYCLE),]

jpeg(file = "figures/p3_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")
p3 <- ggplot(aes(y = 100*PCT_AREA_PROTECTED.0, x = LIFE_CYCLE, fill = LIFE_CYCLE), 
            data = plot.data) + 
  geom_boxplot() + 
  labs(x = "Life Cycle" )+
  labs(y = "% Current Range in Protected Areas") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p3
dev.off()


### Future protection as a function of initial range size (???), 
### number of hosts, transmission and how relate to future protection 
### for both 0 and 100 scenario

# Make new column in figdf called AVG_PCT_AREA_PROTECTED.RCP
figdf$AVG_PCT_AREA_PROTECTED.RCP <- 0

# Calculate this new column as average of PCT_AREA_PROTECTED for all RCP levels
figdf$AVG_PCT_AREA_PROTECTED.RCP <- ((figdf$PCT_AREA_PROTECTED.26 + figdf$PCT_AREA_PROTECTED.45 + figdf$PCT_AREA_PROTECTED.60 + figdf$PCT_AREA_PROTECTED.85)/4)

# Future protection as function of initial range size
jpeg(file = "figures/p4_plot.jpg", res = 400, width = 9, height = 6,
     units = "in")

plot.data <- figdf[figdf$DISPERSAL == "100",]
plot.data <- plot.data[!is.na(plot.data$AVG_PCT_AREA_PROTECTED.RCP),]
p4 <- ggplot(aes(y = 100*AVG_PCT_AREA_PROTECTED.RCP , x = TOTAL_DIST_AREA.0, color = CLADE), 
            data = plot.data) + geom_point() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(y = "% Future Range in Protected Areas", 
       x = "Current Range Size (km sq.)")
p4
dev.off()
# Make new column in hostDF called AVG_PCT_AREA_PROTECTED.RCP
hostDF$AVG_PCT_AREA_PROTECTED.RCP <- 0

# Calculate this new column as average of PCT_AREA_PROTECTED for all RCP levels
res$AVG_PCT_AREA_PROTECTED.RCP <- ((res$PCT_AREA_PROTECTED.26 +
                                      res$PCT_AREA_PROTECTED.45 + 
                                      res$PCT_AREA_PROTECTED.60 + 
                                      res$PCT_AREA_PROTECTED.85)/4)

# Subset out rows in hostDF that have NA
hostDF <- hostDF[complete.cases(hostDF), ]

n_hosts.df <- hostDF[,c("SPECIES", "N_HOSTS")]
res <- merge(res, n_hosts.df, all.x = T)

# Future protection as a function of number of hosts
# Q: Do clades vary by percent change in protection from current to 2070?
jpeg(file = "figures/p5_PRP_v_NHOSTS.jpg", res = 400, width = 9, height = 6,
     units = "in")
p5 <- ggplot(aes(y = 100*AVG_PCT_AREA_PROTECTED.RCP , x = N_HOSTS, color = DISPERSAL), 
             data = res) + geom_point() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(y = "Percent of 2070 Range in Protected Areas", 
       x = "Number of Definitive Hosts")
p5
dev.off()

# Make new column in lcycle called AVG_PCT_AREA_PROTECTED.RCP
lcycle$AVG_PCT_AREA_PROTECTED.RCP <- 0

# Calculate this new column as average of PCT_AREA_PROTECTED for all RCP levels
lcycle$AVG_PCT_AREA_PROTECTED.RCP <- ((lcycle$PCT_AREA_PROTECTED.26 + lcycle$PCT_AREA_PROTECTED.45 + lcycle$PCT_AREA_PROTECTED.60 + lcycle$PCT_AREA_PROTECTED.85)/4)

# Subset out NAs in lcycle$AVG_PCT_AREA_PROTECTED.RCP
lcycle <- lcycle[!is.na(lcycle$AVG_PCT_AREA_PROTECTED.RCP),]

# Boxplots of future protection by transmission (direct or indirect)
# Q: Do clades vary by percent change in protection from current to 2070?
jpeg(file = "figures/p6_boxplot_lifecycle.jpg", res = 400, width = 9, height = 6,
     units = "in")
plot.data <- lcycle[!is.na(lcycle$LIFE_CYCLE),]
p6 <- ggplot(aes(y = 100*AVG_PCT_AREA_PROTECTED.RCP, x = LIFE_CYCLE, fill = LIFE_CYCLE), 
             data = plot.data) + 
  geom_boxplot() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(x = "Life Cycle" )+
  labs(y = "% Future Range in Protected Areas") 
p6

dev.off()

# # Q: Do clades vary by percent change in protection from current to 2070?
# jpeg(file = "figures/p7_boxplot_change_PRP.jpg", res = 400, width = 9, height = 6,
#      units = "in")
# p7 <- ggplot(aes(y = 100*PERC_CHANGE_GROSS_PA, x = CLADE, fill = DISPERSAL), 
#             data = res) + 
#   geom_boxplot()+ 
#   labs(x = "Clade")+
#   labs(y = "Percent Change in Protected Range Size by 2070") + 
#   theme(axis.text.x = element_text(angle = -45, hjust = 0))
# p7

# Q: Do clades vary by percent change in protection from current to 2070?
jpeg(file = "figures/p8_boxplot_change_PRP.jpg", res = 400, width = 9, height = 6,
     units = "in")
p8 <- ggplot(aes(y = 100*AVG_CHANGE_PROP_PROT, x = CLADE, fill = DISPERSAL), 
            data = res) + 
  geom_boxplot()+ 
  labs(x = "Clade")+
  labs(y = "Percent Change in Percent Range Protected") + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12))
p8

dev.off()


# Q: Do clades vary by percent change in protection from current to 2070?
jpeg(file = "figures/p8.1_boxplot_change_PRP.jpg", res = 400, width = 9, height = 6,
     units = "in")
p8.1 <- ggplot(aes(y = 100*AVG_CHANGE_PROP_PROT, x=DISPERSAL, fill = DISPERSAL), 
             data = res) + 
  geom_boxplot()+ 
  labs(x = "Dispersal")+
  labs(y = "Percent Change in Percent Range Protected") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p8.1

dev.off()

# Q: Do clades vary by percent change in protection from current to 2070?
jpeg(file = "figures/p8.2_boxplot_change_PRP.jpg", res = 400, width = 9, height = 6,
     units = "in")
p8.2 <- ggplot(aes(y = 100*PERC_CHANGE_GROSS_PA, x=DISPERSAL, fill = DISPERSAL), 
               data = res) + 
  geom_boxplot()+ 
  labs(x = "Dispersal")+
  labs(y = "Percent Change in Range Area in Protected Areas") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p8.2

dev.off()




#------------------------------------------------------------------------------#
# Q: Do clades vary by percent change in ratio of protected range to total range 

labs(x = "Clade")+
  labs(y = "% Change in ratio of Protected:Total Range by 2070")
p

# make violin plot of percent change in protection by driving process 
# Categorization Scheme:
# 1. Protected Range gain and most gain inside of PAs
# 2. Protected Range gain and most gain outside of PAs
# 3. Protected Range loss and most loss inside of PAs
# 4. Protected Range loss and most loss outside of PAs

jpeg(file = "figures/p2_violin.jpg", res = 400, width = 9, height = 6,
     units = "in")

plot.data <- res[res$DISPERSAL == "100",]
p9 <- ggplot(aes(y = 100*AVG_CHANGE_PROP_PROT, x = DRIVER_CAT, fill = DRIVER_CAT), 
            data = plot.data) + geom_violin()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12)) +
  labs(x = "Driving Process") +
  labs(y = "% Change in Percent Range Protected by 2070") +
  geom_abline(mapping = NULL, data = NULL, slope = 0, intercept = 0,
              na.rm = FALSE, show.legend = NA, col = 'blue')
p9

table(plot.data$DRIVER_CAT)
table((plot.data[plot.data$AVG_CHANGE_PROP_PROT > 0, ]$DRIVER_CAT))

dev.off()


####### SUMMARY STATS

# median change in RPA
median(res[res$DISPERSAL == "0",]$PERC_CHANGE_GROSS_PA)
median(res[res$DISPERSAL == "100",]$PERC_CHANGE_GROSS_PA)

# median change in PRP
median(res[res$DISPERSAL == "0",]$AVG_CHANGE_PROP_PROT)
median(res[res$DISPERSAL == "100",]$AVG_CHANGE_PROP_PROT)


# lifecyle types
median(na.omit((res[res$LIFE_CYCLE == "DIRECT",]$AVG_PCT_AREA_PROTECTED.RCP)))
median(na.omit(res[res$LIFE_CYCLE == "INDIRECT",]$AVG_PCT_AREA_PROTECTED.RCP))


# get median levels of current protection by clade
clades <- unique(res$CLADE)

for (c in clades) {
  print (c)
  print (quantile(res[res$CLADE == c,]$PCT_AREA_PROTECTED.0))
}









