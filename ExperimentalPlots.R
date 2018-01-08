# sandbox for developing figures

require(ggplot2)

# set working directory
setwd("~/Dropbox/parks_for_parasites")

# load gap analysis results
fd.df <- read.csv("output/wide_df_FDZD.csv")
fd.df <- fd.df[, !(names(fd.df) %in% c("X"))]


# load species clade key
paras <- read.csv("US_para_species_data.csv")
names(paras)[names(paras)=="ScientificName"] <- "SPECIES"

# generate col for avg percent change among all RCP levels
change.cols <- grep("CHANGE", names(fd.df))
fd.df$AVG_CHANGE<- rowMeans(fd.df[, change.cols])

# merge clade data with gap results
fd.df <- merge(fd.df, paras)
fd.df$DISPERSAL <- as.factor(fd.df$DISPERSAL)

# make boxplot of percent change in protection by rcp level
p <- ggplot(aes(y = AVG_CHANGE, x = Clade, fill = DISPERSAL), data = fd.df) + geom_boxplot()
p + labs(x = "RCP Level")
p + labs(y = "Percent Change in Protection")



# subset for zero dispersal
fd.df <- fd.df[fd.df$DISPERSAL == "100" | fd.df$DISPERSAL == "current",]
# produce clade-wise boxplot of avg percent change in protection by 2070
ggplot(aes(y = AVG_CHANGE, x = Clade), data = fd.df) + geom_boxplot()


# reshape data to long form
long.df <- reshape(fd.df, varying = change.cols,
                   timevar = "RCP", idvar = "SPECIES", direction = "long")

long.df$RCP <- as.character(long.df$RCP)

# make boxplot of percent change in protection by rcp level
p <- ggplot(aes(y = PCT_CHANGE, x = RCP, fill = Clade), data = long.df) + geom_boxplot()
p + labs(x = "RCP Level")
p + labs(y = "Percent Change in Protection")









