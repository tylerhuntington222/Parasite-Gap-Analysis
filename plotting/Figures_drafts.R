### Figures.R
### 30/10/2017

# The purpose of this is to organize the results of our data frame by Clade and 
# make figures. 

install.packages("ggplot2")

library("ggplot2")

setwd("~/Dropbox/parks_for_parasites/output")

# Read in the 2 dataframes to be merged in order to get a column with just the genus

df <- read.csv(file = "wide_df.csv", header = TRUE)
df <- df[,2:ncol(df)]
df$SPECIES <- as.character(df$SPECIES)

df2 <- read.csv(file = "min5_US_para_species_data.csv", header = TRUE)
df2 <- df2[,2:ncol(df2)]
df2$SPECIES <- as.character(df2$SPECIES)

#####
##### Am I supposed to be using "min5_US_para_species_data.csv"?
#####

# Call merge on df and df2

df_merge <- merge(df, df2, by = "SPECIES")

# Get rid of third column titled "X"

df4 <- df_merge[-c(2)]

write.csv(df4, file = "Results_by_clade.csv")

#############################################################################
### Figure 1: % of species that had increases or no increases in PA 
### overlap for each scenario. 
#############################################################################

# Create data frame that sums each PCT_CHANGE column with 



# Make Figure

figure1 <- ggplot(F1, 
                    aes(x = RCP, y = PCT_CHANGE, fill = CLADE)) +
  geom_bar(stat = 'identity', position='dodge') +
  scale_fill_manual("Model System",
                    labels = c("Increased", "Decreased"),
                    values=c(red, blue)) +
  xlab("Spatial Scale (m)") +
  #theme(axis.text.x=element_text(size=axis.text), 
#        axis.text.y=element_text(size=axis.text),
#        axis.title.x = element_text(size = axis.title),
#        axis.title.y = element_text(size = axis.title),
#        legend.text = element_text(size = legend.text),
#        legend.title = element_text(size = legend.title),
#        legend.position = "right", 
#        legend.direction = "vertical",
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_blank(), 
#        axis.line = element_line(colour = "black")) +
  ylab("Number of Species")

plot(figure1)









