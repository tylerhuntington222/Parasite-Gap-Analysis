### Figures_prelim.R
### November 6, 2017

install.packages("ggplot2")

library("ggplot2")
library("reshape2")

setwd("~/Desktop/Parks for Parasites Tests")

df <- read.csv(file = "Results_by_clade.csv", header = TRUE)

# Subset each clade into its own data frame

trem <- subset(df, df$CLADE == "TREMATODA")
ixo <- subset(df, df$CLADE == "IXODIDA")
nem <- subset(df, df$CLADE == "NEMATODA")
sipho <- subset(df, df$CLADE == "SIPHONAPTERA")
ces <- subset(df, df$CLADE == "CESTODA")
acanth <- subset(df, df$CLADE == "ACANTHOCEPHALA")
astig <- subset(df, df$CLADE == "ASTIGMATA")
phthir <- subset(df, df$CLADE == "PHTHIRAPTERA")

# Get rid of all rows with any NA values
ixo <- ixo[complete.cases(ixo), ]
nem <- nem[complete.cases(nem), ]

df2 <- df[complete.cases(df), ]

# check <- nrow(trem) + nrow(ixo) + nrow(nem) + nrow(sipho) + nrow(ces) + nrow(acanth) + nrow(astig) + nrow(phthir)
# check  # check should equal 217, and it does

# Figure 1
# % of species that had increases and decreases in PA overlap for each scenario 

# Create data frame to store calculated % with increases and % with decreases
F1 <- data.frame(POS_NEG = c("Increase", "Decrease", "Increase", "Decrease", "Increase", "Decrease", "Increase", "Decrease"), 
                 RCP = c(2.6, 2.6, 4.5, 4.5, 6.0, 6.0, 8.5, 8.5), 
                 NUM_SPECIES = c(0,0,0,0,0,0,0,0),
                 PCT_SPECIES = c(0,0,0,0,0,0,0,0))

# Populate data frame with calculated NUM_SPECIES from df

for (i in 1:nrow(df2)){
  if (df2$PCT_CHANGE.26[i] > 0){
    F1$NUM_SPECIES[1] <- F1$NUM_SPECIES[1] + 1
  }
  else {
    F1$NUM_SPECIES[2] <- F1$NUM_SPECIES[2] + 1
  }
}

for (i in 1:nrow(df2)){
  if (df2$PCT_CHANGE.45[i] > 0){
    F1$NUM_SPECIES[3] <- F1$NUM_SPECIES[3] + 1
  }
  else {
    F1$NUM_SPECIES[4] <- F1$NUM_SPECIES[4] + 1
  }
}

for (i in 1:nrow(df2)){
  if (df2$PCT_CHANGE.60[i] > 0){
    F1$NUM_SPECIES[5] <- F1$NUM_SPECIES[5] + 1
  }
  else {
    F1$NUM_SPECIES[6] <- F1$NUM_SPECIES[6] + 1
  }
}

for (i in 1:nrow(df2)){
  if (df2$PCT_CHANGE.85[i] > 0){
    F1$NUM_SPECIES[7] <- F1$NUM_SPECIES[7] + 1
  }
  else {
    F1$NUM_SPECIES[8] <- F1$NUM_SPECIES[8] + 1
  }
}

# Calculate PCT_SPECIES column of F1
F1$PCT_SPECIES[1] <- (F1$NUM_SPECIES[1])/(F1$NUM_SPECIES[1] + F1$NUM_SPECIES[2])
F1$PCT_SPECIES[2] <- (F1$NUM_SPECIES[2])/(F1$NUM_SPECIES[1] + F1$NUM_SPECIES[2])

F1$PCT_SPECIES[3] <- (F1$NUM_SPECIES[3])/(F1$NUM_SPECIES[3] + F1$NUM_SPECIES[4])
F1$PCT_SPECIES[4] <- (F1$NUM_SPECIES[4])/(F1$NUM_SPECIES[3] + F1$NUM_SPECIES[4])

F1$PCT_SPECIES[5] <- (F1$NUM_SPECIES[5])/(F1$NUM_SPECIES[5] + F1$NUM_SPECIES[6])
F1$PCT_SPECIES[6] <- (F1$NUM_SPECIES[6])/(F1$NUM_SPECIES[5] + F1$NUM_SPECIES[6])

F1$PCT_SPECIES[7] <- (F1$NUM_SPECIES[7])/(F1$NUM_SPECIES[7] + F1$NUM_SPECIES[8])
F1$PCT_SPECIES[8] <- (F1$NUM_SPECIES[8])/(F1$NUM_SPECIES[7] + F1$NUM_SPECIES[8])

# Create barplot for Figure 1
p1<- ggplot(F1, aes(fill = POS_NEG, y = PCT_SPECIES, x = RCP)) + 
  geom_bar(position="dodge", stat="identity")
p1

# Save Figure 1 to Figure1.pdf
pdf("Figure1.pdf")
plot(p1)
dev.off()

# Figure 2
# What is the % change in %PA overlap in the distribution of all the species 
# from each clade in each scenario (nematodes, trematodes…)
# Ex. Nematodes experienced on average x% change in percent overlap with PAs 
# in their distribution in scenario...

# Create empty data frame
F2 <- data.frame(CLADE = c("TREMATODA", "IXODIDA", "NEMATODA", "SIPHONAPTERA", 
                           "CESTODA", "ACANTHOCEPHALA", "ASTIGMATA", "PHTHIRAPTERA"), 
                 PCT_CHANGE_2.6 = c(1,1,1,1,1,1,1,1),
                 PCT_CHANGE_4.5 = c(1,1,1,1,1,1,1,1),
                 PCT_CHANGE_6.0 = c(1,1,1,1,1,1,1,1),
                 PCT_CHANGE_8.5 = c(1,1,1,1,1,1,1,1))

# Populate the data frame witht the averages from each clade for each RCP scenario
F2$PCT_CHANGE_2.6[1] <- mean(trem$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[1] <- mean(trem$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[1] <- mean(trem$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[1] <- mean(trem$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[2] <- mean(ixo$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[2] <- mean(ixo$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[2] <- mean(ixo$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[2] <- mean(ixo$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[3] <- mean(nem$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[3] <- mean(nem$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[3] <- mean(nem$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[3] <- mean(nem$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[4] <- mean(sipho$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[4] <- mean(sipho$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[4] <- mean(sipho$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[4] <- mean(sipho$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[5] <- mean(ces$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[5] <- mean(ces$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[5] <- mean(ces$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[5] <- mean(ces$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[6] <- mean(acanth$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[6] <- mean(acanth$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[6] <- mean(acanth$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[6] <- mean(acanth$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[7] <- mean(astig$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[7] <- mean(astig$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[7] <- mean(astig$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[7] <- mean(astig$PCT_CHANGE.85)

F2$PCT_CHANGE_2.6[8] <- mean(phthir$PCT_CHANGE.26)
F2$PCT_CHANGE_4.5[8] <- mean(phthir$PCT_CHANGE.45)
F2$PCT_CHANGE_6.0[8] <- mean(phthir$PCT_CHANGE.60)
F2$PCT_CHANGE_8.5[8] <- mean(phthir$PCT_CHANGE.85)


# Transform F2 dataframe from wide to long form

F2_long <- melt(F2,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars = "CLADE",
                  # The source columns
                  measure.vars=c("PCT_CHANGE_2.6", "PCT_CHANGE_4.5", "PCT_CHANGE_6.0", "PCT_CHANGE_8.5"),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="RCP_initial",
                  value.name="PCT_CHANGE"
)

# Create new column to store RCP value
F2_long$RCP <- NA
F2_long$RCP_initial <- as.character(F2_long$RCP_initial)

# Split RCP value from string in RCP_initial column and store it in RCP column
for (i in 1:nrow(F2_long)){
  temp <- F2_long$RCP_initial[i]
  temp <- unlist(strsplit(temp, "_"))[3]
  F2_long$RCP[i] <- temp
}

# Create barplot for Figure 2
p2<- ggplot(F2_long, aes(fill = CLADE, y = PCT_CHANGE, x = RCP)) + 
  geom_bar(position="dodge", stat="identity")
p2

# Save Figure 2 to Figure2.pdf
pdf("Figure2.pdf")
plot(p2)
dev.off()
###############################################################################







