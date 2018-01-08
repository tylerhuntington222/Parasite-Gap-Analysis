### makeFullResultsCSV.R
### 27/10/2017

### Calculates the % distribution area protected for each species and 
### percent change protection from current to each climate scenario and
### converts the results in averaged_gap_analysis_results_min5..rds 
### from long-form to wide-form. Then merges species attributes to 
### data frame including number of vertebrate final hosts. 
### 

### OUTPUT: exports `gap_analysis_full_results.csv` to /output directory.
### This dataset is the most comprehensive set of results from the 
### entire analysis. 



setwd("~/Dropbox/parks_for_parasites/")

df <- readRDS("output/averaged_gap_analysis_results_FDZD.RDS")

head(df)

# subset for 100% dispersal results
df <- df[df$DISPERSAL == "100" | df$DISPERSAL == "current",]

# eliminate dispersal col
df <- df[, !(names(df) %in% c("DISPERSAL"))]


# Adds a new column (PCT_AREA_PROTECTED) to df, fills in each row with NA
df$PCT_AREA_PROTECTED <- NA

# Loop calculates the % area protected for each row and puts it in 
# PCT_AREA_PROTECTED column

for (i in 1:nrow(df)){
  df$PCT_AREA_PROTECTED[i] <- (df[i, "PA_OVERLAP_AREA"])/(df[i, "TOTAL_DIST_AREA"])
}

# Make new column called PCT_CHANGE
df$PCT_CHANGE <- NA

# Loop calculates: Percent change = 1 - POcurrent/POscenario
# currentP is the most recent value of PCT_AREA_PROTECTED when RCP = 0
# currentP is updated each time the loop encounters a new species

for (i in 1:nrow(df)){
  if (df$RCP[i] == 0){
    currentP <- df$PCT_AREA_PROTECTED[i]   
  }
  else {
    df$PCT_CHANGE[i] <- 1 - currentP/df$PCT_AREA_PROTECTED[i]
  }
}


# Transforms data frame from long to wide format
# timevar specifies variable that should be decomposed (RCP)
# idvar specifies variable that reshape function should not take into account (SPECIES)
# By not naming TOTAL_DIST_AREA and PA_OVERLAP_AREA, function knows that 
# they should be recombined

wide_df <- reshape(df, timevar = "RCP", idvar = "SPECIES", direction = "wide")

# Get rid of PCT_CHANGE.0 column

wide_df$PCT_CHANGE.0 <- NULL

# Add field to denote dispersal scenario
wide_df$DISPERSAL <- "100"

# store full dispersal wide df safely for later binding with zero dispersal results
wide_df_FD <- wide_df


############# TRANSFORM RESULTS FOR ZERO PERCENT DISPERSAL SCENARIO ############# 
df <- readRDS("output/averaged_gap_analysis_results_FDZD.RDS")

head(df)

# subset for 100% dispersal results
df <- df[df$DISPERSAL == "0" | df$DISPERSAL == "current",]

# eliminate dispersal col
df <- df[, !(names(df) %in% c("DISPERSAL"))]

# Adds a new column (PCT_AREA_PROTECTED) to df, fills in each row with NA
df$PCT_AREA_PROTECTED <- NA
head(df)

# Loop calculates the % area protected for each row and puts it in 
# PCT_AREA_PROTECTED column

for (i in 1:nrow(df)) {
  df$PCT_AREA_PROTECTED[i] <- (df[i, "PA_OVERLAP_AREA"])/(df[i, "TOTAL_DIST_AREA"])
}

# Make new column called PCT_CHANGE
df$PCT_CHANGE <- NA

# Loop calculates: Percent change = 1 - POcurrent/POscenario
# currentP is the most recent value of PCT_AREA_PROTECTED when RCP = 0
# currentP is updated each time the loop encounters a new species

for (i in 1:nrow(df)) {
  if (df$RCP[i] == 0){
    currentP <- df$PCT_AREA_PROTECTED[i]   
  }
  else {
    df$PCT_CHANGE[i] <- (df$PCT_AREA_PROTECTED[i] - currentP)/df$PCT_AREA_PROTECTED[i]
  }
}


# Transforms data frame from long to wide format
# timevar specifies variable that should be decomposed (RCP)
# idvar specifies variable that reshape function should not take into account (SPECIES)
# By not naming TOTAL_DIST_AREA and PA_OVERLAP_AREA, function knows that 
# they should be recombined

wide_df <- reshape(df, timevar = "RCP", idvar = "SPECIES", direction = "wide")

# head(wide_df)

# Get rid of PCT_CHANGE.0 column

wide_df$PCT_CHANGE.0 <- NULL

# add field denoting dispersal assumption
wide_df$DISPERSAL <- "0"

# bind zero and 100 percent disperal results
combined.wide.df <- rbind(wide_df, wide_df_FD)

# Write combined data frame to wide_df.csv
# write.csv(combined.wide.df, file = "wide_df_FDZD.csv")



# Add species attributes to results data
para <- read.csv("output/min5_US_para_species_data.csv")
para <- para[,!(names(para) %in% c("X"))]
res <- combined.wide.df
res <- res[,!(names(res) %in% c("X"))]

# generate col for avg percent change among all RCP levels
change.cols <- grep("CHANGE", names(res))
res$AVG_CHANGE <- rowMeans(res[, change.cols], na.rm = T)
res$AVG_CHANGE[res$AVG_CHANGE == -Inf] <- 0


res <- merge(res, para)
nrow(res)

head(res)



####  merge speceis lifecycle types to df

# load and tidy lifeycle data
lifecycles.df <- read.csv("data/us_endo_paras_lifecycle.csv")
lifecycles.df <- lifecycles.df[, c("SCI_NAME", "LIFECYCLE")]
names(lifecycles.df) <- c("SPECIES", "LIFE_CYCLE")
lifecycles.df$LIFE_CYCLE <- as.character(lifecycles.df$LIFE_CYCLE)
lifecycles.df$LIFE_CYCLE <- toupper(lifecycles.df$LIFE_CYCLE)
nons <- which(!(lifecycles.df$LIFE_CYCLE %in% c("DIRECT", "INDIRECT")))
lifecycles.df$LIFE_CYCLE[nons] <- NA

# merge step
res <- merge(res, lifecycles.df, all.x = T)


# merge host data
hostDF <- read.csv("output/GeneralismDF.csv")
n_hosts.df <- hostDF[,c("SPECIES", "N_HOSTS")]
res <- merge(res, n_hosts.df, all.x = T)


write.csv(res, "results/gap_analysis_full_results.csv")



###########################################################################
# edit data frame to have only 1 row per species

# First calculate column that takes PA_OVERLAP_AREA/TOTAL_DIST_AREA

# calculate the % total that falls under current protection and then for
# each climate scenario calculate % total that overlaps with park areas

# For each climate scenario create a column for percent change from 
# current scenario. Percent change = 1 - POcurrent/POscenario
###########################################################################

