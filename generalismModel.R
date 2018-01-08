# script to model host specificity and current range size of parasites

setwd("~/Dropbox/parks_for_parasites")


# NOTE: first time running, need to use the folllowing to install packages
# install.packages("R2admb")
# install.packages("glmmADMB", 
#                  repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                          getOption("repos")),
#                  type="source")

# Load packages
require(plyr)
require(dplyr)
require(glmmADMB)

# load parasite host association data
para.host.df <- read.csv("data/master_para_host_table.csv")

# subset for helminths
helminths <- c("CESTODA", "TREMATODA", "NEMATODA")
para.host.df <- para.host.df[para.host.df$CLADE %in% helminths,]

# susbet for vertebrate hosts
para.host.df <- para.host.df[para.host.df$VERT==1,]

# subset for non-fish hosts
para.host.df <- para.host.df[para.host.df$FISH == 0,]


 
# manually check paras with few hosts to make sure at least one is wild
few.hosts.df <- para.host.df[para.host.df$N_HOSTS < 5,]

# load in list of domesticated species and subset them out
domest.df <- read.csv("data/domestic_animals_list.csv", header = F)
names(domest.df) <- c("SPECIES")
domest.df$SPECIES <- as.character(domest.df$SPECIES)
para.host.df$HOST <- as.character(para.host.df$HOST)
para.host.df <- para.host.df[!(para.host.df$HOST %in% domest.df$SPECIES),]

# add counter column
para.host.df$COUNTER <- 1

# sum total number of wild hosts per parasite
para.host.counts <- ddply(para.host.df, .variables = c("PARASITE", "CLADE"),
                          summarise,
                          N_HOSTS = sum(COUNTER))

# merge host counts to para.host.df
para.host.df <- merge(para.host.df, para.host.counts)



# plot distribution of number of hosts per parasite
hist(para.host.counts$N_HOSTS)

# load parasite gap analysis data
para.ranges.full.df <- read.csv("output/wide_df.csv")

# generate col for avg percent change among all RCP levels
change.cols <- grep("CHANGE", names(para.ranges.full.df))
para.ranges.full.df$AVG_CHANGE<- rowMeans(para.ranges.full.df[, change.cols])

# subset for current range size per parasite
para.ranges.df <- para.ranges.full.df[, c("SPECIES", "TOTAL_DIST_AREA.0")]
names(para.ranges.df) <- c("PARASITE", "RANGE_KM2")

# merge ranges sizes with para host associations
para.df <- merge(para.host.counts, para.ranges.df)

# merge lifecycle data to para.df
lifecycles.df <- read.csv("data/us_endo_paras_lifecycle.csv")
lifecycles.df <- lifecycles.df[, c("SCI_NAME", "LIFECYCLE")]
names(lifecycles.df) <- c("PARASITE", "LIFE_CYCLE")
para.df <- merge(para.df, lifecycles.df)
para.df$LIFE_CYCLE <- toupper(as.character(para.df$LIFE_CYCLE))
nons <- which(!(para.df$LIFE_CYCLE %in% c("DIRECT", "INDIRECT")))
para.df$LIFE_CYCLE[nons] <- NA
para.df$LIFE_CYCLE <- as.factor(para.df$LIFE_CYCLE)


########## MODEL CURRENT RANGE SIZE ~  N_HOSTS ########## 

#  = glmmadmb(RANGE_KM2 ~ N_HOSTS,
            #  random = ~ 1|LIFE_CYCLE, family = "gamma",  data = para.df)

m2 = glm(RANGE_KM2 ~ N_HOSTS * LIFE_CYCLE, data = para.df, family = Gamma)


m3 = glm(RANGE_KM2 ~ N_HOSTS + LIFE_CYCLE, data = para.df, family = Gamma)

summary(m3)

m_3_anova <- aov(RANGE_KM2 ~ LIFE_CYCLE, data = para.df)
posthoc <- TukeyHSD(x = m_3_anova, "RANGE_KM2")

t.test(para.df$RANGE_KM2 ~ para.df$LIFE_CYCLE)


m3_1 = glm(RANGE_KM2 ~ LIFE_CYCLE, data = para.df, family = Gamma)

summary(m3_1)

# m3_1 = glmmadmb(RANGE_KM2 ~ N_HOSTS + LIFE_CYCLE, family = "gamma",  data = para.df)

m4 = glm(RANGE_KM2 ~ N_HOSTS, data = para.df, family = Gamma)

m5 = glm(RANGE_KM2 ~ LIFE_CYCLE, data = para.df, family = Gamma)



hist(para.df$RANGE_KM2)



########## PLOTS  ########## 


# make scatterplot of number of hosts vs range size for all clades
plot(para.df$N_HOSTS, para.df$RANGE_KM2,
     main = paste0("All Helminths: Host-Specificity vs. Current Range Size"),
     xlab = "Number of Vertebrate Hosts",
     ylab = "Current Range Size (km sq.)",
     col = para.df$LIFE_CYCLE)

model <- lm(para.df$RANGE_KM2 ~ para.df$N_HOSTS, col = para.df$LIFE_CYCLE)
abline(model, col = 'red')
# red = indirect
# black = direct

boxplot(para.df$RANGE_KM2, para.df$LIFE_CYCLE)

# from current to 2070?
p <- ggplot(aes(y = RANGE_KM2, x = LIFE_CYCLE), 
            data = para.df) + 
  geom_boxplot()


# from current to 2070?
p <- ggplot(aes(y = N_HOSTS, x = LIFE_CYCLE), 
            data = para.df) + 
  geom_boxplot()
p

# make plots for each helminth clade
for (i in helminths) {
  clade.df <- para.df[para.df$CLADE == i,]
  plot(clade.df$N_HOSTS, clade.df$RANGE_KM2, 
       main = paste0(i, " Host-Specificity vs. Current Range Size"),
       xlab = "Number of Vertebrate Hosts",
       ylab = "Current Range Size (km sq.)")
  model <- lm(clade.df$RANGE_KM2 ~ clade.df$N_HOSTS)
  abline(model, col = 'red')
}



# build models
model <- lm(para.df$RANGE_KM2 ~ para.df$N_HOSTS)
summary(model)




# model changes in para ranges as a function of N hosts
names(para.host.counts)[1] <- "SPECIES"
para.df <- merge(para.ranges.full.df, para.host.counts)

# Added 12/12/2017
write.csv(para.df, file = "GeneralismDF.csv")

plot(para.df$N_HOSTS, para.df$AVG_CHANGE, 
     xlab = "Number of Hosts",
     ylab = "Avg. % Change in Protected Range:Total Range",
     main = "Host-specificity vs. Change in % PA Overlap")
     
     
abline(lm(para.df$AVG_CHANGE ~ para.df$N_HOSTS), col = 'red')

mod <- lm(para.df$AVG_CHANGE ~ para.df$N_HOSTS)


plot(para.df$N_HOSTS, para.df$PCT_CHANGE.26,
     xlab = "Number of Hosts",
     ylab = "% Change in Protected Range:Total Range @ RCP 2.6",
     main = "Host-specificity vs. Change in % PA Overlap")
abline(lm(para.df$PCT_CHANGE.26 ~ para.df$N_HOSTS), col = 'red')

plot(para.df$N_HOSTS, para.df$PCT_CHANGE.45,
     xlab = "Number of Hosts",
     ylab = "% Change in Protected Range:Total Range @ RCP 4.5",
     main = "Host-specificity vs. Change in % PA Overlap")

abline(lm(para.df$PCT_CHANGE.45 ~ para.df$N_HOSTS), col = 'red')

plot(para.df$N_HOSTS, para.df$PCT_CHANGE.60,
     xlab = "Number of Hosts",
     ylab = "% Change in Protected Range:Total Range @ RCP 6.0",
     main = "Host-specificity vs. Change in % PA Overlap")
abline(lm(para.df$PCT_CHANGE.60 ~ para.df$N_HOSTS), col = 'red')

plot(para.df$N_HOSTS, para.df$PCT_CHANGE.85,
     xlab = "Number of Hosts",
     ylab = "% Change in Protected Range:Total Range @ RCP 8.5",
     main = "Host-specificity vs. Change in % PA Overlap")
abline(lm(para.df$PCT_CHANGE.85 ~ para.df$N_HOSTS), col = 'red')




