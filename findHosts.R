
setwd("~/Dropbox/parks_for_parasites")

library(helminthR)
require(tikzDevice)
require(plyr)
require(rgbif)

paras <- read.csv("output/min5_US_para_species_data.csv")


paras <- paras[,c(2,3)]

names(paras) <- c("SCI_NAME", "CLADE")
paras$SCI_NAME <- as.character(paras$SCI_NAME)

paras$GENUS <- NA
paras$SPECIES <- NA

for (i in 1:nrow(paras)) {
  paras$GENUS[i] <- unlist(strsplit(paras$SCI_NAME[i], "_"))[1]
  paras$SPECIES[i] <- unlist(strsplit(paras$SCI_NAME[i], "_"))[2]
}

# paras$N_HOSTS <- NA
# for (i in 1:nrow(paras)) {
#   p.hosts.df <- findParasite(genus = paras$GENUS[i], 
#                              species = paras$SPECIES[i], 
#                              speciesOnly = T)
#   
#   n.hosts <- nrow(p.hosts.df)
#   paras$N_HOSTS[i] <- n.hosts
#   
#   if (i == 1) {
#     comb.host.df <- p.hosts.df
#   } else {
#     comb.host.df <- rbind(p.hosts.df, comb.host.df)
#   }
#   Sys.sleep(0.2)
# }
# # rename combined results from NHM queries
# nhm.df <- comb.host.df
# 
# # create new df with parasites that were not found in NHM database
# dd.paras <- paras[paras$N_HOSTS==0,]
# 
# # save nhm database results after first time looping through parasites to query
# write.csv(nhm.df, "nhm_contig_us_paras.csv")

# load nhm database results
nhm.df <- read.csv("nhm_contig_us_paras.csv")

# add uniform parasite species col
nhm.df$SCI_NAME <- gsub(" ", "_", toupper(nhm.df$Parasite))

# generate list of unique hosts for NHM search results
nhm.hosts <- unique(nhm.df$Host)





# # determing the mammalian hosts among NHM results
# nhm.df$GENUS <- NA
# nhm.genera <- c()
# for (i in 1:length(comb.host.df$Host)) {
#   gen <- unlist(strsplit(nhm.hosts[i], " "))[1]
#   nhm.df$GENUS[i] <- gen
#   nhm.genera <- c(nhm.genera, gen)
# }
# 
# print(nhm.genera)
# 
# nhm.genera <- unique(nhm.genera)
# 
# length(nhm.genera)
# 
# nhm.mams.df <- nhm.df[(nhm.df$GENUS %in% mam.genera),]
# 
# nhm.mams <- nhm.mams.df$Host
# 
# 
# helminths.few.hosts <- paras[paras$N_HOSTS>0 & paras$N_HOSTS <5, ]

# still need to find which of the parasites in paras are not in the UK databse

# we need to determine the breakdown of parasites that don't have host info
# int the database based on clade

# how to keep track of the parasites for each p

#
# 
# dd.paras.list <- dd.paras$SCI_NAME
# 
# 
# dd.paras
# 
# # Mammal taxonomy data
# mammals.df <- read.csv("data/msw3-all.csv")
# mam.genera <- as.character(mammals.df$Genus)

# load and organize GMPD data
gmpd <- read.csv("data/GMPD/GMPD_main.csv")
gmpd.paras <- gmpd$ParasiteCorrectedName
gmpd.paras <- na.omit(as.character(gmpd.paras))
gmpd.paras <- toupper(gmpd.paras)


res <- c()
gmpd$SCI_NAME <- NA
for (i in 1:length(gmpd.paras)) {
  if (!anyMultibyteUTF8Characters(gmpd.paras[i])) {
    add <- toupper(gmpd.paras[i])
    add <- gsub(" ", "_", add)
    gmpd$SCI_NAME[i] <- add
    res <- c(res, add)
  }
}

# get subset of gmpd results for colins paras
gmpd.df <- gmpd[(gmpd$SCI_NAME %in% paras$SCI_NAME),]

# get unique hosts of colins parasites in gmpd
gmpd.hosts <- unique(as.character(gmpd.colin.subset$HostCorrectedName))

gmpd.paras <-  unique(as.character(gmpd.colin.subset$ParasiteCorrectedName))
# 111 of colin's parasites were matched to hosts in GMPD database

gmpd.paras.upper <- gsub(" ", "_", toupper(gmpd.paras))

paras.not.in.gmpd <- paras[(paras$SCI_NAME %in% gmpd.paras.upper),]

nrow(paras.not.in.gmpd[paras.not.in.gmpd$N_HOSTS == 0,])



                         
nhm.matches <- nhm.df[, c("Host", "Parasite")]
names(nhm.matches) <- c("HOST", "PARASITE")                         
nhm.matches$NHM_COUNT = 1
nhm.matches$GMPD_COUNT = 0

                         
gmpd.matches <- gmpd.df[, c("HostCorrectedName", "ParasiteCorrectedName")]
names(gmpd.matches) <- c("HOST", "PARASITE")                         
gmpd.matches$GMPD_COUNT = 1
gmpd.matches$NHM_COUNT = 0


nhm.gmpd.comb <- rbind(gmpd.matches, nhm.matches)
nhm.gmpd.comb <- unique(nhm.gmpd.comb)


host.df <- data.frame(nhm.gmpd.comb$HOST)
names(host.df) <- "HOST"
host.df$VERT <- NA

host.df <- unique(host.df)

# iterate through hosts, determining which are vertebrates
for (i in 1:nrow(host.df)) {
  
  host <- as.character(host.df$HOST[i])
  # key <- name_suggest(q=host, rank='species')$key[1]
  # occ_search(taxonKey=key, limit=2)
  # 
  # # Return 20 results, this is the default by the way
  # occ_search(taxonKey=key, limit=20)
  
  # get query result
  try({
    res <- unlist(name_lookup(query=host, return="hierarchy")[1:3])
    
    if ("Chordata" %in% res) {
      host.df[i, "VERT"] <- 1
      print(paste0(host, ": vertebrate"))
    } else {
      host.df[i, "VERT"] <- 0
      print(paste0(host, ": invertebrate"))
      
    }
    
  })
}


# determine which species are fish
fish.classes <- c("Agnatha", "Cyclostomata", "Ostracodermi", "Chondrichthyes",
                  "Elasmobranchii", "Holocephali", "Placodermi", "Acanthodii",
                  "Osteichthyes", "Actinopterygii", "Sarcopterygii", "Myxini",
                  "Pteraspidomorphi", "Thelodonti", "Anaspida", "Petromyzontida",
                  "Hyperoartia", "Petromyzontidae", "Conodonta", 
                  "Cephalaspidomorphi",
                  "Galeaspida", "Pituriaspida", "Osteostraci", "Gnathostomata",
                  "Placodermi", "Chondrichthyes", "Acanthodii", "Osteichthyes",
                  "Actinopterygii", "Chondrostei", "Acipenseriformes", 
                  "Polypteriformes", "Neopterygii", "Holostei", "Teleostei",
                  "Sarcopterygii", "Actinistia", "Dipnoi")
                  
          
host.df$FISH <- NA

for (i in 1:nrow(host.df)) {
  
  host <- as.character(host.df$HOST[i])
  # key <- name_suggest(q=host, rank='species')$key[1]
  # occ_search(taxonKey=key, limit=2)
  # 
  # # Return 20 results, this is the default by the way
  # occ_search(taxonKey=key, limit=20)
  
  # get query result
  try({
    res <- unlist(name_lookup(query=host, return="hierarchy")[1:3])
    
    for (c in fish.classes) {
      if (c %in% res) {
        host.df[i, "FISH"] <- 1
        print(paste0(host, ": fish"))
      } else {
        host.df[i, "FISH"] <- 0
      }
    }
  })
}


manuals <- read.csv("host.df.csv")
manuals <- manuals[, c("HOST", "VERT", "FISH")]


host.df <- rbind(host.df, manuals)
host.df <- unique(host.df)

# save key of hosts verts and fish
write.csv(host.df, "data/host_vert_fish_key.csv")

# merge vert, fish data to master host df
nhm.gmpd.comb <- merge(nhm.gmpd.comb, host.df)

non.db <- read.csv("host.df.csv")

head(non.db)
non.db <- non.db[,1:6]

nhm.gmpd.comb <- rbind(nhm.gmpd.comb, non.db)
nhm.gmpd.comb$PARASITE <- toupper(nhm.gmpd.comb$PARASITE)
nhm.gmpd.comb$PARASITE <- gsub(" ", "_", nhm.gmpd.comb

# generate master parasite df
names(paras)[1] <- "PARASITE"
master.para.df <- merge(nhm.gmpd.comb, paras)


length(unique(master.para.df$PARASITE))


# check how many invert and vert hosts are in df
print(paste0("Number of invert hosts: ", sum(na.omit(host.df$VERT == 0))))
print(paste0("Number of vertebrate hosts: ", sum(na.omit(host.df$VERT == 1))))


    
    
  
  
  
  
# create df with counts of hosts for each para from each database
nhm.gmpd.comb.collapse <- ddply(nhm.gmpd.comb, .variables = "PARASITE",
                                summarise,
                                GMPD_COUNT = sum(GMPD_COUNT),
                                NHM_COUNT = sum(NHM_COUNT))



# read in US endo parasite lifecycles data
endo.lc.df <- read.csv("data/us_endo_paras_lifecycle.csv")
endo.lc.df$LIFECYCLE <- toupper(as.character(endo.lc.df$LIFECYCLE))
direct.endos.df <- endo.lc.df[endo.lc.df$LIFECYCLE == "DIRECT",]
direct.endos.ls <- direct.endos.df$SCI_NAME                         
                         
nhm.paras <- unique(nhm.df$SCI_NAME)
gmpd.paras <- unique(gmpd.df$SCI_NAME)

nhm.gmpd.paras <- unique(c(nhm.paras, gmpd.paras))

# determine which paras are not in NHM or GMPD
not.in.dbs <- paras$SCI_NAME[!(paras$SCI_NAME %in% nhm.gmpd.paras)]

write.csv(data.frame(not.in.dbs), "not_in_db.csv")


# determine which direct parasites are in either the NHM or GMPD dataset
# these are the paras we don't need to manually lookup definitive hosts for
direct.endos.in.nhm.gmpd <- direct.endos.ls[direct.endos.ls %in% 
                                              nhm.gmpd.paras]

exclude <- read.csv("data/temp.csv")
ex <- unique(as.character(exclude$SCI_NAME

paras.to.lookup <- paras[!(paras$SCI_NAME %in% direct.endos.in.nhm.gmpd)
                         & !(paras$SCI_NAME %in% ex),]
                         
write.csv(paras.to.lookup, "us_paras_to_lookup_hosts.csv")   



paras.to.lookup <- paras[(paras$SCI_NAME %in% not.in.dbs), ]

write.csv(paras.to.lookup, "us_paras_to_lookup_hosts.csv")                         

                         
                         
                         

