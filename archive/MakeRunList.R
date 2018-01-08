# MakeRunList.R

# A script to determine which parasite-scenario combos still need to be run

setwd("~/Dropbox/parks_for_parasites/")

# init scenarios vector
scenarios <- c("current",
               "futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")

# load list of parasites in contiguouos US
contig.us.paras <- readRDS("output/contig_US_parasites.RDS")
contig.us.paras <- as.character(contig.us.paras)


# generate list of all parasite combo runs

all.runs <- c()

for (p in contig.us.paras) {
  for (s in scenarios) {
    para <- gsub(" ", "_", p)
    this.run <- paste0(para, "_", s, "_", "overlap.RDS")
    all.runs <- c(all.runs, this.run)
  }
}

# get list of done runs
done.runs <- list.files("output/overlap_pts/")

# determine which runs still need to be done
todo.runs <- all.runs[!(all.runs %in% done.runs)]

# export run list
saveRDS(todo.runs, "output/todo_run_list.RDS")







    
