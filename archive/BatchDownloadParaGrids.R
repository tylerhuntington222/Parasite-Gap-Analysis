
require(rdrop2)

# authentication step - requires action in browser after executing line
drop_auth()

# load the contents of the folder with all parasite .gri/.grd files
file.dir <- drop_dir("/GapAnalysisSwarthmore/ActualMaps/")

scenarios <- c("current",
               "futureac45", "futureac85", 
               "futurebc26", "futurebc45", "futurebc60", "futurebc85", 
               "futurecc26", "futurecc45", "futurecc60", "futurecc85",  
               "futurehd26", "futurehd45", "futurehd60", "futurehd85", 
               "futurehe26", "futurehe45", "futurehe60", "futurehe85")

contig.us.paras <- readRDS("output/contig_US_parasites.RDS")
contig.us.paras <- as.character(contig.us.paras)

for (p in contig.us.paras) {
  for (s in scenarios) {
  
    # download .gri file
    path <- "/gapanalysisswarthmore/actualmaps/"
    query <- paste0(path, p, " ", s, ".gri") 
    drop_download(query, local_path = "data/PEARL/")
    
    # download .grd file
    path <- "/gapanalysisswarthmore/actualmaps/"
    query <- paste0(path, p, " ", s, ".grd") 
    drop_download(query, local_path = "data/PEARL/")
  }
}
  




