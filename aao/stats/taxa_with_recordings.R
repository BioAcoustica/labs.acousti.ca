library(DrupalR) #Used to authenticate to bio.acousti.ca (replace with bioacousticaR when ready)
library(RCurl)
library(xtable)
library(VennDiagram)

#Authenticate to bio.acousti.ca, using credentials in authenticate.R  (not in version control)
source("../authenticate.R")
c <- drupalr.authenticate("bio.acousti.ca", user, pass)



#Get list of species and subspecies with recordings from bio.acousti.ca
#This relies on custom views for this project at bio.acousti.ca/aao
ba_names <- unique(read.csv(text = drupalr.get("http://bio.acousti.ca/", "aao/orthoptera", c)))
ba_names <- trimws(as.character(ba_names[,"SupertreeTaxon"]))
ba_subspecies <- unique(read.csv(text = drupalr.get("http://bio.acousti.ca/", "aao/orthoptera/subspecies", c)))
ba_subspecies <- as.character(ba_subspecies[,"SupertreeTaxon"])
ba_names <- sort(c(ba_names, ba_subspecies))


write.table(ba_names, file ="taxa_with_recordings.csv", quote=FALSE, row.names=FALSE, col.names=FALSE)

#Get a list of species known to not stridulate from bio.acousti.ca
ba_silent <- read.csv(text = drupalr.get("http://bio.acousti.ca/", "aao/silent_species", c))

ba_names <- c(ba_names, as.character(ba_silent[,"Taxon"]))
ba_names <- gsub(' ', '_', ba_names)

write.table(sort(ba_names), file ="taxa_with_recordings_or_silent.csv", quote=FALSE, row.names=FALSE, col.names=FALSE)
