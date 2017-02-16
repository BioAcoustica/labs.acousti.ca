library(DrupalR) #Used to authenticate to bio.acousti.ca (replace with bioacousticaR when ready)
library(RCurl)
library(xtable)
library(VennDiagram)

#Authenticate to bio.acousti.ca, using credentials in authenticate.R  (not in version control)
source("authenticate.R")
c <- drupalr.authenticate("bio.acousti.ca", user, pass)

#Load and parse STK file - this could be broken out into another package? PErhaps with extra functions
stk_file <- readLines("data/stk.txt")

start <- NULL;
end <- length(stk_file);
stk_names = c();

for (i in 1:end) {
  if (stk_file[i] == "Taxa List:") {
    start <- i + 2;
  }
  if (!is.null(start) && i > start) {
    if (stk_file[i] == "----------------------") {
      end <- i;
    }
    if (i < end) {
      stk_names <- c(stk_names, gsub("_", " ", gsub("^\\s+|\\s+$", "", stk_file[i])));
    }
  }
}

#Get list of species and subspecies with recordings from bio.acousti.ca
#This relies on custom views for this project at bio.acousti.ca/aao
ba_names <- unique(read.csv(text = drupalr.get("http://bio.acousti.ca/", "aao/orthoptera", c)))
ba_names <- trimws(as.character(ba_names[,"Taxa"]))
ba_subspecies <- unique(read.csv(text = drupalr.get("http://bio.acousti.ca/", "aao/orthoptera/subspecies", c)))
ba_subspecies <- as.character(ba_subspecies[,"Taxa"])
ba_names <- c(ba_names, ba_subspecies)

#Get a list of species known to not stridulate from bio.acousti.ca
ba_silent <- read.csv(text = drupalr.get("http://bio.acousti.ca/", "aao/silent_species", c))

#Process Adam's traits CSV
#traits <- read.csv("data/Orthoptera database.csv");
traits <- read.csv("data/trait_species.csv", header = FALSE, col.names=c("SPECIES"));
traits_names <- as.character(traits[,"SPECIES"])

#Orthoptera Species File
#Hand processed reuslt of Taxon 'complex search' for taxa with recordings
osf_names <- read.csv("data/osf_sounds.csv", header=FALSE, col.names=c("SPECIES"));
osf_names <- gsub("\\s+", " ", as.character(osf_names[,"SPECIES"]));

#GBIF-ML
#Downloaded dataset doi:10.15468/dl.yagbjz
ml_names <- read.csv("data/0057836-160910150852091.csv", header=FALSE, col.names=c("SPECIES"));
ml_names <- unique(as.character(ml_names[,"SPECIES"]))

#Concatenate name strings
names <- unique(sort(c(ba_names, stk_names, traits_names, osf_names, ml_names)))

#VennDiagram of overlap
png("data_overlap_venn.png")
draw.triple.venn(
  length(ba_names),
  length(stk_names),
  length(traits_names),
  length(intersect(ba_names,stk_names)),
  length(intersect(stk_names,traits_names)),
  length(intersect(ba_names,traits_names)),
  length(intersect(ba_names, intersect(stk_names, traits_names))),
  category = c("BioAcoustica", "Supertree", "Traits"),
  main = "Data overlap in AAO",
  euler.d = TRUE,
  scaled = TRUE
);
dev.off()

#Compare overlap
ba <- stk <- trait <- osf <- ml <- c()
for (i in 1:length(names)) {
  if (names[i] %in% ba_names) {
    ba <- c(ba, "Yes")
  } else {
    silent <- ba_silent[ba_silent$Taxon == names[i],]
    ba <- c(ba, as.character(silent[1, "Value"]))
  }
  if (names[i] %in% stk_names) {
    stk <- c(stk, "Yes")
  } else {
    stk <- c(stk, "")
  }
  if (names[i] %in% traits_names) {
    trait <- c(trait, "Yes")
  } else {
    trait <- c(trait, "")
  }
  if (names[i] %in% osf_names) {
    osf <- c(osf, "Yes")
  } else {
    osf <- c(osf, "")
  }
  if (names[i] %in% ml_names) {
    ml <- c(ml, "Yes")
  } else {
    ml <- c(ml, "")
  }}
table <- data.frame(names, ba, stk, trait, osf, ml)
names(table) <- c("Species", "BioAcoustica", "Supertree", "Traits", "OSF Sounds", "Macaulay Library")

#Generate HTML table of data overlap
print(xtable(table), type="html", file="overlap.html")
