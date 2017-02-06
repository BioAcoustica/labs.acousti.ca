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

ba_names <- unique(read.csv("http://bio.acousti.ca/aao/orthoptera"))
test <- ba_names;
ba_names <- as.character(ba_names[,"Taxa"])


traits <- read.csv("data/Orthoptera database.csv");
traits_names <- as.character(traits[,"SPECIES"])

names <- unique(c(ba_names, stk_names, traits_names))

library(VennDiagram)

draw.triple.venn(
  length(ba_names),
  length(stk_names),
  length(traits_names),
  length(intersect(ba_names,stk_names)),
  length(intersect(stk_names,traits_names)),
  length(intersect(ba_names,traits_names)),
  length(intersect(ba_names, intersect(stk_names, traits_names))),
  category = c("BioAcoustica", "Supertree", "Traits"),
  main = "Data overlap in AAO"
);

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

outersect(ba_names, intersect(stk_names, traits_names)) -> demo