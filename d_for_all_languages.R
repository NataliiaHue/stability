library("ape")
library("caper")
library("phytools")
library("reshape")
library("TreePar")
library("dplyr")


FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)

add_outgroup_to_tree <- function(tree, tipname="ROOT"){
  age <- max(ape::branching.times(tree))
  tree <- TreePar::addroot(tree, age)
  phytools::bind.tip(tree, tipname, where=NULL)
}

setwd("/Users/neshcheret/Documents/GitHub/articles/stability")

# prepare the structural data
all_languages_data <- read.csv(
  "all_languages_data_categories_full_names.csv",
  sep = ";",
  strip.white = TRUE,
  na.strings = c("?", "-"),
  stringsAsFactors = FALSE
)
# remove unnecessary features
all_languages_data <- all_languages_data[! all_languages_data$ID %in% FEATURES_TO_IGNORE, ]

all_languages_data <- melt(all_languages_data, id=c("ID", "Feature", "PoS", "Function", "Level"))
all_languages_data$variable <- as.character(all_languages_data$variable)  # remove factorisation
features <- unique(all_languages_data$ID)

all_languages_data <- all_languages_data[,-c(3:5)]

# add fake root data -- this means we can get stability measures for features that
# are all 1
for (gbid in unique(all_languages_data$ID)) {
  all_languages_data <- rbind(all_languages_data, data.frame(
    ID=gbid, Feature="", variable='ROOT', value="0"))
}

# returns a vector of non-NA states for the given variable
get_states <- function(var, df) { unique(na.exclude(df[df$ID == var, 'value'])) }

# returns TRUE if the variable `var` in dataframe `df` is constant
is_constant <- function(var, df=df) { length(get_states(var, df)) <= 1 }

result_test <- sapply(features, is_constant, df=all_languages_data)

# a list of features that are constant, good to know
constant_features <- names(result_test[result_test])
# remove constants
for (cf in constant_features) {
  states <- get_states(cf, all_languages_data)
  if (length(states) == 1 & states == '0') {
    cat(sprintf("removing %s -- all zero\n", cf))
  } else {
    cat(sprintf("removing %s -- states:\n"))
    print(states)  # generate warning?
    cat("\n")
  }
}

all_languages_data <- all_languages_data[all_languages_data$ID %in% constant_features == FALSE, ]

# read in the phylogeny
trees <- read.nexus("beast/stability_covarion_relaxed.1000.trees")

# calculate d for each feature
cat("Tree\tFeature\tD\tPVal1\tPval0\n")
results <- data.frame(Tree=NULL, Feature=NULL, D=NULL, PVal1=NULL, PVal0=NULL)

for (gbid in unique(all_languages_data$ID)) {

  data <- all_languages_data[all_languages_data$ID == gbid,]
  data <- data[c("variable", "value")]

  for (treeid in 1:length(trees)) {

    tree <- add_outgroup_to_tree(trees[[treeid]], "ROOT")

    # add explicit warning checks to make debugging easier.
    not_in_tree <- setdiff(data$variable, tree$tip.label)
    for (tip in not_in_tree) {
      warning(sprintf('Language %s missing from tree', tip))
    }
    not_in_data <- setdiff(tree$tip.label, data$variable)
    for (tip in not_in_data) {
      warning(sprintf('Language %s missing from data', tip))
    }

    d <- phylo.d(data, tree, names.col=variable, binvar = value, permut = 1000)

    results <- rbind(results, data.frame(
      Tree=treeid, Feature=gbid, D=d$DEstimate, PVal1=d$Pval1, PVal0=d$Pval0
    ))

    cat(sprintf(
      "%d\t%s\t%0.5f\t%0.5f\t%0.5f\n",
      treeid, gbid, d$DEstimate, d$Pval1, d$Pval0
    ))
  }
}

write.csv(results, 'results.csv', quote=FALSE, row.names=FALSE)
