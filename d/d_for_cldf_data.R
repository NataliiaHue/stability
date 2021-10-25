#!/usr/bin/env Rscript
library("ape")
library("caper")
library("phytools")
library("reshape")
library("TreePar")
library("dplyr")
library("tidyverse")
library("readr")

VALUES_URL <- "https://raw.githubusercontent.com/cldf-datasets/hueblerstability/main/cldf/values.csv"

TREES_URL <- "https://raw.githubusercontent.com/NataliiaHue/stability/main/beast/stability_covarion_relaxed.1000.trees"

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



args <- commandArgs(trailingOnly=TRUE)
if ((length(args) == 0) || (any(grep("^(--help|-h)$", args))))
{
  cat("usage: ./asr-per-tree.r treeid", sep="\n")
  quit("no", 1)
}

treeid <- as.integer(args[[1]])


# Read in coded values
if (!file.exists('values.csv')) {
    download.file(VALUES_URL, 'values.csv')
}
values <- read_csv('values.csv')


# Merge the data
data <- values %>%
  select(Parameter_ID, Language_ID, Value) %>%
  rename(ID = Parameter_ID, variable = Language_ID, value = Value) %>%
  mutate(value = replace(value, which(value == "?"), NA))

data <- as.data.frame(data)

# remove unnecessary features
data <- data[! data$ID %in% FEATURES_TO_IGNORE, ]

features <- unique(data$ID)

# add fake root data -- this means we can get stability measures for features that
# are all 1
for (gbid in features) {
  data <- rbind(data, data.frame(ID=gbid, variable='ROOT', value="0"))
}

# returns a vector of non-NA states for the given variable
get_states <- function(var, df) { unique(na.exclude(df[df$ID == var, 'value'])) }

# returns TRUE if the variable `var` in dataframe `df` is constant
is_constant <- function(var, df=df) { length(get_states(var, df)) <= 1 }

result_test <- sapply(features, is_constant, df=data)

# a list of features that are constant, good to know
constant_features <- names(result_test[result_test])
# remove constants
for (cf in constant_features) {
  states <- get_states(cf, data)
  if (length(states) == 1 & states == '0') {
    cat(sprintf("removing %s -- all zero\n", cf))
  } else {
    cat(sprintf("removing %s -- states:\n"))
    print(states)  # generate warning?
    cat("\n")
  }
}

data <- data[data$ID %in% constant_features == FALSE, ]

# read in the phylogeny
if (!file.exists('stability_covarion_relaxed.1000.trees')) {
    download.file(TREES_URL, 'stability_covarion_relaxed.1000.trees')
}
trees <- read.nexus('stability_covarion_relaxed.1000.trees')


cat(paste("Working on tree", treeid, "\n"))
tree <- add_outgroup_to_tree(trees[[treeid]], "ROOT")
rm(trees)  # cleanup some RAM



# calculate d for each feature
cat("Tree\tFeature\tD\tPVal1\tPval0\n")
results <- data.frame(Tree=NULL, Feature=NULL, D=NULL, PVal1=NULL, PVal0=NULL)

for (gbid in unique(data$ID)) {

  d <- data[data$ID == gbid,]
  d <- d[c("variable", "value")]

  # add explicit warning checks to make debugging easier.
  not_in_tree <- setdiff(d$variable, d$tip.label)
  for (tip in not_in_tree) {
    warning(sprintf('Language %s missing from tree', tip))
  }
  not_in_data <- setdiff(tree$tip.label, d$variable)
  for (tip in not_in_data) {
    warning(sprintf('Language %s missing from data', tip))
  }

  pd <- phylo.d(d, tree, names.col=variable, binvar = value, permut = 1000)

  results <- rbind(results, data.frame(
    Tree=treeid, Feature=gbid, D=pd$DEstimate, PVal1=pd$Pval1, PVal0=pd$Pval0
  ))

  cat(sprintf(
    "%d\t%s\t%0.5f\t%0.5f\t%0.5f\n",
    treeid, gbid, pd$DEstimate, pd$Pval1, pd$Pval0
  ))
}

write.csv(results, sprintf('results.%s.csv', treeid), quote=FALSE, row.names=FALSE)
