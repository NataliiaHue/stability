#!/usr/bin/env Rscript
library("ape")
library("phytools")
library("reshape")
library("corHMM")
library("TreePar")
library("tidyverse")

args <- commandArgs(trailingOnly=TRUE)
if ((length(args) == 0) || (any(grep("^(--help|-h)$", args))))
{ 
  cat("usage: ./asr-per-tree.r command", sep="\n")
  quit("no", 1)
}

treeid <- as.integer(args[[1]])



FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)


CLADES <- list(
  "Turkic" = c(
    'anat1259', 'bash1264', 'chag1247', 'chuv1255', 'crim1257', 'dolg1241', 'gaga1249', 'kara1467', 'kaza1248', 'khak1248', 'noga1249', 'nort2690', 'nort2697', 'oldu1238', 'shor1247', 'tata1255', 'turk1303', 'turk1304', 'tuvi1240', 'uigh1240', 'yaku1245'),
  
  "Tungusic" = c(
    'berj1234', 'huih1238', 'manc1252', 'moma1245', 'nana1257', 'negi1245', 'oroc1248', 'orok1265', 'sibe1251', 'udih1248', 'ulch1241'),
  
  "Mongolic" = c(
    'bona1250', 'daur1238', 'dong1285', 'east2337', 'halh1238', 'huzh1238', 'kham1281', 'midd1351', 'minh1238', 'mogh1245', 'oira1262', 'ordo1245', 'russ1264', 'torg1245'),
  
  "Japonic" = c(
    'cent2126', 'hate1238', 'ikem1235', 'ogam1239', 'nort2935', 'nucl1643', 'okin1246', 'oldj1239', 'shur1243', 'sout2954', 'tara1319', 'yona1241'),
  
  "Koreanic" = c('kore1280', 'midd1372')
)

add_outgroup_to_tree <- function(tree, tipname="ROOT"){
  age <- max(ape::branching.times(tree))
  tree <- TreePar::addroot(tree, age)
  phytools::bind.tip(tree, tipname, where=NULL)
}


# Read in coded values
values <- read_csv("cldf/values.csv")
# values <- read_csv("https://raw.githubusercontent.com/cldf-datasets/hueblerstability/main/cldf/values.csv")


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
for (gbid in unique(data$ID)) {
  data <- rbind(data, data.frame(
    ID=gbid,  variable='ROOT', value="0"))
}

# returns TRUE if the variable `var` in dataframe `df` is constant
is_constant <- function(var, df=df) {
  states <- unique(na.exclude(df[df$ID == var, 'value']))
  length(states) <= 1
}

result_test <- sapply(features, is_constant, df=data)

# a list of features that are constant, good to know
constant_features <- names(result_test[result_test])
# remove constants
data <- data[data$ID %in% constant_features == FALSE, ]

# replace <NA> with how RayDISC deals with NA
data[is.na(data$value),]$value <- '-'



# read in the phylogeny

trees <- read.nexus("beast/stability_covarion_relaxed.1000.trees")
# trees <- read.nexus("https://raw.githubusercontent.com/NataliiaHue/stability/main/beast/stability_covarion_relaxed.1000.trees")

cat(paste("Working on tree", treeid, "\n"))
tree <- add_outgroup_to_tree(trees[[treeid]], "ROOT")
rm(trees)  # cleanup some RAM

# set up storage
states <- NULL
rates <- NULL

for (gbid in unique(data$ID)) {
  data <- data[data$ID == gbid,]
  data <- data[c("variable", "value")]
  
  # add explicit warning checks to make debugging easier.
  not_in_tree <- setdiff(data$variable, tree$tip.label)
  for (tip in not_in_tree) {
    warning(sprintf('Language %s missing from tree', tip))
  }
  not_in_data <- setdiff(tree$tip.label, data$variable)
  for (tip in not_in_data) {
    warning(sprintf('Language %s missing from data', tip))
  }
  
  # go through two models for discrete characters:
  # ER  = EQual Rates (same rate of gain and loss: 0 <-> 1)
  # ARD = All Rates Different (rate of gains 0->1 different to losses 1->0)
  for (model in c("ER", "ARD")) {
    cat(sprintf("Fitting %s onto tree %d using %s", gbid, treeid, model), sep="\n")
    r <- corHMM::rayDISC(
      tree, data, ntraits=1, charnum=1, model=model,
      # correct for ascertainment bias
      lewis.asc.bias = TRUE,
      # calculate marginal probabilities.
      node.states = "marginal",  # joint, marginal, scaled
      # set prior weight on root to follow maddison fitz.
      # root.p = NULL  # (DEFAULT) assumes equal weighting among all possible root states
      # follows Maddison et al. (2007) and FitzJohn et al. (2009).
      root.p = "maddfitz"
    )
    
    # rates
    rates <- rbind(rates, data.frame(
      Tree = treeid,
      Model = model,
      Feature = gbid,
      LogLikelihood = r$loglik,
      AICc = r$AICc,
      q01 = r$solution[1,2],   #qij
      q10 = r$solution[2,1]
    ))
    
    # reconstructions
    for (clade in names(CLADES)) {
      n <- getMRCA(tree, CLADES[[clade]]) - Ntip(tree)
      states <- rbind(states, data.frame(
        Tree = treeid,
        Model = model,
        Feature = gbid,
        LogLikelihood = r$loglik,
        AICc = r$AICc,
        Clade = clade,
        p0 = r$states[n, '0'],  # = Prob that node has this trait.
        p1 = r$states[n, '1']
      ))
    }
  }  # model
}
write.csv(rates, sprintf('asr_results/asr_rates.%s.csv', treeid), quote=FALSE, row.names=FALSE)
write.csv(states, sprintf('asr_results/asr_states.%s.csv', treeid), quote=FALSE, row.names=FALSE)
