library(ape) # for reading in nexus files

setwd("/Users/neshcheret/Documents/GitHub/articles/stability")
data <- read.csv(
  "all_languages_data_categories_full_names.csv",
  sep = ";",
  strip.white = TRUE,
  na.strings = c("?", "-"),
  stringsAsFactors = FALSE
)

# save unnecessary features in a vector to further delete them from the data
FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)

data <- data[! data$ID %in% FEATURES_TO_IGNORE, ]
data <- data[ data$ID %in% stats$Feature, ] # from the script "explore_results.R"
rownames(data) <- data$ID
data <- data[,-c(1:5)]
data <- unlist(t(data))
data <- as.data.frame(data)
data_sorted <- data[mcct.tree$tip.label,]

setwd("/Users/neshcheret/Documents/GitHub/articles/stability/beast")

# read in the tree
mcct.tree<-read.nexus("stability_covarion_relaxed.mcct_renamed.trees")

data_feature <- function(feature_ID){
  data_frame <- cbind(data_sorted$V1, data_sorted[, colnames(data_sorted) == feature_ID])
  data_frame <- as.data.frame(data_frame)
}
get_feature <- data_feature("TE027")

setwd("/Users/neshcheret/Documents/GitHub/articles/stability")
pdf(file="asr-te027.pdf", width = 6, height = 8)
#nodelabels(node = c(100,62,78,77,89),col=pies, bg=get_feature$V1,pch=21,cex = 1)
co <- c("black", "white")

asr_matrix <- data.frame(
  p1 = as.numeric(info_feature[,22:26]),
  p0 = as.numeric(1-info_feature[,22:26]))

# find out, which nodes are language families
#plot(mcct.tree, type = "phylogram",tip.color = "black",font=1,label.offset = 0.05,cex = 0.5)
#nodelabels()
# 107 Mongolic, 106 Koreanic, 96 Tungusic, 84 Japonic, 64 Turkic

plot(mcct.tree, type = "phylogram",tip.color = "black",font=1,label.offset = 0.05,cex = 0.5)
tiplabels(col="black", bg=get_feature$V1,pch=21,cex = 1)
nodelabels(node = c(64,107,96,106,84),pie = unlist(asr_matrix), piecol = co, cex = 0.5)
nodelabels(node = c(64,107,96,106,84),text = c("Turkic", "Mongolic", "Tungusic","Koreanic","Japonic"),adj = c(1.4, 0), cex = .75, bg = "white")
dev.off()

